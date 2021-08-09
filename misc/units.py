#!/usr/bin/env python3

#
# units.py - Units test harness for ctags
#
# Copyright (C) 2019 Ken Takata
# (Based on "units" written by Masatake YAMATO.)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# Python 3.5 or later is required.
# On Windows, unix-like shell (e.g. bash) and some unix tools (sed,
# diff, etc.) are needed.
#

import time     # for debugging
import argparse
import filecmp
import glob
import io
import os
import platform
import queue
import re
import shutil
import stat
import subprocess
import sys
import threading

#
# Global Parameters
#
SHELL = '/bin/sh'
CTAGS = './ctags'
READTAGS = './readtags'
OPTSCRIPT = './optscript'
WITH_TIMEOUT = 0
WITH_VALGRIND = False
COLORIZED_OUTPUT = True
CATEGORIES = []
UNITS = []
LANGUAGES = []
PRETENSE_OPTS = ''
RUN_SHRINK = False
SHOW_DIFF_OUTPUT = False
NUM_WORKER_THREADS = 4
DIFF_U_NUM = 0

#
# Internal variables and constants
#
_FEATURE_LIST = []
_PREPERE_ENV = ''
_DEFAULT_CATEGORY = 'ROOT'
_TIMEOUT_EXIT = 124
_VG_TIMEOUT_FACTOR = 10
_VALGRIND_EXIT = 58
_STDERR_OUTPUT_NAME = 'STDERR.tmp'
_DIFF_OUTPUT_NAME = 'DIFF.tmp'
_VALGRIND_OUTPUT_NAME = 'VALGRIND.tmp'

#
# Results
#
L_PASSED = []
L_FIXED = []
L_FAILED_BY_STATUS = []
L_FAILED_BY_DIFF = []
L_SKIPPED_BY_FEATURES = []
L_SKIPPED_BY_LANGUAGES = []
L_SKIPPED_BY_ILOOP = []
L_KNOWN_BUGS = []
L_FAILED_BY_TIMEED_OUT = []
L_BROKEN_ARGS_CTAGS = []
L_VALGRIND = []
TMAIN_STATUS = True
TMAIN_FAILED = []

def remove_prefix(string, prefix):
    if string.startswith(prefix):
        return string[len(prefix):]
    else:
        return string

def is_cygwin():
    system = platform.system()
    return system.startswith('CYGWIN_NT') or system.startswith('MINGW32_NT')

def isabs(path):
    if is_cygwin():
        import ntpath
        if ntpath.isabs(path):
            return True
    return os.path.isabs(path)

def action_help(parser, action, *args):
    parser.print_help()
    return 0

def error_exit(status, msg):
    print(msg, file=sys.stderr)
    sys.exit(status)

def line(*args, file=sys.stdout):
    if len(args) > 0:
        ch = args[0]
    else:
        ch = '-'
    print(ch * 60, file=file)

def remove_readonly(func, path, _):
    # Clear the readonly bit and reattempt the removal
    os.chmod(path, stat.S_IWRITE | stat.S_IREAD)
    dname = os.path.dirname(path)
    os.chmod(dname, os.stat(dname).st_mode | stat.S_IWRITE)
    func(path)

def clean_bundles(bundles):
    if not os.path.isfile(bundles):
        return
    with open(bundles, 'r') as f:
        for fn in f.read().splitlines():
            if os.path.isdir(fn):
                shutil.rmtree(fn, onerror=remove_readonly)
            elif os.path.isfile(fn):
                os.remove(fn)
    os.remove(bundles)

def clean_tcase(d, bundles):
    if os.path.isdir(d):
        clean_bundles(bundles)
        for fn in glob.glob(d + '/*.tmp'):
            os.remove(fn)
        for fn in glob.glob(d + '/*.TMP'):
            os.remove(fn)

def check_availability(cmd):
    if not shutil.which(cmd):
        error_exit(1, cmd + ' command is not available')

def check_units(name, category):
    if len(UNITS) == 0:
        return True

    for u in UNITS:
        ret = re.match(r'(.+)/(.+)', u)
        if ret:
            if ret.group(1, 2) == (category, name):
                return True
        elif u == name:
            return True
    return False

def init_features():
    global _FEATURE_LIST
    ret = subprocess.run([CTAGS, '--quiet', '--options=NONE', '--list-features', '--with-list=no'],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    _FEATURE_LIST = re.sub(r'(?m)^([^ ]+).*$', r'\1',
            ret.stdout.decode('utf-8')).splitlines()

def check_features(feature, ffile):
    features = []
    if feature:
        features = [feature]
    elif os.path.isfile(ffile):
        with open(ffile, 'r') as f:
            features = f.read().splitlines()

    for expected in features:
        if expected == '':
            continue
        found = False
        found_unexpectedly = False
        if expected[0] == '!':
            if expected[1:] in _FEATURE_LIST:
                found_unexpectedly = True
        else:
            if expected in _FEATURE_LIST:
                found = True
        if found_unexpectedly:
            return (False, expected)
        elif not found:
            return (False, expected)
    return (True, '')

def check_languages(cmdline, lfile):
    if not os.path.isfile(lfile):
        return (True, '')

    ret = subprocess.run(cmdline + ['--list-languages'],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    langs = ret.stdout.decode('utf-8').splitlines()

    with open(lfile, 'r') as f:
        for expected in f.read().splitlines():
            found = False
            if expected in langs:
                found = True
            if not found:
                return (False, expected)
    return (True, '')

def decorate(decorator, msg, colorized):
    if decorator == 'red':
        num = '31'
    elif decorator == 'green':
        num = '32'
    elif decorator == 'yellow':
        num = '33'
    else:
        error_exit(1, 'INTERNAL ERROR: wrong run_result function')

    if colorized:
        return "\x1b[" + num + 'm' + msg + "\x1b[m"
    else:
        return msg

def run_result(result_type, msg, output, *args, file=sys.stdout):
    func_dict = {
            'skip': run_result_skip,
            'error': run_result_error,
            'ok': run_result_ok,
            'known_error': run_result_known_error,
            }

    func_dict[result_type](msg, file, COLORIZED_OUTPUT, *args)
    file.flush()
    if output:
        with open(output, 'w') as f:
            func_dict[result_type](msg, f, False, *args)

def run_result_skip(msg, f, colorized, *args):
    s = msg + decorate('yellow', 'skipped', colorized)
    if len(args) > 0:
        s += ' (' + args[0] + ')'
    print(s, file=f)

def run_result_error(msg, f, colorized, *args):
    s = msg + decorate('red', 'failed', colorized)
    if len(args) > 0:
        s += ' (' + args[0] + ')'
    print(s, file=f)

def run_result_ok(msg, f, colorized, *args):
    s = msg + decorate('green', 'passed', colorized)
    if len(args) > 0:
        s += ' (' + args[0] + ')'
    print(s, file=f)

def run_result_known_error(msg, f, colorized, *args):
    s = msg + decorate('yellow', 'failed', colorized) + ' (KNOWN bug)'
    print(s, file=f)

def run_shrink(cmdline_template, finput, foutput, lang):
    script = sys.argv[0]
    script = os.path.splitext(script)[0]   # remove '.py'

    print('Shrinking ' + finput + ' as ' + lang)
    # fallback to the shell script version
    subprocess.run([SHELL, script, 'shrink',
        '--timeout=1', '--foreground',
        cmdline_template, finput, foutput])

# return a filter for normalizing the basename
#
# If internal is True, return a pair of [pattern, replacement],
# otherwise return a list of command line arguments.
def basename_filter(internal, output_type):
    filters_external = {
            'ctags': 's%\(^[^\t]\{1,\}\t\)\(/\{0,1\}\([^/\t]\{1,\}/\)*\)%\\1%',
            # "input" in the expresion is for finding input file names in the TAGS file.
            # RAWOUT.tmp:
            #
            #   ./Units/parser-ada.r/ada-etags-suffix.d/input_0.adb,238
            #   package body Input_0 is   ^?Input_0/b^A1,0
            #
            # With the original expression, both "./Units/parser-ada.r/ada-etags-suffix.d/"
            # and "package body Input_0 is   Input_0/' are deleted.
            # FILTERED.tmp:
            #
            # input_0.adb,238
            # b^A1,0
            #
            # Adding "input" ot the expression is for deleting only the former one and for
            # skpping the later one.
            #
            # FIXME: if "input" is included as a substring of tag entry names, filtering
            # with this expression makes the test fail.
            'etags': 's%.*\/\(input[-._][[:print:]]\{1,\}\),\([0-9]\{1,\}$\)%\\1,\\2%',
            'xref': 's%\(.*[[:digit:]]\{1,\} \)\([^ ]\{1,\}[^ ]\{1,\}\)/\([^ ].\{1,\}.\{1,\}$\)%\\1\\3%',
            'json': 's%\("path": \)"[^"]\{1,\}/\([^/"]\{1,\}\)"%\\1"\\2"%',
            }
    filters_internal = {
            'ctags': [r'(^[^\t]+\t)(/?([^/\t]+/)*)', r'\1'],
            # See above comments about "input".
            'etags': [r'.*/(input[-._]\S+),([0-9]+$)', r'\1,\2'],
            'xref': [r'(.*\d+ )([^ ]+[^ ]+)/([^ ].+.+$)', r'\1\3'],
            'json': [r'("path": )"[^"]+/([^/"]+)"', r'\1"\2"'],
            }
    if internal:
        return filters_internal[output_type]
    else:
        return ['sed', '-e', filters_external[output_type]]

# convert a command line list to a command line string
def join_cmdline(cmdline):
    # surround with '' if an argument includes spaces or '\'
    # TODO: use more robust way
    return ' '.join("'" + x + "'" if (' ' in x) or ('\\' in x) else x
        for x in cmdline)

def run_record_cmdline(cmdline, ffilter, ocmdline, output_type):
    with open(ocmdline, 'w') as f:
        print("%s\n%s \\\n| %s \\\n| %s\n" % (
            _PREPERE_ENV,
            join_cmdline(cmdline),
            join_cmdline(basename_filter(False, output_type)),
            ffilter), file=f)

def prepare_bundles(frm, to, obundles):
    for src in glob.glob(frm + '/*'):
        fn = os.path.basename(src)
        if fn.startswith('input.'):
            continue
        elif fn.startswith('expected.tags'):
            continue
        elif fn.startswith('README'):
            continue
        elif fn in ['features', 'languages', 'filters']:
            continue
        elif fn == 'args.ctags':
            continue
        else:
            dist = to + '/' + fn
            if os.path.isdir(src):
                shutil.copytree(src, dist, copy_function=shutil.copyfile)
            else:
                shutil.copyfile(src, dist)
            with open(obundles, 'a') as f:
                print(dist, file=f)

def anon_normalize_sub(internal, ctags, input_actual, *args):
    # TODO: "Units" should not be hardcoded.
    input_expected = './Units' + re.sub(r'^.*?/Units', r'', input_actual, 1)

    ret = subprocess.run([CTAGS, '--quiet', '--options=NONE', '--_anonhash=' + input_actual],
            stdout=subprocess.PIPE)
    actual = ret.stdout.decode('utf-8').splitlines()[0]
    ret = subprocess.run([CTAGS, '--quiet', '--options=NONE', '--_anonhash=' + input_expected],
            stdout=subprocess.PIPE)
    expected = ret.stdout.decode('utf-8').splitlines()[0]

    if internal:
        retlist = [[actual, expected]]
    else:
        retlist = ['-e', 's/' + actual + '/' + expected + '/g']
    if len(args) > 0:
        return retlist + anon_normalize_sub(internal, ctags, *args)
    else:
        return retlist

def is_anon_normalize_needed(rawout):
    with open(rawout, 'r', errors='ignore') as f:
        if re.search(r'[0-9a-f]{8}', f.read()):
            return True
    return False

# return a list of filters for normalizing anonhash
#
# If internal is True, return a list of pairs of [pattern, replacement],
# otherwise return a list of command line arguments.
def anon_normalize(internal, rawout, ctags, input_actual, *args):
    if is_anon_normalize_needed(rawout):
        return anon_normalize_sub(internal, ctags, input_actual, *args)
    else:
        return []

def run_filter(finput, foutput, base_filter, anon_filters):
    pat1 = [re.compile(base_filter[0]), base_filter[1]]
    pat2 = [(re.compile(p[0]), p[1]) for p in anon_filters]
    with open(finput, 'r', errors='surrogateescape') as fin, \
            open(foutput, 'w', errors='surrogateescape', newline='\n') as fout:
        for l in fin:
            l = pat1[0].sub(pat1[1], l, 1)
            for p in pat2:
                l = p[0].sub(p[1], l)
            print(l, end='', file=fout)

def guess_lang(cmdline, finput):
    ret = subprocess.run(cmdline + ['--print-language', finput],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    return re.sub(r'^.*: ', r'',
            ret.stdout.decode('utf-8').replace("\r\n", "\n").replace("\n", ''))

def guess_lang_from_log(log):
    with open(log, 'r', encoding='utf-8', errors='ignore') as f:
        for l in f:
            ret = re.match('OPENING.* as (.*) language .*file ', l)
            if ret:
                return ret.group(1)
    return ''

def run_tcase(finput, t, name, tclass, category, build_t, extra_inputs):
    global L_PASSED
    global L_FIXED
    global L_FAILED_BY_STATUS
    global L_FAILED_BY_DIFF
    global L_SKIPPED_BY_FEATURES
    global L_SKIPPED_BY_LANGUAGES
    global L_SKIPPED_BY_ILOOP
    global L_KNOWN_BUGS
    global L_FAILED_BY_TIMEED_OUT
    global L_BROKEN_ARGS_CTAGS
    global L_VALGRIND

    o = build_t

    fargs = t + '/args.ctags'
    ffeatures = t + '/features'
    flanguages = t + '/languages'
    ffilter = t + '/filter'

    fexpected = t + '/expected.tags'
    output_type = 'ctags'
    output_label = ''
    output_tflag = []
    output_feature = ''
    output_lang_extras = ''

    if os.path.isfile(fexpected):
        pass
    elif os.path.isfile(t + '/expected.tags-e'):
        fexpected = t + '/expected.tags-e'
        output_type = 'etags'
        output_label = '/' + output_type
        output_tflag = ['-e', '--tag-relative=no']
    elif os.path.isfile(t + '/expected.tags-x'):
        fexpected = t + '/expected.tags-x'
        output_type = 'xref'
        output_label = '/' + output_type
        output_tflag = ['-x']
    elif os.path.isfile(t + '/expected.tags-json'):
        fexpected = t + '/expected.tags-json'
        output_type = 'json'
        output_label = '/' + output_type
        output_tflag = ['--output-format=json']
        output_feature = 'json'

    if len(extra_inputs) > 0:
        output_lang_extras = ' (multi inputs)'

    if not shutil.which(ffilter):
        ffilter = 'cat'

    ostderr = o + '/' + _STDERR_OUTPUT_NAME
    orawout = o + '/RAWOUT.tmp'
    ofiltered = o + '/FILTERED.tmp'
    odiff = o + '/' + _DIFF_OUTPUT_NAME
    ocmdline = o + '/CMDLINE.tmp'
    ovalgrind = o + '/' + _VALGRIND_OUTPUT_NAME
    oresult = o + '/RESULT.tmp'
    oshrink_template = o + '/SHRINK-%s.tmp'
    obundles = o + '/BUNDLES'

    broken_args_ctags = False

    #
    # Filtered by UNIT
    #
    if not check_units(name, category):
        return False

    #
    # Build cmdline
    #
    cmdline = [CTAGS, '--verbose', '--options=NONE', '--fields=-T']
    if PRETENSE_OPTS != '':
        cmdline += [PRETENSE_OPTS]
    cmdline += ['--optlib-dir=+' + t + '/optlib', '-o', '-']
    if os.path.isfile(fargs):
        cmdline += ['--options=' + fargs]
        ret = subprocess.run(cmdline + ['--_force-quit=0'],
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if ret.returncode != 0:
            broken_args_ctags = True

    #
    # make a backup (basedcmdline) of cmdline.  basedcmdline is used
    # as a command line template for running shrinker.  basedcmdline
    # should not include the name of the input file name.  The
    # shrinker makes a another cmdline by applying a real input file
    # name to the template.  On the other hand, cmdline is
    # destructively updated by appending input file name in this
    # function. The file name should not be included in the cmdline
    # template.
    #
    # To avoid the updating in this function propagating to
    # basecmdline, we copy the cmdline here.
    #
    basecmdline = cmdline[:]

    #
    # Filtered by LANGUAGES
    #
    guessed_lang = None
    if len(LANGUAGES) > 0:
        guessed_lang = guess_lang(basecmdline, finput)
        if not guessed_lang in LANGUAGES:
            return False

    clean_tcase(o, obundles)
    os.makedirs(o, exist_ok=True)
    if not os.path.samefile(o, t):
        prepare_bundles(t, o, obundles)


    # helper function for building some strings based on guessed_lang
    def build_strings(guessed_lang):
        return ('%-59s ' % ('Testing ' + name + ' as ' + guessed_lang + output_lang_extras + output_label),
                join_cmdline(basecmdline) + ' --language-force=' + guessed_lang + ' %s > /dev/null 2>&1',
                oshrink_template % (guessed_lang.replace('/', '-')))

    (tmp, feat) = check_features(output_feature, ffeatures)
    if not tmp:
        if not guessed_lang:
            guessed_lang = guess_lang(basecmdline, finput)
        msg = build_strings(guessed_lang)[0]
        L_SKIPPED_BY_FEATURES += [category + '/' + name]
        if feat.startswith('!'):
            run_result('skip', msg, oresult, 'unwanted feature "' + feat[1:] + '" is available')
        else:
            run_result('skip', msg, oresult, 'required feature "' + feat + '" is not available')
        return False
    (tmp, lang) = check_languages(basecmdline, flanguages)
    if not tmp:
        if not guessed_lang:
            guessed_lang = guess_lang(basecmdline, finput)
        msg = build_strings(guessed_lang)[0]
        L_SKIPPED_BY_LANGUAGES += [category + '/' + name]
        run_result('skip', msg, oresult, 'required language parser "' + lang + '" is not available')
        return False
    if WITH_TIMEOUT == 0 and tclass == 'i':
        if not guessed_lang:
            guessed_lang = guess_lang(basecmdline, finput)
        msg = build_strings(guessed_lang)[0]
        L_SKIPPED_BY_ILOOP += [category + '/' + name]
        run_result('skip', msg, oresult, 'may cause an infinite loop')
        return False
    if broken_args_ctags:
        if not guessed_lang:
            guessed_lang = guess_lang(basecmdline, finput)
        msg = build_strings(guessed_lang)[0]
        L_BROKEN_ARGS_CTAGS += [category + '/' + name]
        run_result('error', msg, None, 'broken args.ctags?')
        return False

    cmdline += output_tflag + [finput]
    if len(extra_inputs) > 0:
        cmdline += extra_inputs

    timeout_value = WITH_TIMEOUT
    if WITH_VALGRIND:
        cmdline = ['valgrind', '--leak-check=full', '--track-origins=yes',
                   '--error-exitcode=' + str(_VALGRIND_EXIT), '--log-file=' + ovalgrind] + cmdline
        timeout_value *= _VG_TIMEOUT_FACTOR
    if timeout_value == 0:
        timeout_value = None

    start = time.time()
    try:
        with open(orawout, 'wb') as fo, \
                open(ostderr, 'wb') as fe:
            ret = subprocess.run(cmdline, stdout=fo, stderr=fe,
                    timeout=timeout_value)
        run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
    except subprocess.TimeoutExpired:
        if not guessed_lang:
            guessed_lang = guess_lang(basecmdline, finput)
        (msg, cmdline_template, oshrink) = build_strings(guessed_lang)
        L_FAILED_BY_TIMEED_OUT += [category + '/' + name]
        run_result('error', msg, oresult, 'TIMED OUT')
        run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
        if RUN_SHRINK and len(extra_inputs) == 0:
            run_shrink(cmdline_template, finput, oshrink, guessed_lang)
        return False
    #print('execute time: %f' % (time.time() - start))

    guessed_lang = guess_lang_from_log(ostderr)
    (msg, cmdline_template, oshrink) = build_strings(guessed_lang)

    if ret.returncode != 0:
        if WITH_VALGRIND and ret.returncode == _VALGRIND_EXIT and \
                tclass != 'v':
            L_VALGRIND += [category + '/' + name]
            run_result('error', msg, oresult, 'valgrind-error')
            run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
            return False
        elif tclass == 'b':
            L_KNOWN_BUGS += [category + '/' + name]
            run_result('known_error', msg, oresult)
            run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
            if RUN_SHRINK and len(extra_inputs) == 0:
                run_shrink(cmdline_template, finput, oshrink, guessed_lang)
            return True
        else:
            L_FAILED_BY_STATUS += [category + '/' + name]
            run_result('error', msg, oresult, 'unexpected exit status: ' + str(ret.returncode))
            run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
            if RUN_SHRINK and len(extra_inputs) == 0:
                run_shrink(cmdline_template, finput, oshrink, guessed_lang)
            return False
    elif WITH_VALGRIND and tclass == 'v':
        L_FIXED += [category + '/' + name]

    if not os.path.isfile(fexpected):
        clean_tcase(o, obundles)
        if tclass == 'b':
            L_FIXED += [category + '/' + name]
        elif tclass == 'i':
            L_FIXED += [category + '/' + name]

        L_PASSED += [category + '/' + name]
        run_result('ok', msg, None, '"expected.tags*" not found')
        return True

    start = time.time()
    if ffilter != 'cat':
        # Use external filter
        filter_cmd = basename_filter(False, output_type) + \
                anon_normalize(False, orawout, CTAGS, finput, *extra_inputs) + \
                ['<', orawout]
        filter_cmd += ['|', ffilter]
        filter_cmd += ['>', ofiltered]
        #print(filter_cmd)
        subprocess.run([SHELL, '-c', join_cmdline(filter_cmd)])
    else:
        # Use internal filter
        run_filter(orawout, ofiltered, basename_filter(True, output_type),
                anon_normalize(True, orawout, CTAGS, finput, *extra_inputs))
    #print('filter time: %f' % (time.time() - start))

    start = time.time()
    if filecmp.cmp(fexpected, ofiltered):
        ret.returncode = 0
    else:
        with open(odiff, 'wb') as f:
            ret = subprocess.run(['diff', '-U', str(DIFF_U_NUM),
                '-I', '^!_TAG', '--strip-trailing-cr', fexpected, ofiltered],
                stdout=f)
    #print('diff time: %f' % (time.time() - start))

    if ret.returncode == 0:
        clean_tcase(o, obundles)
        if tclass == 'b':
            L_FIXED += [category + '/' + name]
        elif WITH_TIMEOUT != 0 and tclass == 'i':
            L_FIXED += [category + '/' + name]

        L_PASSED += [category + '/' + name]
        run_result('ok', msg, None)
        return True
    else:
        if tclass == 'b':
            L_KNOWN_BUGS += [category + '/' + name]
            run_result('known_error', msg, oresult)
            run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
            return True
        else:
            L_FAILED_BY_DIFF += [category + '/' + name]
            run_result('error', msg, oresult, 'unexpected output')
            run_record_cmdline(cmdline, ffilter, ocmdline, output_type)
            return False

def create_thread_queue(func):
    q = queue.Queue()
    threads = []
    for i in range(NUM_WORKER_THREADS):
        t = threading.Thread(target=worker, args=(func, q), daemon=True)
        t.start()
        threads.append(t)
    return (q, threads)

def worker(func, q):
    while True:
        item = q.get()
        if item is None:
            break
        try:
            func(*item)
        except:
            import traceback
            traceback.print_exc()
        q.task_done()

def join_workers(q, threads):
    # block until all tasks are done
    try:
        q.join()
    except KeyboardInterrupt:
        # empty the queue
        while True:
            try:
                q.get_nowait()
            except queue.Empty:
                break
        # try to stop workers
        for i in range(NUM_WORKER_THREADS):
            q.put(None)
        for t in threads:
            t.join(timeout=2)
        # exit regardless that workers are stopped
        sys.exit(1)

    # stop workers
    for i in range(NUM_WORKER_THREADS):
        q.put(None)
    for t in threads:
        t.join()

def accepted_file(fname):
    # Ignore backup files
    return not fname.endswith('~')

def run_dir(category, base_dir, build_base_dir):
    #
    # Filtered by CATEGORIES
    #
    if len(CATEGORIES) > 0 and not category in CATEGORIES:
        return False

    print("\nCategory: " + category)
    line()

    (q, threads) = create_thread_queue(run_tcase)

    for finput in glob.glob(base_dir + '/*.[dbtiv]/input.*'):
        finput = finput.replace('\\', '/')  # for Windows
        if not accepted_file(finput):
            continue

        dname = os.path.dirname(finput)
        extra_inputs = sorted(map(lambda x: x.replace('\\', '/'), # for Windows
            filter(accepted_file,
                glob.glob(dname + '/input[-_][0-9].*') +
                glob.glob(dname + '/input[-_][0-9][-_]*.*')
            )))

        tcase_dir = dname
        build_tcase_dir = build_base_dir + remove_prefix(tcase_dir, base_dir)
        ret = re.match(r'^.*/(.*)\.([dbtiv])$', tcase_dir)
        (name, tclass) = ret.group(1, 2)
        q.put((finput, tcase_dir, name, tclass, category, build_tcase_dir, extra_inputs))

    join_workers(q, threads)

def run_show_diff_output(units_dir, t):
    print("\t", end='')
    line('.')
    for fn in glob.glob(units_dir + '/' + t + '.*/' + _DIFF_OUTPUT_NAME):
        with open(fn, 'r') as f:
            for l in f:
                print("\t" + l, end='')
    print()

def run_show_stderr_output(units_dir, t):
    print("\t", end='')
    line('.')
    for fn in glob.glob(units_dir + '/' + t + '.*/' + _STDERR_OUTPUT_NAME):
        with open(fn, 'r') as f:
            lines = f.readlines()
            for l in lines[-50:]:
                print("\t" + l, end='')
    print()

def run_show_valgrind_output(units_dir, t):
    print("\t", end='')
    line('.')
    for fn in glob.glob(units_dir + '/' + t + '.*/' + _VALGRIND_OUTPUT_NAME):
        with open(fn, 'r') as f:
            for l in f:
                print("\t" + l, end='')
    print()

def run_summary(build_dir):
    print()
    print('Summary (see CMDLINE.tmp to reproduce without test harness)')
    line()

    fmt = '  %-40s%d'
    print(fmt % ('#passed:', len(L_PASSED)))

    print(fmt % ('#FIXED:', len(L_FIXED)))
    for t in L_FIXED:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#FAILED (broken args.ctags?):', len(L_BROKEN_ARGS_CTAGS)))
    for t in L_BROKEN_ARGS_CTAGS:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#FAILED (unexpected-exit-status):', len(L_FAILED_BY_STATUS)))
    for t in L_FAILED_BY_STATUS:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))
        if SHOW_DIFF_OUTPUT:
            run_show_stderr_output(build_dir, remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#FAILED (unexpected-output):', len(L_FAILED_BY_DIFF)))
    for t in L_FAILED_BY_DIFF:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))
        if SHOW_DIFF_OUTPUT:
            run_show_stderr_output(build_dir, remove_prefix(t, _DEFAULT_CATEGORY + '/'))
            run_show_diff_output(build_dir, remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    if WITH_TIMEOUT != 0:
        print(fmt % ('#TIMED-OUT (' + str(WITH_TIMEOUT) + 's):', len(L_FAILED_BY_TIMEED_OUT)))
        for t in L_FAILED_BY_TIMEED_OUT:
            print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#skipped (features):', len(L_SKIPPED_BY_FEATURES)))
    for t in L_SKIPPED_BY_FEATURES:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#skipped (languages):', len(L_SKIPPED_BY_LANGUAGES)))
    for t in L_SKIPPED_BY_LANGUAGES:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    if WITH_TIMEOUT == 0:
        print(fmt % ('#skipped (infinite-loop):', len(L_SKIPPED_BY_ILOOP)))
        for t in L_SKIPPED_BY_ILOOP:
            print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    print(fmt % ('#known-bugs:', len(L_KNOWN_BUGS)))
    for t in L_KNOWN_BUGS:
        print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))

    if WITH_VALGRIND:
        print(fmt % ('#valgrind-error:', len(L_VALGRIND)))
        for t in L_VALGRIND:
            print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))
        if SHOW_DIFF_OUTPUT:
            print(fmt % ('##valgrind-error:', len(L_VALGRIND)))
            for t in L_VALGRIND:
                print("\t" + remove_prefix(t, _DEFAULT_CATEGORY + '/'))
                run_show_valgrind_output(build_dir, remove_prefix(t, _DEFAULT_CATEGORY + '/'))

def make_pretense_map(arg):
    r = ''
    for p in arg.split(','):
        ret = re.match(r'(.*)/(.*)', p)
        if not ret:
            error_exit(1, 'wrong format of --_pretend option arg')

        (newlang, oldlang) = ret.group(1, 2)
        if newlang == '':
            error_exit(1, 'newlang part of --_pretend option arg is empty')
        if oldlang == '':
            error_exit(1, 'oldlang part of --_pretend option arg is empty')

        r += ' --_pretend-' + newlang + '=' + oldlang

    return r

def action_run(parser, action, *args):
    global CATEGORIES
    global CTAGS
    global UNITS
    global LANGUAGES
    global WITH_TIMEOUT
    global WITH_VALGRIND
    global COLORIZED_OUTPUT
    global RUN_SHRINK
    global SHOW_DIFF_OUTPUT
    global PRETENSE_OPTS
    global NUM_WORKER_THREADS
    global SHELL

    parser.add_argument('--categories', metavar='CATEGORY1[,CATEGORY2,...]',
            help='run only CATEGORY* related cases.')
    parser.add_argument('--ctags',
            help='ctags executable file for testing')
    parser.add_argument('--units', metavar='UNITS1[,UNITS2,...]',
            help='run only UNIT(S).')
    parser.add_argument('--languages', metavar='PARSER1[,PARSER2,...]',
            help='run only PARSER* related cases.')
    parser.add_argument('--with-timeout', type=int, default=0,
            metavar='DURATION',
            help='run a test case with specified timeout in seconds. 0 means no timeout (default).')
    parser.add_argument('--with-valgrind', action='store_true', default=False,
            help='run a test case under valgrind')
    parser.add_argument('--colorized-output', choices=['yes', 'no'], default='yes',
            help='print the result in color.')
    parser.add_argument('--run-shrink', action='store_true', default=False,
            help='(TODO: NOT IMPLEMENTED YET)')
    parser.add_argument('--show-diff-output', action='store_true', default=False,
            help='show diff output (and valgrind errors) for failed test cases in the summary.')
    parser.add_argument('--with-pretense-map',
            metavar='NEWLANG0/OLDLANG0[,...]',
            help='make NEWLANG parser pretend OLDLANG.')
    parser.add_argument('--threads', type=int, default=NUM_WORKER_THREADS,
            help='number of worker threads')
    parser.add_argument('--shell',
            help='shell to be used.')
    parser.add_argument('units_dir',
            help='Units directory.')
    parser.add_argument('build_dir', nargs='?', default='',
            help='Build directory. If not given, units_dir is used.')

    res = parser.parse_args(args)
    if res.categories:
        CATEGORIES = [x if x == 'ROOT' or x.endswith('.r') else x + '.r'
                for x in res.categories.split(',')]
    if res.ctags:
        CTAGS = res.ctags
    if res.units:
        UNITS = res.units.split(',')
    if res.languages:
        LANGUAGES = res.languages.split(',')
    WITH_TIMEOUT = res.with_timeout
    WITH_VALGRIND = res.with_valgrind
    COLORIZED_OUTPUT = (res.colorized_output == 'yes')
    RUN_SHRINK = res.run_shrink
    SHOW_DIFF_OUTPUT = res.show_diff_output
    if res.with_pretense_map:
        PRETENSE_OPTS = make_pretense_map(res.with_pretense_map)
    NUM_WORKER_THREADS = res.threads
    if res.shell:
        SHELL = res.shell
    if res.build_dir == '':
        res.build_dir = res.units_dir

    if WITH_VALGRIND:
        check_availability('valgrind')
    check_availability('diff')
    init_features()

    if isabs(res.build_dir):
        build_dir = res.build_dir
    else:
        build_dir = os.path.realpath(res.build_dir)

    category = _DEFAULT_CATEGORY
    if len(CATEGORIES) == 0 or (category in CATEGORIES):
        run_dir(category, res.units_dir, build_dir)

    for d in glob.glob(res.units_dir + '/*.r'):
        d = d.replace('\\', '/')    # for Windows
        if not os.path.isdir(d):
            continue
        category = os.path.basename(d)
        build_d = res.build_dir + '/' + category
        run_dir(category, d, build_d)

    run_summary(build_dir)

    if L_FAILED_BY_STATUS or L_FAILED_BY_DIFF or \
            L_FAILED_BY_TIMEED_OUT or L_BROKEN_ARGS_CTAGS or \
            L_VALGRIND:
        return 1
    else:
        return 0

def action_clean(parser, action, *args):
    parser.add_argument('units_dir',
            help='Build directory for units testing.')

    res = parser.parse_args(args)
    units_dir = res.units_dir

    if not os.path.isdir(units_dir):
        error_exit(0, 'No such directory: ' + units_dir)

    for bundles in glob.glob(units_dir + '/**/BUNDLES', recursive=True):
        clean_bundles(bundles)

    for fn in glob.glob(units_dir + '/**/*.tmp', recursive=True):
        os.remove(fn)
    for fn in glob.glob(units_dir + '/**/*.TMP', recursive=True):
        os.remove(fn)
    return 0

def tmain_compare_result(build_topdir):
    for fn in glob.glob(build_topdir + '/*/*-diff.txt'):
        print(fn)
        print()
        with open(fn, 'r', errors='replace') as f:
            for l in f:
                print("\t" + l, end='')
        print()

    for fn in glob.glob(build_topdir + '/*/gdb-backtrace.txt'):
        with open(fn, 'r', errors='replace') as f:
            for l in f:
                print("\t" + l, end='')

def tmain_compare(subdir, build_subdir, aspect, file):
    msg = '%-59s ' % (aspect)
    generated = build_subdir + '/' + aspect + '-diff.txt'
    actual = build_subdir + '/' + aspect + '-actual.txt'
    expected = subdir + '/' + aspect + '-expected.txt'
    if os.path.isfile(actual) and os.path.isfile(expected) and \
            filecmp.cmp(actual, expected):
        run_result('ok', msg, None, file=file)
        # When successful, remove files generated in the last
        # failure to make the directory clean.
        # Unlike other generated files like gdb-backtrace.txt
        # misc/review script looks at the -diff.txt file.
        # Therefore we handle -diff.txt specially here.
        if os.path.isfile(generated):
            os.remove(generated)
        return True
    else:
        with open(generated, 'wb') as f:
            subprocess.run(['diff', '-U',
                str(DIFF_U_NUM), '--strip-trailing-cr',
                expected, actual],
                stdout=f, stderr=subprocess.STDOUT)
        run_result('error', msg, None, 'diff: ' + generated, file=file)
        return False

def failed_git_marker(fn):
    if shutil.which('git'):
        ret = subprocess.run(['git', 'ls-files', '--', fn],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        if ret.returncode == 0 and ret.stdout == b'':
            return '<G>'
    return ''

def is_crashed(fn):
    with open(fn, 'r') as f:
        if 'core dump' in f.read():
            return True
    return False

def print_backtraces(ctags_exe, cores, fn):
    with open(fn, 'wb') as f:
        for coref in cores:
            subprocess.run(['gdb', ctags_exe, '-c', coref, '-ex', 'where', '-batch'],
                stdout=f, stderr=subprocess.DEVNULL)

def tmain_sub(test_name, basedir, subdir, build_subdir):
    global TMAIN_STATUS
    global TMAIN_FAILED

    CODE_FOR_IGNORING_THIS_TMAIN_TEST = 77

    os.makedirs(build_subdir, exist_ok=True)

    for fn in glob.glob(build_subdir + '/*-actual.txt'):
        os.remove(fn)

    strbuf = io.StringIO()
    print("\nTesting " + test_name, file=strbuf)
    line('-', file=strbuf)

    if isabs(CTAGS):
        ctags_path = CTAGS
    else:
        ctags_path = os.path.join(basedir, CTAGS)

    if isabs(READTAGS):
        readtags_path = READTAGS
    else:
        readtags_path = os.path.join(basedir, READTAGS)

    if isabs(OPTSCRIPT):
        optscript_path = OPTSCRIPT
    else:
        optscript_path = os.path.join(basedir, OPTSCRIPT)

    start = time.time()
    ret = subprocess.run([SHELL, 'run.sh',
            ctags_path,
            build_subdir,
            readtags_path,
            optscript_path],
            cwd=subdir,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #print('execute time: %f' % (time.time() - start), file=strbuf)

    encoding = 'utf-8'
    try:
        stdout = ret.stdout.decode(encoding).replace("\r\n", "\n")
    except UnicodeError:
        encoding = 'iso-8859-1'
        stdout = ret.stdout.decode(encoding).replace("\r\n", "\n")
    stderr = ret.stderr.decode('utf-8').replace("\r\n", "\n")
    if os.path.basename(CTAGS) != 'ctags':
        # program name needs to be canonicalized
        stderr = re.sub('(?m)^' + os.path.basename(CTAGS) + ':', 'ctags:', stderr)

    if ret.returncode == CODE_FOR_IGNORING_THIS_TMAIN_TEST:
        run_result('skip', '', None, stdout.replace("\n", ''), file=strbuf)
        print(strbuf.getvalue(), end='')
        sys.stdout.flush()
        strbuf.close()
        return True

    with open(build_subdir + '/exit-actual.txt', 'w', newline='\n') as f:
        print(ret.returncode, file=f)
    with open(build_subdir + '/stdout-actual.txt', 'w', newline='\n', encoding=encoding) as f:
        print(stdout, end='', file=f)
    with open(build_subdir + '/stderr-actual.txt', 'w', newline='\n') as f:
        print(stderr, end='', file=f)

    if os.path.isfile(build_subdir + '/tags'):
        os.rename(build_subdir + '/tags', build_subdir + '/tags-actual.txt')

    for aspect in ['stdout', 'stderr', 'exit', 'tags']:
        expected_txt = subdir + '/' + aspect + '-expected.txt'
        actual_txt = build_subdir + '/' + aspect + '-actual.txt'
        if os.path.isfile(expected_txt):
            if tmain_compare(subdir, build_subdir, aspect, strbuf):
                os.remove(actual_txt)
            else:
                TMAIN_FAILED += [test_name + '/' + aspect + '-compare' +
                        failed_git_marker(expected_txt)]
                TMAIN_STATUS = False
                if aspect == 'stderr' and \
                        is_crashed(actual_txt) and \
                        shutil.which('gdb'):
                    print_backtraces(ctags_path,
                            glob.glob(build_subdir + '/core*'),
                            build_subdir + '/gdb-backtrace.txt')
        elif os.path.isfile(actual_txt):
            os.remove(actual_txt)

    print(strbuf.getvalue(), end='')
    sys.stdout.flush()
    strbuf.close()
    return True

def tmain_run(topdir, build_topdir, units):
    global TMAIN_STATUS

    TMAIN_STATUS = True

    (q, threads) = create_thread_queue(tmain_sub)

    basedir = os.getcwd()
    for subdir in glob.glob(topdir + '/*.d'):
        test_name = os.path.basename(subdir)[:-2]

        if len(units) > 0 and not test_name in units:
            continue

        build_subdir = build_topdir + '/' + os.path.basename(subdir)
        q.put((test_name, basedir, subdir, build_subdir))

    join_workers(q, threads)

    print()
    if not TMAIN_STATUS:
        print('Failed tests')
        line('=')
        for f in TMAIN_FAILED:
            print(re.sub('<G>', ' (not committed/cached yet)', f))
        print()

        if SHOW_DIFF_OUTPUT:
            print('Detail [compare]')
            line('-')
            tmain_compare_result(build_topdir)

    return TMAIN_STATUS

def action_tmain(parser, action, *args):
    global CTAGS
    global COLORIZED_OUTPUT
    global WITH_VALGRIND
    global SHOW_DIFF_OUTPUT
    global READTAGS
    global OPTSCRIPT
    global UNITS
    global NUM_WORKER_THREADS
    global SHELL

    parser.add_argument('--ctags',
            help='ctags executable file for testing')
    parser.add_argument('--colorized-output', choices=['yes', 'no'], default='yes',
            help='print the result in color.')
    parser.add_argument('--with-valgrind', action='store_true', default=False,
            help='(not implemented) run a test case under valgrind')
    parser.add_argument('--show-diff-output', action='store_true', default=False,
            help='how diff output for failed test cases in the summary.')
    parser.add_argument('--readtags',
            help='readtags executable file for testing')
    parser.add_argument('--optscript',
            help='optscript executable file for testing')
    parser.add_argument('--units', metavar='UNITS1[,UNITS2,...]',
            help='run only Tmain/UNIT*.d (.d is not needed)')
    parser.add_argument('--threads', type=int, default=NUM_WORKER_THREADS,
            help='number of worker threads')
    parser.add_argument('--shell',
            help='shell to be used.')
    parser.add_argument('tmain_dir',
            help='Tmain directory.')
    parser.add_argument('build_dir', nargs='?', default='',
            help='Build directory. If not given, tmain_dir is used.')

    res = parser.parse_args(args)
    if res.ctags:
        CTAGS = res.ctags
    COLORIZED_OUTPUT = (res.colorized_output == 'yes')
    WITH_VALGRIND = res.with_valgrind
    SHOW_DIFF_OUTPUT = res.show_diff_output
    if res.readtags:
        READTAGS = res.readtags
    if res.optscript:
        OPTSCRIPT = res.optscript
    if res.units:
        UNITS = res.units.split(',')
    NUM_WORKER_THREADS = res.threads
    if res.shell:
        SHELL = res.shell
    if res.build_dir == '':
        res.build_dir = res.tmain_dir

    #check_availability('awk')
    check_availability('diff')

    if isabs(res.build_dir):
        build_dir = res.build_dir
    else:
        build_dir = os.path.realpath(res.build_dir)

    ret = tmain_run(res.tmain_dir, build_dir, UNITS)
    if ret:
        return 0
    else:
        return 1

def action_clean_tmain(parser, action, *args):
    parser.add_argument('tmain_dir',
            help='Build directory for tmain testing.')

    res = parser.parse_args(args)
    tmain_dir = res.tmain_dir

    if not os.path.isdir(tmain_dir):
        error_exit(0, 'No such directory: ' + tmain_dir)

    for obj in ['stdout', 'stderr', 'exit', 'tags']:
        for typ in ['actual', 'diff']:
            for fn in glob.glob(tmain_dir + '/**/' + obj + '-' + typ + '.txt', recursive=True):
                os.remove(fn)
    for fn in glob.glob(tmain_dir + '/**/gdb-backtrace.txt', recursive=True):
        os.remove(fn)
    return 0

def prepare_environment():
    global _PREPERE_ENV

    os.environ['LC_ALL'] = 'C'
    os.environ['MSYS2_ARG_CONV_EXCL'] = '--regex-;--_scopesep;--exclude;--exclude-exception'

    _PREPERE_ENV = """LC_ALL="C"; export LC_ALL
MSYS2_ARG_CONV_EXCL='--regex-;--_scopesep;--exclude;--exclude-exception' export MSYS2_ARG_CONV_EXCL
"""

# enable ANSI escape sequences on Windows 10 1511 (10.0.10586) or later
def enable_esc_sequence():
    if os.name != 'nt':
        return

    import ctypes

    kernel32 = ctypes.windll.kernel32

    ENABLE_VIRTUAL_TERMINAL_PROCESSING = 4
    STD_OUTPUT_HANDLE = -11

    out = kernel32.GetStdHandle(STD_OUTPUT_HANDLE)
    mode = ctypes.c_ulong()
    if kernel32.GetConsoleMode(out, ctypes.byref(mode)):
        kernel32.SetConsoleMode(out,
                mode.value | ENABLE_VIRTUAL_TERMINAL_PROCESSING)

def main():
    prepare_environment()
    enable_esc_sequence()

    parser = argparse.ArgumentParser(
            description='Units test harness for ctags.')
    subparsers = parser.add_subparsers(dest='action', metavar='ACTION')
    cmdmap = {}
    cmdmap['run'] = [action_run,
            subparsers.add_parser('run', aliases=['units'],
                description='Run all tests case under units_dir.',
                help='Run all tests case')]
    cmdmap['units'] = cmdmap['run']
    cmdmap['clean'] = [action_clean,
            subparsers.add_parser('clean',
                description='Clean all files created during units testing.',
                help='Clean all files created during units testing')]
    cmdmap['tmain'] = [action_tmain,
            subparsers.add_parser('tmain',
                description='Run tests for main part of ctags.',
                help='Run tests for main part of ctags')]
    cmdmap['clean-tmain'] = [action_clean_tmain,
            subparsers.add_parser('clean-tmain',
                description='Clean all files created during tmain testing.',
                help='Clean all files created during tmain testing')]
    subparsers.add_parser('help',
            help='show this help message and exit')
    cmdmap['help'] = [action_help, parser]

    if len(sys.argv) < 2:
        parser.print_help()
        sys.exit(1)

    res = parser.parse_args(sys.argv[1:2])
    (func, subparser) = cmdmap[res.action]
    sys.exit(func(subparser, *sys.argv[1:]))

if __name__ == '__main__':
    main()
