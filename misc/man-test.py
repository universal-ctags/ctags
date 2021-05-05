#!/usr/bin/env python3

#
# man-test.py - test exapmles in a man page
#
# Copyright (C) 2021 Masatake YAMATO
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
# On Windows, unix-like shell (e.g. bash) and diff command are needed.
#

import sys
import re
import os
import subprocess
import copy

def print_usage(n, f):
    print ('Usage: man-test.py TMPDIR CTAGS ctags-lang-<LANG>.7.rst.in...', file=f)
    sys.exit(n)

def next_segment(line):
    if line.endswith ('\\'):
        return line[0:-1]
    else:
        return (line + '\n')

def wash_cmdline(cmdline):
    return cmdline

def verify_test_case(t):
    prefix = '%(man_file)s[%(nth)d]:%(start_linum)d: '%t
    msg = False
    if not 'code' in t:
        msg = 'cannot find input lines'
    elif not 'tags' in t:
        msg = 'cannot find expected tags output'
    elif not 'cmdline' in t:
        msg = 'cannot find ctags command line'

    if msg:
        msg = prefix + msg
    return msg

def is_option(c):
    if re.search('--[a-z_].*', c):
        return True
    elif re.search('^-[a-z]$', c):
        return True
    return False

def run_test_case(tmpdir, ctags, t):
    d = tmpdir + '/' + str(os.getpid())
    os.makedirs (d,exist_ok=True)
    i = d + '/' + t['input_file_name']
    o0 = 'actual.tags'
    o = d + '/' + o0
    e0 = 'expected.tags'
    e = d + '/' + e0
    D = d + '/' + 'tags.diff'
    O0 = 'args.ctags'
    O = d + '/' + O0
    with open(i, mode='w', encoding='utf-8') as f:
        f.write(t['code'])

    with open(e, mode='w', encoding='utf-8') as g:
        g.write(t['tags'])

    inputf=None
    with open(O, mode='w', encoding='utf-8') as Of:
        in_pattern = False
        for c in t['cmdline'].split():
            if c == '--options=NONE':
                continue
            elif c.startswith('input.'):
                inputf = c
                continue
            elif c.startswith('--regex-'):
                in_pattern = c
            elif in_pattern and not is_option(c):
                # TODO: This doesn't work if whitespace is repeated.
                in_pattern = in_pattern + ' ' + c
            else:
                if in_pattern:
                    print (in_pattern, file=Of)
                    in_pattern = False
                print (c, file=Of)
        if in_pattern:
            print (in_pattern, file=Of)

    with open(o, mode='w', encoding='utf-8') as h:
        cmdline = [ctags, '--quiet', '--options=NONE',
                   '--options=' + O0, inputf]
        subprocess.run(cmdline, cwd=d, stdout=h)

    with open(D, mode='w', encoding='utf-8') as diff:
        r = subprocess.run(['diff', '-uN', '--strip-trailing-cr', o0, e0],
                           cwd=d, stdout=diff).returncode

    if r == 0:
        t['result'] = True
        t['result_readable'] = 'passed'
    else:
        with open(o, encoding='utf-8') as f:
            t['actual_tags'] = f.read()
        t['result'] = False
        t['result_readable'] = 'failed'
        with open(D, encoding='utf-8') as diff:
            t['tags_diff'] = diff.read()
    os.remove(O)
    os.remove(i)
    os.remove(e)
    os.remove(o)
    os.remove(D)
    os.rmdir(d)
    return t

def report_result(r):
    print ('%(man_file)s[%(nth)d]:%(start_linum)d...%(result_readable)s'%r)

def report_failure(r):
    print ('## %(man_file)s[%(nth)d]:%(start_linum)d'%r)
    print ('### input')
    print ('```')
    print (r['code'])
    print ('```')
    print ('### cmdline')
    print ('```')
    print (r['cmdline'])
    print ('```')
    print ('### expected tags')
    print ('```')
    print (r['tags'])
    print ('```')
    print ('### actual tags')
    print ('```')
    print (r['actual_tags'])
    print ('```')
    print ('### diff of tag files')
    print ('```')
    print (r['tags_diff'])
    print ('```')

class state:
    start = 0
    tags  = 1
    code  = 2
    code_done = 3
    input = 4
    output = 5
    output_after_options = 6

def extract_test_cases(f):
    linum=0
    nth=0
    s=state.start
    test_spec = {}

    for line in  f.readlines():
        linum += 1
        line = line.rstrip('\r\n')

        if s == state.tags or s == state.code:
            if prefix:
                m = re.search('^' + prefix + '(.*)$', line)
                if m:
                    sink += next_segment(m.group(1))
                    continue
                if line == '':
                    sink += '\n'
                    continue
            else:
                m = re.search('^([ \t]+)(.+)$', line)
                if m:
                    prefix = m.group(1)
                    sink += next_segment(m.group(2))
                    continue
                elif re.search ('^([ \t]*)$', line):
                    continue

            sink = sink.rstrip('\r\n') + '\n'

            if s == state.code:
                test_spec['code'] = sink
                s = state.code_done
            else:
                test_spec['tags'] = sink
                test_spec['nth'] = nth
                nth += 1
                test_spec['end_linum'] = linum
                s = state.start
                yield test_spec

        m = s == state.start and re.search ('^"(input\.[^"]+)"$', line)
        if m:
            test_spec ['start_linum'] = linum
            test_spec ['input_file_name'] = m.group(1)
            s = state.input
            continue
        m = s == state.input and re.search ('^.. code-block::.*', line)
        if m:
            sink = ''
            prefix = False
            s = state.code
            continue
        m = s == state.code_done and re.search ('^"output.tags"$', line)
        if m:
            s = state.output
            continue
        m = s == state.output and re.search ('with[ \t]"([^"]+)"', line)
        if m:
            test_spec ['cmdline'] = wash_cmdline (m.group(1))
            s = state.output_after_options
            continue
        if s == state.output_after_options \
           and (line == "::" or re.search ('^.. code-block:: *tags$', line)):
            sink = ''
            prefix = False
            s = state.tags
            continue

def man_test (tmpdir, ctags, man_file):
    failures = []
    result = True
    print ('# Run test cases in ' + man_file)
    print ('```')
    with open(man_file, encoding='utf-8') as f:
        for t in extract_test_cases (f):
            t['man_file'] = man_file
            v = verify_test_case (t)
            if v:
                print ("error: " + v, file=sys.stderr)
                result = False
                continue
            r = run_test_case (tmpdir, ctags, t)
            report_result (r)
            if not r['result']:
                result = False
                failures.append(copy.copy(r))
    print ('```')
    if (len(failures) > 0):
        print ('# Failed test case(s)')
        for f in failures:
            report_failure(f)
    return result

def man_tests (tmpdir, ctags, man_files):
    result = 0
    for m in man_files:
        if not man_test(tmpdir, ctags, m):
            result += 1
    print ('OK' if result == 0 else 'FAILED')
    return result == 0

if (len(sys.argv) < 4) or (sys.argv[1] == '-h' or sys.argv[1] == '--help'):
    print_usage (2, sys.stderr)

tmpdir = sys.argv[1]
ctags = os.path.abspath(sys.argv[2])
sys.exit(0 if man_tests (tmpdir, ctags, sys.argv[3:]) else 1)
