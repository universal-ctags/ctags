#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from ctypes import *
import onig
import sys
import codecs
import locale

nerror = 0
nsucc = 0
nfail = 0
region = 0

onig_encoding = onig.ONIG_ENCODING_EUC_JP
encoding = onig_encoding[0].name

class strptr:
    """a helper class to get a pointer to a string"""
    def __init__(self, s):
        if not isinstance(s, bytes):
            raise TypeError
        self._str = s
        self._ptr = cast(self._str, c_void_p)

    def getptr(self, offset=0):
        if offset == -1:    # -1 means the end of the string
            offset = len(self._str)
        elif offset > len(self._str):
            raise IndexError
        return self._ptr.value + offset

def cc_to_cb(s, enc, cc):
    """convert char count to byte count
    
    arguments:
      s -- unicode string
      enc -- encoding name
      cc -- char count
    """
    if cc > len(s):
        raise IndexError
    return len(s[0:cc].encode(enc))

def xx(pattern, target, s_from, s_to, mem, not_match):
    global nerror
    global nsucc
    global nfail
    global region
    
    reg = onig.OnigRegex()
    einfo = onig.OnigErrorInfo()
    syn = onig.OnigSyntaxType()
    msg = create_string_buffer(onig.ONIG_MAX_ERROR_MESSAGE_LEN)
    
    pattern2 = pattern
    if not isinstance(pattern, bytes):
        pattern2 = pattern.encode(encoding)
    patternp = strptr(pattern2)
    
    target2 = target
    if not isinstance(target, bytes):
        s_from = cc_to_cb(target, encoding, s_from)
        s_to = cc_to_cb(target, encoding, s_to)
        target2 = target.encode(encoding)
    targetp = strptr(target2)
    
    onig.onig_copy_syntax(byref(syn), onig.ONIG_SYNTAX_DEFAULT)
    syn.options = syn.options & ~onig.ONIG_OPTION_ASCII_RANGE
    
    r = onig.onig_new(byref(reg), patternp.getptr(), patternp.getptr(-1),
            onig.ONIG_OPTION_DEFAULT, onig_encoding, byref(syn), byref(einfo));
    if r != 0:
        onig.onig_error_code_to_str(msg, r, byref(einfo))
        nerror += 1
        print("ERROR: %s (/%s/ '%s')" % (msg.value, pattern, target),
                file=sys.stderr)
        return
    
    r = onig.onig_search(reg, targetp.getptr(), targetp.getptr(-1),
                    targetp.getptr(), targetp.getptr(-1),
                    region, onig.ONIG_OPTION_NONE);
    if r < onig.ONIG_MISMATCH:
        onig.onig_error_code_to_str(msg, r)
        nerror += 1
        print("ERROR: %s (/%s/ '%s')" % (msg.value, pattern, target),
                file=sys.stderr)
        return
    
    if r == onig.ONIG_MISMATCH:
        if not_match:
            nsucc += 1
            print("OK(N): /%s/ '%s'" % (pattern, target))
        else:
            nfail += 1
            print("FAIL: /%s/ '%s'" % (pattern, target))
    else:
        if not_match:
            nfail += 1
            print("FAIL(N): /%s/ '%s'" % (pattern, target))
        else:
            start = region[0].beg[mem]
            end = region[0].end[mem]
            if (start == s_from) and (end == s_to):
                nsucc += 1
                print("OK: /%s/ '%s'" % (pattern, target))
            else:
                nfail += 1
                print("FAIL: /%s/ '%s' %d-%d : %d-%d\n" % (pattern, target,
                        s_from, s_to, start, end))
    onig.onig_free(reg)

def x2(pattern, target, s_from, s_to):
    xx(pattern, target, s_from, s_to, 0, False)

def x3(pattern, target, s_from, s_to, mem):
    xx(pattern, target, s_from, s_to, mem, False)

def n(pattern, target):
    xx(pattern, target, 0, 0, 0, True)



def main():
    global region
    global onig_encoding
    global encoding
    
    region = onig.onig_region_new()
    
    # set encoding of the test target
    if len(sys.argv) > 1:
        if sys.argv[1] == "EUC-JP":
            onig_encoding = onig.ONIG_ENCODING_EUC_JP
        elif sys.argv[1] == "SJIS":
            onig_encoding = onig.ONIG_ENCODING_SJIS
        elif sys.argv[1] == "UTF-8":
            onig_encoding = onig.ONIG_ENCODING_UTF8
        elif sys.argv[1] == "UTF-16LE":
            onig_encoding = onig.ONIG_ENCODING_UTF16_LE
        elif sys.argv[1] == "UTF-16BE":
            onig_encoding = onig.ONIG_ENCODING_UTF16_BE
        else:
            print("test target encoding error")
            sys.exit()
        encoding = onig_encoding[0].name
    
    # set encoding of stdout/stderr
    if len(sys.argv) > 2:
        outenc = sys.argv[2]
    else:
        outenc = locale.getpreferredencoding()
    sys.stdout = codecs.getwriter(outenc)(sys.stdout)
    sys.stderr = codecs.getwriter(outenc)(sys.stderr)
    
    # onig-5.9.2/testc.c からコピー
    #   trigraph 対策の ?\? は ?? に置き換え
    #   マッチ位置の指定をバイト単位から文字数単位に変更
    
    x2(u"", u"", 0, 0);
    x2(u"^", u"", 0, 0);
    x2(u"$", u"", 0, 0);
    x2(u"\\G", u"", 0, 0);
    x2(u"\\A", u"", 0, 0);
    x2(u"\\Z", u"", 0, 0);
    x2(u"\\z", u"", 0, 0);
    x2(u"^$", u"", 0, 0);
    x2(u"\\ca", u"\001", 0, 1);
    x2(u"\\C-b", u"\002", 0, 1);
    x2(u"\\c\\\\", u"\034", 0, 1);
    x2(u"q[\\c\\\\]", u"q\034", 0, 2);
    x2(u"", u"a", 0, 0);
    x2(u"a", u"a", 0, 1);
    if onig_encoding == onig.ONIG_ENCODING_UTF16_LE:
        x2(u"\\x61\\x00", u"a", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_UTF16_BE:
        x2(u"\\x00\\x61", u"a", 0, 1);
    else:
        x2(u"\\x61", u"a", 0, 1);
    x2(u"aa", u"aa", 0, 2);
    x2(u"aaa", u"aaa", 0, 3);
    x2(u"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", u"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0, 35);
    x2(u"ab", u"ab", 0, 2);
    x2(u"b", u"ab", 1, 2);
    x2(u"bc", u"abc", 1, 3);
    x2(u"(?i:#RET#)", u"#INS##RET#", 5, 10);
    if onig_encoding == onig.ONIG_ENCODING_UTF16_LE:
        x2(u"\\17\\00", u"\017", 0, 1);
        x2(u"\\x1f\\x00", u"\x1f", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_UTF16_BE:
        x2(u"\\00\\17", u"\017", 0, 1);
        x2(u"\\x00\\x1f", u"\x1f", 0, 1);
    else:
        x2(u"\\17", u"\017", 0, 1);
        x2(u"\\x1f", u"\x1f", 0, 1);
    x2(u"a(?#....\\\\JJJJ)b", u"ab", 0, 2);
    x2(u"(?x)  G (o O(?-x)oO) g L", u"GoOoOgLe", 0, 7);
    x2(u".", u"a", 0, 1);
    n(u".", u"");
    x2(u"..", u"ab", 0, 2);
    x2(u"\\w", u"e", 0, 1);
    n(u"\\W", u"e");
    x2(u"\\s", u" ", 0, 1);
    x2(u"\\S", u"b", 0, 1);
    x2(u"\\d", u"4", 0, 1);
    n(u"\\D", u"4");
    x2(u"\\b", u"z ", 0, 0);
    x2(u"\\b", u" z", 1, 1);
    x2(u"\\B", u"zz ", 1, 1);
    x2(u"\\B", u"z ", 2, 2);
    x2(u"\\B", u" z", 0, 0);
    x2(u"[ab]", u"b", 0, 1);
    n(u"[ab]", u"c");
    x2(u"[a-z]", u"t", 0, 1);
    n(u"[^a]", u"a");
    x2(u"[^a]", u"\n", 0, 1);
    x2(u"[]]", u"]", 0, 1);
    n(u"[^]]", u"]");
    x2(u"[\\^]+", u"0^^1", 1, 3);
    x2(u"[b-]", u"b", 0, 1);
    x2(u"[b-]", u"-", 0, 1);
    x2(u"[\\w]", u"z", 0, 1);
    n(u"[\\w]", u" ");
    x2(u"[\\W]", u"b$", 1, 2);
    x2(u"[\\d]", u"5", 0, 1);
    n(u"[\\d]", u"e");
    x2(u"[\\D]", u"t", 0, 1);
    n(u"[\\D]", u"3");
    x2(u"[\\s]", u" ", 0, 1);
    n(u"[\\s]", u"a");
    x2(u"[\\S]", u"b", 0, 1);
    n(u"[\\S]", u" ");
    x2(u"[\\w\\d]", u"2", 0, 1);
    n(u"[\\w\\d]", u" ");
    x2(u"[[:upper:]]", u"B", 0, 1);
    x2(u"[*[:xdigit:]+]", u"+", 0, 1);
    x2(u"[*[:xdigit:]+]", u"GHIKK-9+*", 6, 7);
    x2(u"[*[:xdigit:]+]", u"-@^+", 3, 4);
    n(u"[[:upper]]", u"A");
    x2(u"[[:upper]]", u":", 0, 1);
    if onig_encoding == onig.ONIG_ENCODING_UTF16_LE:
        x2(u"[\\044\\000-\\047\\000]", u"\046", 0, 1);
        x2(u"[\\x5a\\x00-\\x5c\\x00]", u"\x5b", 0, 1);
        x2(u"[\\x6A\\x00-\\x6D\\x00]", u"\x6c", 0, 1);
        n(u"[\\x6A\\x00-\\x6D\\x00]", u"\x6E");
    elif onig_encoding == onig.ONIG_ENCODING_UTF16_BE:
        x2(u"[\\000\\044-\\000\\047]", u"\046", 0, 1);
        x2(u"[\\x00\\x5a-\\x00\\x5c]", u"\x5b", 0, 1);
        x2(u"[\\x00\\x6A-\\x00\\x6D]", u"\x6c", 0, 1);
        n(u"[\\x00\\x6A-\\x00\\x6D]", u"\x6E");
    else:
        x2(u"[\\044-\\047]", u"\046", 0, 1);
        x2(u"[\\x5a-\\x5c]", u"\x5b", 0, 1);
        x2(u"[\\x6A-\\x6D]", u"\x6c", 0, 1);
        n(u"[\\x6A-\\x6D]", u"\x6E");
    n(u"^[0-9A-F]+ 0+ UNDEF ", u"75F 00000000 SECT14A notype ()    External    | _rb_apply");
    x2(u"[\\[]", u"[", 0, 1);
    x2(u"[\\]]", u"]", 0, 1);
    x2(u"[&]", u"&", 0, 1);
    x2(u"[[ab]]", u"b", 0, 1);
    x2(u"[[ab]c]", u"c", 0, 1);
    n(u"[[^a]]", u"a");
    n(u"[^[a]]", u"a");
    x2(u"[[ab]&&bc]", u"b", 0, 1);
    n(u"[[ab]&&bc]", u"a");
    n(u"[[ab]&&bc]", u"c");
    x2(u"[a-z&&b-y&&c-x]", u"w", 0, 1);
    n(u"[^a-z&&b-y&&c-x]", u"w");
    x2(u"[[^a&&a]&&a-z]", u"b", 0, 1);
    n(u"[[^a&&a]&&a-z]", u"a");
    x2(u"[[^a-z&&bcdef]&&[^c-g]]", u"h", 0, 1);
    n(u"[[^a-z&&bcdef]&&[^c-g]]", u"c");
    x2(u"[^[^abc]&&[^cde]]", u"c", 0, 1);
    x2(u"[^[^abc]&&[^cde]]", u"e", 0, 1);
    n(u"[^[^abc]&&[^cde]]", u"f");
    x2(u"[a-&&-a]", u"-", 0, 1);
    n(u"[a\\-&&\\-a]", u"&");
    n(u"\\wabc", u" abc");
    x2(u"a\\Wbc", u"a bc", 0, 4);
    x2(u"a.b.c", u"aabbc", 0, 5);
    x2(u".\\wb\\W..c", u"abb bcc", 0, 7);
    x2(u"\\s\\wzzz", u" zzzz", 0, 5);
    x2(u"aa.b", u"aabb", 0, 4);
    n(u".a", u"ab");
    x2(u".a", u"aa", 0, 2);
    x2(u"^a", u"a", 0, 1);
    x2(u"^a$", u"a", 0, 1);
    x2(u"^\\w$", u"a", 0, 1);
    n(u"^\\w$", u" ");
    x2(u"^\\wab$", u"zab", 0, 3);
    x2(u"^\\wabcdef$", u"zabcdef", 0, 7);
    x2(u"^\\w...def$", u"zabcdef", 0, 7);
    x2(u"\\w\\w\\s\\Waaa\\d", u"aa  aaa4", 0, 8);
    x2(u"\\A\\Z", u"", 0, 0);
    x2(u"\\Axyz", u"xyz", 0, 3);
    x2(u"xyz\\Z", u"xyz", 0, 3);
    x2(u"xyz\\z", u"xyz", 0, 3);
    x2(u"a\\Z", u"a", 0, 1);
    x2(u"\\Gaz", u"az", 0, 2);
    n(u"\\Gz", u"bza");
    n(u"az\\G", u"az");
    n(u"az\\A", u"az");
    n(u"a\\Az", u"az");
    x2(u"\\^\\$", u"^$", 0, 2);
    x2(u"^x?y", u"xy", 0, 2);
    x2(u"^(x?y)", u"xy", 0, 2);
    x2(u"\\w", u"_", 0, 1);
    n(u"\\W", u"_");
    x2(u"(?=z)z", u"z", 0, 1);
    n(u"(?=z).", u"a");
    x2(u"(?!z)a", u"a", 0, 1);
    n(u"(?!z)a", u"z");
    x2(u"(?i:a)", u"a", 0, 1);
    x2(u"(?i:a)", u"A", 0, 1);
    x2(u"(?i:A)", u"a", 0, 1);
    n(u"(?i:A)", u"b");
    x2(u"(?i:[A-Z])", u"a", 0, 1);
    x2(u"(?i:[f-m])", u"H", 0, 1);
    x2(u"(?i:[f-m])", u"h", 0, 1);
    n(u"(?i:[f-m])", u"e");
    x2(u"(?i:[A-c])", u"D", 0, 1);
    n(u"(?i:[^a-z])", u"A");
    n(u"(?i:[^a-z])", u"a");
    x2(u"(?i:[!-k])", u"Z", 0, 1);
    x2(u"(?i:[!-k])", u"7", 0, 1);
    x2(u"(?i:[T-}])", u"b", 0, 1);
    x2(u"(?i:[T-}])", u"{", 0, 1);
    x2(u"(?i:\\?a)", u"?A", 0, 2);
    x2(u"(?i:\\*A)", u"*a", 0, 2);
    n(u".", u"\n");
    x2(u"(?m:.)", u"\n", 0, 1);
    x2(u"(?m:a.)", u"a\n", 0, 2);
    x2(u"(?m:.b)", u"a\nb", 1, 3);
    x2(u".*abc", u"dddabdd\nddabc", 8, 13);
    x2(u"(?m:.*abc)", u"dddabddabc", 0, 10);
    n(u"(?i)(?-i)a", u"A");
    n(u"(?i)(?-i:a)", u"A");
    x2(u"a?", u"", 0, 0);
    x2(u"a?", u"b", 0, 0);
    x2(u"a?", u"a", 0, 1);
    x2(u"a*", u"", 0, 0);
    x2(u"a*", u"a", 0, 1);
    x2(u"a*", u"aaa", 0, 3);
    x2(u"a*", u"baaaa", 0, 0);
    n(u"a+", u"");
    x2(u"a+", u"a", 0, 1);
    x2(u"a+", u"aaaa", 0, 4);
    x2(u"a+", u"aabbb", 0, 2);
    x2(u"a+", u"baaaa", 1, 5);
    x2(u".?", u"", 0, 0);
    x2(u".?", u"f", 0, 1);
    x2(u".?", u"\n", 0, 0);
    x2(u".*", u"", 0, 0);
    x2(u".*", u"abcde", 0, 5);
    x2(u".+", u"z", 0, 1);
    x2(u".+", u"zdswer\n", 0, 6);
    x2(u"(.*)a\\1f", u"babfbac", 0, 4);
    x2(u"(.*)a\\1f", u"bacbabf", 3, 7);
    x2(u"((.*)a\\2f)", u"bacbabf", 3, 7);
    x2(u"(.*)a\\1f", u"baczzzzzz\nbazz\nzzzzbabf", 19, 23);
    x2(u"a|b", u"a", 0, 1);
    x2(u"a|b", u"b", 0, 1);
    x2(u"|a", u"a", 0, 0);
    x2(u"(|a)", u"a", 0, 0);
    x2(u"ab|bc", u"ab", 0, 2);
    x2(u"ab|bc", u"bc", 0, 2);
    x2(u"z(?:ab|bc)", u"zbc", 0, 3);
    x2(u"a(?:ab|bc)c", u"aabc", 0, 4);
    x2(u"ab|(?:ac|az)", u"az", 0, 2);
    x2(u"a|b|c", u"dc", 1, 2);
    x2(u"a|b|cd|efg|h|ijk|lmn|o|pq|rstuvwx|yz", u"pqr", 0, 2);
    n(u"a|b|cd|efg|h|ijk|lmn|o|pq|rstuvwx|yz", u"mn");
    x2(u"a|^z", u"ba", 1, 2);
    x2(u"a|^z", u"za", 0, 1);
    x2(u"a|\\Gz", u"bza", 2, 3);
    x2(u"a|\\Gz", u"za", 0, 1);
    x2(u"a|\\Az", u"bza", 2, 3);
    x2(u"a|\\Az", u"za", 0, 1);
    x2(u"a|b\\Z", u"ba", 1, 2);
    x2(u"a|b\\Z", u"b", 0, 1);
    x2(u"a|b\\z", u"ba", 1, 2);
    x2(u"a|b\\z", u"b", 0, 1);
    x2(u"\\w|\\s", u" ", 0, 1);
    n(u"\\w|\\w", u" ");
    x2(u"\\w|%", u"%", 0, 1);
    x2(u"\\w|[&$]", u"&", 0, 1);
    x2(u"[b-d]|[^e-z]", u"a", 0, 1);
    x2(u"(?:a|[c-f])|bz", u"dz", 0, 1);
    x2(u"(?:a|[c-f])|bz", u"bz", 0, 2);
    x2(u"abc|(?=zz)..f", u"zzf", 0, 3);
    x2(u"abc|(?!zz)..f", u"abf", 0, 3);
    x2(u"(?=za)..a|(?=zz)..a", u"zza", 0, 3);
    n(u"(?>a|abd)c", u"abdc");
    x2(u"(?>abd|a)c", u"abdc", 0, 4);
    x2(u"a?|b", u"a", 0, 1);
    x2(u"a?|b", u"b", 0, 0);
    x2(u"a?|b", u"", 0, 0);
    x2(u"a*|b", u"aa", 0, 2);
    x2(u"a*|b*", u"ba", 0, 0);
    x2(u"a*|b*", u"ab", 0, 1);
    x2(u"a+|b*", u"", 0, 0);
    x2(u"a+|b*", u"bbb", 0, 3);
    x2(u"a+|b*", u"abbb", 0, 1);
    n(u"a+|b+", u"");
    x2(u"(a|b)?", u"b", 0, 1);
    x2(u"(a|b)*", u"ba", 0, 2);
    x2(u"(a|b)+", u"bab", 0, 3);
    x2(u"(ab|ca)+", u"caabbc", 0, 4);
    x2(u"(ab|ca)+", u"aabca", 1, 5);
    x2(u"(ab|ca)+", u"abzca", 0, 2);
    x2(u"(a|bab)+", u"ababa", 0, 5);
    x2(u"(a|bab)+", u"ba", 1, 2);
    x2(u"(a|bab)+", u"baaaba", 1, 4);
    x2(u"(?:a|b)(?:a|b)", u"ab", 0, 2);
    x2(u"(?:a*|b*)(?:a*|b*)", u"aaabbb", 0, 3);
    x2(u"(?:a*|b*)(?:a+|b+)", u"aaabbb", 0, 6);
    x2(u"(?:a+|b+){2}", u"aaabbb", 0, 6);
    x2(u"h{0,}", u"hhhh", 0, 4);
    x2(u"(?:a+|b+){1,2}", u"aaabbb", 0, 6);
    n(u"ax{2}*a", u"0axxxa1");
    n(u"a.{0,2}a", u"0aXXXa0");
    n(u"a.{0,2}?a", u"0aXXXa0");
    n(u"a.{0,2}?a", u"0aXXXXa0");
    x2(u"^a{2,}?a$", u"aaa", 0, 3);
    x2(u"^[a-z]{2,}?$", u"aaa", 0, 3);
    x2(u"(?:a+|\\Ab*)cc", u"cc", 0, 2);
    n(u"(?:a+|\\Ab*)cc", u"abcc");
    x2(u"(?:^a+|b+)*c", u"aabbbabc", 6, 8);
    x2(u"(?:^a+|b+)*c", u"aabbbbc", 0, 7);
    x2(u"a|(?i)c", u"C", 0, 1);
    x2(u"(?i)c|a", u"C", 0, 1);
    x2(u"(?i)c|a", u"A", 0, 1);
    x2(u"(?i:c)|a", u"C", 0, 1);
    n(u"(?i:c)|a", u"A");
    x2(u"[abc]?", u"abc", 0, 1);
    x2(u"[abc]*", u"abc", 0, 3);
    x2(u"[^abc]*", u"abc", 0, 0);
    n(u"[^abc]+", u"abc");
    x2(u"a??", u"aaa", 0, 0);
    x2(u"ba??b", u"bab", 0, 3);
    x2(u"a*?", u"aaa", 0, 0);
    x2(u"ba*?", u"baa", 0, 1);
    x2(u"ba*?b", u"baab", 0, 4);
    x2(u"a+?", u"aaa", 0, 1);
    x2(u"ba+?", u"baa", 0, 2);
    x2(u"ba+?b", u"baab", 0, 4);
    x2(u"(?:a?)??", u"a", 0, 0);
    x2(u"(?:a??)?", u"a", 0, 0);
    x2(u"(?:a?)+?", u"aaa", 0, 1);
    x2(u"(?:a+)??", u"aaa", 0, 0);
    x2(u"(?:a+)??b", u"aaab", 0, 4);
    x2(u"(?:ab)?{2}", u"", 0, 0);
    x2(u"(?:ab)?{2}", u"ababa", 0, 4);
    x2(u"(?:ab)*{0}", u"ababa", 0, 0);
    x2(u"(?:ab){3,}", u"abababab", 0, 8);
    n(u"(?:ab){3,}", u"abab");
    x2(u"(?:ab){2,4}", u"ababab", 0, 6);
    x2(u"(?:ab){2,4}", u"ababababab", 0, 8);
    x2(u"(?:ab){2,4}?", u"ababababab", 0, 4);
    x2(u"(?:ab){,}", u"ab{,}", 0, 5);
    x2(u"(?:abc)+?{2}", u"abcabcabc", 0, 6);
    x2(u"(?:X*)(?i:xa)", u"XXXa", 0, 4);
    x2(u"(d+)([^abc]z)", u"dddz", 0, 4);
    x2(u"([^abc]*)([^abc]z)", u"dddz", 0, 4);
    x2(u"(\\w+)(\\wz)", u"dddz", 0, 4);
    x3(u"(a)", u"a", 0, 1, 1);
    x3(u"(ab)", u"ab", 0, 2, 1);
    x2(u"((ab))", u"ab", 0, 2);
    x3(u"((ab))", u"ab", 0, 2, 1);
    x3(u"((ab))", u"ab", 0, 2, 2);
    x3(u"((((((((((((((((((((ab))))))))))))))))))))", u"ab", 0, 2, 20);
    x3(u"(ab)(cd)", u"abcd", 0, 2, 1);
    x3(u"(ab)(cd)", u"abcd", 2, 4, 2);
    x3(u"()(a)bc(def)ghijk", u"abcdefghijk", 3, 6, 3);
    x3(u"(()(a)bc(def)ghijk)", u"abcdefghijk", 3, 6, 4);
    x2(u"(^a)", u"a", 0, 1);
    x3(u"(a)|(a)", u"ba", 1, 2, 1);
    x3(u"(^a)|(a)", u"ba", 1, 2, 2);
    x3(u"(a?)", u"aaa", 0, 1, 1);
    x3(u"(a*)", u"aaa", 0, 3, 1);
    x3(u"(a*)", u"", 0, 0, 1);
    x3(u"(a+)", u"aaaaaaa", 0, 7, 1);
    x3(u"(a+|b*)", u"bbbaa", 0, 3, 1);
    x3(u"(a+|b?)", u"bbbaa", 0, 1, 1);
    x3(u"(abc)?", u"abc", 0, 3, 1);
    x3(u"(abc)*", u"abc", 0, 3, 1);
    x3(u"(abc)+", u"abc", 0, 3, 1);
    x3(u"(xyz|abc)+", u"abc", 0, 3, 1);
    x3(u"([xyz][abc]|abc)+", u"abc", 0, 3, 1);
    x3(u"((?i:abc))", u"AbC", 0, 3, 1);
    x2(u"(abc)(?i:\\1)", u"abcABC", 0, 6);
    x3(u"((?m:a.c))", u"a\nc", 0, 3, 1);
    x3(u"((?=az)a)", u"azb", 0, 1, 1);
    x3(u"abc|(.abd)", u"zabd", 0, 4, 1);
    x2(u"(?:abc)|(ABC)", u"abc", 0, 3);
    x3(u"(?i:(abc))|(zzz)", u"ABC", 0, 3, 1);
    x3(u"a*(.)", u"aaaaz", 4, 5, 1);
    x3(u"a*?(.)", u"aaaaz", 0, 1, 1);
    x3(u"a*?(c)", u"aaaac", 4, 5, 1);
    x3(u"[bcd]a*(.)", u"caaaaz", 5, 6, 1);
    x3(u"(\\Abb)cc", u"bbcc", 0, 2, 1);
    n(u"(\\Abb)cc", u"zbbcc");
    x3(u"(^bb)cc", u"bbcc", 0, 2, 1);
    n(u"(^bb)cc", u"zbbcc");
    x3(u"cc(bb$)", u"ccbb", 2, 4, 1);
    n(u"cc(bb$)", u"ccbbb");
    n(u"(\\1)", u"");
    n(u"\\1(a)", u"aa");
    n(u"(a(b)\\1)\\2+", u"ababb");
    n(u"(?:(?:\\1|z)(a))+$", u"zaa");
    x2(u"(?:(?:\\1|z)(a))+$", u"zaaa", 0, 4);
    x2(u"(a)(?=\\1)", u"aa", 0, 1);
    n(u"(a)$|\\1", u"az");
    x2(u"(a)\\1", u"aa", 0, 2);
    n(u"(a)\\1", u"ab");
    x2(u"(a?)\\1", u"aa", 0, 2);
    x2(u"(a??)\\1", u"aa", 0, 0);
    x2(u"(a*)\\1", u"aaaaa", 0, 4);
    x3(u"(a*)\\1", u"aaaaa", 0, 2, 1);
    x2(u"a(b*)\\1", u"abbbb", 0, 5);
    x2(u"a(b*)\\1", u"ab", 0, 1);
    x2(u"(a*)(b*)\\1\\2", u"aaabbaaabb", 0, 10);
    x2(u"(a*)(b*)\\2", u"aaabbbb", 0, 7);
    x2(u"(((((((a*)b))))))c\\7", u"aaabcaaa", 0, 8);
    x3(u"(((((((a*)b))))))c\\7", u"aaabcaaa", 0, 3, 7);
    x2(u"(a)(b)(c)\\2\\1\\3", u"abcbac", 0, 6);
    x2(u"([a-d])\\1", u"cc", 0, 2);
    x2(u"(\\w\\d\\s)\\1", u"f5 f5 ", 0, 6);
    n(u"(\\w\\d\\s)\\1", u"f5 f5");
    x2(u"(who|[a-c]{3})\\1", u"whowho", 0, 6);
    x2(u"...(who|[a-c]{3})\\1", u"abcwhowho", 0, 9);
    x2(u"(who|[a-c]{3})\\1", u"cbccbc", 0, 6);
    x2(u"(^a)\\1", u"aa", 0, 2);
    n(u"(^a)\\1", u"baa");
    n(u"(a$)\\1", u"aa");
    n(u"(ab\\Z)\\1", u"ab");
    x2(u"(a*\\Z)\\1", u"a", 1, 1);
    x2(u".(a*\\Z)\\1", u"ba", 1, 2);
    x3(u"(.(abc)\\2)", u"zabcabc", 0, 7, 1);
    x3(u"(.(..\\d.)\\2)", u"z12341234", 0, 9, 1);
    x2(u"((?i:az))\\1", u"AzAz", 0, 4);
    n(u"((?i:az))\\1", u"Azaz");
    x2(u"(?<=a)b", u"ab", 1, 2);
    n(u"(?<=a)b", u"bb");
    x2(u"(?<=a|b)b", u"bb", 1, 2);
    x2(u"(?<=a|bc)b", u"bcb", 2, 3);
    x2(u"(?<=a|bc)b", u"ab", 1, 2);
    x2(u"(?<=a|bc||defghij|klmnopq|r)z", u"rz", 1, 2);
    x2(u"(a)\\g<1>", u"aa", 0, 2);
    x2(u"(?<!a)b", u"cb", 1, 2);
    n(u"(?<!a)b", u"ab");
    x2(u"(?<!a|bc)b", u"bbb", 0, 1);
    n(u"(?<!a|bc)z", u"bcz");
    x2(u"(?<name1>a)", u"a", 0, 1);
    x2(u"(?<name_2>ab)\\g<name_2>", u"abab", 0, 4);
    x2(u"(?<name_3>.zv.)\\k<name_3>", u"azvbazvb", 0, 8);
    x2(u"(?<=\\g<ab>)|-\\zEND (?<ab>XyZ)", u"XyZ", 3, 3);
    x2(u"(?<n>|a\\g<n>)+", u"", 0, 0);
    x2(u"(?<n>|\\(\\g<n>\\))+$", u"()(())", 0, 6);
    x3(u"\\g<n>(?<n>.){0}", u"X", 0, 1, 1);
    x2(u"\\g<n>(abc|df(?<n>.YZ){2,8}){0}", u"XYZ", 0, 3);
    x2(u"\\A(?<n>(a\\g<n>)|)\\z", u"aaaa", 0, 4);
    x2(u"(?<n>|\\g<m>\\g<n>)\\z|\\zEND (?<m>a|(b)\\g<m>)", u"bbbbabba", 0, 8);
    x2(u"(?<name1240>\\w+\\sx)a+\\k<name1240>", u"  fg xaaaaaaaafg x", 2, 18);
    x3(u"(z)()()(?<_9>a)\\g<_9>", u"zaa", 2, 3, 1);
    x2(u"(.)(((?<_>a)))\\k<_>", u"zaa", 0, 3);
    x2(u"((?<name1>\\d)|(?<name2>\\w))(\\k<name1>|\\k<name2>)", u"ff", 0, 2);
    x2(u"(?:(?<x>)|(?<x>efg))\\k<x>", u"", 0, 0);
    x2(u"(?:(?<x>abc)|(?<x>efg))\\k<x>", u"abcefgefg", 3, 9);
    n(u"(?:(?<x>abc)|(?<x>efg))\\k<x>", u"abcefg");
    x2(u"(?:(?<n1>.)|(?<n1>..)|(?<n1>...)|(?<n1>....)|(?<n1>.....)|(?<n1>......)|(?<n1>.......)|(?<n1>........)|(?<n1>.........)|(?<n1>..........)|(?<n1>...........)|(?<n1>............)|(?<n1>.............)|(?<n1>..............))\\k<n1>$", u"a-pyumpyum", 2, 10);
    x3(u"(?:(?<n1>.)|(?<n1>..)|(?<n1>...)|(?<n1>....)|(?<n1>.....)|(?<n1>......)|(?<n1>.......)|(?<n1>........)|(?<n1>.........)|(?<n1>..........)|(?<n1>...........)|(?<n1>............)|(?<n1>.............)|(?<n1>..............))\\k<n1>$", u"xxxxabcdefghijklmnabcdefghijklmn", 4, 18, 14);
    x3(u"(?<name1>)(?<name2>)(?<name3>)(?<name4>)(?<name5>)(?<name6>)(?<name7>)(?<name8>)(?<name9>)(?<name10>)(?<name11>)(?<name12>)(?<name13>)(?<name14>)(?<name15>)(?<name16>aaa)(?<name17>)$", u"aaa", 0, 3, 16);
    x2(u"(?<foo>a|\\(\\g<foo>\\))", u"a", 0, 1);
    x2(u"(?<foo>a|\\(\\g<foo>\\))", u"((((((a))))))", 0, 13);
    x3(u"(?<foo>a|\\(\\g<foo>\\))", u"((((((((a))))))))", 0, 17, 1);
    x2(u"\\g<bar>|\\zEND(?<bar>.*abc$)", u"abcxxxabc", 0, 9);
    x2(u"\\g<1>|\\zEND(.a.)", u"bac", 0, 3);
    x3(u"\\g<_A>\\g<_A>|\\zEND(.a.)(?<_A>.b.)", u"xbxyby", 3, 6, 1);
    x2(u"\\A(?:\\g<pon>|\\g<pan>|\\zEND  (?<pan>a|c\\g<pon>c)(?<pon>b|d\\g<pan>d))$", u"cdcbcdc", 0, 7);
    x2(u"\\A(?<n>|a\\g<m>)\\z|\\zEND (?<m>\\g<n>)", u"aaaa", 0, 4);
    x2(u"(?<n>(a|b\\g<n>c){3,5})", u"baaaaca", 1, 5);
    x2(u"(?<n>(a|b\\g<n>c){3,5})", u"baaaacaaaaa", 0, 10);
    x2(u"(?<pare>\\(([^\\(\\)]++|\\g<pare>)*+\\))", u"((a))", 0, 5);
    x2(u"()*\\1", u"", 0, 0);
    x2(u"(?:()|())*\\1\\2", u"", 0, 0);
    x3(u"(?:\\1a|())*", u"a", 0, 0, 1);
    x2(u"x((.)*)*x", u"0x1x2x3", 1, 6);
    x2(u"x((.)*)*x(?i:\\1)\\Z", u"0x1x2x1X2", 1, 9);
    x2(u"(?:()|()|()|()|()|())*\\2\\5", u"", 0, 0);
    x2(u"(?:()|()|()|(x)|()|())*\\2b\\5", u"b", 0, 1);
    if onig_encoding == onig.ONIG_ENCODING_UTF16_LE:
        x2(u"\\xFA\\x8F", u"\u8ffa", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_UTF16_BE:
        x2(u"\\x8F\\xFA", u"\u8ffa", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_UTF8:
        x2(u"\\xE8\\xBF\\xBA", u"\u8ffa", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_SJIS:
        x2(u"\\xE7\\x92", u"\u8ffa", 0, 1);
    elif onig_encoding == onig.ONIG_ENCODING_EUC_JP:
        x2(u"\\xED\\xF2", u"\u8ffa", 0, 1); # "迺"
    x2(u"", u"あ", 0, 0);
    x2(u"あ", u"あ", 0, 1);
    n(u"い", u"あ");
    x2(u"うう", u"うう", 0, 2);
    x2(u"あいう", u"あいう", 0, 3);
    x2(u"こここここここここここここここここここここここここここここここここここ", u"こここここここここここここここここここここここここここここここここここ", 0, 35);
    x2(u"あ", u"いあ", 1, 2);
    x2(u"いう", u"あいう", 1, 3);
#    x2(b"\\xca\\xb8", b"\xca\xb8", 0, 2);   # "文"
    x2(u".", u"あ", 0, 1);
    x2(u"..", u"かき", 0, 2);
    x2(u"\\w", u"お", 0, 1);
    n(u"\\W", u"あ");
    x2(u"[\\W]", u"う$", 1, 2);
    x2(u"\\S", u"そ", 0, 1);
    x2(u"\\S", u"漢", 0, 1);
    x2(u"\\b", u"気 ", 0, 0);
    x2(u"\\b", u" ほ", 1, 1);
    x2(u"\\B", u"せそ ", 1, 1);
    x2(u"\\B", u"う ", 2, 2);
    x2(u"\\B", u" い", 0, 0);
    x2(u"[たち]", u"ち", 0, 1);
    n(u"[なに]", u"ぬ");
    x2(u"[う-お]", u"え", 0, 1);
    n(u"[^け]", u"け");
    x2(u"[\\w]", u"ね", 0, 1);
    n(u"[\\d]", u"ふ");
    x2(u"[\\D]", u"は", 0, 1);
    n(u"[\\s]", u"く");
    x2(u"[\\S]", u"へ", 0, 1);
    x2(u"[\\w\\d]", u"よ", 0, 1);
    x2(u"[\\w\\d]", u"   よ", 3, 4);
    n(u"\\w鬼車", u" 鬼車");
    x2(u"鬼\\W車", u"鬼 車", 0, 3);
    x2(u"あ.い.う", u"ああいいう", 0, 5);
    x2(u".\\wう\\W..ぞ", u"えうう うぞぞ", 0, 7);
    x2(u"\\s\\wこここ", u" ここここ", 0, 5);
    x2(u"ああ.け", u"ああけけ", 0, 4);
    n(u".い", u"いえ");
    x2(u".お", u"おお", 0, 2);
    x2(u"^あ", u"あ", 0, 1);
    x2(u"^む$", u"む", 0, 1);
    x2(u"^\\w$", u"に", 0, 1);
    x2(u"^\\wかきくけこ$", u"zかきくけこ", 0, 6);
    x2(u"^\\w...うえお$", u"zあいううえお", 0, 7);
    x2(u"\\w\\w\\s\\Wおおお\\d", u"aお  おおお4", 0, 8);
    x2(u"\\Aたちつ", u"たちつ", 0, 3);
    x2(u"むめも\\Z", u"むめも", 0, 3);
    x2(u"かきく\\z", u"かきく", 0, 3);
    x2(u"かきく\\Z", u"かきく\n", 0, 3);
    x2(u"\\Gぽぴ", u"ぽぴ", 0, 2);
    n(u"\\Gえ", u"うえお");
    n(u"とて\\G", u"とて");
    n(u"まみ\\A", u"まみ");
    n(u"ま\\Aみ", u"まみ");
    x2(u"(?=せ)せ", u"せ", 0, 1);
    n(u"(?=う).", u"い");
    x2(u"(?!う)か", u"か", 0, 1);
    n(u"(?!と)あ", u"と");
    x2(u"(?i:あ)", u"あ", 0, 1);
    x2(u"(?i:ぶべ)", u"ぶべ", 0, 2);
    n(u"(?i:い)", u"う");
    x2(u"(?m:よ.)", u"よ\n", 0, 2);
    x2(u"(?m:.め)", u"ま\nめ", 1, 3);
    x2(u"あ?", u"", 0, 0);
    x2(u"変?", u"化", 0, 0);
    x2(u"変?", u"変", 0, 1);
    x2(u"量*", u"", 0, 0);
    x2(u"量*", u"量", 0, 1);
    x2(u"子*", u"子子子", 0, 3);
    x2(u"馬*", u"鹿馬馬馬馬", 0, 0);
    n(u"山+", u"");
    x2(u"河+", u"河", 0, 1);
    x2(u"時+", u"時時時時", 0, 4);
    x2(u"え+", u"ええううう", 0, 2);
    x2(u"う+", u"おうううう", 1, 5);
    x2(u".?", u"た", 0, 1);
    x2(u".*", u"ぱぴぷぺ", 0, 4);
    x2(u".+", u"ろ", 0, 1);
    x2(u".+", u"いうえか\n", 0, 4);
    x2(u"あ|い", u"あ", 0, 1);
    x2(u"あ|い", u"い", 0, 1);
    x2(u"あい|いう", u"あい", 0, 2);
    x2(u"あい|いう", u"いう", 0, 2);
    x2(u"を(?:かき|きく)", u"をかき", 0, 3);
    x2(u"を(?:かき|きく)け", u"をきくけ", 0, 4);
    x2(u"あい|(?:あう|あを)", u"あを", 0, 2);
    x2(u"あ|い|う", u"えう", 1, 2);
    x2(u"あ|い|うえ|おかき|く|けこさ|しすせ|そ|たち|つてとなに|ぬね", u"しすせ", 0, 3);
    n(u"あ|い|うえ|おかき|く|けこさ|しすせ|そ|たち|つてとなに|ぬね", u"すせ");
    x2(u"あ|^わ", u"ぶあ", 1, 2);
    x2(u"あ|^を", u"をあ", 0, 1);
    x2(u"鬼|\\G車", u"け車鬼", 2, 3);
    x2(u"鬼|\\G車", u"車鬼", 0, 1);
    x2(u"鬼|\\A車", u"b車鬼", 2, 3);
    x2(u"鬼|\\A車", u"車", 0, 1);
    x2(u"鬼|車\\Z", u"車鬼", 1, 2);
    x2(u"鬼|車\\Z", u"車", 0, 1);
    x2(u"鬼|車\\Z", u"車\n", 0, 1);
    x2(u"鬼|車\\z", u"車鬼", 1, 2);
    x2(u"鬼|車\\z", u"車", 0, 1);
    x2(u"\\w|\\s", u"お", 0, 1);
    x2(u"\\w|%", u"%お", 0, 1);
    x2(u"\\w|[&$]", u"う&", 0, 1);
    x2(u"[い-け]", u"う", 0, 1);
    x2(u"[い-け]|[^か-こ]", u"あ", 0, 1);
    x2(u"[い-け]|[^か-こ]", u"か", 0, 1);
    x2(u"[^あ]", u"\n", 0, 1);
    x2(u"(?:あ|[う-き])|いを", u"うを", 0, 1);
    x2(u"(?:あ|[う-き])|いを", u"いを", 0, 2);
    x2(u"あいう|(?=けけ)..ほ", u"けけほ", 0, 3);
    x2(u"あいう|(?!けけ)..ほ", u"あいほ", 0, 3);
    x2(u"(?=をあ)..あ|(?=をを)..あ", u"ををあ", 0, 3);
    x2(u"(?<=あ|いう)い", u"いうい", 2, 3);
    n(u"(?>あ|あいえ)う", u"あいえう");
    x2(u"(?>あいえ|あ)う", u"あいえう", 0, 4);
    x2(u"あ?|い", u"あ", 0, 1);
    x2(u"あ?|い", u"い", 0, 0);
    x2(u"あ?|い", u"", 0, 0);
    x2(u"あ*|い", u"ああ", 0, 2);
    x2(u"あ*|い*", u"いあ", 0, 0);
    x2(u"あ*|い*", u"あい", 0, 1);
    x2(u"[aあ]*|い*", u"aあいいい", 0, 2);
    x2(u"あ+|い*", u"", 0, 0);
    x2(u"あ+|い*", u"いいい", 0, 3);
    x2(u"あ+|い*", u"あいいい", 0, 1);
    x2(u"あ+|い*", u"aあいいい", 0, 0);
    n(u"あ+|い+", u"");
    x2(u"(あ|い)?", u"い", 0, 1);
    x2(u"(あ|い)*", u"いあ", 0, 2);
    x2(u"(あ|い)+", u"いあい", 0, 3);
    x2(u"(あい|うあ)+", u"うああいうえ", 0, 4);
    x2(u"(あい|うえ)+", u"うああいうえ", 2, 6);
    x2(u"(あい|うあ)+", u"ああいうあ", 1, 5);
    x2(u"(あい|うあ)+", u"あいをうあ", 0, 2);
    x2(u"(あい|うあ)+", u"$$zzzzあいをうあ", 6, 8);
    x2(u"(あ|いあい)+", u"あいあいあ", 0, 5);
    x2(u"(あ|いあい)+", u"いあ", 1, 2);
    x2(u"(あ|いあい)+", u"いあああいあ", 1, 4);
    x2(u"(?:あ|い)(?:あ|い)", u"あい", 0, 2);
    x2(u"(?:あ*|い*)(?:あ*|い*)", u"あああいいい", 0, 3);
    x2(u"(?:あ*|い*)(?:あ+|い+)", u"あああいいい", 0, 6);
    x2(u"(?:あ+|い+){2}", u"あああいいい", 0, 6);
    x2(u"(?:あ+|い+){1,2}", u"あああいいい", 0, 6);
    x2(u"(?:あ+|\\Aい*)うう", u"うう", 0, 2);
    n(u"(?:あ+|\\Aい*)うう", u"あいうう");
    x2(u"(?:^あ+|い+)*う", u"ああいいいあいう", 6, 8);
    x2(u"(?:^あ+|い+)*う", u"ああいいいいう", 0, 7);
    x2(u"う{0,}", u"うううう", 0, 4);
    x2(u"あ|(?i)c", u"C", 0, 1);
    x2(u"(?i)c|あ", u"C", 0, 1);
    x2(u"(?i:あ)|a", u"a", 0, 1);
    n(u"(?i:あ)|a", u"A");
    x2(u"[あいう]?", u"あいう", 0, 1);
    x2(u"[あいう]*", u"あいう", 0, 3);
    x2(u"[^あいう]*", u"あいう", 0, 0);
    n(u"[^あいう]+", u"あいう");
    x2(u"あ??", u"あああ", 0, 0);
    x2(u"いあ??い", u"いあい", 0, 3);
    x2(u"あ*?", u"あああ", 0, 0);
    x2(u"いあ*?", u"いああ", 0, 1);
    x2(u"いあ*?い", u"いああい", 0, 4);
    x2(u"あ+?", u"あああ", 0, 1);
    x2(u"いあ+?", u"いああ", 0, 2);
    x2(u"いあ+?い", u"いああい", 0, 4);
    x2(u"(?:天?)??", u"天", 0, 0);
    x2(u"(?:天??)?", u"天", 0, 0);
    x2(u"(?:夢?)+?", u"夢夢夢", 0, 1);
    x2(u"(?:風+)??", u"風風風", 0, 0);
    x2(u"(?:雪+)??霜", u"雪雪雪霜", 0, 4);
    x2(u"(?:あい)?{2}", u"", 0, 0);
    x2(u"(?:鬼車)?{2}", u"鬼車鬼車鬼", 0, 4);
    x2(u"(?:鬼車)*{0}", u"鬼車鬼車鬼", 0, 0);
    x2(u"(?:鬼車){3,}", u"鬼車鬼車鬼車鬼車", 0, 8);
    n(u"(?:鬼車){3,}", u"鬼車鬼車");
    x2(u"(?:鬼車){2,4}", u"鬼車鬼車鬼車", 0, 6);
    x2(u"(?:鬼車){2,4}", u"鬼車鬼車鬼車鬼車鬼車", 0, 8);
    x2(u"(?:鬼車){2,4}?", u"鬼車鬼車鬼車鬼車鬼車", 0, 4);
    x2(u"(?:鬼車){,}", u"鬼車{,}", 0, 5);
    x2(u"(?:かきく)+?{2}", u"かきくかきくかきく", 0, 6);
    x3(u"(火)", u"火", 0, 1, 1);
    x3(u"(火水)", u"火水", 0, 2, 1);
    x2(u"((時間))", u"時間", 0, 2);
    x3(u"((風水))", u"風水", 0, 2, 1);
    x3(u"((昨日))", u"昨日", 0, 2, 2);
    x3(u"((((((((((((((((((((量子))))))))))))))))))))", u"量子", 0, 2, 20);
    x3(u"(あい)(うえ)", u"あいうえ", 0, 2, 1);
    x3(u"(あい)(うえ)", u"あいうえ", 2, 4, 2);
    x3(u"()(あ)いう(えおか)きくけこ", u"あいうえおかきくけこ", 3, 6, 3);
    x3(u"(()(あ)いう(えおか)きくけこ)", u"あいうえおかきくけこ", 3, 6, 4);
    x3(u".*(フォ)ン・マ(ン()シュタ)イン", u"フォン・マンシュタイン", 5, 9, 2);
    x2(u"(^あ)", u"あ", 0, 1);
    x3(u"(あ)|(あ)", u"いあ", 1, 2, 1);
    x3(u"(^あ)|(あ)", u"いあ", 1, 2, 2);
    x3(u"(あ?)", u"あああ", 0, 1, 1);
    x3(u"(ま*)", u"ままま", 0, 3, 1);
    x3(u"(と*)", u"", 0, 0, 1);
    x3(u"(る+)", u"るるるるるるる", 0, 7, 1);
    x3(u"(ふ+|へ*)", u"ふふふへへ", 0, 3, 1);
    x3(u"(あ+|い?)", u"いいいああ", 0, 1, 1);
    x3(u"(あいう)?", u"あいう", 0, 3, 1);
    x3(u"(あいう)*", u"あいう", 0, 3, 1);
    x3(u"(あいう)+", u"あいう", 0, 3, 1);
    x3(u"(さしす|あいう)+", u"あいう", 0, 3, 1);
    x3(u"([なにぬ][かきく]|かきく)+", u"かきく", 0, 3, 1);
    x3(u"((?i:あいう))", u"あいう", 0, 3, 1);
    x3(u"((?m:あ.う))", u"あ\nう", 0, 3, 1);
    x3(u"((?=あん)あ)", u"あんい", 0, 1, 1);
    x3(u"あいう|(.あいえ)", u"んあいえ", 0, 4, 1);
    x3(u"あ*(.)", u"ああああん", 4, 5, 1);
    x3(u"あ*?(.)", u"ああああん", 0, 1, 1);
    x3(u"あ*?(ん)", u"ああああん", 4, 5, 1);
    x3(u"[いうえ]あ*(.)", u"えああああん", 5, 6, 1);
    x3(u"(\\Aいい)うう", u"いいうう", 0, 2, 1);
    n(u"(\\Aいい)うう", u"んいいうう");
    x3(u"(^いい)うう", u"いいうう", 0, 2, 1);
    n(u"(^いい)うう", u"んいいうう");
    x3(u"ろろ(るる$)", u"ろろるる", 2, 4, 1);
    n(u"ろろ(るる$)", u"ろろるるる");
    x2(u"(無)\\1", u"無無", 0, 2);
    n(u"(無)\\1", u"無武");
    x2(u"(空?)\\1", u"空空", 0, 2);
    x2(u"(空??)\\1", u"空空", 0, 0);
    x2(u"(空*)\\1", u"空空空空空", 0, 4);
    x3(u"(空*)\\1", u"空空空空空", 0, 2, 1);
    x2(u"あ(い*)\\1", u"あいいいい", 0, 5);
    x2(u"あ(い*)\\1", u"あい", 0, 1);
    x2(u"(あ*)(い*)\\1\\2", u"あああいいあああいい", 0, 10);
    x2(u"(あ*)(い*)\\2", u"あああいいいい", 0, 7);
    x3(u"(あ*)(い*)\\2", u"あああいいいい", 3, 5, 2);
    x2(u"(((((((ぽ*)ぺ))))))ぴ\\7", u"ぽぽぽぺぴぽぽぽ", 0, 8);
    x3(u"(((((((ぽ*)ぺ))))))ぴ\\7", u"ぽぽぽぺぴぽぽぽ", 0, 3, 7);
    x2(u"(は)(ひ)(ふ)\\2\\1\\3", u"はひふひはふ", 0, 6);
    x2(u"([き-け])\\1", u"くく", 0, 2);
    x2(u"(\\w\\d\\s)\\1", u"あ5 あ5 ", 0, 6);
    n(u"(\\w\\d\\s)\\1", u"あ5 あ5");
    x2(u"(誰？|[あ-う]{3})\\1", u"誰？誰？", 0, 4);
    x2(u"...(誰？|[あ-う]{3})\\1", u"あaあ誰？誰？", 0, 7);
    x2(u"(誰？|[あ-う]{3})\\1", u"ういうういう", 0, 6);
    x2(u"(^こ)\\1", u"ここ", 0, 2);
    n(u"(^む)\\1", u"めむむ");
    n(u"(あ$)\\1", u"ああ");
    n(u"(あい\\Z)\\1", u"あい");
    x2(u"(あ*\\Z)\\1", u"あ", 1, 1);
    x2(u".(あ*\\Z)\\1", u"いあ", 1, 2);
    x3(u"(.(やいゆ)\\2)", u"zやいゆやいゆ", 0, 7, 1);
    x3(u"(.(..\\d.)\\2)", u"あ12341234", 0, 9, 1);
    x2(u"((?i:あvず))\\1", u"あvずあvず", 0, 6);
    x2(u"(?<愚か>変|\\(\\g<愚か>\\))", u"((((((変))))))", 0, 13);
    x2(u"\\A(?:\\g<阿_1>|\\g<云_2>|\\z終了  (?<阿_1>観|自\\g<云_2>自)(?<云_2>在|菩薩\\g<阿_1>菩薩))$", u"菩薩自菩薩自在自菩薩自菩薩", 0, 13);
    x2(u"[[ひふ]]", u"ふ", 0, 1);
    x2(u"[[いおう]か]", u"か", 0, 1);
    n(u"[[^あ]]", u"あ");
    n(u"[^[あ]]", u"あ");
    x2(u"[^[^あ]]", u"あ", 0, 1);
    x2(u"[[かきく]&&きく]", u"く", 0, 1);
    n(u"[[かきく]&&きく]", u"か");
    n(u"[[かきく]&&きく]", u"け");
    x2(u"[あ-ん&&い-を&&う-ゑ]", u"ゑ", 0, 1);
    n(u"[^あ-ん&&い-を&&う-ゑ]", u"ゑ");
    x2(u"[[^あ&&あ]&&あ-ん]", u"い", 0, 1);
    n(u"[[^あ&&あ]&&あ-ん]", u"あ");
    x2(u"[[^あ-ん&&いうえお]&&[^う-か]]", u"き", 0, 1);
    n(u"[[^あ-ん&&いうえお]&&[^う-か]]", u"い");
    x2(u"[^[^あいう]&&[^うえお]]", u"う", 0, 1);
    x2(u"[^[^あいう]&&[^うえお]]", u"え", 0, 1);
    n(u"[^[^あいう]&&[^うえお]]", u"か");
    x2(u"[あ-&&-あ]", u"-", 0, 1);
    x2(u"[^[^a-zあいう]&&[^bcdefgうえお]q-w]", u"え", 0, 1);
    x2(u"[^[^a-zあいう]&&[^bcdefgうえお]g-w]", u"f", 0, 1);
    x2(u"[^[^a-zあいう]&&[^bcdefgうえお]g-w]", u"g", 0, 1);
    n(u"[^[^a-zあいう]&&[^bcdefgうえお]g-w]", u"2");
    x2(u"a<b>バージョンのダウンロード<\\/b>", u"a<b>バージョンのダウンロード</b>", 0, 20);
    x2(u".<b>バージョンのダウンロード<\\/b>", u"a<b>バージョンのダウンロード</b>", 0, 20);
    
    
    # additional test patterns
    if (onig_encoding == onig.ONIG_ENCODING_UTF16_LE or
            onig_encoding == onig.ONIG_ENCODING_UTF16_BE or
            onig_encoding == onig.ONIG_ENCODING_UTF8):
        x2(u"\\x{3042}\\x{3044}", u"あい", 0, 2)
    elif onig_encoding == onig.ONIG_ENCODING_SJIS:
        x2(u"\\x{82a0}\\x{82A2}", u"あい", 0, 2)
    elif onig_encoding == onig.ONIG_ENCODING_EUC_JP:
        x2(u"\\x{a4a2}\\x{A4A4}", u"あい", 0, 2)
    x2(u"\\p{Hiragana}\\p{Katakana}", u"あイ", 0, 2)
    x2(u"(?m)^A.B$", u"X\nA\nB\nZ", 2, 5)
    n(u"(?<!(?<=a)b|c)d", u"abd")
    n(u"(?<!(?<=a)b|c)d", u"cd")
    x2(u"(?<!(?<=a)b|c)d", u"bd", 1, 2)
    x2(u"(a){2}z", u"aaz", 0, 3)
    x2(u"(?<=a).*b", u"aab", 1, 3)
    x2(u"(?<=(?<!A)B)C", u"BBC", 2, 3)
    n(u"(?<=(?<!A)B)C", u"ABC")
    n(u"(?i)(?<!aa|b)c", u"Aac")
    n(u"(?i)(?<!b|aa)c", u"Aac")
    x2(u"a\\b?a", u"aa", 0, 2)
    x2(u"[^x]*x", u"aaax", 0, 4)
    x2(u"(?i)[\\x{0}-B]+", u"\x00\x01\x02\x1f\x20@AaBbC", 0, 10)
    
    # character classes (tests for character class optimization)
    x2(u"[@][a]", u"@a", 0, 2);
    x2(u".*[a][b][c][d][e]", u"abcde", 0, 5);
    x2(u"(?i)[A\\x{41}]", u"a", 0, 1);
    x2(u"[abA]", u"a", 0, 1);
    x2(u"[[ab]&&[ac]]+", u"aaa", 0, 3);
    x2(u"[[あい]&&[あう]]+", u"あああ", 0, 3);
    
    # possessive quantifiers
    n(u"a?+a", u"a")
    n(u"a*+a", u"aaaa")
    n(u"a++a", u"aaaa")
#    n(u"a{2,3}+a", u"aaa")    # ONIG_SYNTAX_DEFAULT doesn't support this
    
    # linebreak
    x2(u"\\R", u"\n", 0, 1)
    x2(u"\\R", u"\r", 0, 1)
    x2(u"\\R{3}", u"\r\r\n\n", 0, 4)
    
#    if (onig_encoding == onig.ONIG_ENCODING_UTF16_LE or
#            onig_encoding == onig.ONIG_ENCODING_UTF16_BE or
#            onig_encoding == onig.ONIG_ENCODING_UTF8):
#        # USE_UNICODE_ALL_LINE_TERMINATORS must be defined
#        x2(u"\\R", u"\u0085", 0, 1)
#        x2(u"\\R", u"\u2028", 0, 1)
#        x2(u"\\R", u"\u2029", 0, 1)
    
    # extended grapheme cluster
    x2(u"\\X{5}", u"あいab\n", 0, 5)
    if (onig_encoding == onig.ONIG_ENCODING_UTF16_LE or
            onig_encoding == onig.ONIG_ENCODING_UTF16_BE or
            onig_encoding == onig.ONIG_ENCODING_UTF8):
        try:
            x2(u"\\X", u"\u306F\u309A\n", 0, 2)
        except UnicodeEncodeError:
            # "\u309A" can not be encoded by some encodings
            pass
    
    # keep
    x2(u"ab\\Kcd", u"abcd", 2, 4)
    x2(u"ab\\Kc(\\Kd|z)", u"abcd", 3, 4)
    x2(u"ab\\Kc(\\Kz|d)", u"abcd", 2, 4)
    x2(u"(a\\K)*", u"aaab", 3, 3)
    x3(u"(a\\K)*", u"aaab", 2, 3, 1)
#    x2(u"a\\K?a", u"aa", 0, 2)        # error: differ from perl
    x2(u"ab(?=c\Kd)", u"abcd", 2, 2)          # This behaviour is currently not well defined. (see: perlre)
    x2(u"(?<=a\\Kb|aa)cd", u"abcd", 1, 4)     # This behaviour is currently not well defined. (see: perlre)
    x2(u"(?<=ab|a\\Ka)cd", u"abcd", 2, 4)     # This behaviour is currently not well defined. (see: perlre)
    
    # named group and subroutine call
#    x2(u"(?<name_2>ab)(?&name_2)", u"abab", 0, 4);
#    x2(u"(?<name_2>ab)(?1)", u"abab", 0, 4);
#    x2(u"(?<n>|\\((?&n)\\))+$", u"()(())", 0, 6);
#    x2(u"(a|x(?-1)x)", u"xax", 0, 3);
#    x2(u"(a|(x(?-2)x))", u"xax", 0, 3);
#    x2(u"a|x(?0)x", u"xax", 0, 3);
#    x2(u"a|x(?R)x", u"xax", 0, 3);
    x2(u"(a|x\g<0>x)", u"xax", 0, 3);
    x2(u"(a|x\g'0'x)", u"xax", 0, 3);
#    x2(u"(?-i:(?+1))(?i:(a)){0}", u"A", 0, 1);
    x2(u"(?-i:\g<+1>)(?i:(a)){0}", u"A", 0, 1);
    x2(u"(?-i:\g'+1')(?i:(a)){0}", u"A", 0, 1);
    
    # character set modifiers
    x2(u"(?u)\\w+", u"あa#", 0, 2);
    x2(u"(?a)\\w+", u"あa#", 1, 2);
    x2(u"(?u)\\W+", u"あa#", 2, 3);
    x2(u"(?a)\\W+", u"あa#", 0, 1);
    
    x2(u"(?a)\\b", u"あa", 1, 1);
    x2(u"(?a)\\w\\b", u"aあ", 0, 1);
    x2(u"(?a)\\B", u"a ああ ", 2, 2);
    
    x2(u"(?u)\\B", u"あ ", 2, 2);
    x2(u"(?a)\\B", u"あ ", 0, 0);
    x2(u"(?a)\\B", u"aあ ", 2, 2);
    
    x2(u"(?a)\\p{Alpha}\\P{Alpha}", u"a。", 0, 2);
    x2(u"(?u)\\p{Alpha}\\P{Alpha}", u"a。", 0, 2);
    x2(u"(?a)[[:word:]]+", u"aあ", 0, 1);
    x2(u"(?a)[[:^word:]]+", u"aあ", 1, 2);
    x2(u"(?u)[[:word:]]+", u"aあ", 0, 2);
    n(u"(?u)[[:^word:]]+", u"aあ");
    
    # \g{} backref
#    x2(u"((?<name1>\\d)|(?<name2>\\w))(\\g{name1}|\\g{name2})", u"ff", 0, 2);
#    x2(u"(?:(?<x>)|(?<x>efg))\\g{x}", u"", 0, 0);
#    x2(u"(?:(?<x>abc)|(?<x>efg))\\g{x}", u"abcefgefg", 3, 9);
#    n(u"(?:(?<x>abc)|(?<x>efg))\\g{x}", u"abcefg");
#    x2(u"((.*)a\\g{2}f)", u"bacbabf", 3, 7);
#    x2(u"(.*)a\\g{1}f", u"baczzzzzz\nbazz\nzzzzbabf", 19, 23);
#    x2(u"((.*)a\\g{-1}f)", u"bacbabf", 3, 7);
#    x2(u"(.*)a\\g{-1}f", u"baczzzzzz\nbazz\nzzzzbabf", 19, 23);
#    x2(u"(あ*)(い*)\\g{-2}\\g{-1}", u"あああいいあああいい", 0, 10);
    
    # Python/PCRE compatible named group
#    x2(u"(?P<name_2>ab)(?P>name_2)", u"abab", 0, 4);
#    x2(u"(?P<n>|\\((?P>n)\\))+$", u"()(())", 0, 6);
#    x2(u"((?P<name1>\\d)|(?P<name2>\\w))((?P=name1)|(?P=name2))", u"ff", 0, 2);
    
    # Fullwidth Alphabet
    n(u"ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ", u"ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ");
    x2(u"(?i)ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ", u"ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ", 0, 26);
    x2(u"(?i)ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ", u"ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ", 0, 26);
    x2(u"(?i)ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ", u"ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ", 0, 26);
    x2(u"(?i)ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ", u"ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ", 0, 26);
    
    # Greek
    n(u"αβγδεζηθικλμνξοπρστυφχψω", u"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ");
    x2(u"(?i)αβγδεζηθικλμνξοπρστυφχψω", u"αβγδεζηθικλμνξοπρστυφχψω", 0, 24);
    x2(u"(?i)αβγδεζηθικλμνξοπρστυφχψω", u"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ", 0, 24);
    x2(u"(?i)ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ", u"αβγδεζηθικλμνξοπρστυφχψω", 0, 24);
    x2(u"(?i)ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ", u"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ", 0, 24);
    
    # Cyrillic
    n(u"абвгдеёжзийклмнопрстуфхцчшщъыьэюя", u"АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ");
    x2(u"(?i)абвгдеёжзийклмнопрстуфхцчшщъыьэюя", u"абвгдеёжзийклмнопрстуфхцчшщъыьэюя", 0, 33);
    x2(u"(?i)абвгдеёжзийклмнопрстуфхцчшщъыьэюя", u"АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", 0, 33);
    x2(u"(?i)АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", u"абвгдеёжзийклмнопрстуфхцчшщъыьэюя", 0, 33);
    x2(u"(?i)АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", u"АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ", 0, 33);
    
    # multiple name definition
    x2(u"(?<a>a)(?<a>b)\\k<a>", u"aba", 0, 3)
#    x2(u"(?<a>a)(?<a>b)(?&a)", u"aba", 0, 3)
#    x2(u"(?<a>(a|.)(?<a>b))(?&a)", u"abcb", 0, 4)
    
    # branch reset
#    x3(u"(?|(c)|(?:(b)|(a)))", u"a", 0, 1, 2)
#    x3(u"(?|(c)|(?|(b)|(a)))", u"a", 0, 1, 1)
    
    # conditional expression
    x2(u"(?:(a)|(b))(?(1)cd)e", u"acde", 0, 4)
    n(u"(?:(a)|(b))(?(1)cd)e", u"ae")
    x2(u"(?:(a)|(b))(?(2)cd)e", u"ae", 0, 2)
    n(u"(?:(a)|(b))(?(2)cd)e", u"acde")
    x2(u"(?:(a)|(b))(?(1)c|d)", u"ac", 0, 2)
    x2(u"(?:(a)|(b))(?(1)c|d)", u"bd", 0, 2)
    n(u"(?:(a)|(b))(?(1)c|d)", u"ad")
    n(u"(?:(a)|(b))(?(1)c|d)", u"bc")
    x2(u"(?:(a)|(b))(?:(?(1)cd)e|fg)", u"acde", 0, 4)
    x2(u"(?:(a)|(b))(?:(?(1)cd|x)e|fg)", u"bxe", 0, 3)
    n(u"(?:(a)|(b))(?:(?(2)cd|x)e|fg)", u"bxe")
    x2(u"(?:(?<x>a)|(?<y>b))(?:(?(<x>)cd|x)e|fg)", u"bxe", 0, 3)
    n(u"(?:(?<x>a)|(?<y>b))(?:(?(<y>)cd|x)e|fg)", u"bxe")
    x2(u"((?<=a))?(?(1)b|c)", u"abc", 1, 2)
    x2(u"((?<=a))?(?(1)b|c)", u"bc", 1, 2)
    
    # Implicit-anchor optimization
    x2(u"(?m:.*abc)", u"dddabdd\nddabc", 0, 13)   # optimized /(?m:.*abc)/ ==> /\A(?m:.*abc)/
    x2(u"(?m:.+abc)", u"dddabdd\nddabc", 0, 13)   # optimized
    x2(u"(?-m:.*abc)", u"dddabdd\nddabc", 8, 13)  # optimized /(?-m:.*abc)/ ==> /(?:^|\A)(?m:.*abc)/
    x2(u"(?-m:.+abc)", u"dddabdd\nddabc", 8, 13)  # optimized
    x2(u"(?-m:.*abc)", u"dddabdd\nabc", 8, 11)    # optimized
    n(u"(?-m:.+abc)", u"dddabdd\nabc")            # optimized
    x2(u"(?m:.*\\Z)", u"dddabdd\nddabc", 0, 13)   # optimized /(?m:.*\Z)/ ==> /\A(?m:.*\Z)/
    x2(u"(?-m:.*\\Z)", u"dddabdd\nddabc", 8, 13)  # optimized /(?-m:.*\Z)/ ==> /(?:^|\A)(?m:.*\Z)/
    x2(u"(.*)X\\1", u"1234X2345", 1, 8)           # not optimized
    
    
    print("\nRESULT   SUCC: %d,  FAIL: %d,  ERROR: %d      (by Onigmo %s)" % (
          nsucc, nfail, nerror, onig.onig_version()))
    
    onig.onig_region_free(region, 1)
    onig.onig_end()

if __name__ == '__main__':
    main()

