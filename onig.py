# -*- coding: utf-8 -*-

"""Using Onigmo (Oniguruma-mod) regular expression library.

This is a low level wrapper for Onigmo regular expression DLL/shared object.
(This module does not support static link library.)
This provides almost same API as the original C API, so the API is not
object oriented.

Onigmo DLL (onig.dll, libonig.so, etc.) must be placed in the
default search path. The default search path depends on the system.
"""

import ctypes
import os
import sys

#__all__ = ["onig_new", "onig_free",
#           "onig_search", "onig_match",
#           "onig_region_new", "onig_region_free",
#           "onig_version", "onig_copyright"]


#
# Type Definitions
#

OnigCodePoint = ctypes.c_uint

class OnigRegexType(ctypes.Structure):
    _fields_ = [
    ]
regex_t = OnigRegexType
OnigRegex = ctypes.POINTER(OnigRegexType)

try:
    # Python 2.7
    _c_ssize_t = ctypes.c_ssize_t
except AttributeError:
    # Python 2.6
    if ctypes.sizeof(ctypes.c_int) == ctypes.sizeof(ctypes.c_void_p):
        _c_ssize_t = ctypes.c_int
    elif ctypes.sizeof(ctypes.c_long) == ctypes.sizeof(ctypes.c_void_p):
        _c_ssize_t = ctypes.c_long
    elif ctypes.sizeof(ctypes.c_longlong) == ctypes.sizeof(ctypes.c_void_p):
        _c_ssize_t = ctypes.c_longlong

class OnigRegion(ctypes.Structure):
    _fields_ = [
        ("allocated",   ctypes.c_int),
        ("num_regs",    ctypes.c_int),
        ("beg",         ctypes.POINTER(_c_ssize_t)),
        ("end",         ctypes.POINTER(_c_ssize_t)),
        ("history_root",ctypes.c_void_p),
    ]
re_registers = OnigRegion

OnigOptionType = ctypes.c_int

class OnigEncodingType(ctypes.Structure):
    _fields_ = [
        ("mbc_enc_len",     ctypes.c_void_p),
        ("name",            ctypes.c_char_p),
        ("max_enc_len",     ctypes.c_int),
        ("min_enc_len",     ctypes.c_int),
        ("is_mbc_newline",  ctypes.c_void_p),
        ("mbc_to_code",     ctypes.c_void_p),
        ("code_to_mbclen",  ctypes.c_void_p),
        ("code_to_mbc",     ctypes.c_void_p),
        ("mbc_case_fold",   ctypes.c_void_p),
        ("apply_all_case_fold",     ctypes.c_void_p),
        ("get_case_fold_codes_by_str",  ctypes.c_void_p),
        ("property_name_to_ctype",  ctypes.c_void_p),
        ("is_code_ctype",           ctypes.c_void_p),
        ("get_ctype_code_range",    ctypes.c_void_p),
        ("left_adjust_char_head",   ctypes.c_void_p),
        ("is_allowed_reverse_match",ctypes.c_void_p),
    ]
OnigEncoding = ctypes.POINTER(OnigEncodingType)

class OnigMetaCharTableType(ctypes.Structure):
    _fields_ = [
        ("esc",             OnigCodePoint),
        ("anychar",         OnigCodePoint),
        ("anytime",         OnigCodePoint),
        ("zero_or_one_time",OnigCodePoint),
        ("one_or_one_time", OnigCodePoint),
        ("anychar_anytime", OnigCodePoint),
    ]

class OnigSyntaxType(ctypes.Structure):
    _fields_ = [
        ("op",              ctypes.c_uint),
        ("op2",             ctypes.c_uint),
        ("behavior",        ctypes.c_uint),
        ("options",         OnigOptionType),
        ("meta_char_table", OnigMetaCharTableType),
    ]

class OnigErrorInfo(ctypes.Structure):
    _fields_ = [
        ("enc",     OnigEncoding),
        ("par",     ctypes.c_char_p),
        ("par_end", ctypes.c_char_p),
    ]


# load the DLL or the shared library

if os.name in ("nt", "ce"):
    _libname = "onig.dll"
elif sys.platform == "cygwin":
    _libname = "libonig.dll"
else:
    _libname = "libonig.so"

libonig = ctypes.cdll.LoadLibrary(_libname)

#
# Encodings
#
def _load_encoding(enc):
    return ctypes.pointer(OnigEncodingType.in_dll(libonig, enc))

ONIG_ENCODING_ASCII         = _load_encoding("OnigEncodingASCII")
ONIG_ENCODING_ISO_8859_1    = _load_encoding("OnigEncodingISO_8859_1")
ONIG_ENCODING_ISO_8859_2    = _load_encoding("OnigEncodingISO_8859_2")
ONIG_ENCODING_ISO_8859_3    = _load_encoding("OnigEncodingISO_8859_3")
ONIG_ENCODING_ISO_8859_4    = _load_encoding("OnigEncodingISO_8859_4")
ONIG_ENCODING_ISO_8859_5    = _load_encoding("OnigEncodingISO_8859_5")
ONIG_ENCODING_ISO_8859_6    = _load_encoding("OnigEncodingISO_8859_6")
ONIG_ENCODING_ISO_8859_7    = _load_encoding("OnigEncodingISO_8859_7")
ONIG_ENCODING_ISO_8859_8    = _load_encoding("OnigEncodingISO_8859_8")
ONIG_ENCODING_ISO_8859_9    = _load_encoding("OnigEncodingISO_8859_9")
ONIG_ENCODING_ISO_8859_10   = _load_encoding("OnigEncodingISO_8859_10")
ONIG_ENCODING_ISO_8859_11   = _load_encoding("OnigEncodingISO_8859_11")
ONIG_ENCODING_ISO_8859_13   = _load_encoding("OnigEncodingISO_8859_13")
ONIG_ENCODING_ISO_8859_14   = _load_encoding("OnigEncodingISO_8859_14")
ONIG_ENCODING_ISO_8859_15   = _load_encoding("OnigEncodingISO_8859_15")
ONIG_ENCODING_ISO_8859_16   = _load_encoding("OnigEncodingISO_8859_16")
ONIG_ENCODING_UTF8          = _load_encoding("OnigEncodingUTF8")
ONIG_ENCODING_UTF16_LE      = _load_encoding("OnigEncodingUTF16_LE")
ONIG_ENCODING_UTF16_BE      = _load_encoding("OnigEncodingUTF16_BE")
ONIG_ENCODING_UTF32_LE      = _load_encoding("OnigEncodingUTF32_LE")
ONIG_ENCODING_UTF32_BE      = _load_encoding("OnigEncodingUTF32_BE")
ONIG_ENCODING_EUC_JP        = _load_encoding("OnigEncodingEUC_JP")
ONIG_ENCODING_EUC_TW        = _load_encoding("OnigEncodingEUC_TW")
ONIG_ENCODING_EUC_KR        = _load_encoding("OnigEncodingEUC_KR")
ONIG_ENCODING_EUC_CN        = _load_encoding("OnigEncodingEUC_CN")
ONIG_ENCODING_SJIS          = _load_encoding("OnigEncodingSJIS")
try:
    ONIG_ENCODING_CP932     = _load_encoding("OnigEncodingCP932")
except ValueError:
    pass
#ONIG_ENCODING_KOI8         = _load_encoding("OnigEncodingKOI8")
ONIG_ENCODING_KOI8_R        = _load_encoding("OnigEncodingKOI8_R")
ONIG_ENCODING_CP1251        = _load_encoding("OnigEncodingCP1251")
ONIG_ENCODING_BIG5          = _load_encoding("OnigEncodingBIG5")
ONIG_ENCODING_GB18030       = _load_encoding("OnigEncodingGB18030")

#ONIG_ENCODING_UNDEF         = None


#
# Syntaxes
#
def _load_syntax(syn):
    return ctypes.pointer(OnigSyntaxType.in_dll(libonig, syn))

ONIG_SYNTAX_ASIS            = _load_syntax("OnigSyntaxASIS")
ONIG_SYNTAX_POSIX_BASIC     = _load_syntax("OnigSyntaxPosixBasic")
ONIG_SYNTAX_POSIX_EXTENDED  = _load_syntax("OnigSyntaxPosixExtended")
ONIG_SYNTAX_EMACS           = _load_syntax("OnigSyntaxEmacs")
ONIG_SYNTAX_GREP            = _load_syntax("OnigSyntaxGrep")
ONIG_SYNTAX_GNU_REGEX       = _load_syntax("OnigSyntaxGnuRegex")
ONIG_SYNTAX_JAVA            = _load_syntax("OnigSyntaxJava")
ONIG_SYNTAX_PERL            = _load_syntax("OnigSyntaxPerl")
try:
    ONIG_SYNTAX_PERL58      = _load_syntax("OnigSyntaxPerl58")
    ONIG_SYNTAX_PERL58_NG   = _load_syntax("OnigSyntaxPerl58_NG")
except ValueError:
    pass
try:
    ONIG_SYNTAX_PERL_NG     = _load_syntax("OnigSyntaxPerl_NG")
except ValueError:
    pass
ONIG_SYNTAX_RUBY            = _load_syntax("OnigSyntaxRuby")
try:
    ONIG_SYNTAX_PYTHON      = _load_syntax("OnigSyntaxPython")
except ValueError:
    pass

ONIG_SYNTAX_DEFAULT         = ctypes.POINTER(OnigSyntaxType).in_dll(
                                    libonig, "OnigDefaultSyntax")


#
# Constants
#

ONIG_MAX_ERROR_MESSAGE_LEN = 90

# options
ONIG_OPTION_NONE                = 0
ONIG_OPTION_IGNORECASE          = 1
ONIG_OPTION_EXTEND              = (ONIG_OPTION_IGNORECASE         << 1)
ONIG_OPTION_MULTILINE           = (ONIG_OPTION_EXTEND             << 1)
ONIG_OPTION_DOTALL              =  ONIG_OPTION_MULTILINE
ONIG_OPTION_SINGLELINE          = (ONIG_OPTION_MULTILINE          << 1)
ONIG_OPTION_FIND_LONGEST        = (ONIG_OPTION_SINGLELINE         << 1)
ONIG_OPTION_FIND_NOT_EMPTY      = (ONIG_OPTION_FIND_LONGEST       << 1)
ONIG_OPTION_NEGATE_SINGLELINE   = (ONIG_OPTION_FIND_NOT_EMPTY     << 1)
ONIG_OPTION_DONT_CAPTURE_GROUP  = (ONIG_OPTION_NEGATE_SINGLELINE  << 1)
ONIG_OPTION_CAPTURE_GROUP       = (ONIG_OPTION_DONT_CAPTURE_GROUP << 1)
# options (search time)
ONIG_OPTION_NOTBOL              = (ONIG_OPTION_CAPTURE_GROUP << 1)
ONIG_OPTION_NOTEOL              = (ONIG_OPTION_NOTBOL << 1)
ONIG_OPTION_POSIX_REGION        = (ONIG_OPTION_NOTEOL << 1)
# options (ctype range)
ONIG_OPTION_ASCII_RANGE         = (ONIG_OPTION_POSIX_REGION << 1)
# options (newline)
ONIG_OPTION_NEWLINE_CRLF        = (ONIG_OPTION_ASCII_RANGE << 1)

ONIG_OPTION_DEFAULT             = ONIG_OPTION_NONE


# error codes
def ONIG_IS_PATTERN_ERROR(ecode):
    return ((ecode) <= -100 and (ecode) > -1000)
# normal return
ONIG_NORMAL             =  0
ONIG_MISMATCH           = -1
ONIG_NO_SUPPORT_CONFIG  = -2
# internal error
# general error
ONIGERR_INVALID_ARGUMENT    = -30
# syntax error
# values error (syntax error)
# errors related to thread
ONIGERR_OVER_THREAD_PASS_LIMIT_COUNT    = -1001


#
# Onigmo APIs
#

# onig_init
onig_init = libonig.onig_init

# onig_error_code_to_str
libonig.onig_error_code_to_str.argtypes = [ctypes.c_char_p, ctypes.c_int,
        ctypes.POINTER(OnigErrorInfo)]
def onig_error_code_to_str(err_buf, err_code, err_info=None):
    return libonig.onig_error_code_to_str(err_buf, err_code, err_info)

# onig_set_warn_func
# onig_set_verb_warn_func

# onig_new
libonig.onig_new.argtypes = [ctypes.POINTER(OnigRegex),
        ctypes.c_void_p, ctypes.c_void_p,
        OnigOptionType, OnigEncoding, ctypes.POINTER(OnigSyntaxType),
        ctypes.POINTER(OnigErrorInfo)]
onig_new = libonig.onig_new

# onig_reg_init
# onig_new_without_alloc
# onig_new_deluxe

# onig_free
libonig.onig_free.argtypes = [OnigRegex]
onig_free = libonig.onig_free

# onig_free_body
# onig_recompile
# onig_recompile_deluxe

# onig_search
libonig.onig_search.argtypes = [OnigRegex,
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p,
        ctypes.POINTER(OnigRegion), OnigOptionType]
libonig.onig_search.restype = _c_ssize_t
onig_search = libonig.onig_search

# onig_search_gpos

# onig_match
libonig.onig_match.argtypes = [OnigRegex,
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p,
        ctypes.POINTER(OnigRegion), OnigOptionType]
libonig.onig_match.restype = _c_ssize_t
onig_match = libonig.onig_match

# onig_region_new
libonig.onig_region_new.argtypes = []
libonig.onig_region_new.restype = ctypes.POINTER(OnigRegion)
onig_region_new = libonig.onig_region_new

# onig_region_init

# onig_region_free
libonig.onig_region_free.argtypes = [ctypes.POINTER(OnigRegion), ctypes.c_int]
onig_region_free = libonig.onig_region_free

# onig_region_copy
# onig_region_clear
# onig_region_resize
# onig_region_set
# onig_name_to_group_numbers
# onig_name_to_backref_number
# onig_foreach_name
# onig_number_of_names
# onig_number_of_captures
# onig_number_of_capture_histories
# onig_get_capture_tree
# onig_capture_tree_traverse
# onig_noname_group_capture_is_active
# onig_get_encoding
# onig_get_options
# onig_get_case_fold_flag
# onig_get_syntax
# onig_set_default_syntax

# onig_copy_syntax
libonig.onig_copy_syntax.argtypes = [ctypes.POINTER(OnigSyntaxType),
        ctypes.POINTER(OnigSyntaxType)]
onig_copy_syntax = libonig.onig_copy_syntax

# onig_get_syntax_op
# onig_get_syntax_op2
# onig_get_syntax_behavior
# onig_get_syntax_options
# onig_set_syntax_op
# onig_set_syntax_op2
# onig_set_syntax_behavior
# onig_set_syntax_options
# onig_set_meta_char
# onig_copy_encoding
# onig_get_default_case_fold_flag
# onig_set_default_case_fold_flag
# onig_get_match_stack_limit_size
# onig_set_match_stack_limit_size

# onig_end
libonig.onig_end.argtypes = []
onig_end = libonig.onig_end

# onig_version
libonig.onig_version.argtypes = []
libonig.onig_version.restype = ctypes.c_char_p
onig_version = libonig.onig_version

# onig_copyright
libonig.onig_copyright.argtypes = []
libonig.onig_copyright.restype = ctypes.c_char_p
onig_copyright = libonig.onig_copyright
