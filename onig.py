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

class OnigRegion(ctypes.Structure):
    _fields_ = [
        ("allocated",   ctypes.c_int),
        ("num_regs",    ctypes.c_int),
        ("beg",         ctypes.POINTER(ctypes.c_int)),
        ("end",         ctypes.POINTER(ctypes.c_int)),
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
def load_encoding(enc):
    return ctypes.cast(enc, OnigEncoding)

ONIG_ENCODING_ASCII         = load_encoding(libonig.OnigEncodingASCII)
ONIG_ENCODING_ISO_8859_1    = load_encoding(libonig.OnigEncodingISO_8859_1)
ONIG_ENCODING_ISO_8859_2    = load_encoding(libonig.OnigEncodingISO_8859_2)
ONIG_ENCODING_ISO_8859_3    = load_encoding(libonig.OnigEncodingISO_8859_3)
ONIG_ENCODING_ISO_8859_4    = load_encoding(libonig.OnigEncodingISO_8859_4)
ONIG_ENCODING_ISO_8859_5    = load_encoding(libonig.OnigEncodingISO_8859_5)
ONIG_ENCODING_ISO_8859_6    = load_encoding(libonig.OnigEncodingISO_8859_6)
ONIG_ENCODING_ISO_8859_7    = load_encoding(libonig.OnigEncodingISO_8859_7)
ONIG_ENCODING_ISO_8859_8    = load_encoding(libonig.OnigEncodingISO_8859_8)
ONIG_ENCODING_ISO_8859_9    = load_encoding(libonig.OnigEncodingISO_8859_9)
ONIG_ENCODING_ISO_8859_10   = load_encoding(libonig.OnigEncodingISO_8859_10)
ONIG_ENCODING_ISO_8859_11   = load_encoding(libonig.OnigEncodingISO_8859_11)
ONIG_ENCODING_ISO_8859_13   = load_encoding(libonig.OnigEncodingISO_8859_13)
ONIG_ENCODING_ISO_8859_14   = load_encoding(libonig.OnigEncodingISO_8859_14)
ONIG_ENCODING_ISO_8859_15   = load_encoding(libonig.OnigEncodingISO_8859_15)
ONIG_ENCODING_ISO_8859_16   = load_encoding(libonig.OnigEncodingISO_8859_16)
ONIG_ENCODING_UTF8          = load_encoding(libonig.OnigEncodingUTF8)
ONIG_ENCODING_UTF16_LE      = load_encoding(libonig.OnigEncodingUTF16_LE)
ONIG_ENCODING_UTF16_BE      = load_encoding(libonig.OnigEncodingUTF16_BE)
ONIG_ENCODING_UTF32_LE      = load_encoding(libonig.OnigEncodingUTF32_LE)
ONIG_ENCODING_UTF32_BE      = load_encoding(libonig.OnigEncodingUTF32_BE)
ONIG_ENCODING_EUC_JP        = load_encoding(libonig.OnigEncodingEUC_JP)
ONIG_ENCODING_EUC_TW        = load_encoding(libonig.OnigEncodingEUC_TW)
ONIG_ENCODING_EUC_KR        = load_encoding(libonig.OnigEncodingEUC_KR)
ONIG_ENCODING_EUC_CN        = load_encoding(libonig.OnigEncodingEUC_CN)
ONIG_ENCODING_SJIS          = load_encoding(libonig.OnigEncodingSJIS)
try:
    ONIG_ENCODING_CP932     = load_encoding(libonig.OnigEncodingCP932)
except AttributeError:
    pass
#ONIG_ENCODING_KOI8         = load_encoding(libonig.OnigEncodingKOI8)
ONIG_ENCODING_KOI8_R        = load_encoding(libonig.OnigEncodingKOI8_R)
ONIG_ENCODING_CP1251        = load_encoding(libonig.OnigEncodingCP1251)
ONIG_ENCODING_BIG5          = load_encoding(libonig.OnigEncodingBIG5)
ONIG_ENCODING_GB18030       = load_encoding(libonig.OnigEncodingGB18030)

ONIG_ENCODING_UNDEF         = OnigEncoding()


#
# Syntaxes
#
def load_syntax(syn):
    return ctypes.cast(syn, ctypes.POINTER(OnigSyntaxType))

ONIG_SYNTAX_ASIS            = load_syntax(libonig.OnigSyntaxASIS)
ONIG_SYNTAX_POSIX_BASIC     = load_syntax(libonig.OnigSyntaxPosixBasic)
ONIG_SYNTAX_POSIX_EXTENDED  = load_syntax(libonig.OnigSyntaxPosixExtended)
ONIG_SYNTAX_EMACS           = load_syntax(libonig.OnigSyntaxEmacs)
ONIG_SYNTAX_GREP            = load_syntax(libonig.OnigSyntaxGrep)
ONIG_SYNTAX_GNU_REGEX       = load_syntax(libonig.OnigSyntaxGnuRegex)
ONIG_SYNTAX_JAVA            = load_syntax(libonig.OnigSyntaxJava)
ONIG_SYNTAX_PERL            = load_syntax(libonig.OnigSyntaxPerl)
try:
    ONIG_SYNTAX_PERL58      = load_syntax(libonig.OnigSyntaxPerl58)
    ONIG_SYNTAX_PERL58_NG   = load_syntax(libonig.OnigSyntaxPerl58_NG)
except AttributeError:
    pass
try:
    ONIG_SYNTAX_PERL_NG     = load_syntax(libonig.OnigSyntaxPerl_NG)
except AttributeError:
    pass
ONIG_SYNTAX_RUBY            = load_syntax(libonig.OnigSyntaxRuby)

ONIG_SYNTAX_DEFAULT = ctypes.cast(libonig.OnigDefaultSyntax,
        ctypes.POINTER(ctypes.POINTER(OnigSyntaxType))).contents


#
# Constants
#

ONIG_MAX_ERROR_MESSAGE_LEN = 90

# options
ONIG_OPTION_NONE                = 0
ONIG_OPTION_IGNORECASE          = 1
ONIG_OPTION_EXTEND              = (ONIG_OPTION_IGNORECASE         << 1)
ONIG_OPTION_MULTILINE           = (ONIG_OPTION_EXTEND             << 1)
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
ONIG_OPTION_ASCII_RANGE         = (ONIG_OPTION_POSIX_REGION    << 1)

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
onig_search = libonig.onig_search

# onig_search_gpos

# onig_match
libonig.onig_match.argtypes = [OnigRegex,
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p,
        ctypes.POINTER(OnigRegion), OnigOptionType]
onig_match = libonig.onig_match

# onig_region_new
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
onig_end = libonig.onig_end

# onig_version
libonig.onig_version.restype = ctypes.c_char_p
onig_version = libonig.onig_version

# onig_copyright
libonig.onig_copyright.restype = ctypes.c_char_p
onig_copyright = libonig.onig_copyright
