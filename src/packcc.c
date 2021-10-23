/*
 * PackCC: a packrat parser generator for C.
 *
 * Copyright (c) 2014, 2019-2021 Arihiro Yoshida. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/*
 * The algorithm is based on the paper "Packrat Parsers Can Support Left Recursion"
 * authored by A. Warth, J. R. Douglass, and T. Millstein.
 *
 * The specification is determined by referring to peg/leg developed by Ian Piumarta.
 */

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#ifndef _MSC_VER
#if defined __GNUC__ && defined _WIN32 /* MinGW */
#ifndef PCC_USE_SYSTEM_STRNLEN
#define strnlen(str, maxlen) strnlen_(str, maxlen)
static size_t strnlen_(const char *str, size_t maxlen) {
    size_t i;
    for (i = 0; i < maxlen && str[i]; i++);
    return i;
}
#endif /* !PCC_USE_SYSTEM_STRNLEN */
#endif /* defined __GNUC__ && defined _WIN32 */
#endif /* !_MSC_VER */

#ifdef _MSC_VER
#define snprintf _snprintf
#define unlink _unlink
#else
#include <unistd.h> /* for unlink() */
#endif

#if !defined __has_attribute || defined _MSC_VER
#define __attribute__(x)
#endif

#undef TRUE  /* to avoid macro definition conflicts with the system header file of IBM AIX */
#undef FALSE

#define VERSION "1.6.0"

#ifndef BUFFER_MIN_SIZE
#define BUFFER_MIN_SIZE 256
#endif
#ifndef ARRAY_MIN_SIZE
#define ARRAY_MIN_SIZE 2
#endif

#define VOID_VALUE (~(size_t)0)

#ifdef _WIN64 /* 64-bit Windows including MSVC and MinGW-w64 */
#define FMT_LU "%llu"
typedef unsigned long long ulong_t;
/* NOTE: "%llu" and "long long" are not C89-compliant, but they are required to deal with a 64-bit integer value in 64-bit Windows. */
#else
#define FMT_LU "%lu"
typedef unsigned long ulong_t;
#endif
/* FMT_LU and ulong_t are used to print size_t values safely (ex. printf(FMT_LU "\n", (ulong_t)value);) */
/* NOTE: Neither "%z" nor <inttypes.h> is used since PackCC complies with the C89 standard as much as possible. */

typedef enum bool_tag {
    FALSE = 0,
    TRUE
} bool_t;

typedef struct char_array_tag {
    char *buf;
    size_t max;
    size_t len;
} char_array_t;

typedef enum node_type_tag {
    NODE_RULE = 0,
    NODE_REFERENCE,
    NODE_STRING,
    NODE_CHARCLASS,
    NODE_QUANTITY,
    NODE_PREDICATE,
    NODE_SEQUENCE,
    NODE_ALTERNATE,
    NODE_CAPTURE,
    NODE_EXPAND,
    NODE_ACTION,
    NODE_ERROR
} node_type_t;

typedef struct node_tag node_t;

typedef struct node_array_tag {
    node_t **buf;
    size_t max;
    size_t len;
} node_array_t;

typedef struct node_const_array_tag {
    const node_t **buf;
    size_t max;
    size_t len;
} node_const_array_t;

typedef struct node_hash_table_tag {
    const node_t **buf;
    size_t max;
    size_t mod;
} node_hash_table_t;

typedef struct node_rule_tag {
    char *name;
    node_t *expr;
    int ref; /* mutable */
    node_const_array_t vars;
    node_const_array_t capts;
    node_const_array_t codes;
    size_t line;
    size_t col;
} node_rule_t;

typedef struct node_reference_tag {
    char *var; /* NULL if no variable name */
    size_t index;
    char *name;
    const node_t *rule;
    size_t line;
    size_t col;
} node_reference_t;

typedef struct node_string_tag {
    char *value;
} node_string_t;

typedef struct node_charclass_tag {
    char *value; /* NULL means any character */
} node_charclass_t;

typedef struct node_quantity_tag {
    int min;
    int max;
    node_t *expr;
} node_quantity_t;

typedef struct node_predicate_tag {
    bool_t neg;
    node_t *expr;
} node_predicate_t;

typedef struct node_sequence_tag {
    node_array_t nodes;
} node_sequence_t;

typedef struct node_alternate_tag {
    node_array_t nodes;
} node_alternate_t;

typedef struct node_capture_tag {
    node_t *expr;
    size_t index;
} node_capture_t;

typedef struct node_expand_tag {
    size_t index;
    size_t line;
    size_t col;
} node_expand_t;

typedef struct node_action_tag {
    char *value;
    size_t index;
    node_const_array_t vars;
    node_const_array_t capts;
} node_action_t;

typedef struct node_error_tag {
    node_t *expr;
    char *value;
    size_t index;
    node_const_array_t vars;
    node_const_array_t capts;
} node_error_t;

typedef union node_data_tag {
    node_rule_t      rule;
    node_reference_t reference;
    node_string_t    string;
    node_charclass_t charclass;
    node_quantity_t  quantity;
    node_predicate_t predicate;
    node_sequence_t  sequence;
    node_alternate_t alternate;
    node_capture_t   capture;
    node_expand_t    expand;
    node_action_t    action;
    node_error_t     error;
} node_data_t;

struct node_tag {
    node_type_t type;
    node_data_t data;
};

typedef enum code_flag_tag {
    CODE_FLAG__NONE = 0,
    CODE_FLAG__UTF8_CHARCLASS_USED = 1
} code_flag_t;

typedef struct context_tag {
    char *iname;  /* the path name of the PEG file being parsed */
    char *sname;  /* the path name of the C source file being generated */
    char *hname;  /* the path name of the C header file being generated */
    FILE *ifile;  /* the input stream of the PEG file */
    char *hid;    /* the macro name for the include guard of the C header file */
    char *vtype;  /* the type name of the data output by the parsing API function (NULL means the default) */
    char *atype;  /* the type name of the user-defined data passed to the parser creation API function (NULL means the default) */
    char *prefix; /* the prefix of the API function names (NULL means the default) */
    bool_t ascii; /* UTF-8 support is disabled if true  */
    bool_t debug; /* debug information is output if true */
    code_flag_t flags;   /* bitwise flags to control code generation; updated during PEG parsing */
    size_t errnum;       /* the current number of PEG parsing errors */
    size_t linenum;      /* the current line number (0-based) */
    size_t linepos;      /* the beginning position in the PEG file of the current line */
    size_t bufpos;       /* the position in the PEG file of the first character currently buffered */
    size_t bufcur;       /* the current parsing position in the character buffer */
    char_array_t buffer; /* the character buffer */
    node_array_t rules;  /* the PEG rules */
    node_hash_table_t rulehash; /* the hash table to accelerate access of desired PEG rules */
    char_array_t esource; /* the code blocks from %earlysource and %earlycommon directives to be added into the generated source file */
    char_array_t eheader; /* the code blocks from %earlyheader and %earlycommon directives to be added into the generated header file */
    char_array_t source;  /* the code blocks from %source and %common directives to be added into the generated source file */
    char_array_t header;  /* the code blocks from %header and %common directives to be added into the generated header file */
} context_t;

typedef struct generate_tag {
    FILE *stream;
    const node_t *rule;
    int label;
    bool_t ascii;
} generate_t;

typedef enum string_flag_tag {
    STRING_FLAG__NONE = 0,
    STRING_FLAG__NOTEMPTY = 1,
    STRING_FLAG__NOTVOID = 2,
    STRING_FLAG__IDENTIFIER = 4
} string_flag_t;

typedef enum code_reach_tag {
    CODE_REACH__BOTH = 0,
    CODE_REACH__ALWAYS_SUCCEED = 1,
    CODE_REACH__ALWAYS_FAIL = -1
} code_reach_t;

static const char *g_cmdname = "packcc"; /* replaced later with actual one */

__attribute__((format(printf, 1, 2)))
static int print_error(const char *format, ...) {
    int n;
    va_list a;
    va_start(a, format);
    n = fprintf(stderr, "%s: ", g_cmdname);
    if (n >= 0) {
        const int k = vfprintf(stderr, format, a);
        if (k < 0) n = k; else n += k;
    }
    va_end(a);
    return n;
}

static FILE *fopen_rb_e(const char *path) {
    FILE *const f = fopen(path, "rb");
    if (f == NULL) {
        print_error("Cannot open file '%s' to read\n", path);
        exit(2);
    }
    return f;
}

static FILE *fopen_wt_e(const char *path) {
    FILE *const f = fopen(path, "wt");
    if (f == NULL) {
        print_error("Cannot open file '%s' to write\n", path);
        exit(2);
    }
    return f;
}

static int fclose_e(FILE *stream) {
    const int r = fclose(stream);
    if (r == EOF) {
        print_error("File closing error\n");
        exit(2);
    }
    return r;
}

static int fgetc_e(FILE *stream) {
    const int c = fgetc(stream);
    if (c == EOF && ferror(stream)) {
        print_error("File read error\n");
        exit(2);
    }
    return c;
}

static int fputc_e(int c, FILE *stream) {
    const int r = fputc(c, stream);
    if (r == EOF) {
        print_error("File write error\n");
        exit(2);
    }
    return r;
}

static int fputs_e(const char *s, FILE *stream) {
    const int r = fputs(s, stream);
    if (r == EOF) {
        print_error("File write error\n");
        exit(2);
    }
    return r;
}

__attribute__((format(printf, 2, 3)))
static int fprintf_e(FILE *stream, const char *format, ...) {
    int n;
    va_list a;
    va_start(a, format);
    n = vfprintf(stream, format, a);
    va_end(a);
    if (n < 0) {
        print_error("File write error\n");
        exit(2);
    }
    return n;
}

static void *malloc_e(size_t size) {
    void *const p = malloc(size);
    if (p == NULL) {
        print_error("Out of memory\n");
        exit(3);
    }
    return p;
}

static void *realloc_e(void *ptr, size_t size) {
    void *const p = realloc(ptr, size);
    if (p == NULL) {
        print_error("Out of memory\n");
        exit(3);
    }
    return p;
}

static char *strdup_e(const char *str) {
    const size_t m = strlen(str);
    char *const s = (char *)malloc_e(m + 1);
    memcpy(s, str, m);
    s[m] = '\0';
    return s;
}

static char *strndup_e(const char *str, size_t len) {
    const size_t m = strnlen(str, len);
    char *const s = (char *)malloc_e(m + 1);
    memcpy(s, str, m);
    s[m] = '\0';
    return s;
}

static size_t string_to_size_t(const char *str) {
#define N (~(size_t)0 / 10)
#define M (~(size_t)0 - 10 * N)
    size_t n = 0, i, k;
    for (i = 0; str[i]; i++) {
        const char c = str[i];
        if (c < '0' || c > '9') return VOID_VALUE;
        k = (size_t)(c - '0');
        if (n >= N && k > M) return VOID_VALUE; /* overflow */
        n = k + 10 * n;
    }
    return n;
#undef N
#undef M
}

static size_t find_first_trailing_space(const char *str, size_t start, size_t end, size_t *next) {
    size_t j = start, i;
    for (i = start; i < end; i++) {
        switch (str[i]) {
        case ' ':
        case '\v':
        case '\f':
        case '\t':
            break;
        case '\n':
            if (next) *next = i + 1;
            return j;
        case '\r':
            if (i + 1 < end && str[i + 1] == '\n') i++;
            if (next) *next = i + 1;
            return j;
        default:
            j = i + 1;
        }
    }
    if (next) *next = end;
    return j;
}

static size_t count_indent_spaces(const char *str, size_t start, size_t end, size_t *next) {
    size_t n = 0, i;
    for (i = start; i < end; i++) {
        switch (str[i]) {
        case ' ':
        case '\v':
        case '\f':
            n++;
            break;
        case '\t':
            n = (n + 8) & ~7;
            break;
        default:
            if (next) *next = i;
            return n;
        }
    }
    if (next) *next = end;
    return n;
}

static bool_t is_filled_string(const char *str) {
    size_t i;
    for (i = 0; str[i]; i++) {
        if (
            str[i] != ' '  &&
            str[i] != '\v' &&
            str[i] != '\f' &&
            str[i] != '\t' &&
            str[i] != '\n' &&
            str[i] != '\r'
        ) return TRUE;
    }
    return FALSE;
}

static bool_t is_identifier_string(const char *str) {
    size_t i;
    if (!(
        (str[0] >= 'a' && str[0] <= 'z') ||
        (str[0] >= 'A' && str[0] <= 'Z') ||
        str[0] == '_'
    )) return FALSE;
    for (i = 1; str[i]; i++) {
        if (!(
            (str[i] >= 'a' && str[i] <= 'z') ||
            (str[i] >= 'A' && str[i] <= 'Z') ||
            (str[i] >= '0' && str[i] <= '9') ||
            str[i] == '_'
        )) return FALSE;
    }
    return TRUE;
}

static bool_t is_pointer_type(const char *str) {
    const size_t n = strlen(str);
    return (n > 0 && str[n - 1] == '*') ? TRUE : FALSE;
}

static bool_t is_valid_utf8_string(const char *str) {
    int k = 0, n = 0, u = 0;
    size_t i;
    for (i = 0; str[i]; i++) {
        const int c = (int)(unsigned char)str[i];
        switch (k) {
        case 0:
            if (c >= 0x80) {
                if ((c & 0xe0) == 0xc0) {
                    u = c & 0x1f;
                    n = k = 1;
                }
                else if ((c & 0xf0) == 0xe0) {
                    u = c & 0x0f;
                    n = k = 2;
                }
                else if ((c & 0xf8) == 0xf0) {
                    u = c & 0x07;
                    n = k = 3;
                }
                else {
                    return FALSE;
                }
            }
            break;
        case 1:
        case 2:
        case 3:
            if ((c & 0xc0) == 0x80) {
                u <<= 6;
                u |= c & 0x3f;
                k--;
                if (k == 0) {
                    switch (n) {
                    case 1:
                        if (u < 0x80) return FALSE;
                        break;
                    case 2:
                        if (u < 0x800) return FALSE;
                        break;
                    case 3:
                        if (u < 0x10000 || u > 0x10ffff) return FALSE;
                        break;
                    default:
                        assert(((void)"unexpected control flow", 0));
                        return FALSE; /* never reached */
                    }
                    u = 0;
                    n = 0;
                }
            }
            else {
                return FALSE;
            }
            break;
        default:
            assert(((void)"unexpected control flow", 0));
            return FALSE; /* never reached */
        }
    }
    return (k == 0) ? TRUE : FALSE;
}

static size_t utf8_to_utf32(const char *seq, int *out) { /* without checking UTF-8 validity */
    const int c = (int)(unsigned char)seq[0];
    const size_t n =
        (c == 0) ? 0 : (c < 0x80) ? 1 :
        ((c & 0xe0) == 0xc0) ? 2 :
        ((c & 0xf0) == 0xe0) ? 3 :
        ((c & 0xf8) == 0xf0) ? 4 : 1;
    int u = 0;
    switch (n) {
    case 0:
    case 1:
        u = c;
        break;
    case 2:
        u = ((c & 0x1f) << 6) |
            ((int)(unsigned char)seq[1] & 0x3f);
        break;
    case 3:
        u = ((c & 0x0f) << 12) |
            (((int)(unsigned char)seq[1] & 0x3f) << 6) |
            (seq[1] ? ((int)(unsigned char)seq[2] & 0x3f) : 0);
        break;
    default:
        u = ((c & 0x07) << 18) |
            (((int)(unsigned char)seq[1] & 0x3f) << 12) |
            (seq[1] ? (((int)(unsigned char)seq[2] & 0x3f) << 6) : 0) |
            (seq[2] ? ((int)(unsigned char)seq[3] & 0x3f) : 0);
    }
    if (out) *out = u;
    return n;
}

static bool_t unescape_string(char *str, bool_t cls) { /* cls: TRUE if used for character class matching */
    bool_t b = TRUE;
    size_t i, j;
    for (j = 0, i = 0; str[i]; i++) {
        if (str[i] == '\\') {
            i++;
            switch (str[i]) {
            case '\0': str[j++] = '\\'; str[j] = '\0'; return FALSE;
            case '\'': str[j++] = '\''; break;
            case '\"': str[j++] = '\"'; break;
            case '0': str[j++] = '\x00'; break;
            case 'a': str[j++] = '\x07'; break;
            case 'b': str[j++] = '\x08'; break;
            case 'f': str[j++] = '\x0c'; break;
            case 'n': str[j++] = '\x0a'; break;
            case 'r': str[j++] = '\x0d'; break;
            case 't': str[j++] = '\x09'; break;
            case 'v': str[j++] = '\x0b'; break;
            case 'x':
                {
                    char s = 0, c;
                    size_t k;
                    for (k = 0; k < 2; k++) {
                        char d;
                        c = str[i + k + 1];
                        d = (c >= '0' && c <= '9') ? c - '0' :
                            (c >= 'a' && c <= 'f') ? c - 'a' + 10 :
                            (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1;
                        if (d < 0) break;
                        s = (s << 4) | d;
                    }
                    if (k < 2) {
                        const size_t l = i + k;
                        str[j++] = '\\'; str[j++] = 'x';
                        while (i <= l) str[j++] = str[++i];
                        if (c == '\0') return FALSE;
                        b = FALSE;
                        continue;
                    }
                    str[j++] = s;
                    i += 2;
                }
                break;
            case 'u':
                {
                    int s = 0, t = 0;
                    char c;
                    size_t k;
                    for (k = 0; k < 4; k++) {
                        char d;
                        c = str[i + k + 1];
                        d = (c >= '0' && c <= '9') ? c - '0' :
                            (c >= 'a' && c <= 'f') ? c - 'a' + 10 :
                            (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1;
                        if (d < 0) break;
                        s = (s << 4) | d;
                    }
                    if (k < 4 || (s & 0xfc00) == 0xdc00) { /* invalid character or invalid surrogate code point */
                        const size_t l = i + k;
                        str[j++] = '\\'; str[j++] = 'u';
                        while (i <= l) str[j++] = str[++i];
                        if (c == '\0') return FALSE;
                        b = FALSE;
                        continue;
                    }
                    if ((s & 0xfc00) == 0xd800) { /* surrogate pair */
                        for (k = 4; k < 10; k++) {
                            c = str[i + k + 1];
                            if (k == 4) {
                                if (c != '\\') break;
                            }
                            else if (k == 5) {
                                if (c != 'u') break;
                            }
                            else {
                                const char d =
                                    (c >= '0' && c <= '9') ? c - '0' :
                                    (c >= 'a' && c <= 'f') ? c - 'a' + 10 :
                                    (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1;
                                if (d < 0) break;
                                t = (t << 4) | d;
                            }
                        }
                        if (k < 10 || (t & 0xfc00) != 0xdc00) { /* invalid character or invalid surrogate code point */
                            const size_t l = i + 4; /* NOTE: Not i + k to redo with recovery. */
                            str[j++] = '\\'; str[j++] = 'u';
                            while (i <= l) str[j++] = str[++i];
                            b = FALSE;
                            continue;
                        }
                    }
                    {
                        const int u = t ? ((((s & 0x03ff) + 0x0040) << 10) | (t & 0x03ff)) : s;
                        if (u < 0x0080) {
                            str[j++] = (char)u;
                        }
                        else if (u < 0x0800) {
                            str[j++] = (char)(0xc0 | (u >> 6));
                            str[j++] = (char)(0x80 | (u & 0x3f));
                        }
                        else if (u < 0x010000) {
                            str[j++] = (char)(0xe0 | (u >> 12));
                            str[j++] = (char)(0x80 | ((u >> 6) & 0x3f));
                            str[j++] = (char)(0x80 | (u & 0x3f));
                        }
                        else if (u < 0x110000) {
                            str[j++] = (char)(0xf0 | (u >> 18));
                            str[j++] = (char)(0x80 | ((u >> 12) & 0x3f));
                            str[j++] = (char)(0x80 | ((u >>  6) & 0x3f));
                            str[j++] = (char)(0x80 | (u & 0x3f));
                        }
                        else { /* never reached theoretically; in case */
                            const size_t l = i + 10;
                            str[j++] = '\\'; str[j++] = 'u';
                            while (i <= l) str[j++] = str[++i];
                            b = FALSE;
                            continue;
                        }
                    }
                    i += t ? 10 : 4;
                }
                break;
            case '\n': break;
            case '\r': if (str[i + 1] == '\n') i++; break;
            case '\\':
                if (cls) str[j++] = '\\'; /* left for character class matching (ex. considering [\^\]\\]) */
                str[j++] = '\\';
                break;
            default: str[j++] = '\\'; str[j++] = str[i];
            }
        }
        else {
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
    return b;
}

static const char *escape_character(char ch, char (*buf)[5]) {
    switch (ch) {
    case '\x00': strncpy(*buf, "\\0", 5); break;
    case '\x07': strncpy(*buf, "\\a", 5); break;
    case '\x08': strncpy(*buf, "\\b", 5); break;
    case '\x0c': strncpy(*buf, "\\f", 5); break;
    case '\x0a': strncpy(*buf, "\\n", 5); break;
    case '\x0d': strncpy(*buf, "\\r", 5); break;
    case '\x09': strncpy(*buf, "\\t", 5); break;
    case '\x0b': strncpy(*buf, "\\v", 5); break;
    case '\\':  strncpy(*buf, "\\\\", 5); break;
    case '\'':  strncpy(*buf, "\\\'", 5); break;
    case '\"':  strncpy(*buf, "\\\"", 5); break;
    default:
        if (ch >= '\x20' && ch < '\x7f')
            snprintf(*buf, 5, "%c", ch);
        else
            snprintf(*buf, 5, "\\x%02x", (int)(unsigned char)ch);
    }
    (*buf)[4] = '\0';
    return *buf;
}

static void remove_heading_blank(char *str) {
    size_t i, j;
    for (i = 0; str[i]; i++) {
        if (
            str[i] != ' '  &&
            str[i] != '\v' &&
            str[i] != '\f' &&
            str[i] != '\t' &&
            str[i] != '\n' &&
            str[i] != '\r'
        ) break;
    }
    for (j = 0; str[i]; i++) {
        str[j++] = str[i];
    }
    str[j] = '\0';
}

static void remove_trailing_blank(char *str) {
    size_t i, j;
    for (j = 0, i = 0; str[i]; i++) {
        if (
            str[i] != ' '  &&
            str[i] != '\v' &&
            str[i] != '\f' &&
            str[i] != '\t' &&
            str[i] != '\n' &&
            str[i] != '\r'
        ) j = i + 1;
    }
    str[j] = '\0';
}

static void make_header_identifier(char *str) {
    size_t i;
    for (i = 0; str[i]; i++) {
        str[i] =
            ((str[i] >= 'A' && str[i] <= 'Z') || (str[i] >= '0' && str[i] <= '9')) ? str[i] :
            (str[i] >= 'a' && str[i] <= 'z') ? str[i] - 'a' + 'A' : '_';
    }
}

static void write_characters(FILE *stream, char ch, size_t len) {
    size_t i;
    for (i = 0; i < len; i++) fputc_e(ch, stream);
}

static void write_text(FILE *stream, const char *ptr, size_t len) {
    size_t i;
    for (i = 0; i < len; i++) {
        if (ptr[i] == '\r') {
            if (i + 1 < len && ptr[i + 1] == '\n') i++;
            fputc_e('\n', stream);
        }
        else {
            fputc_e(ptr[i], stream);
        }
    }
}

static void write_code_block(FILE *stream, const char *ptr, size_t len, size_t indent) {
    size_t i, j, k;
    j = find_first_trailing_space(ptr, 0, len, &k);
    for (i = 0; i < j; i++) {
        if (
            ptr[i] != ' '  &&
            ptr[i] != '\v' &&
            ptr[i] != '\f' &&
            ptr[i] != '\t'
        ) break;
    }
    if (i < j) {
        write_characters(stream, ' ', indent);
        write_text(stream, ptr + i, j - i);
        fputc_e('\n', stream);
    }
    if (k < len) {
        size_t m = VOID_VALUE;
        size_t h;
        for (i = k; i < len; i = h) {
            j = find_first_trailing_space(ptr, i, len, &h);
            if (i < j) {
                const size_t l = count_indent_spaces(ptr, i, j, NULL);
                if (m == VOID_VALUE || m > l) m = l;
            }
        }
        for (i = k; i < len; i = h) {
            j = find_first_trailing_space(ptr, i, len, &h);
            if (i < j) {
                const size_t l = count_indent_spaces(ptr, i, j, &i);
                assert(m != VOID_VALUE); /* m must have a valid value */
                assert(l >= m);
                write_characters(stream, ' ', l - m + indent);
                write_text(stream, ptr + i, j - i);
                fputc_e('\n', stream);
            }
            else if (h < len) {
                fputc_e('\n', stream);
            }
        }
    }
}

static const char *extract_filename(const char *path) {
    size_t i = strlen(path);
    while (i > 0) {
        i--;
        if (path[i] == '/' || path[i] == '\\' || path[i] == ':') return path + i + 1;
    }
    return path;
}

static const char *extract_fileext(const char *path) {
    const size_t n = strlen(path);
    size_t i = n;
    while (i > 0) {
        i--;
        if (path[i] == '/' || path[i] == '\\' || path[i] == ':') break;
        if (path[i] == '.') return path + i;
    }
    return path + n;
}

static char *replace_fileext(const char *path, const char *ext) {
    const char *const p = extract_fileext(path);
    const size_t m = p - path;
    const size_t n = strlen(ext);
    char *const s = (char *)malloc_e(m + n + 2);
    memcpy(s, path, m);
    s[m] = '.';
    memcpy(s + m + 1, ext, n + 1);
    return s;
}

static char *add_fileext(const char *path, const char *ext) {
    const size_t m = strlen(path);
    const size_t n = strlen(ext);
    char *const s = (char *)malloc_e(m + n + 2);
    memcpy(s, path, m);
    s[m] = '.';
    memcpy(s + m + 1, ext, n + 1);
    return s;
}

static size_t hash_string(const char *str) {
    size_t i, h = 0;
    for (i = 0; str[i]; i++) {
        h = h * 31 + str[i];
    }
    return h;
}

static size_t populate_bits(size_t x) {
    x |= x >>  1;
    x |= x >>  2;
    x |= x >>  4;
    x |= x >>  8;
    x |= x >> 16;
#if (defined __SIZEOF_SIZE_T__ && __SIZEOF_SIZE_T__ == 8) /* gcc or clang */ || defined _WIN64 /* MSVC */
    x |= x >> 32;
#endif
    return x;
}

static size_t column_number(const context_t *ctx) { /* 0-based */
    assert(ctx->bufpos + ctx->bufcur >= ctx->linepos);
    return ctx->bufpos + ctx->bufcur - ctx->linepos;
}

static void char_array__init(char_array_t *array) {
    array->len = 0;
    array->max = 0;
    array->buf = NULL;
}

static void char_array__add(char_array_t *array, char ch) {
    if (array->max <= array->len) {
        const size_t n = array->len + 1;
        size_t m = array->max;
        if (m == 0) m = BUFFER_MIN_SIZE;
        while (m < n && m != 0) m <<= 1;
        if (m == 0) m = n; /* in case of shift overflow */
        array->buf = (char *)realloc_e(array->buf, m);
        array->max = m;
    }
    array->buf[array->len++] = ch;
}

static void char_array__append(char_array_t *array, const char *str, size_t len) {
    if (array->max < array->len + len) {
        const size_t n = array->len + len;
        size_t m = array->max;
        if (m == 0) m = BUFFER_MIN_SIZE;
        while (m < n && m != 0) m <<= 1;
        if (m == 0) m = n; /* in case of shift overflow */
        array->buf = (char *)realloc_e(array->buf, m);
        array->max = m;
    }
    memcpy(array->buf + array->len, str, len);
    array->len += len;
}

static void char_array__term(char_array_t *array) {
    free(array->buf);
}

static void node_array__init(node_array_t *array) {
    array->len = 0;
    array->max = 0;
    array->buf = NULL;
}

static void node_array__add(node_array_t *array, node_t *node) {
    if (array->max <= array->len) {
        const size_t n = array->len + 1;
        size_t m = array->max;
        if (m == 0) m = ARRAY_MIN_SIZE;
        while (m < n && m != 0) m <<= 1;
        if (m == 0) m = n; /* in case of shift overflow */
        array->buf = (node_t **)realloc_e(array->buf, sizeof(node_t *) * m);
        array->max = m;
    }
    array->buf[array->len++] = node;
}

static void destroy_node(node_t *node);

static void node_array__term(node_array_t *array) {
    while (array->len > 0) {
        array->len--;
        destroy_node(array->buf[array->len]);
    }
    free(array->buf);
}

static void node_const_array__init(node_const_array_t *array) {
    array->len = 0;
    array->max = 0;
    array->buf = NULL;
}

static void node_const_array__add(node_const_array_t *array, const node_t *node) {
    if (array->max <= array->len) {
        const size_t n = array->len + 1;
        size_t m = array->max;
        if (m == 0) m = ARRAY_MIN_SIZE;
        while (m < n && m != 0) m <<= 1;
        if (m == 0) m = n; /* in case of shift overflow */
        array->buf = (const node_t **)realloc_e((node_t **)array->buf, sizeof(const node_t *) * m);
        array->max = m;
    }
    array->buf[array->len++] = node;
}

static void node_const_array__clear(node_const_array_t *array) {
    array->len = 0;
}

static void node_const_array__copy(node_const_array_t *array, const node_const_array_t *src) {
    size_t i;
    node_const_array__clear(array);
    for (i = 0; i < src->len; i++) {
        node_const_array__add(array, src->buf[i]);
    }
}

static void node_const_array__term(node_const_array_t *array) {
    free((node_t **)array->buf);
}

static context_t *create_context(const char *iname, const char *oname, bool_t ascii, bool_t debug) {
    context_t *const ctx = (context_t *)malloc_e(sizeof(context_t));
    ctx->iname = strdup_e((iname && iname[0]) ? iname : "-");
    ctx->sname = (oname && oname[0]) ? add_fileext(oname, "c") : replace_fileext(ctx->iname, "c");
    ctx->hname = (oname && oname[0]) ? add_fileext(oname, "h") : replace_fileext(ctx->iname, "h");
    ctx->ifile = (iname && iname[0]) ? fopen_rb_e(ctx->iname) : stdin;
    ctx->hid = strdup_e(ctx->hname); make_header_identifier(ctx->hid);
    ctx->vtype = NULL;
    ctx->atype = NULL;
    ctx->prefix = NULL;
    ctx->ascii = ascii;
    ctx->debug = debug;
    ctx->flags = CODE_FLAG__NONE;
    ctx->errnum = 0;
    ctx->linenum = 0;
    ctx->linepos = 0;
    ctx->bufpos = 0;
    ctx->bufcur = 0;
    char_array__init(&ctx->buffer);
    node_array__init(&ctx->rules);
    ctx->rulehash.mod = 0;
    ctx->rulehash.max = 0;
    ctx->rulehash.buf = NULL;
    char_array__init(&ctx->esource);
    char_array__init(&ctx->eheader);
    char_array__init(&ctx->source);
    char_array__init(&ctx->header);
    return ctx;
}

static node_t *create_node(node_type_t type) {
    node_t *const node = (node_t *)malloc_e(sizeof(node_t));
    node->type = type;
    switch (node->type) {
    case NODE_RULE:
        node->data.rule.name = NULL;
        node->data.rule.expr = NULL;
        node->data.rule.ref = 0;
        node_const_array__init(&node->data.rule.vars);
        node_const_array__init(&node->data.rule.capts);
        node_const_array__init(&node->data.rule.codes);
        node->data.rule.line = VOID_VALUE;
        node->data.rule.col = VOID_VALUE;
        break;
    case NODE_REFERENCE:
        node->data.reference.var = NULL;
        node->data.reference.index = VOID_VALUE;
        node->data.reference.name = NULL;
        node->data.reference.rule = NULL;
        node->data.reference.line = VOID_VALUE;
        node->data.reference.col = VOID_VALUE;
        break;
    case NODE_STRING:
        node->data.string.value = NULL;
        break;
    case NODE_CHARCLASS:
        node->data.charclass.value = NULL;
        break;
    case NODE_QUANTITY:
        node->data.quantity.min = node->data.quantity.max = 0;
        node->data.quantity.expr = NULL;
        break;
    case NODE_PREDICATE:
        node->data.predicate.neg = FALSE;
        node->data.predicate.expr = NULL;
        break;
    case NODE_SEQUENCE:
        node_array__init(&node->data.sequence.nodes);
        break;
    case NODE_ALTERNATE:
        node_array__init(&node->data.alternate.nodes);
        break;
    case NODE_CAPTURE:
        node->data.capture.expr = NULL;
        node->data.capture.index = VOID_VALUE;
        break;
    case NODE_EXPAND:
        node->data.expand.index = VOID_VALUE;
        node->data.expand.line = VOID_VALUE;
        node->data.expand.col = VOID_VALUE;
        break;
    case NODE_ACTION:
        node->data.action.value = NULL;
        node->data.action.index = VOID_VALUE;
        node_const_array__init(&node->data.action.vars);
        node_const_array__init(&node->data.action.capts);
        break;
    case NODE_ERROR:
        node->data.error.expr = NULL;
        node->data.error.value = NULL;
        node->data.error.index = VOID_VALUE;
        node_const_array__init(&node->data.error.vars);
        node_const_array__init(&node->data.error.capts);
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
    return node;
}

static void destroy_node(node_t *node) {
    if (node == NULL) return;
    switch (node->type) {
    case NODE_RULE:
        node_const_array__term(&node->data.rule.codes);
        node_const_array__term(&node->data.rule.capts);
        node_const_array__term(&node->data.rule.vars);
        destroy_node(node->data.rule.expr);
        free(node->data.rule.name);
        break;
    case NODE_REFERENCE:
        free(node->data.reference.name);
        free(node->data.reference.var);
        break;
    case NODE_STRING:
        free(node->data.string.value);
        break;
    case NODE_CHARCLASS:
        free(node->data.charclass.value);
        break;
    case NODE_QUANTITY:
        destroy_node(node->data.quantity.expr);
        break;
    case NODE_PREDICATE:
        destroy_node(node->data.predicate.expr);
        break;
    case NODE_SEQUENCE:
        node_array__term(&node->data.sequence.nodes);
        break;
    case NODE_ALTERNATE:
        node_array__term(&node->data.alternate.nodes);
        break;
    case NODE_CAPTURE:
        destroy_node(node->data.capture.expr);
        break;
    case NODE_EXPAND:
        break;
    case NODE_ACTION:
        node_const_array__term(&node->data.action.capts);
        node_const_array__term(&node->data.action.vars);
        free(node->data.action.value);
        break;
    case NODE_ERROR:
        node_const_array__term(&node->data.error.capts);
        node_const_array__term(&node->data.error.vars);
        free(node->data.error.value);
        destroy_node(node->data.error.expr);
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
    free(node);
}

static void destroy_context(context_t *ctx) {
    if (ctx == NULL) return;
    char_array__term(&ctx->header);
    char_array__term(&ctx->source);
    char_array__term(&ctx->eheader);
    char_array__term(&ctx->esource);
    free((node_t **)ctx->rulehash.buf);
    node_array__term(&ctx->rules);
    char_array__term(&ctx->buffer);
    free(ctx->prefix);
    free(ctx->atype);
    free(ctx->vtype);
    free(ctx->hid);
    fclose_e(ctx->ifile);
    free(ctx->hname);
    free(ctx->sname);
    free(ctx->iname);
    free(ctx);
}

static void make_rulehash(context_t *ctx) {
    size_t i, j;
    ctx->rulehash.mod = populate_bits(ctx->rules.len * 4);
    ctx->rulehash.max = ctx->rulehash.mod + 1;
    ctx->rulehash.buf = (const node_t **)realloc_e((node_t **)ctx->rulehash.buf, sizeof(const node_t *) * ctx->rulehash.max);
    for (i = 0; i < ctx->rulehash.max; i++) {
        ctx->rulehash.buf[i] = NULL;
    }
    for (i = 0; i < ctx->rules.len; i++) {
        assert(ctx->rules.buf[i]->type == NODE_RULE);
        j = hash_string(ctx->rules.buf[i]->data.rule.name) & ctx->rulehash.mod;
        while (ctx->rulehash.buf[j] != NULL) {
            if (strcmp(ctx->rules.buf[i]->data.rule.name, ctx->rulehash.buf[j]->data.rule.name) == 0) {
                assert(ctx->rules.buf[i]->data.rule.ref == 0);
                assert(ctx->rulehash.buf[j]->data.rule.ref == 0);
                ctx->rules.buf[i]->data.rule.ref = -1;
                goto EXCEPTION;
            }
            j = (j + 1) & ctx->rulehash.mod;
        }
        ctx->rulehash.buf[j] = ctx->rules.buf[i];

    EXCEPTION:;
    }
}

static const node_t *lookup_rulehash(const context_t *ctx, const char *name) {
    size_t j = hash_string(name) & ctx->rulehash.mod;
    while (ctx->rulehash.buf[j] != NULL && strcmp(name, ctx->rulehash.buf[j]->data.rule.name) != 0) {
        j = (j + 1) & ctx->rulehash.mod;
    }
    return (ctx->rulehash.buf[j] != NULL) ? ctx->rulehash.buf[j] : NULL;
}

static void link_references(context_t *ctx, node_t *node) {
    if (node == NULL) return;
    switch (node->type) {
    case NODE_RULE:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    case NODE_REFERENCE:
        node->data.reference.rule = lookup_rulehash(ctx, node->data.reference.name);
        if (node->data.reference.rule == NULL) {
            print_error("%s:" FMT_LU ":" FMT_LU ": No definition of rule '%s'\n",
                ctx->iname, (ulong_t)(node->data.reference.line + 1), (ulong_t)(node->data.reference.col + 1),
                node->data.reference.name);
            ctx->errnum++;
        }
        else {
            assert(node->data.reference.rule->type == NODE_RULE);
            ((node_t *)node->data.reference.rule)->data.rule.ref++;
        }
        break;
    case NODE_STRING:
        break;
    case NODE_CHARCLASS:
        break;
    case NODE_QUANTITY:
        link_references(ctx, node->data.quantity.expr);
        break;
    case NODE_PREDICATE:
        link_references(ctx, node->data.predicate.expr);
        break;
    case NODE_SEQUENCE:
        {
            size_t i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                link_references(ctx, node->data.sequence.nodes.buf[i]);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            size_t i;
            for (i = 0; i < node->data.alternate.nodes.len; i++) {
                link_references(ctx, node->data.alternate.nodes.buf[i]);
            }
        }
        break;
    case NODE_CAPTURE:
        link_references(ctx, node->data.capture.expr);
        break;
    case NODE_EXPAND:
        break;
    case NODE_ACTION:
        break;
    case NODE_ERROR:
        link_references(ctx, node->data.error.expr);
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
}

static void verify_variables(context_t *ctx, node_t *node, node_const_array_t *vars) {
    node_const_array_t a;
    const bool_t b = (vars == NULL) ? TRUE : FALSE;
    if (node == NULL) return;
    if (b) {
        node_const_array__init(&a);
        vars = &a;
    }
    switch (node->type) {
    case NODE_RULE:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    case NODE_REFERENCE:
        if (node->data.reference.index != VOID_VALUE) {
            size_t i;
            for (i = 0; i < vars->len; i++) {
                assert(vars->buf[i]->type == NODE_REFERENCE);
                if (node->data.reference.index == vars->buf[i]->data.reference.index) break;
            }
            if (i == vars->len) node_const_array__add(vars, node);
        }
        break;
    case NODE_STRING:
        break;
    case NODE_CHARCLASS:
        break;
    case NODE_QUANTITY:
        verify_variables(ctx, node->data.quantity.expr, vars);
        break;
    case NODE_PREDICATE:
        verify_variables(ctx, node->data.predicate.expr, vars);
        break;
    case NODE_SEQUENCE:
        {
            size_t i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                verify_variables(ctx, node->data.sequence.nodes.buf[i], vars);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            size_t i, j, k, m = vars->len;
            node_const_array_t v;
            node_const_array__init(&v);
            node_const_array__copy(&v, vars);
            for (i = 0; i < node->data.alternate.nodes.len; i++) {
                v.len = m;
                verify_variables(ctx, node->data.alternate.nodes.buf[i], &v);
                for (j = m; j < v.len; j++) {
                    for (k = m; k < vars->len; k++) {
                        if (v.buf[j]->data.reference.index == vars->buf[k]->data.reference.index) break;
                    }
                    if (k == vars->len) node_const_array__add(vars, v.buf[j]);
                }
            }
            node_const_array__term(&v);
        }
        break;
    case NODE_CAPTURE:
        verify_variables(ctx, node->data.capture.expr, vars);
        break;
    case NODE_EXPAND:
        break;
    case NODE_ACTION:
        node_const_array__copy(&node->data.action.vars, vars);
        break;
    case NODE_ERROR:
        node_const_array__copy(&node->data.error.vars, vars);
        verify_variables(ctx, node->data.error.expr, vars);
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
    if (b) {
        node_const_array__term(&a);
    }
}

static void verify_captures(context_t *ctx, node_t *node, node_const_array_t *capts) {
    node_const_array_t a;
    const bool_t b = (capts == NULL) ? TRUE : FALSE;
    if (node == NULL) return;
    if (b) {
        node_const_array__init(&a);
        capts = &a;
    }
    switch (node->type) {
    case NODE_RULE:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    case NODE_REFERENCE:
        break;
    case NODE_STRING:
        break;
    case NODE_CHARCLASS:
        break;
    case NODE_QUANTITY:
        verify_captures(ctx, node->data.quantity.expr, capts);
        break;
    case NODE_PREDICATE:
        verify_captures(ctx, node->data.predicate.expr, capts);
        break;
    case NODE_SEQUENCE:
        {
            size_t i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                verify_captures(ctx, node->data.sequence.nodes.buf[i], capts);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            size_t i, j, m = capts->len;
            node_const_array_t v;
            node_const_array__init(&v);
            node_const_array__copy(&v, capts);
            for (i = 0; i < node->data.alternate.nodes.len; i++) {
                v.len = m;
                verify_captures(ctx, node->data.alternate.nodes.buf[i], &v);
                for (j = m; j < v.len; j++) {
                    node_const_array__add(capts, v.buf[j]);
                }
            }
            node_const_array__term(&v);
        }
        break;
    case NODE_CAPTURE:
        verify_captures(ctx, node->data.capture.expr, capts);
        node_const_array__add(capts, node);
        break;
    case NODE_EXPAND:
        {
            size_t i;
            for (i = 0; i < capts->len; i++) {
                assert(capts->buf[i]->type == NODE_CAPTURE);
                if (node->data.expand.index == capts->buf[i]->data.capture.index) break;
            }
            if (i >= capts->len && node->data.expand.index != VOID_VALUE) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Capture " FMT_LU " not available at this position\n",
                    ctx->iname, (ulong_t)(node->data.expand.line + 1), (ulong_t)(node->data.expand.col + 1), (ulong_t)(node->data.expand.index + 1));
                ctx->errnum++;
            }
        }
        break;
    case NODE_ACTION:
        node_const_array__copy(&node->data.action.capts, capts);
        break;
    case NODE_ERROR:
        node_const_array__copy(&node->data.error.capts, capts);
        verify_captures(ctx, node->data.error.expr, capts);
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
    if (b) {
        node_const_array__term(&a);
    }
}

static void dump_escaped_string(const char *s) {
    char buf[5];
    if (s == NULL) {
        fprintf(stdout, "null");
        return;
    }
    while (*s) {
        escape_character(*s++, &buf);
        fprintf(stdout, "%s", buf);
    }
}

static void dump_integer_value(size_t value) {
    if (value == VOID_VALUE) {
        fprintf(stdout, "void");
    }
    else {
        fprintf(stdout, FMT_LU, (ulong_t)value);
    }
}

static void dump_node(context_t *ctx, const node_t *node, const int indent) {
    if (node == NULL) return;
    switch (node->type) {
    case NODE_RULE:
        fprintf(stdout, "%*sRule(name:'%s', ref:%d, vars.len:" FMT_LU ", capts.len:" FMT_LU ", codes.len:" FMT_LU ") {\n",
            indent, "", node->data.rule.name, node->data.rule.ref,
            (ulong_t)node->data.rule.vars.len, (ulong_t)node->data.rule.capts.len, (ulong_t)node->data.rule.codes.len);
        dump_node(ctx, node->data.rule.expr, indent + 2);
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_REFERENCE:
        fprintf(stdout, "%*sReference(var:'%s', index:", indent, "", node->data.reference.var);
        dump_integer_value(node->data.reference.index);
        fprintf(stdout, ", name:'%s', rule:'%s')\n", node->data.reference.name,
            (node->data.reference.rule) ? node->data.reference.rule->data.rule.name : NULL);
        break;
    case NODE_STRING:
        fprintf(stdout, "%*sString(value:'", indent, "");
        dump_escaped_string(node->data.string.value);
        fprintf(stdout, "')\n");
        break;
    case NODE_CHARCLASS:
        fprintf(stdout, "%*sCharclass(value:'", indent, "");
        dump_escaped_string(node->data.charclass.value);
        fprintf(stdout, "')\n");
        break;
    case NODE_QUANTITY:
        fprintf(stdout, "%*sQuantity(min:%d, max%d) {\n", indent, "", node->data.quantity.min, node->data.quantity.max);
        dump_node(ctx, node->data.quantity.expr, indent + 2);
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_PREDICATE:
        fprintf(stdout, "%*sPredicate(neg:%d) {\n", indent, "", node->data.predicate.neg);
        dump_node(ctx, node->data.predicate.expr, indent + 2);
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_SEQUENCE:
        fprintf(stdout, "%*sSequence(max:" FMT_LU ", len:" FMT_LU ") {\n",
            indent, "", (ulong_t)node->data.sequence.nodes.max, (ulong_t)node->data.sequence.nodes.len);
        {
            size_t i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                dump_node(ctx, node->data.sequence.nodes.buf[i], indent + 2);
            }
        }
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_ALTERNATE:
        fprintf(stdout, "%*sAlternate(max:" FMT_LU ", len:" FMT_LU ") {\n",
            indent, "", (ulong_t)node->data.alternate.nodes.max, (ulong_t)node->data.alternate.nodes.len);
        {
            size_t i;
            for (i = 0; i < node->data.alternate.nodes.len; i++) {
                dump_node(ctx, node->data.alternate.nodes.buf[i], indent + 2);
            }
        }
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_CAPTURE:
        fprintf(stdout, "%*sCapture(index:", indent, "");
        dump_integer_value(node->data.capture.index);
        fprintf(stdout, ") {\n");
        dump_node(ctx, node->data.capture.expr, indent + 2);
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    case NODE_EXPAND:
        fprintf(stdout, "%*sExpand(index:", indent, "");
        dump_integer_value(node->data.expand.index);
        fprintf(stdout, ")\n");
        break;
    case NODE_ACTION:
        fprintf(stdout, "%*sAction(index:", indent, "");
        dump_integer_value(node->data.action.index);
        fprintf(stdout, ", value:{");
        dump_escaped_string(node->data.action.value);
        fprintf(stdout, "}, vars:");
        if (node->data.action.vars.len + node->data.action.capts.len > 0) {
            size_t i;
            fprintf(stdout, "\n");
            for (i = 0; i < node->data.action.vars.len; i++) {
                fprintf(stdout, "%*s'%s'\n", indent + 2, "", node->data.action.vars.buf[i]->data.reference.var);
            }
            for (i = 0; i < node->data.action.capts.len; i++) {
                fprintf(stdout, "%*s$" FMT_LU "\n", indent + 2, "", (ulong_t)(node->data.action.capts.buf[i]->data.capture.index + 1));
            }
            fprintf(stdout, "%*s)\n", indent, "");
        }
        else {
            fprintf(stdout, "none)\n");
        }
        break;
    case NODE_ERROR:
        fprintf(stdout, "%*sError(index:", indent, "");
        dump_integer_value(node->data.error.index);
        fprintf(stdout, ", value:{");
        dump_escaped_string(node->data.error.value);
        fprintf(stdout, "}, vars:\n");
        {
            size_t i;
            for (i = 0; i < node->data.error.vars.len; i++) {
                fprintf(stdout, "%*s'%s'\n", indent + 2, "", node->data.error.vars.buf[i]->data.reference.var);
            }
            for (i = 0; i < node->data.error.capts.len; i++) {
                fprintf(stdout, "%*s$" FMT_LU "\n", indent + 2, "", (ulong_t)(node->data.error.capts.buf[i]->data.capture.index + 1));
            }
        }
        fprintf(stdout, "%*s) {\n", indent, "");
        dump_node(ctx, node->data.error.expr, indent + 2);
        fprintf(stdout, "%*s}\n", indent, "");
        break;
    default:
        print_error("%*sInternal error [%d]\n", indent, "", __LINE__);
        exit(-1);
    }
}

static size_t refill_buffer(context_t *ctx, size_t num) {
    if (ctx->buffer.len >= ctx->bufcur + num) return ctx->buffer.len - ctx->bufcur;
    while (ctx->buffer.len < ctx->bufcur + num) {
        const int c = fgetc_e(ctx->ifile);
        if (c == EOF) break;
        char_array__add(&ctx->buffer, (char)c);
    }
    return ctx->buffer.len - ctx->bufcur;
}

static void commit_buffer(context_t *ctx) {
    assert(ctx->buffer.len >= ctx->bufcur);
    memmove(ctx->buffer.buf, ctx->buffer.buf + ctx->bufcur, ctx->buffer.len - ctx->bufcur);
    ctx->buffer.len -= ctx->bufcur;
    ctx->bufpos += ctx->bufcur;
    ctx->bufcur = 0;
}

static bool_t match_eof(context_t *ctx) {
    return (refill_buffer(ctx, 1) < 1) ? TRUE : FALSE;
}

static bool_t match_eol(context_t *ctx) {
    if (refill_buffer(ctx, 1) >= 1) {
        switch (ctx->buffer.buf[ctx->bufcur]) {
        case '\n':
            ctx->bufcur++;
            ctx->linenum++;
            ctx->linepos = ctx->bufpos + ctx->bufcur;
            return TRUE;
        case '\r':
            ctx->bufcur++;
            if (refill_buffer(ctx, 1) >= 1) {
                if (ctx->buffer.buf[ctx->bufcur] == '\n') ctx->bufcur++;
            }
            ctx->linenum++;
            ctx->linepos = ctx->bufpos + ctx->bufcur;
            return TRUE;
        }
    }
    return FALSE;
}

static bool_t match_character(context_t *ctx, char ch) {
    if (refill_buffer(ctx, 1) >= 1) {
        if (ctx->buffer.buf[ctx->bufcur] == ch) {
            ctx->bufcur++;
            return TRUE;
        }
    }
    return FALSE;
}

static bool_t match_character_range(context_t *ctx, char min, char max) {
    if (refill_buffer(ctx, 1) >= 1) {
        const char c = ctx->buffer.buf[ctx->bufcur];
        if (c >= min && c <= max) {
            ctx->bufcur++;
            return TRUE;
        }
    }
    return FALSE;
}

static bool_t match_character_set(context_t *ctx, const char *chs) {
    if (refill_buffer(ctx, 1) >= 1) {
        const char c = ctx->buffer.buf[ctx->bufcur];
        size_t i;
        for (i = 0; chs[i]; i++) {
            if (c == chs[i]) {
                ctx->bufcur++;
                return TRUE;
            }
        }
    }
    return FALSE;
}

static bool_t match_character_any(context_t *ctx) {
    if (refill_buffer(ctx, 1) >= 1) {
        ctx->bufcur++;
        return TRUE;
    }
    return FALSE;
}

static bool_t match_string(context_t *ctx, const char *str) {
    const size_t n = strlen(str);
    if (refill_buffer(ctx, n) >= n) {
        if (strncmp(ctx->buffer.buf + ctx->bufcur, str, n) == 0) {
            ctx->bufcur += n;
            return TRUE;
        }
    }
    return FALSE;
}

static bool_t match_blank(context_t *ctx) {
    return match_character_set(ctx, " \t\v\f");
}

static bool_t match_section_line_(context_t *ctx, const char *head) {
    if (match_string(ctx, head)) {
        while (!match_eol(ctx) && !match_eof(ctx)) match_character_any(ctx);
        return TRUE;
    }
    return FALSE;
}

static bool_t match_section_line_continuable_(context_t *ctx, const char *head) {
    if (match_string(ctx, head)) {
        while (!match_eof(ctx)) {
            const size_t p = ctx->bufcur;
            if (match_eol(ctx)) {
                if (ctx->buffer.buf[p - 1] != '\\') break;
            }
            else {
                match_character_any(ctx);
            }
        }
        return TRUE;
    }
    return FALSE;
}

static bool_t match_section_block_(context_t *ctx, const char *left, const char *right, const char *name) {
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    if (match_string(ctx, left)) {
        while (!match_string(ctx, right)) {
            if (match_eof(ctx)) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Premature EOF in %s\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
                ctx->errnum++;
                break;
            }
            if (!match_eol(ctx)) match_character_any(ctx);
        }
        return TRUE;
    }
    return FALSE;
}

static bool_t match_quotation_(context_t *ctx, const char *left, const char *right, const char *name) {
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    if (match_string(ctx, left)) {
        while (!match_string(ctx, right)) {
            if (match_eof(ctx)) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Premature EOF in %s\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
                ctx->errnum++;
                break;
            }
            if (match_character(ctx, '\\')) {
                if (!match_eol(ctx)) match_character_any(ctx);
            }
            else {
                if (match_eol(ctx)) {
                    print_error("%s:" FMT_LU ":" FMT_LU ": Premature EOL in %s\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
                    ctx->errnum++;
                    break;
                }
                match_character_any(ctx);
            }
        }
        return TRUE;
    }
    return FALSE;
}

static bool_t match_directive_c(context_t *ctx) {
    return match_section_line_continuable_(ctx, "#");
}

static bool_t match_comment(context_t *ctx) {
    return match_section_line_(ctx, "#");
}

static bool_t match_comment_c(context_t *ctx) {
    return match_section_block_(ctx, "/*", "*/", "C comment");
}

static bool_t match_comment_cxx(context_t *ctx) {
    return match_section_line_(ctx, "//");
}

static bool_t match_quotation_single(context_t *ctx) {
    return match_quotation_(ctx, "\'", "\'", "single quotation");
}

static bool_t match_quotation_double(context_t *ctx) {
    return match_quotation_(ctx, "\"", "\"", "double quotation");
}

static bool_t match_character_class(context_t *ctx) {
    return match_quotation_(ctx, "[", "]", "character class");
}

static bool_t match_spaces(context_t *ctx) {
    size_t n = 0;
    while (match_blank(ctx) || match_eol(ctx) || match_comment(ctx)) n++;
    return (n > 0) ? TRUE : FALSE;
}

static bool_t match_number(context_t *ctx) {
    if (match_character_range(ctx, '0', '9')) {
        while (match_character_range(ctx, '0', '9'));
        return TRUE;
    }
    return FALSE;
}

static bool_t match_identifier(context_t *ctx) {
    if (
        match_character_range(ctx, 'a', 'z') ||
        match_character_range(ctx, 'A', 'Z') ||
        match_character(ctx, '_')
    ) {
        while (
            match_character_range(ctx, 'a', 'z') ||
            match_character_range(ctx, 'A', 'Z') ||
            match_character_range(ctx, '0', '9') ||
            match_character(ctx, '_')
        );
        return TRUE;
    }
    return FALSE;
}

static bool_t match_code_block(context_t *ctx) {
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    if (match_character(ctx, '{')) {
        int d = 1;
        for (;;) {
            if (match_eof(ctx)) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Premature EOF in code block\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
                ctx->errnum++;
                break;
            }
            if (
                match_directive_c(ctx) ||
                match_comment_c(ctx) ||
                match_comment_cxx(ctx) ||
                match_quotation_single(ctx) ||
                match_quotation_double(ctx)
            ) continue;
            if (match_character(ctx, '{')) {
                d++;
            }
            else if (match_character(ctx, '}')) {
                d--;
                if (d == 0) break;
            }
            else {
                if (!match_eol(ctx)) {
                    if (match_character(ctx, '$')) {
                        ctx->buffer.buf[ctx->bufcur - 1] = '_';
                    }
                    else {
                        match_character_any(ctx);
                    }
                }
            }
        }
        return TRUE;
    }
    return FALSE;
}

static bool_t match_footer_start(context_t *ctx) {
    return match_string(ctx, "%%");
}

static node_t *parse_expression(context_t *ctx, node_t *rule);

static node_t *parse_primary(context_t *ctx, node_t *rule) {
    const size_t p = ctx->bufcur;
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    node_t *n_p = NULL;
    if (match_identifier(ctx)) {
        const size_t q = ctx->bufcur;
        size_t r = VOID_VALUE, s = VOID_VALUE;
        match_spaces(ctx);
        if (match_character(ctx, ':')) {
            match_spaces(ctx);
            r = ctx->bufcur;
            if (!match_identifier(ctx)) goto EXCEPTION;
            s = ctx->bufcur;
            match_spaces(ctx);
        }
        if (match_string(ctx, "<-")) goto EXCEPTION;
        n_p = create_node(NODE_REFERENCE);
        if (r == VOID_VALUE) {
            assert(q >= p);
            n_p->data.reference.var = NULL;
            n_p->data.reference.index = VOID_VALUE;
            n_p->data.reference.name = strndup_e(ctx->buffer.buf + p, q - p);
        }
        else {
            assert(s != VOID_VALUE); /* s should have a valid value when r has a valid value */
            assert(q >= p);
            n_p->data.reference.var = strndup_e(ctx->buffer.buf + p, q - p);
            if (n_p->data.reference.var[0] == '_') {
                print_error("%s:" FMT_LU ":" FMT_LU ": Leading underscore in variable name '%s'\n",
                    ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), n_p->data.reference.var);
                ctx->errnum++;
            }
            {
                size_t i;
                for (i = 0; i < rule->data.rule.vars.len; i++) {
                    assert(rule->data.rule.vars.buf[i]->type == NODE_REFERENCE);
                    if (strcmp(n_p->data.reference.var, rule->data.rule.vars.buf[i]->data.reference.var) == 0) break;
                }
                if (i == rule->data.rule.vars.len) node_const_array__add(&rule->data.rule.vars, n_p);
                n_p->data.reference.index = i;
            }
            assert(s >= r);
            n_p->data.reference.name = strndup_e(ctx->buffer.buf + r, s - r);
        }
        n_p->data.reference.line = l;
        n_p->data.reference.col = m;
    }
    else if (match_character(ctx, '(')) {
        match_spaces(ctx);
        n_p = parse_expression(ctx, rule);
        if (n_p == NULL) goto EXCEPTION;
        if (!match_character(ctx, ')')) goto EXCEPTION;
        match_spaces(ctx);
    }
    else if (match_character(ctx, '<')) {
        match_spaces(ctx);
        n_p = create_node(NODE_CAPTURE);
        n_p->data.capture.index = rule->data.rule.capts.len;
        node_const_array__add(&rule->data.rule.capts, n_p);
        n_p->data.capture.expr = parse_expression(ctx, rule);
        if (n_p->data.capture.expr == NULL || !match_character(ctx, '>')) {
            rule->data.rule.capts.len = n_p->data.capture.index;
            goto EXCEPTION;
        }
        match_spaces(ctx);
    }
    else if (match_character(ctx, '$')) {
        size_t p;
        match_spaces(ctx);
        p = ctx->bufcur;
        if (match_number(ctx)) {
            const size_t q = ctx->bufcur;
            char *s;
            match_spaces(ctx);
            n_p = create_node(NODE_EXPAND);
            assert(q >= p);
            s = strndup_e(ctx->buffer.buf + p, q - p);
            n_p->data.expand.index = string_to_size_t(s);
            if (n_p->data.expand.index == VOID_VALUE) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Invalid unsigned number '%s'\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), s);
                ctx->errnum++;
            }
            else if (n_p->data.expand.index == 0) {
                print_error("%s:" FMT_LU ":" FMT_LU ": 0 not allowed\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
                ctx->errnum++;
            }
            else if (s[0] == '0') {
                print_error("%s:" FMT_LU ":" FMT_LU ": 0-prefixed number not allowed\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
                ctx->errnum++;
                n_p->data.expand.index = 0;
            }
            free(s);
            if (n_p->data.expand.index > 0 && n_p->data.expand.index != VOID_VALUE) {
                n_p->data.expand.index--;
                n_p->data.expand.line = l;
                n_p->data.expand.col = m;
            }
        }
        else {
            goto EXCEPTION;
        }
    }
    else if (match_character(ctx, '.')) {
        match_spaces(ctx);
        n_p = create_node(NODE_CHARCLASS);
        n_p->data.charclass.value = NULL;
        if (!ctx->ascii) {
            ctx->flags |= CODE_FLAG__UTF8_CHARCLASS_USED;
        }
    }
    else if (match_character_class(ctx)) {
        const size_t q = ctx->bufcur;
        match_spaces(ctx);
        n_p = create_node(NODE_CHARCLASS);
        n_p->data.charclass.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
        if (!unescape_string(n_p->data.charclass.value, TRUE)) {
            print_error("%s:" FMT_LU ":" FMT_LU ": Illegal escape sequence\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
            ctx->errnum++;
        }
        if (!ctx->ascii && !is_valid_utf8_string(n_p->data.charclass.value)) {
            print_error("%s:" FMT_LU ":" FMT_LU ": Invalid UTF-8 string\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
            ctx->errnum++;
        }
        if (!ctx->ascii && n_p->data.charclass.value[0] != '\0') {
            ctx->flags |= CODE_FLAG__UTF8_CHARCLASS_USED;
        }
    }
    else if (match_quotation_single(ctx) || match_quotation_double(ctx)) {
        const size_t q = ctx->bufcur;
        match_spaces(ctx);
        n_p = create_node(NODE_STRING);
        n_p->data.string.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
        if (!unescape_string(n_p->data.string.value, FALSE)) {
            print_error("%s:" FMT_LU ":" FMT_LU ": Illegal escape sequence\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
            ctx->errnum++;
        }
        if (!ctx->ascii && !is_valid_utf8_string(n_p->data.string.value)) {
            print_error("%s:" FMT_LU ":" FMT_LU ": Invalid UTF-8 string\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
            ctx->errnum++;
        }
    }
    else if (match_code_block(ctx)) {
        const size_t q = ctx->bufcur;
        match_spaces(ctx);
        n_p = create_node(NODE_ACTION);
        n_p->data.action.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
        n_p->data.action.index = rule->data.rule.codes.len;
        node_const_array__add(&rule->data.rule.codes, n_p);
    }
    else {
        goto EXCEPTION;
    }
    return n_p;

EXCEPTION:;
    destroy_node(n_p);
    ctx->bufcur = p;
    ctx->linenum = l;
    ctx->linepos = ctx->bufpos + p - m;
    return NULL;
}

static node_t *parse_term(context_t *ctx, node_t *rule) {
    const size_t p = ctx->bufcur;
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    node_t *n_p = NULL;
    node_t *n_q = NULL;
    node_t *n_r = NULL;
    node_t *n_t = NULL;
    const char t = match_character(ctx, '&') ? '&' : match_character(ctx, '!') ? '!' : '\0';
    if (t) match_spaces(ctx);
    n_p = parse_primary(ctx, rule);
    if (n_p == NULL) goto EXCEPTION;
    if (match_character(ctx, '*')) {
        match_spaces(ctx);
        n_q = create_node(NODE_QUANTITY);
        n_q->data.quantity.min = 0;
        n_q->data.quantity.max = -1;
        n_q->data.quantity.expr = n_p;
    }
    else if (match_character(ctx, '+')) {
        match_spaces(ctx);
        n_q = create_node(NODE_QUANTITY);
        n_q->data.quantity.min = 1;
        n_q->data.quantity.max = -1;
        n_q->data.quantity.expr = n_p;
    }
    else if (match_character(ctx, '?')) {
        match_spaces(ctx);
        n_q = create_node(NODE_QUANTITY);
        n_q->data.quantity.min = 0;
        n_q->data.quantity.max = 1;
        n_q->data.quantity.expr = n_p;
    }
    else {
        n_q = n_p;
    }
    switch (t) {
    case '&':
        n_r = create_node(NODE_PREDICATE);
        n_r->data.predicate.neg = FALSE;
        n_r->data.predicate.expr = n_q;
        break;
    case '!':
        n_r = create_node(NODE_PREDICATE);
        n_r->data.predicate.neg = TRUE;
        n_r->data.predicate.expr = n_q;
        break;
    default:
        n_r = n_q;
    }
    if (match_character(ctx, '~')) {
        size_t p;
        match_spaces(ctx);
        p = ctx->bufcur;
        if (match_code_block(ctx)) {
            const size_t q = ctx->bufcur;
            match_spaces(ctx);
            n_t = create_node(NODE_ERROR);
            n_t->data.error.expr = n_r;
            n_t->data.error.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
            n_t->data.error.index = rule->data.rule.codes.len;
            node_const_array__add(&rule->data.rule.codes, n_t);
        }
        else {
            goto EXCEPTION;
        }
    }
    else {
        n_t = n_r;
    }
    return n_t;

EXCEPTION:;
    destroy_node(n_r);
    ctx->bufcur = p;
    ctx->linenum = l;
    ctx->linepos = ctx->bufpos + p - m;
    return NULL;
}

static node_t *parse_sequence(context_t *ctx, node_t *rule) {
    const size_t p = ctx->bufcur;
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    node_array_t *a_t = NULL;
    node_t *n_t = NULL;
    node_t *n_u = NULL;
    node_t *n_s = NULL;
    n_t = parse_term(ctx, rule);
    if (n_t == NULL) goto EXCEPTION;
    n_u = parse_term(ctx, rule);
    if (n_u != NULL) {
        n_s = create_node(NODE_SEQUENCE);
        a_t = &n_s->data.sequence.nodes;
        node_array__add(a_t, n_t);
        node_array__add(a_t, n_u);
        while ((n_t = parse_term(ctx, rule)) != NULL) {
            node_array__add(a_t, n_t);
        }
    }
    else {
        n_s = n_t;
    }
    return n_s;

EXCEPTION:;
    ctx->bufcur = p;
    ctx->linenum = l;
    ctx->linepos = ctx->bufpos + p - m;
    return NULL;
}

static node_t *parse_expression(context_t *ctx, node_t *rule) {
    const size_t p = ctx->bufcur;
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    size_t q;
    node_array_t *a_s = NULL;
    node_t *n_s = NULL;
    node_t *n_e = NULL;
    n_s = parse_sequence(ctx, rule);
    if (n_s == NULL) goto EXCEPTION;
    q = ctx->bufcur;
    if (match_character(ctx, '/')) {
        ctx->bufcur = q;
        n_e = create_node(NODE_ALTERNATE);
        a_s = &n_e->data.alternate.nodes;
        node_array__add(a_s, n_s);
        while (match_character(ctx, '/')) {
            match_spaces(ctx);
            n_s = parse_sequence(ctx, rule);
            if (n_s == NULL) goto EXCEPTION;
            node_array__add(a_s, n_s);
        }
    }
    else {
        n_e = n_s;
    }
    return n_e;

EXCEPTION:;
    destroy_node(n_e);
    ctx->bufcur = p;
    ctx->linenum = l;
    ctx->linepos = ctx->bufpos + p - m;
    return NULL;
}

static node_t *parse_rule(context_t *ctx) {
    const size_t p = ctx->bufcur;
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    size_t q;
    node_t *n_r = NULL;
    if (!match_identifier(ctx)) goto EXCEPTION;
    q = ctx->bufcur;
    match_spaces(ctx);
    if (!match_string(ctx, "<-")) goto EXCEPTION;
    match_spaces(ctx);
    n_r = create_node(NODE_RULE);
    n_r->data.rule.expr = parse_expression(ctx, n_r);
    if (n_r->data.rule.expr == NULL) goto EXCEPTION;
    assert(q >= p);
    n_r->data.rule.name = strndup_e(ctx->buffer.buf + p, q - p);
    n_r->data.rule.line = l;
    n_r->data.rule.col = m;
    return n_r;

EXCEPTION:;
    destroy_node(n_r);
    ctx->bufcur = p;
    ctx->linenum = l;
    ctx->linepos = ctx->bufpos + p - m;
    return NULL;
}

static const char *get_value_type(context_t *ctx) {
    return (ctx->vtype && ctx->vtype[0]) ? ctx->vtype : "int";
}

static const char *get_auxil_type(context_t *ctx) {
    return (ctx->atype && ctx->atype[0]) ? ctx->atype : "void *";
}

static const char *get_prefix(context_t *ctx) {
    return (ctx->prefix && ctx->prefix[0]) ? ctx->prefix : "pcc";
}

static void dump_options(context_t *ctx) {
    fprintf(stdout, "value_type: '%s'\n", get_value_type(ctx));
    fprintf(stdout, "auxil_type: '%s'\n", get_auxil_type(ctx));
    fprintf(stdout, "prefix: '%s'\n", get_prefix(ctx));
}

static bool_t parse_directive_include_(context_t *ctx, const char *name, char_array_t *output1, char_array_t *output2) {
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    if (!match_string(ctx, name)) return FALSE;
    match_spaces(ctx);
    {
        const size_t p = ctx->bufcur;
        if (match_code_block(ctx)) {
            const size_t q = ctx->bufcur;
            match_spaces(ctx);
            if (output1 != NULL) {
                char_array__append(output1, ctx->buffer.buf + p + 1, q - p - 2);
                char_array__add(output1, '\n');
            }
            if (output2 != NULL) {
                char_array__append(output2, ctx->buffer.buf + p + 1, q - p - 2);
                char_array__add(output2, '\n');
            }
        }
        else {
            print_error("%s:" FMT_LU ":" FMT_LU ": Illegal %s syntax\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
            ctx->errnum++;
        }
    }
    return TRUE;
}

static bool_t parse_directive_string_(context_t *ctx, const char *name, char **output, string_flag_t mode) {
    const size_t l = ctx->linenum;
    const size_t m = column_number(ctx);
    if (!match_string(ctx, name)) return FALSE;
    match_spaces(ctx);
    {
        char *s = NULL;
        const size_t p = ctx->bufcur;
        const size_t lv = ctx->linenum;
        const size_t mv = column_number(ctx);
        size_t q;
        if (match_quotation_single(ctx) || match_quotation_double(ctx)) {
            q = ctx->bufcur;
            match_spaces(ctx);
            s = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
            if (!unescape_string(s, FALSE)) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Illegal escape sequence\n", ctx->iname, (ulong_t)(lv + 1), (ulong_t)(mv + 1));
                ctx->errnum++;
            }
        }
        else {
            print_error("%s:" FMT_LU ":" FMT_LU ": Illegal %s syntax\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
            ctx->errnum++;
        }
        if (s != NULL) {
            string_flag_t f = STRING_FLAG__NONE;
            bool_t b = TRUE;
            remove_heading_blank(s);
            remove_trailing_blank(s);
            assert((mode & ~7) == 0);
            if ((mode & STRING_FLAG__NOTEMPTY) && !is_filled_string(s)) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Empty string\n", ctx->iname, (ulong_t)(lv + 1), (ulong_t)(mv + 1));
                ctx->errnum++;
                f |= STRING_FLAG__NOTEMPTY;
            }
            if ((mode & STRING_FLAG__NOTVOID) && strcmp(s, "void") == 0) {
                print_error("%s:" FMT_LU ":" FMT_LU ": 'void' not allowed\n", ctx->iname, (ulong_t)(lv + 1), (ulong_t)(mv + 1));
                ctx->errnum++;
                f |= STRING_FLAG__NOTVOID;
            }
            if ((mode & STRING_FLAG__IDENTIFIER) && !is_identifier_string(s)) {
                if (!(f & STRING_FLAG__NOTEMPTY)) {
                    print_error("%s:" FMT_LU ":" FMT_LU ": Invalid identifier\n", ctx->iname, (ulong_t)(lv + 1), (ulong_t)(mv + 1));
                    ctx->errnum++;
                }
                f |= STRING_FLAG__IDENTIFIER;
            }
            if (*output != NULL) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Multiple %s definition\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1), name);
                ctx->errnum++;
                b = FALSE;
            }
            if (f == STRING_FLAG__NONE && b) {
                *output = s;
            }
            else {
                free(s); s = NULL;
            }
        }
    }
    return TRUE;
}

static bool_t parse(context_t *ctx) {
    {
        bool_t b = TRUE;
        match_spaces(ctx);
        for (;;) {
            size_t p, l, m;
            if (match_eof(ctx) || match_footer_start(ctx)) break;
            p = ctx->bufcur;
            l = ctx->linenum;
            m = column_number(ctx);
            if (
                parse_directive_include_(ctx, "%earlysource", &ctx->esource, NULL) ||
                parse_directive_include_(ctx, "%earlyheader", &ctx->eheader, NULL) ||
                parse_directive_include_(ctx, "%earlycommon", &ctx->esource, &ctx->eheader) ||
                parse_directive_include_(ctx, "%source", &ctx->source, NULL) ||
                parse_directive_include_(ctx, "%header", &ctx->header, NULL) ||
                parse_directive_include_(ctx, "%common", &ctx->source, &ctx->header) ||
                parse_directive_string_(ctx, "%value", &ctx->vtype, STRING_FLAG__NOTEMPTY | STRING_FLAG__NOTVOID) ||
                parse_directive_string_(ctx, "%auxil", &ctx->atype, STRING_FLAG__NOTEMPTY | STRING_FLAG__NOTVOID) ||
                parse_directive_string_(ctx, "%prefix", &ctx->prefix, STRING_FLAG__NOTEMPTY | STRING_FLAG__IDENTIFIER)
            ) {
                b = TRUE;
            }
            else if (match_character(ctx, '%')) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Invalid directive\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
                ctx->errnum++;
                match_identifier(ctx);
                match_spaces(ctx);
                b = TRUE;
            }
            else {
                node_t *const n_r = parse_rule(ctx);
                if (n_r == NULL) {
                    if (b) {
                        print_error("%s:" FMT_LU ":" FMT_LU ": Illegal rule syntax\n", ctx->iname, (ulong_t)(l + 1), (ulong_t)(m + 1));
                        ctx->errnum++;
                        b = FALSE;
                    }
                    ctx->linenum = l;
                    ctx->linepos = ctx->bufpos + p - m;
                    if (!match_identifier(ctx) && !match_spaces(ctx)) match_character_any(ctx);
                    continue;
                }
                node_array__add(&ctx->rules, n_r);
                b = TRUE;
            }
            commit_buffer(ctx);
        }
        commit_buffer(ctx);
    }
    {
        size_t i;
        make_rulehash(ctx);
        for (i = 0; i < ctx->rules.len; i++) {
            link_references(ctx, ctx->rules.buf[i]->data.rule.expr);
        }
        for (i = 1; i < ctx->rules.len; i++) {
            if (ctx->rules.buf[i]->data.rule.ref == 0) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Never used rule '%s'\n",
                    ctx->iname,
                    (ulong_t)(ctx->rules.buf[i]->data.rule.line + 1), (ulong_t)(ctx->rules.buf[i]->data.rule.col + 1),
                    ctx->rules.buf[i]->data.rule.name);
                ctx->errnum++;
            }
            else if (ctx->rules.buf[i]->data.rule.ref < 0) {
                print_error("%s:" FMT_LU ":" FMT_LU ": Multiple definition of rule '%s'\n",
                    ctx->iname,
                    (ulong_t)(ctx->rules.buf[i]->data.rule.line + 1), (ulong_t)(ctx->rules.buf[i]->data.rule.col + 1),
                    ctx->rules.buf[i]->data.rule.name);
                ctx->errnum++;
            }
        }
    }
    {
        size_t i;
        for (i = 0; i < ctx->rules.len; i++) {
            verify_variables(ctx, ctx->rules.buf[i]->data.rule.expr, NULL);
            verify_captures(ctx, ctx->rules.buf[i]->data.rule.expr, NULL);
        }
    }
    if (ctx->debug) {
        size_t i;
        for (i = 0; i < ctx->rules.len; i++) {
            dump_node(ctx, ctx->rules.buf[i], 0);
        }
        dump_options(ctx);
    }
    return (ctx->errnum == 0) ? TRUE : FALSE;
}

static code_reach_t generate_matching_string_code(generate_t *gen, const char *value, int onfail, size_t indent, bool_t bare) {
    const size_t n = (value != NULL) ? strlen(value) : 0;
    if (n > 0) {
        char s[5];
        if (n > 1) {
            size_t i;
            write_characters(gen->stream, ' ', indent);
            fputs_e("if (\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fprintf_e(gen->stream, "pcc_refill_buffer(ctx, " FMT_LU ") < " FMT_LU " ||\n", (ulong_t)n, (ulong_t)n);
            for (i = 0; i < n - 1; i++) {
                write_characters(gen->stream, ' ', indent + 4);
                fprintf_e(gen->stream, "(ctx->buffer.buf + ctx->cur)[" FMT_LU "] != '%s' ||\n", (ulong_t)i, escape_character(value[i], &s));
            }
            write_characters(gen->stream, ' ', indent + 4);
            fprintf_e(gen->stream, "(ctx->buffer.buf + ctx->cur)[" FMT_LU "] != '%s'\n", (ulong_t)i, escape_character(value[i], &s));
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, ") goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "ctx->cur += " FMT_LU ";\n", (ulong_t)n);
            return CODE_REACH__BOTH;
        }
        else {
            write_characters(gen->stream, ' ', indent);
            fputs_e("if (\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fputs_e("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fprintf_e(gen->stream, "ctx->buffer.buf[ctx->cur] != '%s'\n", escape_character(value[0], &s));
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, ") goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fputs_e("ctx->cur++;\n", gen->stream);
            return CODE_REACH__BOTH;
        }
    }
    else {
        /* no code to generate */
        return CODE_REACH__ALWAYS_SUCCEED;
    }
}

static code_reach_t generate_matching_charclass_code(generate_t *gen, const char *value, int onfail, size_t indent, bool_t bare) {
    assert(gen->ascii);
    if (value != NULL) {
        const size_t n = strlen(value);
        if (n > 0) {
            char s[5], t[5];
            if (n > 1) {
                const bool_t a = (value[0] == '^') ? TRUE : FALSE;
                size_t i = a ? 1 : 0;
                if (i + 1 == n) { /* fulfilled only if a == TRUE */
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("if (\n", gen->stream);
                    write_characters(gen->stream, ' ', indent + 4);
                    fputs_e("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
                    write_characters(gen->stream, ' ', indent + 4);
                    fprintf_e(gen->stream, "ctx->buffer.buf[ctx->cur] == '%s'\n", escape_character(value[i], &s));
                    write_characters(gen->stream, ' ', indent);
                    fprintf_e(gen->stream, ") goto L%04d;\n", onfail);
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("ctx->cur++;\n", gen->stream);
                    return CODE_REACH__BOTH;
                }
                else {
                    if (!bare) {
                        write_characters(gen->stream, ' ', indent);
                        fputs_e("{\n", gen->stream);
                        indent += 4;
                    }
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("char c;\n", gen->stream);
                    write_characters(gen->stream, ' ', indent);
                    fprintf_e(gen->stream, "if (pcc_refill_buffer(ctx, 1) < 1) goto L%04d;\n", onfail);
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("c = ctx->buffer.buf[ctx->cur];\n", gen->stream);
                    if (i + 3 == n && value[i] != '\\' && value[i + 1] == '-') {
                        write_characters(gen->stream, ' ', indent);
                        fprintf_e(gen->stream,
                            a ? "if (c >= '%s' && c <= '%s') goto L%04d;\n"
                              : "if (!(c >= '%s' && c <= '%s')) goto L%04d;\n",
                            escape_character(value[i], &s), escape_character(value[i + 2], &t), onfail);
                    }
                    else {
                        write_characters(gen->stream, ' ', indent);
                        fputs_e(a ? "if (\n" : "if (!(\n", gen->stream);
                        for (; i < n; i++) {
                            write_characters(gen->stream, ' ', indent + 4);
                            if (value[i] == '\\' && i + 1 < n) i++;
                            if (i + 2 < n && value[i + 1] == '-') {
                                fprintf_e(gen->stream, "(c >= '%s' && c <= '%s')%s\n",
                                    escape_character(value[i], &s), escape_character(value[i + 2], &t), (i + 3 == n) ? "" : " ||");
                                i += 2;
                            }
                            else {
                                fprintf_e(gen->stream, "c == '%s'%s\n",
                                    escape_character(value[i], &s), (i + 1 == n) ? "" : " ||");
                            }
                        }
                        write_characters(gen->stream, ' ', indent);
                        fprintf_e(gen->stream, a ? ") goto L%04d;\n" : ")) goto L%04d;\n", onfail);
                    }
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("ctx->cur++;\n", gen->stream);
                    if (!bare) {
                        indent -= 4;
                        write_characters(gen->stream, ' ', indent);
                        fputs_e("}\n", gen->stream);
                    }
                    return CODE_REACH__BOTH;
                }
            }
            else {
                write_characters(gen->stream, ' ', indent);
                fputs_e("if (\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fputs_e("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fprintf_e(gen->stream, "ctx->buffer.buf[ctx->cur] != '%s'\n", escape_character(value[0], &s));
                write_characters(gen->stream, ' ', indent);
                fprintf_e(gen->stream, ") goto L%04d;\n", onfail);
                write_characters(gen->stream, ' ', indent);
                fputs_e("ctx->cur++;\n", gen->stream);
                return CODE_REACH__BOTH;
            }
        }
        else {
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", onfail);
            return CODE_REACH__ALWAYS_FAIL;
        }
    }
    else {
        write_characters(gen->stream, ' ', indent);
        fprintf_e(gen->stream, "if (pcc_refill_buffer(ctx, 1) < 1) goto L%04d;\n", onfail);
        write_characters(gen->stream, ' ', indent);
        fputs_e("ctx->cur++;\n", gen->stream);
        return CODE_REACH__BOTH;
    }
}

static code_reach_t generate_matching_utf8_charclass_code(generate_t *gen, const char *value, int onfail, size_t indent, bool_t bare) {
    const size_t n = (value != NULL) ? strlen(value) : 0;
    if (value == NULL || n > 0) {
        const bool_t a = (n > 0 && value[0] == '^') ? TRUE : FALSE;
        size_t i = a ? 1 : 0;
        if (!bare) {
            write_characters(gen->stream, ' ', indent);
            fputs_e("{\n", gen->stream);
            indent += 4;
        }
        write_characters(gen->stream, ' ', indent);
        fputs_e("int u;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs_e("const size_t n = pcc_get_char_as_utf32(ctx, &u);\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fprintf_e(gen->stream, "if (n == 0) goto L%04d;\n", onfail);
        if (value != NULL && !(a && n == 1)) { /* not '.' or '[^]' */
            int u0 = 0;
            bool_t r = FALSE;
            write_characters(gen->stream, ' ', indent);
            fputs_e(a ? "if (\n" : "if (!(\n", gen->stream);
            while (i < n) {
                int u = 0;
                if (value[i] == '\\' && i + 1 < n) i++;
                i += utf8_to_utf32(value + i, &u);
                if (r) { /* character range */
                    write_characters(gen->stream, ' ', indent + 4);
                    fprintf_e(gen->stream, "(u >= 0x%06x && u <= 0x%06x)%s\n", u0, u, (i < n) ? " ||" : "");
                    u0 = 0;
                    r = FALSE;
                }
                else if (value[i] != '-') { /* single character */
                    write_characters(gen->stream, ' ', indent + 4);
                    fprintf_e(gen->stream, "u == 0x%06x%s\n", u, (i < n) ? " ||" : "");
                    u0 = 0;
                    r = FALSE;
                }
                else {
                    assert(value[i] == '-');
                    i++;
                    u0 = u;
                    r = TRUE;
                }
            }
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, a ? ") goto L%04d;\n" : ")) goto L%04d;\n", onfail);
        }
        write_characters(gen->stream, ' ', indent);
        fputs_e("ctx->cur += n;\n", gen->stream);
        if (!bare) {
            indent -= 4;
            write_characters(gen->stream, ' ', indent);
            fputs_e("}\n", gen->stream);
        }
        return CODE_REACH__BOTH;
    }
    else {
        write_characters(gen->stream, ' ', indent);
        fprintf_e(gen->stream, "goto L%04d;\n", onfail);
        return CODE_REACH__ALWAYS_FAIL;
    }
}

static code_reach_t generate_code(generate_t *gen, const node_t *node, int onfail, size_t indent, bool_t bare);

static code_reach_t generate_quantifying_code(generate_t *gen, const node_t *expr, int min, int max, int onfail, size_t indent, bool_t bare) {
    if (max > 1 || max < 0) {
        code_reach_t r;
        if (!bare) {
            write_characters(gen->stream, ' ', indent);
            fputs_e("{\n", gen->stream);
            indent += 4;
        }
        if (min > 0) {
            write_characters(gen->stream, ' ', indent);
            fputs_e("const size_t p0 = ctx->cur;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fputs_e("const size_t n0 = chunk->thunks.len;\n", gen->stream);
        }
        write_characters(gen->stream, ' ', indent);
        fputs_e("int i;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        if (max < 0)
            fputs_e("for (i = 0;; i++) {\n", gen->stream);
        else
            fprintf_e(gen->stream, "for (i = 0; i < %d; i++) {\n", max);
        write_characters(gen->stream, ' ', indent + 4);
        fputs_e("const size_t p = ctx->cur;\n", gen->stream);
        write_characters(gen->stream, ' ', indent + 4);
        fputs_e("const size_t n = chunk->thunks.len;\n", gen->stream);
        {
            const int l = ++gen->label;
            r = generate_code(gen, expr, l, indent + 4, FALSE);
            write_characters(gen->stream, ' ', indent + 4);
            fputs_e("if (ctx->cur == p) break;\n", gen->stream);
            if (r != CODE_REACH__ALWAYS_SUCCEED) {
                write_characters(gen->stream, ' ', indent + 4);
                fputs_e("continue;\n", gen->stream);
                write_characters(gen->stream, ' ', indent);
                fprintf_e(gen->stream, "L%04d:;\n", l);
                write_characters(gen->stream, ' ', indent + 4);
                fputs_e("ctx->cur = p;\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fputs_e("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fputs_e("break;\n", gen->stream);
            }
        }
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
        if (min > 0) {
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "if (i < %d) {\n", min);
            write_characters(gen->stream, ' ', indent + 4);
            fputs_e("ctx->cur = p0;\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fputs_e("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n0);\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fprintf_e(gen->stream, "goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fputs_e("}\n", gen->stream);
        }
        if (!bare) {
            indent -= 4;
            write_characters(gen->stream, ' ', indent);
            fputs_e("}\n", gen->stream);
        }
        return (min > 0) ? ((r == CODE_REACH__ALWAYS_FAIL) ? CODE_REACH__ALWAYS_FAIL : CODE_REACH__BOTH) : CODE_REACH__ALWAYS_SUCCEED;
    }
    else if (max == 1) {
        if (min > 0) {
            return generate_code(gen, expr, onfail, indent, bare);
        }
        else {
            if (!bare) {
                write_characters(gen->stream, ' ', indent);
                fputs_e("{\n", gen->stream);
                indent += 4;
            }
            write_characters(gen->stream, ' ', indent);
            fputs_e("const size_t p = ctx->cur;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fputs_e("const size_t n = chunk->thunks.len;\n", gen->stream);
            {
                const int l = ++gen->label;
                if (generate_code(gen, expr, l, indent, FALSE) != CODE_REACH__ALWAYS_SUCCEED) {
                    const int m = ++gen->label;
                    write_characters(gen->stream, ' ', indent);
                    fprintf_e(gen->stream, "goto L%04d;\n", m);
                    if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
                    fprintf_e(gen->stream, "L%04d:;\n", l);
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("ctx->cur = p;\n", gen->stream);
                    write_characters(gen->stream, ' ', indent);
                    fputs_e("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
                    if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
                    fprintf_e(gen->stream, "L%04d:;\n", m);
                }
            }
            if (!bare) {
                indent -= 4;
                write_characters(gen->stream, ' ', indent);
                fputs_e("}\n", gen->stream);
            }
            return CODE_REACH__ALWAYS_SUCCEED;
        }
    }
    else {
        /* no code to generate */
        return CODE_REACH__ALWAYS_SUCCEED;
    }
}

static code_reach_t generate_predicating_code(generate_t *gen, const node_t *expr, bool_t neg, int onfail, size_t indent, bool_t bare) {
    code_reach_t r;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs_e("const size_t p = ctx->cur;\n", gen->stream);
    if (neg) {
        const int l = ++gen->label;
        r = generate_code(gen, expr, l, indent, FALSE);
        if (r != CODE_REACH__ALWAYS_FAIL) {
            write_characters(gen->stream, ' ', indent);
            fputs_e("ctx->cur = p;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", onfail);
        }
        if (r != CODE_REACH__ALWAYS_SUCCEED) {
            if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
            fprintf_e(gen->stream, "L%04d:;\n", l);
            write_characters(gen->stream, ' ', indent);
            fputs_e("ctx->cur = p;\n", gen->stream);
        }
        switch (r) {
        case CODE_REACH__ALWAYS_SUCCEED: r = CODE_REACH__ALWAYS_FAIL; break;
        case CODE_REACH__ALWAYS_FAIL: r = CODE_REACH__ALWAYS_SUCCEED; break;
        case CODE_REACH__BOTH: break;
        }
    }
    else {
        const int l = ++gen->label;
        const int m = ++gen->label;
        r = generate_code(gen, expr, l, indent, FALSE);
        if (r != CODE_REACH__ALWAYS_FAIL) {
            write_characters(gen->stream, ' ', indent);
            fputs_e("ctx->cur = p;\n", gen->stream);
        }
        if (r == CODE_REACH__BOTH) {
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", m);
        }
        if (r != CODE_REACH__ALWAYS_SUCCEED) {
            if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
            fprintf_e(gen->stream, "L%04d:;\n", l);
            write_characters(gen->stream, ' ', indent);
            fputs_e("ctx->cur = p;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", onfail);
        }
        if (r == CODE_REACH__BOTH) {
            if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
            fprintf_e(gen->stream, "L%04d:;\n", m);
        }
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_sequential_code(generate_t *gen, const node_array_t *nodes, int onfail, size_t indent, bool_t bare) {
    bool_t b = FALSE;
    size_t i;
    for (i = 0; i < nodes->len; i++) {
        switch (generate_code(gen, nodes->buf[i], onfail, indent, FALSE)) {
        case CODE_REACH__ALWAYS_FAIL:
            if (i + 1 < nodes->len) {
                write_characters(gen->stream, ' ', indent);
                fputs_e("/* unreachable codes omitted */\n", gen->stream);
            }
            return CODE_REACH__ALWAYS_FAIL;
        case CODE_REACH__ALWAYS_SUCCEED:
            break;
        default:
            b = TRUE;
        }
    }
    return b ? CODE_REACH__BOTH : CODE_REACH__ALWAYS_SUCCEED;
}

static code_reach_t generate_alternative_code(generate_t *gen, const node_array_t *nodes, int onfail, size_t indent, bool_t bare) {
    bool_t b = FALSE;
    int m = ++gen->label;
    size_t i;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs_e("const size_t p = ctx->cur;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fputs_e("const size_t n = chunk->thunks.len;\n", gen->stream);
    for (i = 0; i < nodes->len; i++) {
        const bool_t c = (i + 1 < nodes->len) ? TRUE : FALSE;
        const int l = ++gen->label;
        switch (generate_code(gen, nodes->buf[i], l, indent, FALSE)) {
        case CODE_REACH__ALWAYS_SUCCEED:
            if (c) {
                write_characters(gen->stream, ' ', indent);
                fputs_e("/* unreachable codes omitted */\n", gen->stream);
            }
            if (b) {
                if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
                fprintf_e(gen->stream, "L%04d:;\n", m);
            }
            if (!bare) {
                indent -= 4;
                write_characters(gen->stream, ' ', indent);
                fputs_e("}\n", gen->stream);
            }
            return CODE_REACH__ALWAYS_SUCCEED;
        case CODE_REACH__ALWAYS_FAIL:
            break;
        default:
            b = TRUE;
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", m);
        }
        if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
        fprintf_e(gen->stream, "L%04d:;\n", l);
        write_characters(gen->stream, ' ', indent);
        fputs_e("ctx->cur = p;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs_e("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
        if (!c) {
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "goto L%04d;\n", onfail);
        }
    }
    if (b) {
        if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
        fprintf_e(gen->stream, "L%04d:;\n", m);
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return b ? CODE_REACH__BOTH : CODE_REACH__ALWAYS_FAIL;
}

static code_reach_t generate_capturing_code(generate_t *gen, const node_t *expr, size_t index, int onfail, size_t indent, bool_t bare) {
    code_reach_t r;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs_e("const size_t p = ctx->cur;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fputs_e("size_t q;\n", gen->stream);
    r = generate_code(gen, expr, onfail, indent, FALSE);
    write_characters(gen->stream, ' ', indent);
    fputs_e("q = ctx->cur;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "chunk->capts.buf[" FMT_LU "].range.start = p;\n", (ulong_t)index);
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "chunk->capts.buf[" FMT_LU "].range.end = q;\n", (ulong_t)index);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_expanding_code(generate_t *gen, size_t index, int onfail, size_t indent, bool_t bare) {
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "const size_t n = chunk->capts.buf[" FMT_LU "].range.end - chunk->capts.buf[" FMT_LU "].range.start;\n", (ulong_t)index, (ulong_t)index);
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "if (pcc_refill_buffer(ctx, n) < n) goto L%04d;\n", onfail);
    write_characters(gen->stream, ' ', indent);
    fputs_e("if (n > 0) {\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs_e("const char *const p = ctx->buffer.buf + ctx->cur;\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fprintf_e(gen->stream, "const char *const q = ctx->buffer.buf + chunk->capts.buf[" FMT_LU "].range.start;\n", (ulong_t)index);
    write_characters(gen->stream, ' ', indent + 4);
    fputs_e("size_t i;\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs_e("for (i = 0; i < n; i++) {\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 8);
    fprintf_e(gen->stream, "if (p[i] != q[i]) goto L%04d;\n", onfail);
    write_characters(gen->stream, ' ', indent + 4);
    fputs_e("}\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs_e("ctx->cur += n;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fputs_e("}\n", gen->stream);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return CODE_REACH__BOTH;
}

static code_reach_t generate_thunking_action_code(
    generate_t *gen, size_t index, const node_const_array_t *vars, const node_const_array_t *capts, bool_t error, int onfail, size_t indent, bool_t bare
) {
    assert(gen->rule->type == NODE_RULE);
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    if (error) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("pcc_value_t null;\n", gen->stream);
    }
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "pcc_thunk_t *const thunk = pcc_thunk__create_leaf(ctx->auxil, pcc_action_%s_" FMT_LU ", " FMT_LU ", " FMT_LU ");\n",
        gen->rule->data.rule.name, (ulong_t)index, (ulong_t)gen->rule->data.rule.vars.len, (ulong_t)gen->rule->data.rule.capts.len);
    {
        size_t i;
        for (i = 0; i < vars->len; i++) {
            assert(vars->buf[i]->type == NODE_REFERENCE);
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "thunk->data.leaf.values.buf[" FMT_LU "] = &(chunk->values.buf[" FMT_LU "]);\n",
                (ulong_t)vars->buf[i]->data.reference.index, (ulong_t)vars->buf[i]->data.reference.index);
        }
        for (i = 0; i < capts->len; i++) {
            assert(capts->buf[i]->type == NODE_CAPTURE);
            write_characters(gen->stream, ' ', indent);
            fprintf_e(gen->stream, "thunk->data.leaf.capts.buf[" FMT_LU "] = &(chunk->capts.buf[" FMT_LU "]);\n",
                (ulong_t)capts->buf[i]->data.capture.index, (ulong_t)capts->buf[i]->data.capture.index);
        }
        write_characters(gen->stream, ' ', indent);
        fputs_e("thunk->data.leaf.capt0.range.start = chunk->pos;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs_e("thunk->data.leaf.capt0.range.end = ctx->cur;\n", gen->stream);
    }
    if (error) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("memset(&null, 0, sizeof(pcc_value_t)); /* in case */\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs_e("thunk->data.leaf.action(ctx, thunk, &null);\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs_e("pcc_thunk__destroy(ctx->auxil, thunk);\n", gen->stream);
    }
    else {
        write_characters(gen->stream, ' ', indent);
        fputs_e("pcc_thunk_array__add(ctx->auxil, &chunk->thunks, thunk);\n", gen->stream);
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return CODE_REACH__ALWAYS_SUCCEED;
}

static code_reach_t generate_thunking_error_code(
    generate_t *gen, const node_t *expr, size_t index, const node_const_array_t *vars, const node_const_array_t *capts, int onfail, size_t indent, bool_t bare
) {
    code_reach_t r;
    const int l = ++gen->label;
    const int m = ++gen->label;
    assert(gen->rule->type == NODE_RULE);
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs_e("{\n", gen->stream);
        indent += 4;
    }
    r = generate_code(gen, expr, l, indent, TRUE);
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "goto L%04d;\n", m);
    if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
    fprintf_e(gen->stream, "L%04d:;\n", l);
    generate_thunking_action_code(gen, index, vars, capts, TRUE, l, indent, FALSE);
    write_characters(gen->stream, ' ', indent);
    fprintf_e(gen->stream, "goto L%04d;\n", onfail);
    if (indent > 4) write_characters(gen->stream, ' ', indent - 4);
    fprintf_e(gen->stream, "L%04d:;\n", m);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs_e("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_code(generate_t *gen, const node_t *node, int onfail, size_t indent, bool_t bare) {
    if (node == NULL) {
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
    switch (node->type) {
    case NODE_RULE:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    case NODE_REFERENCE:
        write_characters(gen->stream, ' ', indent);
        if (node->data.reference.index != VOID_VALUE) {
            fprintf_e(gen->stream, "if (!pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &chunk->thunks, &(chunk->values.buf[" FMT_LU "]))) goto L%04d;\n",
                node->data.reference.name, (ulong_t)node->data.reference.index, onfail);
        }
        else {
            fprintf_e(gen->stream, "if (!pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &chunk->thunks, NULL)) goto L%04d;\n",
                node->data.reference.name, onfail);
        }
        return CODE_REACH__BOTH;
    case NODE_STRING:
        return generate_matching_string_code(gen, node->data.string.value, onfail, indent, bare);
    case NODE_CHARCLASS:
        return gen->ascii ?
               generate_matching_charclass_code(gen, node->data.charclass.value, onfail, indent, bare) :
               generate_matching_utf8_charclass_code(gen, node->data.charclass.value, onfail, indent, bare);
    case NODE_QUANTITY:
        return generate_quantifying_code(gen, node->data.quantity.expr, node->data.quantity.min, node->data.quantity.max, onfail, indent, bare);
    case NODE_PREDICATE:
        return generate_predicating_code(gen, node->data.predicate.expr, node->data.predicate.neg, onfail, indent, bare);
    case NODE_SEQUENCE:
        return generate_sequential_code(gen, &node->data.sequence.nodes, onfail, indent, bare);
    case NODE_ALTERNATE:
        return generate_alternative_code(gen, &node->data.alternate.nodes, onfail, indent, bare);
    case NODE_CAPTURE:
        return generate_capturing_code(gen, node->data.capture.expr, node->data.capture.index, onfail, indent, bare);
    case NODE_EXPAND:
        return generate_expanding_code(gen, node->data.expand.index, onfail, indent, bare);
    case NODE_ACTION:
        return generate_thunking_action_code(
            gen, node->data.action.index, &node->data.action.vars, &node->data.action.capts, FALSE, onfail, indent, bare
        );
    case NODE_ERROR:
        return generate_thunking_error_code(
            gen, node->data.error.expr, node->data.error.index, &node->data.error.vars, &node->data.error.capts, onfail, indent, bare
        );
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
}

static bool_t generate(context_t *ctx) {
    const char *const vt = get_value_type(ctx);
    const char *const at = get_auxil_type(ctx);
    const bool_t vp = is_pointer_type(vt);
    const bool_t ap = is_pointer_type(at);
    FILE *const sstream = fopen_wt_e(ctx->sname);
    FILE *const hstream = fopen_wt_e(ctx->hname);
    fprintf_e(sstream, "/* A packrat parser generated by PackCC %s */\n\n", VERSION);
    fprintf_e(hstream, "/* A packrat parser generated by PackCC %s */\n\n", VERSION);
    {
        write_code_block(hstream, ctx->eheader.buf, ctx->eheader.len, 0);
        if (ctx->eheader.len > 0) fputs_e("\n", hstream);
        fprintf_e(
            hstream,
            "#ifndef PCC_INCLUDED_%s\n"
            "#define PCC_INCLUDED_%s\n"
            "\n",
            ctx->hid, ctx->hid
        );
        write_code_block(hstream, ctx->header.buf, ctx->header.len, 0);
    }
    {
        write_code_block(sstream, ctx->esource.buf, ctx->esource.len, 0);
        if (ctx->esource.len > 0) fputs_e("\n", hstream);
        fputs_e(
            "#ifdef _MSC_VER\n"
            "#undef _CRT_SECURE_NO_WARNINGS\n"
            "#define _CRT_SECURE_NO_WARNINGS\n"
            "#endif /* _MSC_VER */\n"
            "#include <stdio.h>\n"
            "#include <stdlib.h>\n"
            "#include <string.h>\n"
            "\n"
            "#ifndef _MSC_VER\n"
            "#if defined __GNUC__ && defined _WIN32 /* MinGW */\n"
            "#ifndef PCC_USE_SYSTEM_STRNLEN\n"
            "#define strnlen(str, maxlen) pcc_strnlen(str, maxlen)\n"
            "static size_t pcc_strnlen(const char *str, size_t maxlen) {\n"
            "    size_t i;\n"
            "    for (i = 0; i < maxlen && str[i]; i++);\n"
            "    return i;\n"
            "}\n"
            "#endif /* !PCC_USE_SYSTEM_STRNLEN */\n"
            "#endif /* defined __GNUC__ && defined _WIN32 */\n"
            "#endif /* !_MSC_VER */\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "#include \"%s\"\n"
            "\n",
            ctx->hname
        );
        write_code_block(sstream, ctx->source.buf, ctx->source.len, 0);
    }
    {
        fputs_e(
            "#if !defined __has_attribute || defined _MSC_VER\n"
            "#define __attribute__(x)\n"
            "#endif\n"
            "\n"
            "#ifdef _MSC_VER\n"
            "#define MARK_USED_FUNC __pragma(warning(suppress:4505))\n"
            "#else\n"
            "#define MARK_USED_FUNC __attribute__((__unused__))\n"
            "#endif\n"
            "\n"
            "#ifndef PCC_BUFFERSIZE\n"
            "#define PCC_BUFFERSIZE 256\n"
            "#endif /* !PCC_BUFFERSIZE */\n"
            "\n"
            "#ifndef PCC_ARRAYSIZE\n"
            "#define PCC_ARRAYSIZE 2\n"
            "#endif /* !PCC_ARRAYSIZE */\n"
            "\n"
            "#define PCC_DBG_EVALUATE 0\n"
            "#define PCC_DBG_MATCH    1\n"
            "#define PCC_DBG_NOMATCH  2\n"
            "\n"
            "#define PCC_VOID_VALUE (~(size_t)0)\n"
            "\n"
            "typedef enum pcc_bool_tag {\n"
            "    PCC_FALSE = 0,\n"
            "    PCC_TRUE\n"
            "} pcc_bool_t;\n"
            "\n"
            "typedef struct pcc_char_array_tag {\n"
            "    char *buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_char_array_t;\n"
            "\n"
            "typedef struct pcc_range_tag {\n"
            "    size_t start;\n"
            "    size_t end;\n"
            "} pcc_range_t;\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "typedef %s%spcc_value_t;\n"
            "\n",
            vt, vp ? "" : " "
        );
        fprintf_e(
            sstream,
            "typedef %s%spcc_auxil_t;\n"
            "\n",
            at, ap ? "" : " "
        );
        fputs_e(
            "typedef struct pcc_value_table_tag {\n"
            "    pcc_value_t *buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_value_table_t;\n"
            "\n"
            "typedef struct pcc_value_refer_table_tag {\n"
            "    pcc_value_t **buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_value_refer_table_t;\n"
            "\n"
            "typedef struct pcc_capture_tag {\n"
            "    pcc_range_t range;\n"
            "    char *string; /* mutable */\n"
            "} pcc_capture_t;\n"
            "\n"
            "typedef struct pcc_capture_table_tag {\n"
            "    pcc_capture_t *buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_capture_table_t;\n"
            "\n"
            "typedef struct pcc_capture_const_table_tag {\n"
            "    const pcc_capture_t **buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_capture_const_table_t;\n"
            "\n"
            "typedef struct pcc_thunk_tag pcc_thunk_t;\n"
            "typedef struct pcc_thunk_array_tag pcc_thunk_array_t;\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "typedef void (*pcc_action_t)(%s_context_t *, pcc_thunk_t *, pcc_value_t *);\n"
            "\n",
            get_prefix(ctx)
        );
        fputs_e(
            "typedef enum pcc_thunk_type_tag {\n"
            "    PCC_THUNK_LEAF,\n"
            "    PCC_THUNK_NODE\n"
            "} pcc_thunk_type_t;\n"
            "\n"
            "typedef struct pcc_thunk_leaf_tag {\n"
            "    pcc_value_refer_table_t values;\n"
            "    pcc_capture_const_table_t capts;\n"
            "    pcc_capture_t capt0;\n"
            "    pcc_action_t action;\n"
            "} pcc_thunk_leaf_t;\n"
            "\n"
            "typedef struct pcc_thunk_node_tag {\n"
            "    const pcc_thunk_array_t *thunks; /* just a reference */\n"
            "    pcc_value_t *value; /* just a reference */\n"
            "} pcc_thunk_node_t;\n"
            "\n"
            "typedef union pcc_thunk_data_tag {\n"
            "    pcc_thunk_leaf_t leaf;\n"
            "    pcc_thunk_node_t node;\n"
            "} pcc_thunk_data_t;\n"
            "\n"
            "struct pcc_thunk_tag {\n"
            "    pcc_thunk_type_t type;\n"
            "    pcc_thunk_data_t data;\n"
            "};\n"
            "\n"
            "struct pcc_thunk_array_tag {\n"
            "    pcc_thunk_t **buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "};\n"
            "\n"
            "typedef struct pcc_thunk_chunk_tag {\n"
            "    pcc_value_table_t values;\n"
            "    pcc_capture_table_t capts;\n"
            "    pcc_thunk_array_t thunks;\n"
            "    size_t pos; /* the starting position in the character buffer */\n"
            "} pcc_thunk_chunk_t;\n"
            "\n"
            "typedef struct pcc_lr_entry_tag pcc_lr_entry_t;\n"
            "\n"
            "typedef enum pcc_lr_answer_type_tag {\n"
            "    PCC_LR_ANSWER_LR,\n"
            "    PCC_LR_ANSWER_CHUNK\n"
            "} pcc_lr_answer_type_t;\n"
            "\n"
            "typedef union pcc_lr_answer_data_tag {\n"
            "    pcc_lr_entry_t *lr;\n"
            "    pcc_thunk_chunk_t *chunk;\n"
            "} pcc_lr_answer_data_t;\n"
            "\n"
            "typedef struct pcc_lr_answer_tag pcc_lr_answer_t;\n"
            "\n"
            "struct pcc_lr_answer_tag {\n"
            "    pcc_lr_answer_type_t type;\n"
            "    pcc_lr_answer_data_t data;\n"
            "    size_t pos; /* the absolute position in the input */\n"
            "    pcc_lr_answer_t *hold;\n"
            "};\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "typedef pcc_thunk_chunk_t *(*pcc_rule_t)(%s_context_t *);\n"
            "\n",
            get_prefix(ctx)
        );
        fputs_e(
            "typedef struct pcc_rule_set_tag {\n"
            "    pcc_rule_t *buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_rule_set_t;\n"
            "\n"
            "typedef struct pcc_lr_head_tag pcc_lr_head_t;\n"
            "\n"
            "struct pcc_lr_head_tag {\n"
            "    pcc_rule_t rule;\n"
            "    pcc_rule_set_t invol;\n"
            "    pcc_rule_set_t eval;\n"
            "    pcc_lr_head_t *hold;\n"
            "};\n"
            "\n"
            "typedef struct pcc_lr_memo_tag {\n"
            "    pcc_rule_t rule;\n"
            "    pcc_lr_answer_t *answer;\n"
            "} pcc_lr_memo_t;\n"
            "\n"
            "typedef struct pcc_lr_memo_map_tag {\n"
            "    pcc_lr_memo_t *buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_lr_memo_map_t;\n"
            "\n"
            "typedef struct pcc_lr_table_entry_tag {\n"
            "    pcc_lr_head_t *head; /* just a reference */\n"
            "    pcc_lr_memo_map_t memos;\n"
            "    pcc_lr_answer_t *hold_a;\n"
            "    pcc_lr_head_t *hold_h;\n"
            "} pcc_lr_table_entry_t;\n"
            "\n"
            "typedef struct pcc_lr_table_tag {\n"
            "    pcc_lr_table_entry_t **buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "    size_t ofs;\n"
            "} pcc_lr_table_t;\n"
            "\n"
            "struct pcc_lr_entry_tag {\n"
            "    pcc_rule_t rule;\n"
            "    pcc_thunk_chunk_t *seed; /* just a reference */\n"
            "    pcc_lr_head_t *head; /* just a reference */\n"
            "};\n"
            "\n"
            "typedef struct pcc_lr_stack_tag {\n"
            "    pcc_lr_entry_t **buf;\n"
            "    size_t max;\n"
            "    size_t len;\n"
            "} pcc_lr_stack_t;\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "struct %s_context_tag {\n"
            "    size_t pos; /* the position in the input of the first character currently buffered */\n"
            "    size_t cur; /* the current parsing position in the character buffer */\n"
            "    size_t level;\n"
            "    pcc_char_array_t buffer;\n"
            "    pcc_lr_table_t lrtable;\n"
            "    pcc_lr_stack_t lrstack;\n"
            "    pcc_thunk_array_t thunks;\n"
            "    pcc_auxil_t auxil;\n"
            "};\n"
            "\n",
            get_prefix(ctx)
        );
        fputs_e(
            "#ifndef PCC_ERROR\n"
            "#define PCC_ERROR(auxil) pcc_error()\n"
            "MARK_USED_FUNC\n"
            "static void pcc_error(void) {\n"
            "    fprintf(stderr, \"Syntax error\\n\");\n"
            "    exit(1);\n"
            "}\n"
            "#endif /* !PCC_ERROR */\n"
            "\n"
            "#ifndef PCC_GETCHAR\n"
            "#define PCC_GETCHAR(auxil) getchar()\n"
            "#endif /* !PCC_GETCHAR */\n"
            "\n"
            "#ifndef PCC_MALLOC\n"
            "#define PCC_MALLOC(auxil, size) pcc_malloc_e(size)\n"
            "static void *pcc_malloc_e(size_t size) {\n"
            "    void *const p = malloc(size);\n"
            "    if (p == NULL) {\n"
            "        fprintf(stderr, \"Out of memory\\n\");\n"
            "        exit(1);\n"
            "    }\n"
            "    return p;\n"
            "}\n"
            "#endif /* !PCC_MALLOC */\n"
            "\n"
            "#ifndef PCC_REALLOC\n"
            "#define PCC_REALLOC(auxil, ptr, size) pcc_realloc_e(ptr, size)\n"
            "static void *pcc_realloc_e(void *ptr, size_t size) {\n"
            "    void *const p = realloc(ptr, size);\n"
            "    if (p == NULL) {\n"
            "        fprintf(stderr, \"Out of memory\\n\");\n"
            "        exit(1);\n"
            "    }\n"
            "    return p;\n"
            "}\n"
            "#endif /* !PCC_REALLOC */\n"
            "\n"
            "#ifndef PCC_FREE\n"
            "#define PCC_FREE(auxil, ptr) free(ptr)\n"
            "#endif /* !PCC_FREE */\n"
            "\n"
            "#ifndef PCC_DEBUG\n"
            "#define PCC_DEBUG(auxil, event, rule, level, pos, buffer, length) ((void)0)\n"
            "#endif /* !PCC_DEBUG */\n"
            "\n"
            /* not used
            "static char *pcc_strdup_e(pcc_auxil_t auxil, const char *str) {\n"
            "    const size_t m = strlen(str);\n"
            "    char *const s = (char *)PCC_MALLOC(auxil, m + 1);\n"
            "    memcpy(s, str, m);\n"
            "    s[m] = '\\0';\n"
            "    return s;\n"
            "}\n"
            "\n"
            */
            "static char *pcc_strndup_e(pcc_auxil_t auxil, const char *str, size_t len) {\n"
            "    const size_t m = strnlen(str, len);\n"
            "    char *const s = (char *)PCC_MALLOC(auxil, m + 1);\n"
            "    memcpy(s, str, m);\n"
            "    s[m] = '\\0';\n"
            "    return s;\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_char_array__init(pcc_auxil_t auxil, pcc_char_array_t *array) {\n"
            "    array->len = 0;\n"
            "    array->max = 0;\n"
            "    array->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_char_array__add(pcc_auxil_t auxil, pcc_char_array_t *array, char ch) {\n"
            "    if (array->max <= array->len) {\n"
            "        const size_t n = array->len + 1;\n"
            "        size_t m = array->max;\n"
            "        if (m == 0) m = PCC_BUFFERSIZE;\n"
            "        while (m < n && m != 0) m <<= 1;\n"
            "        if (m == 0) m = n;\n"
            "        array->buf = (char *)PCC_REALLOC(auxil, array->buf, m);\n"
            "        array->max = m;\n"
            "    }\n"
            "    array->buf[array->len++] = ch;\n"
            "}\n"
            "\n"
            "static void pcc_char_array__term(pcc_auxil_t auxil, pcc_char_array_t *array) {\n"
            "    PCC_FREE(auxil, array->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_value_table__init(pcc_auxil_t auxil, pcc_value_table_t *table) {\n"
            "    table->len = 0;\n"
            "    table->max = 0;\n"
            "    table->buf = NULL;\n"
            "}\n"
            "\n"
            "MARK_USED_FUNC\n"
            "static void pcc_value_table__resize(pcc_auxil_t auxil, pcc_value_table_t *table, size_t len) {\n"
            "    if (table->max < len) {\n"
            "        size_t m = table->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < len && m != 0) m <<= 1;\n"
            "        if (m == 0) m = len;\n"
            "        table->buf = (pcc_value_t *)PCC_REALLOC(auxil, table->buf, sizeof(pcc_value_t) * m);\n"
            "        table->max = m;\n"
            "    }\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_value_table__term(pcc_auxil_t auxil, pcc_value_table_t *table) {\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_value_refer_table__init(pcc_auxil_t auxil, pcc_value_refer_table_t *table) {\n"
            "    table->len = 0;\n"
            "    table->max = 0;\n"
            "    table->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_value_refer_table__resize(pcc_auxil_t auxil, pcc_value_refer_table_t *table, size_t len) {\n"
            "    size_t i;\n"
            "    if (table->max < len) {\n"
            "        size_t m = table->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < len && m != 0) m <<= 1;\n"
            "        if (m == 0) m = len;\n"
            "        table->buf = (pcc_value_t **)PCC_REALLOC(auxil, table->buf, sizeof(pcc_value_t *) * m);\n"
            "        table->max = m;\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_value_refer_table__term(pcc_auxil_t auxil, pcc_value_refer_table_t *table) {\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_capture_table__init(pcc_auxil_t auxil, pcc_capture_table_t *table) {\n"
            "    table->len = 0;\n"
            "    table->max = 0;\n"
            "    table->buf = NULL;\n"
            "}\n"
            "\n"
            "MARK_USED_FUNC\n"
            "static void pcc_capture_table__resize(pcc_auxil_t auxil, pcc_capture_table_t *table, size_t len) {\n"
            "    size_t i;\n"
            "    for (i = len; i < table->len; i++) PCC_FREE(auxil, table->buf[i].string);\n"
            "    if (table->max < len) {\n"
            "        size_t m = table->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < len && m != 0) m <<= 1;\n"
            "        if (m == 0) m = len;\n"
            "        table->buf = (pcc_capture_t *)PCC_REALLOC(auxil, table->buf, sizeof(pcc_capture_t) * m);\n"
            "        table->max = m;\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) {\n"
            "        table->buf[i].range.start = 0;\n"
            "        table->buf[i].range.end = 0;\n"
            "        table->buf[i].string = NULL;\n"
            "    }\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_capture_table__term(pcc_auxil_t auxil, pcc_capture_table_t *table) {\n"
            "    while (table->len > 0) {\n"
            "        table->len--;\n"
            "        PCC_FREE(auxil, table->buf[table->len].string);\n"
            "    }\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_capture_const_table__init(pcc_auxil_t auxil, pcc_capture_const_table_t *table) {\n"
            "    table->len = 0;\n"
            "    table->max = 0;\n"
            "    table->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_capture_const_table__resize(pcc_auxil_t auxil, pcc_capture_const_table_t *table, size_t len) {\n"
            "    size_t i;\n"
            "    if (table->max < len) {\n"
            "        size_t m = table->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < len && m != 0) m <<= 1;\n"
            "        if (m == 0) m = len;\n"
            "        table->buf = (const pcc_capture_t **)PCC_REALLOC(auxil, (pcc_capture_t **)table->buf, sizeof(const pcc_capture_t *) * m);\n"
            "        table->max = m;\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_capture_const_table__term(pcc_auxil_t auxil, pcc_capture_const_table_t *table) {\n"
            "    PCC_FREE(auxil, (void *)table->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "MARK_USED_FUNC\n"
            "static pcc_thunk_t *pcc_thunk__create_leaf(pcc_auxil_t auxil, pcc_action_t action, size_t valuec, size_t captc) {\n"
            "    pcc_thunk_t *const thunk = (pcc_thunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_t));\n"
            "    thunk->type = PCC_THUNK_LEAF;\n"
            "    pcc_value_refer_table__init(auxil, &thunk->data.leaf.values);\n"
            "    pcc_value_refer_table__resize(auxil, &thunk->data.leaf.values, valuec);\n"
            "    pcc_capture_const_table__init(auxil, &thunk->data.leaf.capts);\n"
            "    pcc_capture_const_table__resize(auxil, &thunk->data.leaf.capts, captc);\n"
            "    thunk->data.leaf.capt0.range.start = 0;\n"
            "    thunk->data.leaf.capt0.range.end = 0;\n"
            "    thunk->data.leaf.capt0.string = NULL;\n"
            "    thunk->data.leaf.action = action;\n"
            "    return thunk;\n"
            "}\n"
            "\n"
            "static pcc_thunk_t *pcc_thunk__create_node(pcc_auxil_t auxil, const pcc_thunk_array_t *thunks, pcc_value_t *value) {\n"
            "    pcc_thunk_t *const thunk = (pcc_thunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_t));\n"
            "    thunk->type = PCC_THUNK_NODE;\n"
            "    thunk->data.node.thunks = thunks;\n"
            "    thunk->data.node.value = value;\n"
            "    return thunk;\n"
            "}\n"
            "\n"
            "static void pcc_thunk__destroy(pcc_auxil_t auxil, pcc_thunk_t *thunk) {\n"
            "    if (thunk == NULL) return;\n"
            "    switch (thunk->type) {\n"
            "    case PCC_THUNK_LEAF:\n"
            "        PCC_FREE(auxil, thunk->data.leaf.capt0.string);\n"
            "        pcc_capture_const_table__term(auxil, &thunk->data.leaf.capts);\n"
            "        pcc_value_refer_table__term(auxil, &thunk->data.leaf.values);\n"
            "        break;\n"
            "    case PCC_THUNK_NODE:\n"
            "        break;\n"
            "    default: /* unknown */\n"
            "        break;\n"
            "    }\n"
            "    PCC_FREE(auxil, thunk);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_thunk_array__init(pcc_auxil_t auxil, pcc_thunk_array_t *array) {\n"
            "    array->len = 0;\n"
            "    array->max = 0;\n"
            "    array->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__add(pcc_auxil_t auxil, pcc_thunk_array_t *array, pcc_thunk_t *thunk) {\n"
            "    if (array->max <= array->len) {\n"
            "        const size_t n = array->len + 1;\n"
            "        size_t m = array->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < n && m != 0) m <<= 1;\n"
            "        if (m == 0) m = n;\n"
            "        array->buf = (pcc_thunk_t **)PCC_REALLOC(auxil, array->buf, sizeof(pcc_thunk_t *) * m);\n"
            "        array->max = m;\n"
            "    }\n"
            "    array->buf[array->len++] = thunk;\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__revert(pcc_auxil_t auxil, pcc_thunk_array_t *array, size_t len) {\n"
            "    while (array->len > len) {\n"
            "        array->len--;\n"
            "        pcc_thunk__destroy(auxil, array->buf[array->len]);\n"
            "    }\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__term(pcc_auxil_t auxil, pcc_thunk_array_t *array) {\n"
            "    while (array->len > 0) {\n"
            "        array->len--;\n"
            "        pcc_thunk__destroy(auxil, array->buf[array->len]);\n"
            "    }\n"
            "    PCC_FREE(auxil, array->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "MARK_USED_FUNC\n"
            "static pcc_thunk_chunk_t *pcc_thunk_chunk__create(pcc_auxil_t auxil) {\n"
            "    pcc_thunk_chunk_t *const chunk = (pcc_thunk_chunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_chunk_t));\n"
            "    pcc_value_table__init(auxil, &chunk->values);\n"
            "    pcc_capture_table__init(auxil, &chunk->capts);\n"
            "    pcc_thunk_array__init(auxil, &chunk->thunks);\n"
            "    chunk->pos = 0;\n"
            "    return chunk;\n"
            "}\n"
            "\n"
            "static void pcc_thunk_chunk__destroy(pcc_auxil_t auxil, pcc_thunk_chunk_t *chunk) {\n"
            "    if (chunk == NULL) return;\n"
            "    pcc_thunk_array__term(auxil, &chunk->thunks);\n"
            "    pcc_capture_table__term(auxil, &chunk->capts);\n"
            "    pcc_value_table__term(auxil, &chunk->values);\n"
            "    PCC_FREE(auxil, chunk);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_rule_set__init(pcc_auxil_t auxil, pcc_rule_set_t *set) {\n"
            "    set->len = 0;\n"
            "    set->max = 0;\n"
            "    set->buf = NULL;\n"
            "}\n"
            "\n"
            "static size_t pcc_rule_set__index(pcc_auxil_t auxil, const pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    size_t i;\n"
            "    for (i = 0; i < set->len; i++) {\n"
            "        if (set->buf[i] == rule) return i;\n"
            "    }\n"
            "    return PCC_VOID_VALUE;\n"
            "}\n"
            "\n"
            "static pcc_bool_t pcc_rule_set__add(pcc_auxil_t auxil, pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    const size_t i = pcc_rule_set__index(auxil, set, rule);\n"
            "    if (i != PCC_VOID_VALUE) return PCC_FALSE;\n"
            "    if (set->max <= set->len) {\n"
            "        const size_t n = set->len + 1;\n"
            "        size_t m = set->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < n && m != 0) m <<= 1;\n"
            "        if (m == 0) m = n;\n"
            "        set->buf = (pcc_rule_t *)PCC_REALLOC(auxil, set->buf, sizeof(pcc_rule_t) * m);\n"
            "        set->max = m;\n"
            "    }\n"
            "    set->buf[set->len++] = rule;\n"
            "    return PCC_TRUE;\n"
            "}\n"
            "\n"
            "static pcc_bool_t pcc_rule_set__remove(pcc_auxil_t auxil, pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    const size_t i = pcc_rule_set__index(auxil, set, rule);\n"
            "    if (i == PCC_VOID_VALUE) return PCC_FALSE;\n"
            "    memmove(set->buf + i, set->buf + (i + 1), sizeof(pcc_rule_t) * (set->len - (i + 1)));\n"
            "    return PCC_TRUE;\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__clear(pcc_auxil_t auxil, pcc_rule_set_t *set) {\n"
            "    set->len = 0;\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__copy(pcc_auxil_t auxil, pcc_rule_set_t *set, const pcc_rule_set_t *src) {\n"
            "    size_t i;\n"
            "    pcc_rule_set__clear(auxil, set);\n"
            "    for (i = 0; i < src->len; i++) {\n"
            "        pcc_rule_set__add(auxil, set, src->buf[i]);\n"
            "    }\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__term(pcc_auxil_t auxil, pcc_rule_set_t *set) {\n"
            "    PCC_FREE(auxil, set->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static pcc_lr_head_t *pcc_lr_head__create(pcc_auxil_t auxil, pcc_rule_t rule) {\n"
            "    pcc_lr_head_t *const head = (pcc_lr_head_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_head_t));\n"
            "    head->rule = rule;\n"
            "    pcc_rule_set__init(auxil, &head->invol);\n"
            "    pcc_rule_set__init(auxil, &head->eval);\n"
            "    head->hold = NULL;\n"
            "    return head;\n"
            "}\n"
            "\n"
            "static void pcc_lr_head__destroy(pcc_auxil_t auxil, pcc_lr_head_t *head) {\n"
            "    if (head == NULL) return;\n"
            "    pcc_lr_head__destroy(auxil, head->hold);\n"
            "    pcc_rule_set__term(auxil, &head->eval);\n"
            "    pcc_rule_set__term(auxil, &head->invol);\n"
            "    PCC_FREE(auxil, head);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_lr_entry__destroy(pcc_auxil_t auxil, pcc_lr_entry_t *lr);\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_answer__create(pcc_auxil_t auxil, pcc_lr_answer_type_t type, size_t pos) {\n"
            "    pcc_lr_answer_t *answer = (pcc_lr_answer_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_answer_t));\n"
            "    answer->type = type;\n"
            "    answer->pos = pos;\n"
            "    answer->hold = NULL;\n"
            "    switch (answer->type) {\n"
            "    case PCC_LR_ANSWER_LR:\n"
            "        answer->data.lr = NULL;\n"
            "        break;\n"
            "    case PCC_LR_ANSWER_CHUNK:\n"
            "        answer->data.chunk = NULL;\n"
            "        break;\n"
            "    default: /* unknown */\n"
            "        PCC_FREE(auxil, answer);\n"
            "        answer = NULL;\n"
            "    }\n"
            "    return answer;\n"
            "}\n"
            "\n"
            "static void pcc_lr_answer__set_chunk(pcc_auxil_t auxil, pcc_lr_answer_t *answer, pcc_thunk_chunk_t *chunk) {\n"
            "    pcc_lr_answer_t *const a = pcc_lr_answer__create(auxil, answer->type, answer->pos);\n"
            "    switch (answer->type) {\n"
            "    case PCC_LR_ANSWER_LR:\n"
            "        a->data.lr = answer->data.lr;\n"
            "        break;\n"
            "    case PCC_LR_ANSWER_CHUNK:\n"
            "        a->data.chunk = answer->data.chunk;\n"
            "        break;\n"
            "    default: /* unknown */\n"
            "        break;\n"
            "    }\n"
            "    a->hold = answer->hold;\n"
            "    answer->hold = a;\n"
            "    answer->type = PCC_LR_ANSWER_CHUNK;\n"
            "    answer->data.chunk = chunk;\n"
            "}\n"
            "\n"
            "static void pcc_lr_answer__destroy(pcc_auxil_t auxil, pcc_lr_answer_t *answer) {\n"
            "    while (answer != NULL) {\n"
            "        pcc_lr_answer_t *const a = answer->hold;\n"
            "        switch (answer->type) {\n"
            "        case PCC_LR_ANSWER_LR:\n"
            "            pcc_lr_entry__destroy(auxil, answer->data.lr);\n"
            "            break;\n"
            "        case PCC_LR_ANSWER_CHUNK:\n"
            "            pcc_thunk_chunk__destroy(auxil, answer->data.chunk);\n"
            "            break;\n"
            "        default: /* unknown */\n"
            "            break;\n"
            "        }\n"
            "        PCC_FREE(auxil, answer);\n"
            "        answer = a;\n"
            "    }\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_lr_memo_map__init(pcc_auxil_t auxil, pcc_lr_memo_map_t *map) {\n"
            "    map->len = 0;\n"
            "    map->max = 0;\n"
            "    map->buf = NULL;\n"
            "}\n"
            "\n"
            "static size_t pcc_lr_memo_map__index(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule) {\n"
            "    size_t i;\n"
            "    for (i = 0; i < map->len; i++) {\n"
            "        if (map->buf[i].rule == rule) return i;\n"
            "    }\n"
            "    return PCC_VOID_VALUE;\n"
            "}\n"
            "\n"
            "static void pcc_lr_memo_map__put(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule, pcc_lr_answer_t *answer) {\n"
            "    const size_t i = pcc_lr_memo_map__index(auxil, map, rule);\n"
            "    if (i != PCC_VOID_VALUE) {\n"
            "        pcc_lr_answer__destroy(auxil, map->buf[i].answer);\n"
            "        map->buf[i].answer = answer;\n"
            "    }\n"
            "    else {\n"
            "        if (map->max <= map->len) {\n"
            "            const size_t n = map->len + 1;\n"
            "            size_t m = map->max;\n"
            "            if (m == 0) m = PCC_ARRAYSIZE;\n"
            "            while (m < n && m != 0) m <<= 1;\n"
            "            if (m == 0) m = n;\n"
            "            map->buf = (pcc_lr_memo_t *)PCC_REALLOC(auxil, map->buf, sizeof(pcc_lr_memo_t) * m);\n"
            "            map->max = m;\n"
            "        }\n"
            "        map->buf[map->len].rule = rule;\n"
            "        map->buf[map->len].answer = answer;\n"
            "        map->len++;\n"
            "    }\n"
            "}\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_memo_map__get(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule) {\n"
            "    const size_t i = pcc_lr_memo_map__index(auxil, map, rule);\n"
            "    return (i != PCC_VOID_VALUE) ? map->buf[i].answer : NULL;\n"
            "}\n"
            "\n"
            "static void pcc_lr_memo_map__term(pcc_auxil_t auxil, pcc_lr_memo_map_t *map) {\n"
            "    while (map->len > 0) {\n"
            "        map->len--;\n"
            "        pcc_lr_answer__destroy(auxil, map->buf[map->len].answer);\n"
            "    }\n"
            "    PCC_FREE(auxil, map->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static pcc_lr_table_entry_t *pcc_lr_table_entry__create(pcc_auxil_t auxil) {\n"
            "    pcc_lr_table_entry_t *const entry = (pcc_lr_table_entry_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_table_entry_t));\n"
            "    entry->head = NULL;\n"
            "    pcc_lr_memo_map__init(auxil, &entry->memos);\n"
            "    entry->hold_a = NULL;\n"
            "    entry->hold_h = NULL;\n"
            "    return entry;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table_entry__destroy(pcc_auxil_t auxil, pcc_lr_table_entry_t *entry) {\n"
            "    if (entry == NULL) return;\n"
            "    pcc_lr_head__destroy(auxil, entry->hold_h);\n"
            "    pcc_lr_answer__destroy(auxil, entry->hold_a);\n"
            "    pcc_lr_memo_map__term(auxil, &entry->memos);\n"
            "    PCC_FREE(auxil, entry);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_lr_table__init(pcc_auxil_t auxil, pcc_lr_table_t *table) {\n"
            "    table->ofs = 0;\n"
            "    table->len = 0;\n"
            "    table->max = 0;\n"
            "    table->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__resize(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t len) {\n"
            "    size_t i;\n"
            "    for (i = len; i < table->len; i++) pcc_lr_table_entry__destroy(auxil, table->buf[i]);\n"
            "    if (table->max < len) {\n"
            "        size_t m = table->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < len && m != 0) m <<= 1;\n"
            "        if (m == 0) m = len;\n"
            "        table->buf = (pcc_lr_table_entry_t **)PCC_REALLOC(auxil, table->buf, sizeof(pcc_lr_table_entry_t *) * m);\n"
            "        table->max = m;\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__set_head(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index, pcc_lr_head_t *head) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    table->buf[index]->head = head;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__hold_head(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index, pcc_lr_head_t *head) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    head->hold = table->buf[index]->hold_h;\n"
            "    table->buf[index]->hold_h = head;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__set_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index, pcc_rule_t rule, pcc_lr_answer_t *answer) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    pcc_lr_memo_map__put(auxil, &table->buf[index]->memos, rule, answer);\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__hold_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index, pcc_lr_answer_t *answer) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    answer->hold = table->buf[index]->hold_a;\n"
            "    table->buf[index]->hold_a = answer;\n"
            "}\n"
            "\n"
            "static pcc_lr_head_t *pcc_lr_table__get_head(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len || table->buf[index] == NULL) return NULL;\n"
            "    return table->buf[index]->head;\n"
            "}\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_table__get_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t index, pcc_rule_t rule) {\n"
            "    index += table->ofs;\n"
            "    if (index >= table->len || table->buf[index] == NULL) return NULL;\n"
            "    return pcc_lr_memo_map__get(auxil, &table->buf[index]->memos, rule);\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__shift(pcc_auxil_t auxil, pcc_lr_table_t *table, size_t count) {\n"
            "    size_t i;\n"
            "    if (count > table->len - table->ofs) count = table->len - table->ofs;\n"
            "    for (i = 0; i < count; i++) pcc_lr_table_entry__destroy(auxil, table->buf[table->ofs++]);\n"
            "    if (table->ofs > (table->max >> 1)) {\n"
            "        memmove(table->buf, table->buf + table->ofs, sizeof(pcc_lr_table_entry_t *) * (table->len - table->ofs));\n"
            "        table->len -= table->ofs;\n"
            "        table->ofs = 0;\n"
            "    }\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__term(pcc_auxil_t auxil, pcc_lr_table_t *table) {\n"
            "    while (table->len > table->ofs) {\n"
            "        table->len--;\n"
            "        pcc_lr_table_entry__destroy(auxil, table->buf[table->len]);\n"
            "    }\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static pcc_lr_entry_t *pcc_lr_entry__create(pcc_auxil_t auxil, pcc_rule_t rule) {\n"
            "    pcc_lr_entry_t *const lr = (pcc_lr_entry_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_entry_t));\n"
            "    lr->rule = rule;\n"
            "    lr->seed = NULL;\n"
            "    lr->head = NULL;\n"
            "    return lr;\n"
            "}\n"
            "\n"
            "static void pcc_lr_entry__destroy(pcc_auxil_t auxil, pcc_lr_entry_t *lr) {\n"
            "    PCC_FREE(auxil, lr);\n"
            "}\n"
            "\n",
            sstream
        );
        fputs_e(
            "static void pcc_lr_stack__init(pcc_auxil_t auxil, pcc_lr_stack_t *stack) {\n"
            "    stack->len = 0;\n"
            "    stack->max = 0;\n"
            "    stack->buf = NULL;\n"
            "}\n"
            "\n"
            "static void pcc_lr_stack__push(pcc_auxil_t auxil, pcc_lr_stack_t *stack, pcc_lr_entry_t *lr) {\n"
            "    if (stack->max <= stack->len) {\n"
            "        const size_t n = stack->len + 1;\n"
            "        size_t m = stack->max;\n"
            "        if (m == 0) m = PCC_ARRAYSIZE;\n"
            "        while (m < n && m != 0) m <<= 1;\n"
            "        if (m == 0) m = n;\n"
            "        stack->buf = (pcc_lr_entry_t **)PCC_REALLOC(auxil, stack->buf, sizeof(pcc_lr_entry_t *) * m);\n"
            "        stack->max = m;\n"
            "    }\n"
            "    stack->buf[stack->len++] = lr;\n"
            "}\n"
            "\n"
            "static pcc_lr_entry_t *pcc_lr_stack__pop(pcc_auxil_t auxil, pcc_lr_stack_t *stack) {\n"
            "    return stack->buf[--stack->len];\n"
            "}\n"
            "\n"
            "static void pcc_lr_stack__term(pcc_auxil_t auxil, pcc_lr_stack_t *stack) {\n"
            "    PCC_FREE(auxil, stack->buf);\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "static %s_context_t *pcc_context__create(pcc_auxil_t auxil) {\n"
            "    %s_context_t *const ctx = (%s_context_t *)PCC_MALLOC(auxil, sizeof(%s_context_t));\n",
            get_prefix(ctx), get_prefix(ctx), get_prefix(ctx), get_prefix(ctx)
        );
        fputs_e(
            "    ctx->pos = 0;\n"
            "    ctx->cur = 0;\n"
            "    ctx->level = 0;\n"
            "    pcc_char_array__init(auxil, &ctx->buffer);\n"
            "    pcc_lr_table__init(auxil, &ctx->lrtable);\n"
            "    pcc_lr_stack__init(auxil, &ctx->lrstack);\n"
            "    pcc_thunk_array__init(auxil, &ctx->thunks);\n"
            "    ctx->auxil = auxil;\n"
            "    return ctx;\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "static void pcc_context__destroy(%s_context_t *ctx) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    if (ctx == NULL) return;\n"
            "    pcc_thunk_array__term(ctx->auxil, &ctx->thunks);\n"
            "    pcc_lr_stack__term(ctx->auxil, &ctx->lrstack);\n"
            "    pcc_lr_table__term(ctx->auxil, &ctx->lrtable);\n"
            "    pcc_char_array__term(ctx->auxil, &ctx->buffer);\n"
            "    PCC_FREE(ctx->auxil, ctx);\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "static size_t pcc_refill_buffer(%s_context_t *ctx, size_t num) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    if (ctx->buffer.len >= ctx->cur + num) return ctx->buffer.len - ctx->cur;\n"
            "    while (ctx->buffer.len < ctx->cur + num) {\n"
            "        const int c = PCC_GETCHAR(ctx->auxil);\n"
            "        if (c < 0) break;\n"
            "        pcc_char_array__add(ctx->auxil, &ctx->buffer, (char)c);\n"
            "    }\n"
            "    return ctx->buffer.len - ctx->cur;\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "MARK_USED_FUNC\n"
            "static void pcc_commit_buffer(%s_context_t *ctx) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    memmove(ctx->buffer.buf, ctx->buffer.buf + ctx->cur, ctx->buffer.len - ctx->cur);\n"
            "    ctx->buffer.len -= ctx->cur;\n"
            "    ctx->pos += ctx->cur;\n"
            "    pcc_lr_table__shift(ctx->auxil, &ctx->lrtable, ctx->cur);\n"
            "    ctx->cur = 0;\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "MARK_USED_FUNC\n"
            "static const char *pcc_get_capture_string(%s_context_t *ctx, const pcc_capture_t *capt) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    if (capt->string == NULL)\n"
            "        ((pcc_capture_t *)capt)->string =\n"
            "            pcc_strndup_e(ctx->auxil, ctx->buffer.buf + capt->range.start, capt->range.end - capt->range.start);\n"
            "    return capt->string;\n"
            "}\n"
            "\n",
            sstream
        );
        if (ctx->flags & CODE_FLAG__UTF8_CHARCLASS_USED) {
            fprintf_e(
                sstream,
                "static size_t pcc_get_char_as_utf32(%s_context_t *ctx, int *out) { /* with checking UTF-8 validity */\n",
                get_prefix(ctx)
            );
            fputs_e(
                "    int c, u;\n"
                "    size_t n;\n"
                "    if (pcc_refill_buffer(ctx, 1) < 1) return 0;\n"
                "    c = (int)(unsigned char)ctx->buffer.buf[ctx->cur];\n"
                "    n = (c < 0x80) ? 1 :\n"
                "        ((c & 0xe0) == 0xc0) ? 2 :\n"
                "        ((c & 0xf0) == 0xe0) ? 3 :\n"
                "        ((c & 0xf8) == 0xf0) ? 4 : 0;\n"
                "    if (n < 1) return 0;\n"
                "    if (pcc_refill_buffer(ctx, n) < n) return 0;\n"
                "    switch (n) {\n"
                "    case 1:\n"
                "        u = c;\n"
                "        break;\n"
                "    case 2:\n"
                "        u = c & 0x1f;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 1];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        if (u < 0x80) return 0;\n"
                "        break;\n"
                "    case 3:\n"
                "        u = c & 0x0f;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 1];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 2];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        if (u < 0x800) return 0;\n"
                "        break;\n"
                "    case 4:\n"
                "        u = c & 0x07;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 1];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 2];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        c = (int)(unsigned char)ctx->buffer.buf[ctx->cur + 3];\n"
                "        if ((c & 0xc0) != 0x80) return 0;\n"
                "        u <<= 6; u |= c & 0x3f;\n"
                "        if (u < 0x10000 || u > 0x10ffff) return 0;\n"
                "        break;\n"
                "    default:\n"
                "        return 0;\n"
                "    }\n"
                "    if (out) *out = u;\n"
                "    return n;\n"
                "}\n"
                "\n",
                sstream
            );
        }
        fprintf_e(
            sstream,
            "MARK_USED_FUNC\n"
            "static pcc_bool_t pcc_apply_rule(%s_context_t *ctx, pcc_rule_t rule, pcc_thunk_array_t *thunks, pcc_value_t *value) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    static pcc_value_t null;\n"
            "    pcc_thunk_chunk_t *c = NULL;\n"
            "    const size_t p = ctx->pos + ctx->cur;\n"
            "    pcc_bool_t b = PCC_TRUE;\n"
            "    pcc_lr_answer_t *a = pcc_lr_table__get_answer(ctx->auxil, &ctx->lrtable, p, rule);\n"
            "    pcc_lr_head_t *h = pcc_lr_table__get_head(ctx->auxil, &ctx->lrtable, p);\n"
            "    if (h != NULL) {\n"
            "        if (a == NULL && rule != h->rule && pcc_rule_set__index(ctx->auxil, &h->invol, rule) == PCC_VOID_VALUE) {\n"
            "            b = PCC_FALSE;\n"
            "            c = NULL;\n"
            "        }\n"
            "        else if (pcc_rule_set__remove(ctx->auxil, &h->eval, rule)) {\n"
            "            b = PCC_FALSE;\n"
            "            c = rule(ctx);\n"
            "            a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_CHUNK, ctx->pos + ctx->cur);\n"
            "            a->data.chunk = c;\n"
            "            pcc_lr_table__hold_answer(ctx->auxil, &ctx->lrtable, p, a);\n"
            "        }\n"
            "    }\n"
            "    if (b) {\n"
            "        if (a != NULL) {\n"
            "            ctx->cur = a->pos - ctx->pos;\n"
            "            switch (a->type) {\n"
            "            case PCC_LR_ANSWER_LR:\n"
            "                if (a->data.lr->head == NULL) {\n"
            "                    a->data.lr->head = pcc_lr_head__create(ctx->auxil, rule);\n"
            "                    pcc_lr_table__hold_head(ctx->auxil, &ctx->lrtable, p, a->data.lr->head);\n"
            "                }\n"
            "                {\n"
            "                    size_t i = ctx->lrstack.len;\n"
            "                    while (i > 0) {\n"
            "                        i--;\n"
            "                        if (ctx->lrstack.buf[i]->head == a->data.lr->head) break;\n"
            "                        ctx->lrstack.buf[i]->head = a->data.lr->head;\n"
            "                        pcc_rule_set__add(ctx->auxil, &a->data.lr->head->invol, ctx->lrstack.buf[i]->rule);\n"
            "                    }\n"
            "                }\n"
            "                c = a->data.lr->seed;\n"
            "                break;\n"
            "            case PCC_LR_ANSWER_CHUNK:\n"
            "                c = a->data.chunk;\n"
            "                break;\n"
            "            default: /* unknown */\n"
            "                break;\n"
            "            }\n"
            "        }\n"
            "        else {\n"
            "            pcc_lr_entry_t *const e = pcc_lr_entry__create(ctx->auxil, rule);\n"
            "            pcc_lr_stack__push(ctx->auxil, &ctx->lrstack, e);\n"
            "            a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_LR, p);\n"
            "            a->data.lr = e;\n"
            "            pcc_lr_table__set_answer(ctx->auxil, &ctx->lrtable, p, rule, a);\n"
            "            c = rule(ctx);\n"
            "            pcc_lr_stack__pop(ctx->auxil, &ctx->lrstack);\n"
            "            a->pos = ctx->pos + ctx->cur;\n"
            "            if (e->head == NULL) {\n"
            "                pcc_lr_answer__set_chunk(ctx->auxil, a, c);\n"
            "            }\n"
            "            else {\n"
            "                e->seed = c;\n"
            "                h = a->data.lr->head;\n"
            "                if (h->rule != rule) {\n"
            "                    c = a->data.lr->seed;\n"
            "                    a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_CHUNK, ctx->pos + ctx->cur);\n"
            "                    a->data.chunk = c;\n"
            "                    pcc_lr_table__hold_answer(ctx->auxil, &ctx->lrtable, p, a);\n"
            "                }\n"
            "                else {\n"
            "                    pcc_lr_answer__set_chunk(ctx->auxil, a, a->data.lr->seed);\n"
            "                    if (a->data.chunk == NULL) {\n"
            "                        c = NULL;\n"
            "                    }\n"
            "                    else {\n"
            "                        pcc_lr_table__set_head(ctx->auxil, &ctx->lrtable, p, h);\n"
            "                        for (;;) {\n"
            "                            ctx->cur = p - ctx->pos;\n"
            "                            pcc_rule_set__copy(ctx->auxil, &h->eval, &h->invol);\n"
            "                            c = rule(ctx);\n"
            "                            if (c == NULL || ctx->pos + ctx->cur <= a->pos) break;\n"
            "                            pcc_lr_answer__set_chunk(ctx->auxil, a, c);\n"
            "                            a->pos = ctx->pos + ctx->cur;\n"
            "                        }\n"
            "                        pcc_thunk_chunk__destroy(ctx->auxil, c);\n"
            "                        pcc_lr_table__set_head(ctx->auxil, &ctx->lrtable, p, NULL);\n"
            "                        ctx->cur = a->pos - ctx->pos;\n"
            "                        c = a->data.chunk;\n"
            "                    }\n"
            "                }\n"
            "            }\n"
            "        }\n"
            "    }\n"
            "    if (c == NULL) return PCC_FALSE;\n"
            "    if (value == NULL) value = &null;\n"
            "    memset(value, 0, sizeof(pcc_value_t)); /* in case */\n"
            "    pcc_thunk_array__add(ctx->auxil, thunks, pcc_thunk__create_node(ctx->auxil, &c->thunks, value));\n"
            "    return PCC_TRUE;\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "MARK_USED_FUNC\n"
            "static void pcc_do_action(%s_context_t *ctx, const pcc_thunk_array_t *thunks, pcc_value_t *value) {\n",
            get_prefix(ctx)
        );
        fputs_e(
            "    size_t i;\n"
            "    for (i = 0; i < thunks->len; i++) {\n"
            "        pcc_thunk_t *const thunk = thunks->buf[i];\n"
            "        switch (thunk->type) {\n"
            "        case PCC_THUNK_LEAF:\n"
            "            thunk->data.leaf.action(ctx, thunk, value);\n"
            "            break;\n"
            "        case PCC_THUNK_NODE:\n"
            "            pcc_do_action(ctx, thunk->data.node.thunks, thunk->data.node.value);\n"
            "            break;\n"
            "        default: /* unknown */\n"
            "            break;\n"
            "        }\n"
            "    }\n"
            "}\n"
            "\n",
            sstream
        );
        {
            size_t i, j, k;
            for (i = 0; i < ctx->rules.len; i++) {
                const node_rule_t *const r = &ctx->rules.buf[i]->data.rule;
                for (j = 0; j < r->codes.len; j++) {
                    const char *s;
                    size_t d;
                    const node_const_array_t *v, *c;
                    switch (r->codes.buf[j]->type) {
                    case NODE_ACTION:
                        s = r->codes.buf[j]->data.action.value;
                        d = r->codes.buf[j]->data.action.index;
                        v = &r->codes.buf[j]->data.action.vars;
                        c = &r->codes.buf[j]->data.action.capts;
                        break;
                    case NODE_ERROR:
                        s = r->codes.buf[j]->data.error.value;
                        d = r->codes.buf[j]->data.error.index;
                        v = &r->codes.buf[j]->data.error.vars;
                        c = &r->codes.buf[j]->data.error.capts;
                        break;
                    default:
                        print_error("Internal error [%d]\n", __LINE__);
                        exit(-1);
                    }
                    fprintf_e(
                        sstream,
                        "static void pcc_action_%s_" FMT_LU "(%s_context_t *__pcc_ctx, pcc_thunk_t *__pcc_in, pcc_value_t *__pcc_out) {\n",
                        r->name, (ulong_t)d, get_prefix(ctx)
                    );
                    fputs_e(
                        "#define auxil (__pcc_ctx->auxil)\n"
                        "#define __ (*__pcc_out)\n",
                        sstream
                    );
                    k = 0;
                    while (k < v->len) {
                        assert(v->buf[k]->type == NODE_REFERENCE);
                        fprintf_e(
                            sstream,
                            "#define %s (*__pcc_in->data.leaf.values.buf[" FMT_LU "])\n",
                            v->buf[k]->data.reference.var, (ulong_t)v->buf[k]->data.reference.index
                        );
                        k++;
                    }
                    fputs_e(
                        "#define _0 pcc_get_capture_string(__pcc_ctx, &__pcc_in->data.leaf.capt0)\n"
                        "#define _0s ((const size_t)(__pcc_ctx->pos + __pcc_in->data.leaf.capt0.range.start))\n"
                        "#define _0e ((const size_t)(__pcc_ctx->pos + __pcc_in->data.leaf.capt0.range.end))\n",
                        sstream
                    );
                    k = 0;
                    while (k < c->len) {
                        assert(c->buf[k]->type == NODE_CAPTURE);
                        fprintf_e(
                            sstream,
                            "#define _" FMT_LU " pcc_get_capture_string(__pcc_ctx, __pcc_in->data.leaf.capts.buf[" FMT_LU "])\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1), (ulong_t)c->buf[k]->data.capture.index
                        );
                        fprintf_e(
                            sstream,
                            "#define _" FMT_LU "s ((const size_t)(__pcc_ctx->pos + __pcc_in->data.leaf.capts.buf[" FMT_LU "]->range.start))\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1), (ulong_t)c->buf[k]->data.capture.index
                        );
                        fprintf_e(
                            sstream,
                            "#define _" FMT_LU "e ((const size_t)(__pcc_ctx->pos + __pcc_in->data.leaf.capts.buf[" FMT_LU "]->range.end))\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1), (ulong_t)c->buf[k]->data.capture.index
                        );
                        k++;
                    }
                    write_code_block(sstream, s, strlen(s), 4);
                    k = c->len;
                    while (k > 0) {
                        k--;
                        assert(c->buf[k]->type == NODE_CAPTURE);
                        fprintf_e(
                            sstream,
                            "#undef _" FMT_LU "e\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1)
                        );
                        fprintf_e(
                            sstream,
                            "#undef _" FMT_LU "s\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1)
                        );
                        fprintf_e(
                            sstream,
                            "#undef _" FMT_LU "\n",
                            (ulong_t)(c->buf[k]->data.capture.index + 1)
                        );
                    }
                    fputs_e(
                        "#undef _0e\n"
                        "#undef _0s\n"
                        "#undef _0\n",
                        sstream
                    );
                    k = v->len;
                    while (k > 0) {
                        k--;
                        assert(v->buf[k]->type == NODE_REFERENCE);
                        fprintf_e(
                            sstream,
                            "#undef %s\n",
                            v->buf[k]->data.reference.var
                        );
                    }
                    fputs_e(
                        "#undef __\n"
                        "#undef auxil\n",
                        sstream
                    );
                    fputs_e(
                        "}\n"
                        "\n",
                        sstream
                    );
                }
            }
        }
        {
            size_t i;
            for (i = 0; i < ctx->rules.len; i++) {
                fprintf_e(
                    sstream,
                    "static pcc_thunk_chunk_t *pcc_evaluate_rule_%s(%s_context_t *ctx);\n",
                    ctx->rules.buf[i]->data.rule.name, get_prefix(ctx)
                );
            }
            fputs_e(
                "\n",
                sstream
            );
            for (i = 0; i < ctx->rules.len; i++) {
                code_reach_t r;
                generate_t g;
                g.stream = sstream;
                g.rule = ctx->rules.buf[i];
                g.label = 0;
                g.ascii = ctx->ascii;
                fprintf_e(
                    sstream,
                    "static pcc_thunk_chunk_t *pcc_evaluate_rule_%s(%s_context_t *ctx) {\n",
                    ctx->rules.buf[i]->data.rule.name, get_prefix(ctx)
                );
                fprintf_e(
                    sstream,
                    "    pcc_thunk_chunk_t *const chunk = pcc_thunk_chunk__create(ctx->auxil);\n"
                    "    chunk->pos = ctx->cur;\n"
                    "    PCC_DEBUG(ctx->auxil, PCC_DBG_EVALUATE, \"%s\", ctx->level, chunk->pos, (ctx->buffer.buf + chunk->pos), (ctx->buffer.len - chunk->pos));\n"
                    "    ctx->level++;\n",
                    ctx->rules.buf[i]->data.rule.name
                );
                fprintf_e(
                    sstream,
                    "    pcc_value_table__resize(ctx->auxil, &chunk->values, " FMT_LU ");\n",
                    (ulong_t)ctx->rules.buf[i]->data.rule.vars.len
                );
                fprintf_e(
                    sstream,
                    "    pcc_capture_table__resize(ctx->auxil, &chunk->capts, " FMT_LU ");\n",
                    (ulong_t)ctx->rules.buf[i]->data.rule.capts.len
                );
                r = generate_code(&g, ctx->rules.buf[i]->data.rule.expr, 0, 4, FALSE);
                fprintf_e(
                    sstream,
                    "    ctx->level--;\n"
                    "    PCC_DEBUG(ctx->auxil, PCC_DBG_MATCH, \"%s\", ctx->level, chunk->pos, (ctx->buffer.buf + chunk->pos), (ctx->cur - chunk->pos));\n"
                    "    return chunk;\n",
                    ctx->rules.buf[i]->data.rule.name
                );
                if (r != CODE_REACH__ALWAYS_SUCCEED) {
                    fprintf_e(
                        sstream,
                        "L0000:;\n"
                        "    ctx->level--;\n"
                        "    PCC_DEBUG(ctx->auxil, PCC_DBG_NOMATCH, \"%s\", ctx->level, chunk->pos, (ctx->buffer.buf + chunk->pos), (ctx->cur - chunk->pos));\n"
                        "    pcc_thunk_chunk__destroy(ctx->auxil, chunk);\n"
                        "    return NULL;\n",
                        ctx->rules.buf[i]->data.rule.name
                    );
                }
                fputs_e(
                    "}\n"
                    "\n",
                    sstream
                );
            }
        }
        fprintf_e(
            sstream,
            "%s_context_t *%s_create(%s%sauxil) {\n",
            get_prefix(ctx), get_prefix(ctx),
            at, ap ? "" : " "
        );
        fputs_e(
            "    return pcc_context__create(auxil);\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "int %s_parse(%s_context_t *ctx, %s%s*ret) {\n",
            get_prefix(ctx), get_prefix(ctx),
            vt, vp ? "" : " "
        );
        if (ctx->rules.len > 0) {
            fprintf_e(
                sstream,
                "    if (pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &ctx->thunks, ret))\n",
                ctx->rules.buf[0]->data.rule.name
            );
            fputs_e(
                "        pcc_do_action(ctx, &ctx->thunks, ret);\n"
                "    else\n"
                "        PCC_ERROR(ctx->auxil);\n"
                "    pcc_commit_buffer(ctx);\n",
                sstream
            );
        }
        fputs_e(
            "    pcc_thunk_array__revert(ctx->auxil, &ctx->thunks, 0);\n"
            "    return pcc_refill_buffer(ctx, 1) >= 1;\n"
            "}\n"
            "\n",
            sstream
        );
        fprintf_e(
            sstream,
            "void %s_destroy(%s_context_t *ctx) {\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fputs_e(
            "    pcc_context__destroy(ctx);\n"
            "}\n",
            sstream
        );
    }
    {
        fputs_e(
            "#ifdef __cplusplus\n"
            "extern \"C\" {\n"
            "#endif\n"
            "\n",
            hstream
        );
        fprintf_e(
            hstream,
            "typedef struct %s_context_tag %s_context_t;\n"
            "\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fprintf_e(
            hstream,
            "%s_context_t *%s_create(%s%sauxil);\n",
            get_prefix(ctx), get_prefix(ctx),
            at, ap ? "" : " "
        );
        fprintf_e(
            hstream,
            "int %s_parse(%s_context_t *ctx, %s%s*ret);\n",
            get_prefix(ctx), get_prefix(ctx),
            vt, vp ? "" : " "
        );
        fprintf_e(
            hstream,
            "void %s_destroy(%s_context_t *ctx);\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fputs_e(
            "\n"
            "#ifdef __cplusplus\n"
            "}\n"
            "#endif\n",
            hstream
        );
        fprintf_e(
            hstream,
            "\n"
            "#endif /* !PCC_INCLUDED_%s */\n",
            ctx->hid
        );
    }
    {
        match_eol(ctx);
        if (!match_eof(ctx)) fputc_e('\n', sstream);
        commit_buffer(ctx);
        while (refill_buffer(ctx, ctx->buffer.max) > 0) {
            const size_t n = (ctx->buffer.len > 0 && ctx->buffer.buf[ctx->buffer.len - 1] == '\r') ? ctx->buffer.len - 1 : ctx->buffer.len;
            write_text(sstream, ctx->buffer.buf, n);
            ctx->bufcur = n;
            commit_buffer(ctx);
        }
    }
    fclose_e(hstream);
    fclose_e(sstream);
    if (ctx->errnum) {
        unlink(ctx->hname);
        unlink(ctx->sname);
        return FALSE;
    }
    return TRUE;
}

static void print_version(FILE *output) {
    fprintf(output, "%s version %s\n", g_cmdname, VERSION);
    fprintf(output, "Copyright (c) 2014, 2019-2021 Arihiro Yoshida. All rights reserved.\n");
}

static void print_usage(FILE *output) {
    fprintf(output, "Usage: %s [OPTIONS] [FILE]\n", g_cmdname);
    fprintf(output, "Generates a packrat parser for C.\n");
    fprintf(output, "\n");
    fprintf(output, "  -o BASENAME    specify a base name of output source and header files\n");
    fprintf(output, "  -a, --ascii    disable UTF-8 support\n");
    fprintf(output, "  -d, --debug    with debug information\n");
    fprintf(output, "  -h, --help     print this help message and exit\n");
    fprintf(output, "  -v, --version  print the version and exit\n");
}

int main(int argc, char **argv) {
    const char *iname = NULL;
    const char *oname = NULL;
    bool_t ascii = FALSE;
    bool_t debug = FALSE;
#ifdef _MSC_VER
#ifdef _DEBUG
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
#endif
#endif
    g_cmdname = extract_filename(argv[0]);
    {
        const char *fname = NULL;
        const char *opt_o = NULL;
        bool_t opt_a = FALSE;
        bool_t opt_d = FALSE;
        bool_t opt_h = FALSE;
        bool_t opt_v = FALSE;
        int i;
        for (i = 1; i < argc; i++) {
            if (argv[i][0] != '-') {
                break;
            }
            else if (strcmp(argv[i], "--") == 0) {
                i++; break;
            }
            else if (argv[i][1] == 'o') {
                const char *const o = (argv[i][2] != '\0') ? argv[i] + 2 : (++i < argc) ?  argv[i] : NULL;
                if (o == NULL) {
                    print_error("Output base name missing\n");
                    fprintf(stderr, "\n");
                    print_usage(stderr);
                    exit(1);
                }
                if (opt_o != NULL) {
                    print_error("Extra output base name '%s'\n", o);
                    fprintf(stderr, "\n");
                    print_usage(stderr);
                    exit(1);
                }
                opt_o = o;
            }
            else if (strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "--ascii") == 0) {
                opt_a = TRUE;
            }
            else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--debug") == 0) {
                opt_d = TRUE;
            }
            else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                opt_h = TRUE;
            }
            else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--version") == 0) {
                opt_v = TRUE;
            }
            else {
                print_error("Invalid option '%s'\n", argv[i]);
                fprintf(stderr, "\n");
                print_usage(stderr);
                exit(1);
            }
        }
        switch (argc - i) {
        case 0:
            break;
        case 1:
            fname = argv[i];
            break;
        default:
            print_error("Multiple input files\n");
            fprintf(stderr, "\n");
            print_usage(stderr);
            exit(1);
        }
        if (opt_h || opt_v) {
            if (opt_v) print_version(stdout);
            if (opt_v && opt_h) fprintf(stdout, "\n");
            if (opt_h) print_usage(stdout);
            exit(0);
        }
        iname = (fname != NULL && fname[0] != '\0') ? fname : NULL;
        oname = (opt_o != NULL && opt_o[0] != '\0') ? opt_o : NULL;
        ascii = opt_a;
        debug = opt_d;
    }
    {
        context_t *const ctx = create_context(iname, oname, ascii, debug);
        const int b = parse(ctx) && generate(ctx);
        destroy_context(ctx);
        if (!b) exit(10);
    }
    return 0;
}
