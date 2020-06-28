/*
 * PackCC: a packrat parser generator for C.
 *
 * Copyright (c) 2014 Arihiro Yoshida. All rights reserved.
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
#if _MSC_VER < 1900
#define snprintf _snprintf
#endif
#define unlink _unlink
#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif
#endif

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#ifndef _MSC_VER
#if ((defined USE_SYSTEM_STRNLEN) == 0) && defined __GNUC__ && defined _WIN32 /* MinGW */
static size_t strnlen(const char *str, size_t maxlen) {
    size_t i;
    for (i = 0; str[i] && i < maxlen; i++);
    return i;
}
#else
#include <unistd.h> /* for strnlen() */
#endif
#endif

#define VERSION "1.2.5"

#ifndef BUFFER_INIT_SIZE
#define BUFFER_INIT_SIZE 256
#endif
#ifndef ARRAY_INIT_SIZE
#define ARRAY_INIT_SIZE 2
#endif

typedef struct char_array_tag {
    char *buf;
    int max;
    int len;
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
    NODE_ERROR,
} node_type_t;

typedef struct node_tag node_t;

typedef struct node_array_tag {
    node_t **buf;
    int max;
    int len;
} node_array_t;

typedef struct node_const_array_tag {
    const node_t **buf;
    int max;
    int len;
} node_const_array_t;

typedef struct node_hash_table_tag {
    const node_t **buf;
    int max;
    int mod;
} node_hash_table_t;

typedef struct node_rule_tag {
    char *name;
    node_t *expr;
    int ref; /* mutable */
    node_const_array_t vars;
    node_const_array_t capts;
    node_const_array_t codes;
    int line;
    int col;
} node_rule_t;

typedef struct node_reference_tag {
    char *var; /* NULL if no variable name */
    int index;
    char *name;
    const node_t *rule;
    int line;
    int col;
} node_reference_t;

typedef struct node_string_tag {
    char *value;
    size_t len;
} node_string_t;

typedef struct node_charclass_tag {
    char *value; /* NULL means any character */
    size_t len;
} node_charclass_t;

typedef struct node_quantity_tag {
    int min;
    int max;
    node_t *expr;
} node_quantity_t;

typedef struct node_predicate_tag {
    bool neg;
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
    int index;
} node_capture_t;

typedef struct node_expand_tag {
    int index;
    int line;
    int col;
} node_expand_t;

typedef struct node_action_tag {
    char *value;
    int index;
    node_const_array_t vars;
    node_const_array_t capts;
} node_action_t;

typedef struct node_error_tag {
    node_t *expr;
    char *value;
    int index;
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

typedef struct context_tag {
    char *iname;
    char *sname;
    char *hname;
    FILE *ifile;
    FILE *sfile;
    FILE *hfile;
    char *hid;
    char *vtype;
    char *atype;
    char *prefix;
    bool debug;
    int errnum;
    int linenum;
    int linepos;
    int bufpos;
    char_array_t buffer;
    node_array_t rules;
    node_hash_table_t rulehash;
    char *earlyinclude;
} context_t;

typedef struct generate_tag {
    FILE *stream;
    const node_t *rule;
    int label;
} generate_t;

typedef enum string_flag_tag {
    STRING_FLAG__NONE = 0,
    STRING_FLAG__NOTEMPTY = 1,
    STRING_FLAG__NOTVOID = 2,
    STRING_FLAG__IDENTIFIER = 4,
} string_flag_t;

typedef enum code_reach_tag {
    CODE_REACH__BOTH = 0,
    CODE_REACH__ALWAYS_SUCCEED = 1,
    CODE_REACH__ALWAYS_FAIL = -1
} code_reach_t;

static const char *g_cmdname = "packcc"; /* replaced later with actual one */

static int print_error(const char *format, ...) {
    int n,n_a,n_b;
    va_list a;
    va_start(a, format);
    n_a = fprintf(stderr, "%s: ", g_cmdname);
    n_b = vfprintf(stderr, format, a);
    n = n_a + n_b;
    va_end(a);
    return n;
}

static FILE *fopen_rb_e(const char *path) {
    FILE *f = fopen(path, "rb");
    if (f == NULL) {
        print_error("Cannot open file '%s' to read\n", path);
        exit(2);
    }
    return f;
}

static FILE *fopen_wt_e(const char *path) {
    FILE *f = fopen(path, "wt");
    if (f == NULL) {
        print_error("Cannot open file '%s' to write\n", path);
        exit(2);
    }
    return f;
}

static void *malloc_e(size_t size) {
    /*@out@*/ void *p = malloc(size);
    if (p == NULL) {
        print_error("Out of memory\n");
        exit(3);
    }
    return p;
}

static void *realloc_e(/*@in@*/ void *ptr, size_t size) {
    /*@out@*/ void *p = realloc(ptr, size);
    if (p == NULL) {
        print_error("Out of memory\n");
        exit(3);
    }
    return p;
}

static char *strdup_e(const char *str) {
    size_t m = strlen(str);
    char *s = (char *)malloc_e(m + 1);
    memcpy(s, str, m);
    s[m] = '\0';
    return s;
}

static char *strndup_e(const char *str, size_t len) {
    size_t m = strnlen(str, len);
    char *s = (char *)malloc_e(m + 1);
    memcpy(s, str, m);
    s[m] = '\0';
    return s;
}

static bool is_filled_string(const char *str) {
    size_t i;
    for (i = 0; str[i]; i++) {
        if (
            str[i] != ' '  &&
            str[i] != '\v' &&
            str[i] != '\f' &&
            str[i] != '\t' &&
            str[i] != '\n' &&
            str[i] != '\r'
        ) return true;
    }
    return false;
}

static bool is_identifier_string(const char *str) {
    size_t i;
    if (!(
        (str[0] >= 'a' && str[0] <= 'z') ||
        (str[0] >= 'A' && str[0] <= 'Z') ||
         str[0] == '_'
    )) return false;
    for (i = 1; str[i]; i++) {
        if (!(
            (str[i] >= 'a' && str[i] <= 'z') ||
            (str[i] >= 'A' && str[i] <= 'Z') ||
            (str[i] >= '0' && str[i] <= '9') ||
             str[i] == '_'
        )) return false;
    }
    return true;
}

static bool is_pointer_type(const char *str) {
    size_t n = strlen(str);
    return (n > 0 && str[n - 1] == '*');
}

static bool unescape_string(char *str, size_t *length) {
    if (!str) {
        if (length) *length = 0;
        return true;
    }
    bool b = true;
    size_t i, j = 0;
    for (j = 0, i = 0; str[i]; i++) {
        if (str[i] == '\\') {
            i++;
            switch (str[i]) {
            case '\0': str[j++] = '\\'; str[j] = '\0'; return false;
            case '0': str[j++] = '\x00'; break;
            case 'a': str[j++] = '\x07'; break;
            case 'b': str[j++] = '\x08'; break;
            case 'f': str[j++] = '\x0c'; break;
            case 'n': str[j++] = '\x0a'; break;
            case 'r': str[j++] = '\x0d'; break;
            case 't': str[j++] = '\x09'; break;
            case 'v': str[j++] = '\x0b'; break;
            case 'x':
                if (str[i + 1] == '\0') {
                    str[j++] = '\\'; str[j++] = 'x'; str[j] = '\0'; return false;
                }
                if (str[i + 2] == '\0') {
                    str[j++] = '\\'; str[j++] = 'x'; str[j++] = str[i + 1]; str[j] = '\0'; return false;
                }
                {
                    char c = str[i + 1];
                    char d = str[i + 2];
                    c = (c >= '0' && c <= '9') ? c - '0' :
                        (c >= 'a' && c <= 'f') ? c - 'a' + 10 :
                        (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1;
                    d = (d >= '0' && d <= '9') ? d - '0' :
                        (d >= 'a' && d <= 'f') ? d - 'a' + 10 :
                        (d >= 'A' && d <= 'F') ? d - 'A' + 10 : -1;
                    if (c < 0 || d < 0) {
                        str[j++] = '\\'; str[j++] = 'x'; str[j++] = str[i + 1]; str[j++] = str[i + 2];
                        b = false;
                    }
                    else {
                        str[j++] = (c << 4) | d;
                    }
                    i += 2;
                }
                break;
            case 'u':
                if (str[i + 1] == '\0') {
                    str[j++] = '\\'; str[j++] = 'u'; str[j] = '\0'; return false;
                }
                if (str[i + 2] == '\0') {
                    str[j++] = '\\'; str[j++] = 'u'; str[j++] = str[i + 1]; str[j] = '\0'; return false;
                }
                if (str[i + 3] == '\0') {
                    str[j++] = '\\'; str[j++] = 'u'; str[j++] = str[i + 1]; str[j++] = str[i + 2]; str[j] = '\0'; return false;
                }
                if (str[i + 4] == '\0') {
                    str[j++] = '\\'; str[j++] = 'u'; str[j++] = str[i + 1]; str[j++] = str[i + 2]; str[j++] = str[i + 3]; str[j] = '\0'; return false;
                }
                {
                    char s[4];
                    for (int k=0;k<4;k++) {
                        char c = str[i + k + 1];
                        s[k] = (c >= '0' && c <= '9') ? c - '0' :
                               (c >= 'a' && c <= 'f') ? c - 'a' + 10 :
                               (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1;
                    }
                    if (s[0] < 0 || s[1] < 0 || s[2] < 0 || s[3] < 0) {
                        str[j++] = '\\'; str[j++] = 'u'; str[j++] = str[i + 1]; str[j++] = str[i + 2]; str[j++] = str[i + 3]; str[j++] = str[i + 4];
                        b = false;
                    }
                    else {
                        int ch=s[0];
                        for (int k=1;k<4;k++) ch = (ch << 4) | s[k];
                        if (ch < 0x80) {
                            str[j++] = (char)ch;
                        } else if (ch < 0x800) {
                            str[j++] = (ch >> 6) | 0xC0;
                            str[j++] = (ch & 0x3F) | 0x80;
                        } else if (ch < 0x10000) {
                            str[j++] = (ch >> 12) | 0xE0;
                            str[j++] = ((ch >> 6) & 0x3F) | 0x80;
                            str[j++] = (ch & 0x3F) | 0x80;
                        } else if (ch < 0x110000) {
                            str[j++] = (ch >> 18) | 0xF0;
                            str[j++] = ((ch >> 12) & 0x3F) | 0x80;
                            str[j++] = ((ch >> 6) & 0x3F) | 0x80;
                            str[j++] = (ch & 0x3F) | 0x80;
                        }
                    }
                    i += 4;
                }
                break;
            case '\n': break;
            case '\r': if (str[i + 1] == '\n') i++; break;
            default: str[j++] = str[i];
            }
        }
        else {
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
    if (length) *length = j;
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
            snprintf(*buf, 5, "\\x%02x", (unsigned char)ch);
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
    for (i = 0; i < len; i++) fputc(ch, stream);
}

static void write_text(FILE *stream, const char *ptr, size_t len) {
    size_t i;
    for (i = 0; i < len; i++) {
        if (ptr[i] == '\r') {
            if (i + 1 < len && ptr[i + 1] == '\n') i++;
            fputc('\n', stream);
        }
        else {
            fputc(ptr[i], stream);
        }
    }
}

static void write_code_block(FILE *stream, const char *ptr, size_t len, int indent) {
    size_t i;
    for (i = 0; i < len; i++) {
        if (ptr[i] == '\n') break;
        if (ptr[i] == '\r') {
            if (i + 1 < len && ptr[i + 1] == '\n') i++;
            break;
        }
    }
    if (i < len) {
        bool s = true;
        size_t k = i + 1;
        int l = 0, m = -1;
        for (i = k; i < len; i++) {
            switch (ptr[i]) {
            case ' ':
            case '\v':
            case '\f':
                if (s) l++;
                break;
            case '\t':
                if (s) l = (l + 8) & ~7;
                break;
            case '\n':
                s = true;
                l = 0;
                break;
            case '\r':
                if (i + 1 < len && ptr[i + 1] == '\n') i++;
                s = true;
                l = 0;
                break;
            default:
                s = false;
                m = (m >= 0 && m < l) ? m : l;
            }
        }
        for (i = 0; i < k; i++) {
            if (
                ptr[i] != ' '  &&
                ptr[i] != '\v' &&
                ptr[i] != '\f' &&
                ptr[i] != '\t' &&
                ptr[i] != '\n' &&
                ptr[i] != '\r'
            ) break;
        }
        if (i < k) {
            write_characters(stream, ' ', indent);
            write_text(stream, ptr + i, k - i);
        }
        s = true;
        l = 0;
        for (i = k; i < len; i++) {
            switch (ptr[i]) {
            case ' ':
            case '\v':
            case '\f':
                if (s) l++; else fputc(ptr[i], stream);
                break;
            case '\t':
                if (s) l = (l + 8) & ~7; else fputc(ptr[i], stream);
                break;
            case '\n':
                fputc('\n', stream);
                s = true;
                l = 0;
                break;
            case '\r':
                if (i + 1 < len && ptr[i + 1] == '\n') i++;
                fputc('\n', stream);
                s = true;
                l = 0;
                break;
            default:
                if (s) {
                    write_characters(stream, ' ', l - m + indent);
                    s = false;
                }
                fputc(ptr[i], stream);
            }
        }
        if (!s) fputc('\n', stream);
    }
    else {
        for (i = 0; i < len; i++) {
            if (
                ptr[i] != ' '  &&
                ptr[i] != '\v' &&
                ptr[i] != '\f' &&
                ptr[i] != '\t'
            ) break;
        }
        if (i < len) {
            write_characters(stream, ' ', indent);
            write_text(stream, ptr + i, len - i);
            fputc('\n', stream);
        }
    }
}

static const char *extract_filename(const char *path) {
    int i, n = strlen(path);
    for (i = n - 1; i >= 0; i--) {
        if (path[i] == '/' || path[i] == '\\' || path[i] == ':') break;
    }
    return path + i + 1;
}

static const char *extract_fileext(const char *path) {
    int i, n = strlen(path);
    for (i = n - 1; i >= 0; i--) {
        if (path[i] == '/' || path[i] == '\\' || path[i] == ':') break;
        if (path[i] == '.') return path + i;
    }
    return path + n;
}

static char *replace_fileext(const char *path, const char *ext) {
    const char *p = extract_fileext(path);
    int m = p - path;
    int n = strlen(ext);
    char *s = (char *)malloc_e(m + n + 2);
    memcpy(s, path, m);
    s[m] = '.';
    memcpy(s + m + 1, ext, n + 1);
    return s;
}

static char *add_fileext(const char *path, const char *ext) {
    int m = strlen(path);
    int n = strlen(ext);
    char *s = (char *)malloc_e(m + n + 2);
    memcpy(s, path, m);
    s[m] = '.';
    memcpy(s + m + 1, ext, n + 1);
    return s;
}

static int hash_string(const char *str) {
    int i, h = 0;
    for (i = 0; str[i]; i++) {
        h = h * 31 + str[i];
    }
    return h;
}

static int populate_bits(int x) {
    x |= x >>  1;
    x |= x >>  2;
    x |= x >>  4;
    x |= x >>  8;
    x |= x >> 16;
    return x;
}

static void char_array__init(char_array_t *array, int max) {
    array->len = 0;
    array->max = max;
    array->buf = (char *)malloc_e(array->max);
}

static void char_array__add(char_array_t *array, char ch) {
    if (array->max <= 0) array->max = 1;
    while (array->max <= array->len) array->max <<= 1;
    array->buf = (char *)realloc_e(array->buf, array->max);
    array->buf[array->len++] = ch;
}

static void char_array__term(char_array_t *array) {
    free(array->buf);
}

static void node_array__init(node_array_t *array, int max) {
    array->len = 0;
    array->max = max;
    array->buf = (node_t **)malloc_e(array->max * sizeof(node_t *));
}

static void node_array__add(node_array_t *array, node_t *node) {
    if (array->max <= 0) array->max = 1;
    while (array->max <= array->len) array->max <<= 1;
    array->buf = (node_t **)realloc_e(array->buf, array->max * sizeof(node_t *));
    array->buf[array->len++] = node;
}

static void destroy_node(node_t *node);

static void node_array__term(node_array_t *array) {
    int i;
    for (i = array->len - 1; i >= 0; i--) {
        destroy_node(array->buf[i]);
    }
    free(array->buf);
}

static void node_const_array__init(node_const_array_t *array, int max) {
    array->len = 0;
    array->max = max;
    array->buf = (const node_t **)malloc_e(array->max * sizeof(const node_t *));
}

static void node_const_array__add(node_const_array_t *array, const node_t *node) {
    if (array->max <= 0) array->max = 1;
    while (array->max <= array->len) array->max <<= 1;
    array->buf = (const node_t **)realloc_e((node_t **)array->buf, array->max * sizeof(const node_t *));
    array->buf[array->len++] = node;
}

static void node_const_array__clear(node_const_array_t *array) {
    array->len = 0;
}

static void node_const_array__copy(node_const_array_t *array, const node_const_array_t *src) {
    int i;
    node_const_array__clear(array);
    for (i = 0; i < src->len; i++) {
        node_const_array__add(array, src->buf[i]);
    }
}

static void node_const_array__term(node_const_array_t *array) {
    free((node_t **)array->buf);
}

static context_t *create_context(const char *iname, const char *earlyinclude, const char *oname, bool debug) {
    context_t *ctx = (context_t *)malloc_e(sizeof(context_t));
    ctx->iname = strdup_e((iname && iname[0]) ? iname : "-");
    ctx->sname = (oname && oname[0]) ? add_fileext(oname, "c") : replace_fileext(ctx->iname, "c");
    ctx->hname = (oname && oname[0]) ? add_fileext(oname, "h") : replace_fileext(ctx->iname, "h");
    ctx->ifile = (iname && iname[0]) ? fopen_rb_e(ctx->iname) : stdin;
    ctx->sfile = fopen_wt_e(ctx->sname);
    ctx->hfile = fopen_wt_e(ctx->hname);
    ctx->hid = strdup_e(ctx->hname); make_header_identifier(ctx->hid);
    ctx->vtype = NULL;
    ctx->atype = NULL;
    ctx->prefix = NULL;
    ctx->debug = debug;
    ctx->errnum = 0;
    ctx->linenum = 0;
    ctx->linepos = 0;
    ctx->bufpos = 0;
    char_array__init(&ctx->buffer, BUFFER_INIT_SIZE);
    node_array__init(&ctx->rules, ARRAY_INIT_SIZE);
    ctx->rulehash.mod = 0;
    ctx->rulehash.max = 0;
    ctx->rulehash.buf = NULL;
    ctx->earlyinclude = earlyinclude? strdup_e(earlyinclude): NULL;
    return ctx;
}

static node_t *create_node(node_type_t type) {
    node_t *node = (node_t *)malloc_e(sizeof(node_t));
    node->type = type;
    switch (node->type) {
    case NODE_RULE:
        node->data.rule.name = NULL;
        node->data.rule.expr = NULL;
        node->data.rule.ref = 0;
        node_const_array__init(&node->data.rule.vars, ARRAY_INIT_SIZE);
        node_const_array__init(&node->data.rule.capts, ARRAY_INIT_SIZE);
        node_const_array__init(&node->data.rule.codes, ARRAY_INIT_SIZE);
        node->data.rule.line = -1;
        node->data.rule.col = -1;
        break;
    case NODE_REFERENCE:
        node->data.reference.var = NULL;
        node->data.reference.index = -1;
        node->data.reference.name = NULL;
        node->data.reference.rule = NULL;
        node->data.reference.line = -1;
        node->data.reference.col = -1;
        break;
    case NODE_STRING:
        node->data.string.value = NULL;
        node->data.string.len = 0;
        break;
    case NODE_CHARCLASS:
        node->data.charclass.value = NULL;
        node->data.charclass.len = 0;
        break;
    case NODE_QUANTITY:
        node->data.quantity.min = node->data.quantity.max = 0;
        node->data.quantity.expr = NULL;
        break;
    case NODE_PREDICATE:
        node->data.predicate.neg = false;
        node->data.predicate.expr = NULL;
        break;
    case NODE_SEQUENCE:
        node_array__init(&node->data.sequence.nodes, ARRAY_INIT_SIZE);
        break;
    case NODE_ALTERNATE:
        node_array__init(&node->data.alternate.nodes, ARRAY_INIT_SIZE);
        break;
    case NODE_CAPTURE:
        node->data.capture.expr = NULL;
        node->data.capture.index = -1;
        break;
    case NODE_EXPAND:
        node->data.expand.index = -1;
        node->data.expand.line = -1;
        node->data.expand.col = -1;
        break;
    case NODE_ACTION:
        node->data.action.value = NULL;
        node->data.action.index = -1;
        node_const_array__init(&node->data.action.vars, ARRAY_INIT_SIZE);
        node_const_array__init(&node->data.action.capts, ARRAY_INIT_SIZE);
        break;
    case NODE_ERROR:
        node->data.error.expr = NULL;
        node->data.error.value = NULL;
        node->data.error.index = -1;
        node_const_array__init(&node->data.error.vars, ARRAY_INIT_SIZE);
        node_const_array__init(&node->data.error.capts, ARRAY_INIT_SIZE);
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
    free((node_t **)ctx->rulehash.buf);
    node_array__term(&ctx->rules);
    char_array__term(&ctx->buffer);
    free(ctx->prefix);
    free(ctx->atype);
    free(ctx->vtype);
    free(ctx->hid);
    fclose(ctx->hfile); if (ctx->errnum) unlink(ctx->hname);
    fclose(ctx->sfile); if (ctx->errnum) unlink(ctx->sname);
    fclose(ctx->ifile);
    free(ctx->hname);
    free(ctx->sname);
    free(ctx->iname);
    free(ctx->earlyinclude);
    free(ctx);
}

static void make_rulehash(context_t *ctx) {
    int i, j;
    ctx->rulehash.mod = populate_bits(ctx->rules.len * 4);
    ctx->rulehash.max = ctx->rulehash.mod + 1;
    ctx->rulehash.buf = (const node_t **)realloc_e((node_t **)ctx->rulehash.buf, ctx->rulehash.max * sizeof(const node_t *));
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
    int j = hash_string(name) & ctx->rulehash.mod;
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
            print_error("%s:%d:%d: No definition of rule '%s'\n",
                ctx->iname, node->data.reference.line + 1, node->data.reference.col + 1, node->data.reference.name);
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
            int i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                link_references(ctx, node->data.sequence.nodes.buf[i]);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            int i;
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
    bool b = (vars == NULL);
    if (node == NULL) return;
    if (b) {
        node_const_array__init(&a, ARRAY_INIT_SIZE);
        vars = &a;
    }
    switch (node->type) {
    case NODE_RULE:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    case NODE_REFERENCE:
        if (node->data.reference.index >= 0) {
            int i;
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
            int i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                verify_variables(ctx, node->data.sequence.nodes.buf[i], vars);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            int i, j, k, m = vars->len;
            node_const_array_t v;
            node_const_array__init(&v, ARRAY_INIT_SIZE);
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
    bool b = (capts == NULL);
    if (node == NULL) return;
    if (b) {
        node_const_array__init(&a, ARRAY_INIT_SIZE);
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
            int i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                verify_captures(ctx, node->data.sequence.nodes.buf[i], capts);
            }
        }
        break;
    case NODE_ALTERNATE:
        {
            int i, j, m = capts->len;
            node_const_array_t v;
            node_const_array__init(&v, ARRAY_INIT_SIZE);
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
            int i;
            for (i = 0; i < capts->len; i++) {
                assert(capts->buf[i]->type == NODE_CAPTURE);
                if (node->data.expand.index == capts->buf[i]->data.capture.index) break;
            }
            if (i >= capts->len && node->data.expand.index >= 0) {
                print_error("%s:%d:%d: Capture %d not available at this position\n",
                    ctx->iname, node->data.expand.line + 1, node->data.expand.col + 1, node->data.expand.index + 1);
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

static void dump_node(context_t *ctx, const node_t *node) {
    if (node == NULL) return;
    switch (node->type) {
    case NODE_RULE:
        fprintf(stdout, "Rule(name:'%s',ref:%d,vars.len:%d,capts.len:%d,codes.len:%d){\n",
            node->data.rule.name, node->data.rule.ref, node->data.rule.vars.len, node->data.rule.capts.len, node->data.rule.codes.len);
        dump_node(ctx, node->data.rule.expr);
        fprintf(stdout, "}\n");
        break;
    case NODE_REFERENCE:
        fprintf(stdout, "Reference(var:'%s',index:%d,name:'%s',rule:'%s')\n",
            node->data.reference.var, node->data.reference.index, node->data.reference.name,
            (node->data.reference.rule) ? node->data.reference.rule->data.rule.name : NULL);
        break;
    case NODE_STRING:
        fprintf(stdout, "String(value:'%s')\n", node->data.string.value);
        break;
    case NODE_CHARCLASS:
        fprintf(stdout, "Charclass(value:'%s')\n", node->data.charclass.value);
        break;
    case NODE_QUANTITY:
        fprintf(stdout, "Quantity(min:%d,max%d){\n", node->data.quantity.min, node->data.quantity.max);
        dump_node(ctx, node->data.quantity.expr);
        fprintf(stdout, "}\n");
        break;
    case NODE_PREDICATE:
        fprintf(stdout, "Predicate(neg:%d){\n", node->data.predicate.neg);
        dump_node(ctx, node->data.predicate.expr);
        fprintf(stdout, "}\n");
        break;
    case NODE_SEQUENCE:
        fprintf(stdout, "Sequence(max:%d,len:%d){\n", node->data.sequence.nodes.max, node->data.sequence.nodes.len);
        {
            int i;
            for (i = 0; i < node->data.sequence.nodes.len; i++) {
                dump_node(ctx, node->data.sequence.nodes.buf[i]);
            }
        }
        fprintf(stdout, "}\n");
        break;
    case NODE_ALTERNATE:
        fprintf(stdout, "Alternate(max:%d,len:%d){\n", node->data.alternate.nodes.max, node->data.alternate.nodes.len);
        {
            int i;
            for (i = 0; i < node->data.alternate.nodes.len; i++) {
                dump_node(ctx, node->data.alternate.nodes.buf[i]);
            }
        }
        fprintf(stdout, "}\n");
        break;
    case NODE_CAPTURE:
        fprintf(stdout, "Capture(index:%d){\n", node->data.capture.index);
        dump_node(ctx, node->data.capture.expr);
        fprintf(stdout, "}\n");
        break;
    case NODE_EXPAND:
        fprintf(stdout, "Expand(index:%d)\n", node->data.expand.index);
        break;
    case NODE_ACTION:
        fprintf(stdout, "Action(index:%d,value:{%s},vars:\n", node->data.action.index, node->data.action.value);
        {
            int i;
            for (i = 0; i < node->data.action.vars.len; i++) {
                fprintf(stdout, "    '%s'\n", node->data.action.vars.buf[i]->data.reference.var);
            }
            for (i = 0; i < node->data.action.capts.len; i++) {
                fprintf(stdout, "    $%d\n", node->data.action.capts.buf[i]->data.capture.index + 1);
            }
        }
        fprintf(stdout, ")\n");
        break;
    case NODE_ERROR:
        fprintf(stdout, "Error(index:%d,value:{%s},vars:\n", node->data.error.index, node->data.error.value);
        {
            int i;
            for (i = 0; i < node->data.error.vars.len; i++) {
                fprintf(stdout, "    '%s'\n", node->data.error.vars.buf[i]->data.reference.var);
            }
            for (i = 0; i < node->data.error.capts.len; i++) {
                fprintf(stdout, "    $%d\n", node->data.error.capts.buf[i]->data.capture.index + 1);
            }
        }
        fprintf(stdout, "){\n");
        dump_node(ctx, node->data.error.expr);
        fprintf(stdout, "}\n");
        break;
    default:
        print_error("Internal error [%d]\n", __LINE__);
        exit(-1);
    }
}

static int refill_buffer(context_t *ctx, int num) {
    int n, c;
    n = ctx->buffer.len - ctx->bufpos;
    if (n >= num) return n;
    while (ctx->buffer.len < ctx->bufpos + num) {
        c = fgetc(ctx->ifile);
        if (c == EOF) break;
        char_array__add(&ctx->buffer, (char)c);
    }
    return ctx->buffer.len - ctx->bufpos;
}

static void commit_buffer(context_t *ctx) {
    ctx->linepos -= ctx->bufpos;
    memmove(ctx->buffer.buf, ctx->buffer.buf + ctx->bufpos, ctx->buffer.len - ctx->bufpos);
    ctx->buffer.len -= ctx->bufpos;
    ctx->bufpos = 0;
}

static bool match_eof(context_t *ctx) {
    return refill_buffer(ctx, 1) < 1;
}

static bool match_eol(context_t *ctx) {
    if (refill_buffer(ctx, 1) >= 1) {
        switch (ctx->buffer.buf[ctx->bufpos]) {
        case '\n':
            ctx->bufpos++;
            ctx->linenum++;
            ctx->linepos = ctx->bufpos;
            return true;
        case '\r':
            ctx->bufpos++;
            if (refill_buffer(ctx, 1) >= 1) {
                if (ctx->buffer.buf[ctx->bufpos] == '\n') ctx->bufpos++;
            }
            ctx->linenum++;
            ctx->linepos = ctx->bufpos;
            return true;
        }
    }
    return false;
}

static bool match_character(context_t *ctx, char ch) {
    if (refill_buffer(ctx, 1) >= 1) {
        if (ctx->buffer.buf[ctx->bufpos] == ch) {
            ctx->bufpos++;
            return true;
        }
    }
    return false;
}

static bool match_character_range(context_t *ctx, char min, char max) {
    if (refill_buffer(ctx, 1) >= 1) {
        char c = ctx->buffer.buf[ctx->bufpos];
        if (c >= min && c <= max) {
            ctx->bufpos++;
            return true;
        }
    }
    return false;
}

static bool match_character_set(context_t *ctx, const char *chs) {
    if (refill_buffer(ctx, 1) >= 1) {
        char c = ctx->buffer.buf[ctx->bufpos];
        int i;
        for (i = 0; chs[i]; i++) {
            if (c == chs[i]) {
                ctx->bufpos++;
                return true;
            }
        }
    }
    return false;
}

static bool match_character_any(context_t *ctx) {
    if (refill_buffer(ctx, 1) >= 1) {
        ctx->bufpos++;
        return true;
    }
    return false;
}

static bool match_string(context_t *ctx, const char *str) {
    int n = strlen(str);
    if (refill_buffer(ctx, n) >= n) {
        if (strncmp(ctx->buffer.buf + ctx->bufpos, str, n) == 0) {
            ctx->bufpos += n;
            return true;
        }
    }
    return false;
}

static bool match_blank(context_t *ctx) {
    return match_character_set(ctx, " \t\v\f");
}

static bool match_section_line_(context_t *ctx, const char *head) {
    if (match_string(ctx, head)) {
        while (!match_eol(ctx) && !match_eof(ctx)) match_character_any(ctx);
        return true;
    }
    return false;
}

static bool match_section_line_continuable_(context_t *ctx, const char *head) {
    if (match_string(ctx, head)) {
        while (!match_eof(ctx)) {
            int p = ctx->bufpos;
            if (match_eol(ctx)) {
                if (ctx->buffer.buf[p - 1] != '\\') break;
            }
            else {
                match_character_any(ctx);
            }
        }
        return true;
    }
    return false;
}

static bool match_section_block_(context_t *ctx, const char *left, const char *right, const char *name) {
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    if (match_string(ctx, left)) {
        while (!match_string(ctx, right)) {
            if (match_eof(ctx)) {
                print_error("%s:%d:%d: Premature EOF in %s\n", ctx->iname, l + 1, m + 1, name);
                ctx->errnum++;
                break;
            }
            if (!match_eol(ctx)) match_character_any(ctx);
        }
        return true;
    }
    return false;
}

static bool match_quotation_(context_t *ctx, const char *left, const char *right, const char *name) {
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    if (match_string(ctx, left)) {
        while (!match_string(ctx, right)) {
            if (match_eof(ctx)) {
                print_error("%s:%d:%d: Premature EOF in %s\n", ctx->iname, l + 1, m + 1, name);
                ctx->errnum++;
                break;
            }
            if (match_character(ctx, '\\')) {
                if (!match_eol(ctx)) match_character_any(ctx);
            }
            else {
                if (match_eol(ctx)) {
                    print_error("%s:%d:%d: Premature EOL in %s\n", ctx->iname, l + 1, m + 1, name);
                    ctx->errnum++;
                    break;
                }
                match_character_any(ctx);
            }
        }
        return true;
    }
    return false;
}

static bool match_directive_c(context_t *ctx) {
    return match_section_line_continuable_(ctx, "#");
}

static bool match_comment(context_t *ctx) {
    return match_section_line_(ctx, "#");
}

static bool match_comment_c(context_t *ctx) {
    return match_section_block_(ctx, "/*", "*/", "C comment");
}

static bool match_comment_cxx(context_t *ctx) {
    return match_section_line_(ctx, "//");
}

static bool match_quotation_single(context_t *ctx) {
    return match_quotation_(ctx, "\'", "\'", "single quotation");
}

static bool match_quotation_double(context_t *ctx) {
    return match_quotation_(ctx, "\"", "\"", "double quotation");
}

static bool match_character_class(context_t *ctx) {
    return match_quotation_(ctx, "[", "]", "character class");
}

static bool match_spaces(context_t *ctx) {
    int n = 0;
    while (match_blank(ctx) || match_eol(ctx) || match_comment(ctx)) n++;
    return (n > 0);
}

static bool match_number(context_t *ctx) {
    if (match_character_range(ctx, '0', '9')) {
        while (match_character_range(ctx, '0', '9'));
        return true;
    }
    return false;
}

static bool match_identifier(context_t *ctx) {
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
        return true;
    }
    return false;
}

static bool match_code_block(context_t *ctx) {
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    if (match_character(ctx, '{')) {
        int d = 1;
        for (;;) {
            if (match_eof(ctx)) {
                print_error("%s:%d:%d: Premature EOF in code block\n", ctx->iname, l + 1, m + 1);
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
                        ctx->buffer.buf[ctx->bufpos - 1] = '_';
                    }
                    else {
                        match_character_any(ctx);
                    }
                }
            }
        }
        return true;
    }
    return false;
}

static bool match_footer_start(context_t *ctx) {
    return match_string(ctx, "%%");
}

static node_t *parse_expression(context_t *ctx, node_t *rule);

static node_t *parse_primary(context_t *ctx, node_t *rule) {
    int p = ctx->bufpos;
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    node_t *n_p = NULL;
    if (match_identifier(ctx)) {
        int q = ctx->bufpos, r = -1, s = -1;
        match_spaces(ctx);
        if (match_character(ctx, ':')) {
            match_spaces(ctx);
            r = ctx->bufpos;
            if (!match_identifier(ctx)) goto EXCEPTION;
            s = ctx->bufpos;
            match_spaces(ctx);
        }
        if (match_string(ctx, "<-")) goto EXCEPTION;
        n_p = create_node(NODE_REFERENCE);
        if (r < 0) {
            n_p->data.reference.var = NULL;
            n_p->data.reference.index = -1;
            n_p->data.reference.name = strndup_e(ctx->buffer.buf + p, q - p);
        }
        else {
            n_p->data.reference.var = strndup_e(ctx->buffer.buf + p, q - p);
            {
                int i;
                for (i = 0; i < rule->data.rule.vars.len; i++) {
                    assert(rule->data.rule.vars.buf[i]->type == NODE_REFERENCE);
                    if (strcmp(n_p->data.reference.var, rule->data.rule.vars.buf[i]->data.reference.var) == 0) break;
                }
                if (i == rule->data.rule.vars.len) node_const_array__add(&rule->data.rule.vars, n_p);
                n_p->data.reference.index = i;
            }
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
        int p;
        match_spaces(ctx);
        p = ctx->bufpos;
        if (match_number(ctx)) {
            int q = ctx->bufpos;
            char *s;
            match_spaces(ctx);
            n_p = create_node(NODE_EXPAND);
            s = strndup_e(ctx->buffer.buf + p, q - p);
            n_p->data.expand.index = atoi(s);
            if (n_p->data.expand.index == 0) {
                print_error("%s:%d:%d: 0 not allowed\n", ctx->iname, l + 1, m + 1);
                ctx->errnum++;
            }
            else if (s[0] == '0') {
                print_error("%s:%d:%d: 0-prefixed number not allowed\n", ctx->iname, l + 1, m + 1);
                ctx->errnum++;
            }
            free(s);
            n_p->data.expand.index--;
            n_p->data.expand.line = l;
            n_p->data.expand.col = m;
        }
        else {
            goto EXCEPTION;
        }
    }
    else if (match_character(ctx, '.')) {
        match_spaces(ctx);
        n_p = create_node(NODE_CHARCLASS);
        n_p->data.charclass.value = NULL;
        n_p->data.charclass.len = 0;
    }
    else if (match_character_class(ctx)) {
        int q = ctx->bufpos;
        match_spaces(ctx);
        n_p = create_node(NODE_CHARCLASS);
        n_p->data.charclass.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
        if (!unescape_string(n_p->data.charclass.value, &n_p->data.charclass.len)) {
            print_error("%s:%d:%d: Illegal escape sequence\n", ctx->iname, l + 1, m + 1);
            ctx->errnum++;
        }
    }
    else if (match_quotation_single(ctx) || match_quotation_double(ctx)) {
        int q = ctx->bufpos;
        match_spaces(ctx);
        n_p = create_node(NODE_STRING);
        n_p->data.string.value = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
        if (!unescape_string(n_p->data.string.value, &n_p->data.string.len)) {
            print_error("%s:%d:%d: Illegal escape sequence\n", ctx->iname, l + 1, m + 1);
            ctx->errnum++;
        }
    }
    else if (match_code_block(ctx)) {
        int q = ctx->bufpos;
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
    ctx->bufpos = p;
    ctx->linenum = l;
    ctx->linepos = p - m;
    return NULL;
}

static node_t *parse_term(context_t *ctx, node_t *rule) {
    int p = ctx->bufpos;
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    node_t *n_p = NULL;
    node_t *n_q = NULL;
    node_t *n_r = NULL;
    node_t *n_t = NULL;
    char t = match_character(ctx, '&') ? '&' : match_character(ctx, '!') ? '!' : '\0';
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
        n_r->data.predicate.neg = false;
        n_r->data.predicate.expr = n_q;
        break;
    case '!':
        n_r = create_node(NODE_PREDICATE);
        n_r->data.predicate.neg = true;
        n_r->data.predicate.expr = n_q;
        break;
    default:
        n_r = n_q;
    }
    if (match_character(ctx, '~')) {
        int p;
        match_spaces(ctx);
        p = ctx->bufpos;
        if (match_code_block(ctx)) {
            int q = ctx->bufpos;
            match_spaces(ctx);
            n_t = create_node(NODE_ERROR);
            n_t->data.error.expr = n_r;
            n_t->data.error.value = strndup_e(ctx->buffer.buf + p + 1, q - p -2);
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
    ctx->bufpos = p;
    ctx->linenum = l;
    ctx->linepos = p - m;
    return NULL;
}

static node_t *parse_sequence(context_t *ctx, node_t *rule) {
    int p = ctx->bufpos;
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
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
    ctx->bufpos = p;
    ctx->linenum = l;
    ctx->linepos = p - m;
    return NULL;
}

static node_t *parse_expression(context_t *ctx, node_t *rule) {
    int p = ctx->bufpos, q;
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    node_array_t *a_s = NULL;
    node_t *n_s = NULL;
    node_t *n_e = NULL;
    n_s = parse_sequence(ctx, rule);
    if (n_s == NULL) goto EXCEPTION;
    q = ctx->bufpos;
    if (match_character(ctx, '/')) {
        ctx->bufpos = q;
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
    ctx->bufpos = p;
    ctx->linenum = l;
    ctx->linepos = p - m;
    return NULL;
}

static node_t *parse_rule(context_t *ctx) {
    int p = ctx->bufpos, q;
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    node_t *n_r = NULL;
    if (!match_identifier(ctx)) goto EXCEPTION;
    q = ctx->bufpos;
    match_spaces(ctx);
    if (!match_string(ctx, "<-")) goto EXCEPTION;
    match_spaces(ctx);
    n_r = create_node(NODE_RULE);
    n_r->data.rule.expr = parse_expression(ctx, n_r);
    if (n_r->data.rule.expr == NULL) goto EXCEPTION;
    n_r->data.rule.name = strndup_e(ctx->buffer.buf + p, q - p);
    n_r->data.rule.line = l;
    n_r->data.rule.col = m;
    return n_r;

EXCEPTION:;
    destroy_node(n_r);
    ctx->bufpos = p;
    ctx->linenum = l;
    ctx->linepos = p - m;
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
    fprintf(stdout, "value_type:'%s'\n", get_value_type(ctx));
    fprintf(stdout, "auxil_type:'%s'\n", get_auxil_type(ctx));
    fprintf(stdout, "prefix:'%s'\n", get_prefix(ctx));
}

static bool parse_directive_include_(context_t *ctx, const char *name, FILE *output1, FILE *output2) {
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    if (!match_string(ctx, name)) return false;
    match_spaces(ctx);
    {
        int p = ctx->bufpos;
        if (match_code_block(ctx)) {
            int q = ctx->bufpos;
            match_spaces(ctx);
            if (output1 != NULL) {
                write_code_block(output1, ctx->buffer.buf + p + 1, q - p - 2, 0);
                fputc('\n', output1);
            }
            if (output2 != NULL) {
                write_code_block(output2, ctx->buffer.buf + p + 1, q - p - 2, 0);
                fputc('\n', output2);
            }
        }
        else {
            print_error("%s:%d:%d: Illegal %s syntax\n", ctx->iname, l + 1, m + 1, name);
            ctx->errnum++;
        }
    }
    return true;
}

static bool parse_directive_string_(context_t *ctx, const char *name, char **output, string_flag_t mode) {
    int l = ctx->linenum;
    int m = ctx->bufpos - ctx->linepos;
    if (!match_string(ctx, name)) return false;
    match_spaces(ctx);
    {
        char *s = NULL;
        int p = ctx->bufpos, q;
        int lv = ctx->linenum;
        int mv = ctx->bufpos - ctx->linepos;
        if (match_quotation_single(ctx) || match_quotation_double(ctx)) {
            q = ctx->bufpos;
            match_spaces(ctx);
            s = strndup_e(ctx->buffer.buf + p + 1, q - p - 2);
            if (!unescape_string(s, NULL)) {
                print_error("%s:%d:%d: Illegal escape sequence\n", ctx->iname, lv + 1, mv + 1);
                ctx->errnum++;
            }
        }
        else {
            print_error("%s:%d:%d: Illegal %s syntax\n", ctx->iname, l + 1, m + 1, name);
            ctx->errnum++;
        }
        if (s != NULL) {
            string_flag_t f = STRING_FLAG__NONE;
            bool b = true;
            remove_heading_blank(s);
            remove_trailing_blank(s);
            assert((mode & ~7) == 0);
            if ((mode & STRING_FLAG__NOTEMPTY) && !is_filled_string(s)) {
                print_error("%s:%d:%d: Empty string\n", ctx->iname, lv + 1, mv + 1);
                ctx->errnum++;
                f |= STRING_FLAG__NOTEMPTY;
            }
            if ((mode & STRING_FLAG__NOTVOID) && strcmp(s, "void") == 0) {
                print_error("%s:%d:%d: 'void' not allowed\n", ctx->iname, lv + 1, mv + 1);
                ctx->errnum++;
                f |= STRING_FLAG__NOTVOID;
            }
            if ((mode & STRING_FLAG__IDENTIFIER) && !is_identifier_string(s)) {
                if (!(f & STRING_FLAG__NOTEMPTY)) {
                    print_error("%s:%d:%d: Invalid identifier\n", ctx->iname, lv + 1, mv + 1);
                    ctx->errnum++;
                }
                f |= STRING_FLAG__IDENTIFIER;
            }
            if (*output != NULL) {
                print_error("%s:%d:%d: Multiple %s definition\n", ctx->iname, l + 1, m + 1, name);
                ctx->errnum++;
                b = false;
            }
            if (f == STRING_FLAG__NONE && b) {
                *output = s;
            }
            else {
                free(s); s = NULL;
            }
        }
    }
    return true;
}

static bool parse(context_t *ctx) {
    fprintf(ctx->sfile, "/* A packrat parser generated by PackCC %s */\n\n", VERSION);
    fprintf(ctx->hfile, "/* A packrat parser generated by PackCC %s */\n\n", VERSION);
    if (ctx->earlyinclude) {
        fputs("#include ", ctx->sfile);
        fputs(ctx->earlyinclude, ctx->sfile);
        fputc('\n', ctx->sfile);
    }
    {
        fputs(
            "#ifdef _MSC_VER\n"
            "#define _CRT_SECURE_NO_WARNINGS\n"
            "#endif /* _MSC_VER */\n"
            "#include <stdio.h>\n"
            "#include <stdlib.h>\n"
            "#include <string.h>\n"
            "#include <stdbool.h>\n"
            "\n"
            "#ifndef _MSC_VER\n"
            "#if ((defined USE_SYSTEM_STRNLEN) == 0) && defined __GNUC__ && defined _WIN32 /* MinGW */\n"
            "static size_t strnlen(const char *str, size_t maxlen) {\n"
            "    size_t i;\n"
            "    for (i = 0; i < maxlen && str[i]; i++);\n"
            "    return i;\n"
            "}\n"
            "#else\n"
            "#include <unistd.h> /* for strnlen() */\n"
            "#endif /* defined __GNUC__ && defined _WIN32 */ \n"
            "#endif /* _MSC_VER */\n"
            "\n",
            ctx->sfile
        );
        fprintf(
            ctx->sfile,
            "#include \"%s\"\n"
            "\n",
            ctx->hname
        );
    }
    {
        fprintf(
            ctx->hfile,
            "#ifndef PCC_INCLUDED__%s\n"
            "#define PCC_INCLUDED__%s\n"
            "\n",
            ctx->hid, ctx->hid
        );
    }
    {
        bool b = true;
        match_spaces(ctx);
        for (;;) {
            int p, l, m;
            if (match_eof(ctx) || match_footer_start(ctx)) break;
            p = ctx->bufpos;
            l = ctx->linenum;
            m = ctx->bufpos - ctx->linepos;
            if (
                parse_directive_include_(ctx, "%source", ctx->sfile, NULL) ||
                parse_directive_include_(ctx, "%header", ctx->hfile, NULL) ||
                parse_directive_include_(ctx, "%common", ctx->sfile, ctx->hfile) ||
                parse_directive_string_(ctx, "%value", &ctx->vtype, STRING_FLAG__NOTEMPTY | STRING_FLAG__NOTVOID) ||
                parse_directive_string_(ctx, "%auxil", &ctx->atype, STRING_FLAG__NOTEMPTY | STRING_FLAG__NOTVOID) ||
                parse_directive_string_(ctx, "%prefix", &ctx->prefix, STRING_FLAG__NOTEMPTY | STRING_FLAG__IDENTIFIER)
            ) {
                b = true;
            }
            else if (match_character(ctx, '%')) {
                print_error("%s:%d:%d: Invalid directive\n", ctx->iname, l + 1, m + 1);
                ctx->errnum++;
                match_identifier(ctx);
                match_spaces(ctx);
                b = true;
            }
            else {
                node_t *n_r = parse_rule(ctx);
                if (n_r == NULL) {
                    if (b) {
                        print_error("%s:%d:%d: Illegal rule syntax\n", ctx->iname, l + 1, m + 1);
                        ctx->errnum++;
                        b = false;
                    }
                    ctx->linenum = l;
                    ctx->linepos = p - m;
                    if (!match_identifier(ctx) && !match_spaces(ctx)) match_character_any(ctx);
                    continue;
                }
                node_array__add(&ctx->rules, n_r);
                b = true;
            }
            commit_buffer(ctx);
        }
        commit_buffer(ctx);
    }
    {
        int i;
        make_rulehash(ctx);
        for (i = 0; i < ctx->rules.len; i++) {
            link_references(ctx, ctx->rules.buf[i]->data.rule.expr);
        }
        for (i = 1; i < ctx->rules.len; i++) {
            if (ctx->rules.buf[i]->data.rule.ref == 0) {
                print_error("%s:%d:%d: Never used rule '%s'\n",
                    ctx->iname, ctx->rules.buf[i]->data.rule.line + 1, ctx->rules.buf[i]->data.rule.col + 1, ctx->rules.buf[i]->data.rule.name);
                ctx->errnum++;
            }
            else if (ctx->rules.buf[i]->data.rule.ref < 0) {
                print_error("%s:%d:%d: Multiple definition of rule '%s'\n",
                    ctx->iname, ctx->rules.buf[i]->data.rule.line + 1, ctx->rules.buf[i]->data.rule.col + 1, ctx->rules.buf[i]->data.rule.name);
                ctx->errnum++;
            }
        }
    }
    {
        int i;
        for (i = 0; i < ctx->rules.len; i++) {
            verify_variables(ctx, ctx->rules.buf[i]->data.rule.expr, NULL);
            verify_captures(ctx, ctx->rules.buf[i]->data.rule.expr, NULL);
        }
    }
    if (ctx->debug) {
        int i;
        for (i = 0; i < ctx->rules.len; i++) {
            dump_node(ctx, ctx->rules.buf[i]);
        }
        dump_options(ctx);
    }
    return (ctx->errnum == 0);
}

static code_reach_t generate_matching_string_code(generate_t *gen, const char *value, size_t n, int onfail, int indent, bool bare) {
    if (n > 0) {
        char s[5];
        if (n > 1) {
            size_t i;
            if (!bare) {
                write_characters(gen->stream, ' ', indent);
                fputs("{\n", gen->stream);
                indent += 4;
            }
            //write_characters(gen->stream, ' ', indent);
            //fputs("const char *s = ctx->buffer.buf + ctx->pos;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fputs("if (\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fprintf(gen->stream, "pcc_refill_buffer(ctx, %" PRIuPTR ") < %" PRIuPTR " ||\n", n, n);
            for (i = 0; i < n - 1; i++) {
                write_characters(gen->stream, ' ', indent + 4);
                fprintf(gen->stream, "((const char *)(ctx->buffer.buf + ctx->pos))[%" PRIuPTR "] != '%s' ||\n", i, escape_character(value[i], &s));
            }
            write_characters(gen->stream, ' ', indent + 4);
            fprintf(gen->stream, "((const char *)(ctx->buffer.buf + ctx->pos))[%" PRIuPTR "] != '%s'\n", i, escape_character(value[i], &s));
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, ") goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "ctx->pos += %" PRIuPTR ";\n", n);
            if (!bare) {
                indent -= 4;
                write_characters(gen->stream, ' ', indent);
                fputs("}\n", gen->stream);
            }
            return CODE_REACH__BOTH;
        }
        else {
            write_characters(gen->stream, ' ', indent);
            fputs("if (\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fputs("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fprintf(gen->stream, "ctx->buffer.buf[ctx->pos] != '%s'\n", escape_character(value[0], &s));
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, ") goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fputs("ctx->pos++;\n", gen->stream);
            return CODE_REACH__BOTH;
        }
    }
    else {
        /* no code to generate */
        return CODE_REACH__ALWAYS_SUCCEED;
    }
}

static code_reach_t generate_matching_charclass_code(generate_t *gen, const char *value, size_t n, int onfail, int indent, bool bare) {
    if (value != NULL) {
        //size_t n = strlen(value);
        if (n > 0) {
            char s[5], t[5];
            if (n > 1) {
                bool a = (value[0] == '^');
                size_t i = a ? 1 : 0;
                if (i + 1 == n) { /* fulfilled only if a == true */
                    write_characters(gen->stream, ' ', indent);
                    fputs("if (\n", gen->stream);
                    write_characters(gen->stream, ' ', indent + 4);
                    fputs("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
                    write_characters(gen->stream, ' ', indent + 4);
                    fprintf(gen->stream, "ctx->buffer.buf[ctx->pos] == '%s'\n", escape_character(value[i], &s));
                    write_characters(gen->stream, ' ', indent);
                    fprintf(gen->stream, ") goto L%04d;\n", onfail);
                    write_characters(gen->stream, ' ', indent);
                    fputs("ctx->pos++;\n", gen->stream);
                    return CODE_REACH__BOTH;
                }
                else {
                    if (!bare) {
                        write_characters(gen->stream, ' ', indent);
                        fputs("{\n", gen->stream);
                        indent += 4;
                    }
                    write_characters(gen->stream, ' ', indent);
                    fputs("char c;\n", gen->stream);
                    write_characters(gen->stream, ' ', indent);
                    fprintf(gen->stream, "if (pcc_refill_buffer(ctx, 1) < 1) goto L%04d;\n", onfail);
                    write_characters(gen->stream, ' ', indent);
                    fputs("c = ctx->buffer.buf[ctx->pos];\n", gen->stream);
                    if (i + 3 == n && value[i + 1] == '-') {
                        write_characters(gen->stream, ' ', indent);
                        fprintf(gen->stream,
                            a ? "if (c >= '%s' && c <= '%s') goto L%04d;\n"
                              : "if (!(c >= '%s' && c <= '%s')) goto L%04d;\n",
                            escape_character(value[i], &s), escape_character(value[i + 2], &t), onfail);
                    }
                    else {
                        write_characters(gen->stream, ' ', indent);
                        fputs(a ? "if (\n" : "if (!(\n", gen->stream);
                        for (; i < n; i++) {
                            write_characters(gen->stream, ' ', indent + 4);
                            if (i + 2 < n && value[i + 1] == '-') {
                                fprintf(gen->stream, "(c >= '%s' && c <= '%s')%s\n",
                                    escape_character(value[i], &s), escape_character(value[i + 2], &t), (i + 3 == n) ? "" : " ||");
                                i += 2;
                            }
                            else {
                                fprintf(gen->stream, "c == '%s'%s\n",
                                    escape_character(value[i], &s), (i + 1 == n) ? "" : " ||");
                            }
                        }
                        write_characters(gen->stream, ' ', indent);
                        fprintf(gen->stream, a ? ") goto L%04d;\n" : ")) goto L%04d;\n", onfail);
                    }
                    write_characters(gen->stream, ' ', indent);
                    fputs("ctx->pos++;\n", gen->stream);
                    if (!bare) {
                        indent -= 4;
                        write_characters(gen->stream, ' ', indent);
                        fputs("}\n", gen->stream);
                    }
                    return CODE_REACH__BOTH;
                }
            }
            else {
                write_characters(gen->stream, ' ', indent);
                fputs("if (\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fputs("pcc_refill_buffer(ctx, 1) < 1 ||\n", gen->stream);
                write_characters(gen->stream, ' ', indent + 4);
                fprintf(gen->stream, "ctx->buffer.buf[ctx->pos] != '%s'\n", escape_character(value[0], &s));
                write_characters(gen->stream, ' ', indent);
                fprintf(gen->stream, ") goto L%04d;\n", onfail);
                write_characters(gen->stream, ' ', indent);
                fputs("ctx->pos++;\n", gen->stream);
                return CODE_REACH__BOTH;
            }
        }
        else {
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", onfail);
            return CODE_REACH__ALWAYS_FAIL;
        }
    }
    else {
        write_characters(gen->stream, ' ', indent);
        fprintf(gen->stream, "if (pcc_refill_buffer(ctx, 1) < 1) goto L%04d;\n", onfail);
        write_characters(gen->stream, ' ', indent);
        fputs("ctx->pos++;\n", gen->stream);
        return CODE_REACH__BOTH;
    }
}

static code_reach_t generate_code(generate_t *gen, const node_t *node, int onfail, int indent, bool bare);

static code_reach_t generate_quantifying_code(generate_t *gen, const node_t *expr, int min, int max, int onfail, int indent, bool bare) {
    if (max > 1 || max < 0) {
        code_reach_t r;
        if (!bare) {
            write_characters(gen->stream, ' ', indent);
            fputs("{\n", gen->stream);
            indent += 4;
        }
        if (min > 0) {
            write_characters(gen->stream, ' ', indent);
            fputs("int p1 = ctx->pos;\n", gen->stream);
            if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                write_characters(gen->stream, ' ', indent);
                fputs("int n1 = chunk->thunks.len;\n", gen->stream);
            }
        }
        write_characters(gen->stream, ' ', indent);
        fputs(expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE?"int p, n, i;\n":"int i;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        if (max < 0)
            fputs("for (i = 0;; i++) {\n", gen->stream);
        else
            fprintf(gen->stream, "for (i = 0; i < %d; i++) {\n", max);
        if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
            write_characters(gen->stream, ' ', indent + 4);
            fputs("p = ctx->pos;\n", gen->stream);
            write_characters(gen->stream, ' ', indent + 4);
            fputs("n = chunk->thunks.len;\n", gen->stream);
        }
        {
            int l = ++gen->label;
            r = generate_code(gen, expr, l, indent + 4, true);
            write_characters(gen->stream, ' ', indent);
            fputs("}\n", gen->stream);
            if (r != CODE_REACH__ALWAYS_SUCCEED) {
                write_characters(gen->stream, ' ', indent - 4);
                fprintf(gen->stream, "L%04d:;\n", l);
                if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                    write_characters(gen->stream, ' ', indent);
                    fputs("ctx->pos = p;\n", gen->stream);
                    write_characters(gen->stream, ' ', indent);
                    fputs("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
                }
            }
            else if (max < 0) {
                print_error("Warning: Infinite loop detected in generated code\n");
            }
        }
        if (min > 0) {
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "if (i < %d) {\n", min);
            write_characters(gen->stream, ' ', indent + 4);
            fputs("ctx->pos = p1;\n", gen->stream);
            if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                write_characters(gen->stream, ' ', indent + 4);
                fputs("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n1);\n", gen->stream);
            }
            write_characters(gen->stream, ' ', indent + 4);
            fprintf(gen->stream, "goto L%04d;\n", onfail);
            write_characters(gen->stream, ' ', indent);
            fputs("}\n", gen->stream);
        }
        if (!bare) {
            indent -= 4;
            write_characters(gen->stream, ' ', indent);
            fputs("}\n", gen->stream);
        }
        return (min > 0) ? ((r == CODE_REACH__ALWAYS_FAIL) ? CODE_REACH__ALWAYS_FAIL : CODE_REACH__BOTH) : CODE_REACH__ALWAYS_SUCCEED;
    }
    else if (max == 1) {
        if (min > 0) {
            return generate_code(gen, expr, onfail, indent, bare);
        }
        else {
            int l = ++gen->label,l1;
            if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                l1 = ++gen->label;
                if (!bare) {
                    write_characters(gen->stream, ' ', indent);
                    fputs("{\n", gen->stream);
                    indent += 4;
                }
                write_characters(gen->stream, ' ', indent);
                fputs("int p = ctx->pos;\n", gen->stream);
                write_characters(gen->stream, ' ', indent);
                fputs("int n = chunk->thunks.len;\n", gen->stream);
            }
            if (generate_code(gen, expr, l, indent, bare) != CODE_REACH__ALWAYS_SUCCEED) {
                if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                    write_characters(gen->stream, ' ', indent);
                    fprintf(gen->stream, "goto L%04d;\n", l1);
                }
                write_characters(gen->stream, ' ', indent - 4);
                fprintf(gen->stream, "L%04d:;\n", l);
                if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                    write_characters(gen->stream, ' ', indent);
                    fputs("ctx->pos = p;\n", gen->stream);
                    write_characters(gen->stream, ' ', indent);
                    fputs("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
                    write_characters(gen->stream, ' ', indent - 4);
                    fprintf(gen->stream, "L%04d:;\n", l1);
                }
            }
            if (expr->type==NODE_SEQUENCE || expr->type==NODE_ALTERNATE) {
                if (!bare) {
                    indent -= 4;
                    write_characters(gen->stream, ' ', indent);
                    fputs("}\n", gen->stream);
                }
            }
            return CODE_REACH__ALWAYS_SUCCEED;
        }
    }
    else {
        /* no code to generate */
        return CODE_REACH__ALWAYS_SUCCEED;
    }
}

static code_reach_t generate_predicating_code(generate_t *gen, const node_t *expr, bool neg, int onfail, int indent, bool bare) {
    code_reach_t r;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs("int pp = ctx->pos;\n", gen->stream);
    if (neg) {
        int l = ++gen->label;
        r = generate_code(gen, expr, l, indent, false);
        if (r != CODE_REACH__ALWAYS_FAIL) {
            write_characters(gen->stream, ' ', indent);
            fputs("ctx->pos = pp;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", onfail);
        }
        if (r != CODE_REACH__ALWAYS_SUCCEED) {
            write_characters(gen->stream, ' ', indent - 4);
            fprintf(gen->stream, "L%04d:;\n", l);
            write_characters(gen->stream, ' ', indent);
            fputs("ctx->pos = pp;\n", gen->stream);
        }
        switch (r) {
        case CODE_REACH__ALWAYS_SUCCEED: r = CODE_REACH__ALWAYS_FAIL; break;
        case CODE_REACH__ALWAYS_FAIL: r = CODE_REACH__ALWAYS_SUCCEED; break;
        case CODE_REACH__BOTH: break;
        }
    }
    else {
        int l = ++gen->label;
        int m = ++gen->label;
        r = generate_code(gen, expr, l, indent, false);
        if (r != CODE_REACH__ALWAYS_FAIL) {
            write_characters(gen->stream, ' ', indent);
            fputs("ctx->pos = pp;\n", gen->stream);
        }
        if (r == CODE_REACH__BOTH) {
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", m);
        }
        if (r != CODE_REACH__ALWAYS_SUCCEED) {
            write_characters(gen->stream, ' ', indent - 4);
            fprintf(gen->stream, "L%04d:;\n", l);
            write_characters(gen->stream, ' ', indent);
            fputs("ctx->pos = pp;\n", gen->stream);
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", onfail);
        }
        if (r == CODE_REACH__BOTH) {
            write_characters(gen->stream, ' ', indent - 4);
            fprintf(gen->stream, "L%04d:;\n", m);
        }
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_sequential_code(generate_t *gen, const node_array_t *nodes, int onfail, int indent, bool bare) {
    bool b = false;
    int i;
    for (i = 0; i < nodes->len; i++) {
        switch (generate_code(gen, nodes->buf[i], onfail, indent, false)) {
        case CODE_REACH__ALWAYS_FAIL:
            if (i < nodes->len - 1) {
                write_characters(gen->stream, ' ', indent);
                fputs("/* unreachable codes omitted */\n", gen->stream);
            }
            return CODE_REACH__ALWAYS_FAIL;
        case CODE_REACH__ALWAYS_SUCCEED:
            break;
        default:
            b = true;
        }
    }
    return b ? CODE_REACH__BOTH : CODE_REACH__ALWAYS_SUCCEED;
}

static code_reach_t generate_alternative_code(generate_t *gen, const node_array_t *nodes, int onfail, int indent, bool bare) {
    bool b = false;
    int i, m = ++gen->label;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs("int p = ctx->pos;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fputs("int n = chunk->thunks.len;\n", gen->stream);
    for (i = 0; i < nodes->len; i++) {
        bool c = (i < nodes->len - 1);
        int l = ++gen->label;
        switch (generate_code(gen, nodes->buf[i], l, indent, false)) {
        case CODE_REACH__ALWAYS_SUCCEED:
            if (c) {
                write_characters(gen->stream, ' ', indent);
                fputs("/* unreachable codes omitted */\n", gen->stream);
            }
            if (b) {
                write_characters(gen->stream, ' ', indent - 4);
                fprintf(gen->stream, "L%04d:;\n", m);
            }
            if (!bare) {
                indent -= 4;
                write_characters(gen->stream, ' ', indent);
                fputs("}\n", gen->stream);
            }
            return CODE_REACH__ALWAYS_SUCCEED;
        case CODE_REACH__ALWAYS_FAIL:
            break;
        default:
            b = true;
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", m);
        }
        write_characters(gen->stream, ' ', indent - 4);
        fprintf(gen->stream, "L%04d:;\n", l);
        write_characters(gen->stream, ' ', indent);
        fputs("ctx->pos = p;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs("pcc_thunk_array__revert(ctx->auxil, &chunk->thunks, n);\n", gen->stream);
        if (!c) {
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "goto L%04d;\n", onfail);
        }
    }
    if (b) {
        write_characters(gen->stream, ' ', indent - 4);
        fprintf(gen->stream, "L%04d:;\n", m);
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return b ? CODE_REACH__BOTH : CODE_REACH__ALWAYS_FAIL;
}

static code_reach_t generate_capturing_code(generate_t *gen, const node_t *expr, int index, int onfail, int indent, bool bare) {
    code_reach_t r;
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fputs("int cp = ctx->pos, cq;\n", gen->stream);
    r = generate_code(gen, expr, onfail, indent, false);
    write_characters(gen->stream, ' ', indent);
    fputs("cq = ctx->pos;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "chunk->capts.buf[%d].range.start = cp;\n", index);
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "chunk->capts.buf[%d].range.end = cq;\n", index);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_expanding_code(generate_t *gen, int index, int onfail, int indent, bool bare) {
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "int n = chunk->capts.buf[%d].range.end - chunk->capts.buf[%d].range.start;\n", index, index);
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "if (pcc_refill_buffer(ctx, n) < n) goto L%04d;\n", onfail);
    write_characters(gen->stream, ' ', indent);
    fputs("if (n > 0) {\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs("const char *p = ctx->buffer.buf + ctx->pos;\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fprintf(gen->stream, "const char *q = ctx->buffer.buf + chunk->capts.buf[%d].range.start;\n", index);
    write_characters(gen->stream, ' ', indent + 4);
    fputs("int i;\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs("for (i = 0; i < n; i++) {\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 8);
    fprintf(gen->stream, "if (p[i] != q[i]) goto L%04d;\n", onfail);
    write_characters(gen->stream, ' ', indent + 4);
    fputs("}\n", gen->stream);
    write_characters(gen->stream, ' ', indent + 4);
    fputs("ctx->pos += n;\n", gen->stream);
    write_characters(gen->stream, ' ', indent);
    fputs("}\n", gen->stream);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return CODE_REACH__BOTH;
}

static code_reach_t generate_thunking_action_code(
    generate_t *gen, int index, const node_const_array_t *vars, const node_const_array_t *capts, bool error, int onfail, int indent, bool bare
) {
    assert(gen->rule->type == NODE_RULE);
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    if (error) {
        write_characters(gen->stream, ' ', indent);
        fputs("pcc_value_t null;\n", gen->stream);
    }
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "pcc_thunk_t *thunk = pcc_thunk__create_leaf(ctx->auxil, pcc_action_%s_%d, %d, %d);\n",
        gen->rule->data.rule.name, index, gen->rule->data.rule.vars.len, gen->rule->data.rule.capts.len);
    {
        int i;
        for (i = 0; i < vars->len; i++) {
            assert(vars->buf[i]->type == NODE_REFERENCE);
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "thunk->data.leaf.values.buf[%d] = &(chunk->values.buf[%d]);\n",
                vars->buf[i]->data.reference.index, vars->buf[i]->data.reference.index);
        }
        for (i = 0; i < capts->len; i++) {
            assert(capts->buf[i]->type == NODE_CAPTURE);
            write_characters(gen->stream, ' ', indent);
            fprintf(gen->stream, "thunk->data.leaf.capts.buf[%d] = &(chunk->capts.buf[%d]);\n",
                capts->buf[i]->data.capture.index, capts->buf[i]->data.capture.index);
        }
        write_characters(gen->stream, ' ', indent);
        fputs("thunk->data.leaf.capt0.range.start = chunk->pos;\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs("thunk->data.leaf.capt0.range.end = ctx->pos;\n", gen->stream);
    }
    if (error) {
        write_characters(gen->stream, ' ', indent);
        fputs("memset(&null, 0, sizeof(pcc_value_t)); /* in case */\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs("thunk->data.leaf.action(ctx, thunk, &null);\n", gen->stream);
        write_characters(gen->stream, ' ', indent);
        fputs("pcc_thunk__destroy(ctx->auxil, thunk);\n", gen->stream);
    }
    else {
        write_characters(gen->stream, ' ', indent);
        fputs("pcc_thunk_array__add(ctx->auxil, &chunk->thunks, thunk);\n", gen->stream);
    }
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return CODE_REACH__ALWAYS_SUCCEED;
}

static code_reach_t generate_thunking_error_code(
    generate_t *gen, const node_t *expr, int index, const node_const_array_t *vars, const node_const_array_t *capts, int onfail, int indent, bool bare
) {
    code_reach_t r;
    int l = ++gen->label;
    int m = ++gen->label;
    assert(gen->rule->type == NODE_RULE);
    if (!bare) {
        write_characters(gen->stream, ' ', indent);
        fputs("{\n", gen->stream);
        indent += 4;
    }
    r = generate_code(gen, expr, l, indent, true);
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "goto L%04d;\n", m);
    write_characters(gen->stream, ' ', indent - 4);
    fprintf(gen->stream, "L%04d:;\n", l);
    generate_thunking_action_code(gen, index, vars, capts, true, l, indent, false);
    write_characters(gen->stream, ' ', indent);
    fprintf(gen->stream, "goto L%04d;\n", onfail);
    write_characters(gen->stream, ' ', indent - 4);
    fprintf(gen->stream, "L%04d:;\n", m);
    if (!bare) {
        indent -= 4;
        write_characters(gen->stream, ' ', indent);
        fputs("}\n", gen->stream);
    }
    return r;
}

static code_reach_t generate_code(generate_t *gen, const node_t *node, int onfail, int indent, bool bare) {
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
        if (node->data.reference.index >= 0) {
            fprintf(gen->stream, "if (!pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &chunk->thunks, &(chunk->values.buf[%d]))) goto L%04d;\n",
                node->data.reference.name, node->data.reference.index, onfail);
        }
        else {
            fprintf(gen->stream, "if (!pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &chunk->thunks, NULL)) goto L%04d;\n",
                node->data.reference.name, onfail);
        }
        return CODE_REACH__BOTH;
    case NODE_STRING:
        return generate_matching_string_code(gen, node->data.string.value, node->data.string.len, onfail, indent, bare);
    case NODE_CHARCLASS:
        return generate_matching_charclass_code(gen, node->data.charclass.value, node->data.charclass.len, onfail, indent, bare);
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
            gen, node->data.action.index, &node->data.action.vars, &node->data.action.capts, false, onfail, indent, bare
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

static bool generate(context_t *ctx) {
    const char *vt = get_value_type(ctx);
    const char *at = get_auxil_type(ctx);
    bool vp = is_pointer_type(vt);
    bool ap = is_pointer_type(at);
    FILE *stream = ctx->sfile;
    {
        fputs(
            "#ifndef PCC_BUFFERSIZE\n"
            "#define PCC_BUFFERSIZE 256\n"
            "#endif /* PCC_BUFFERSIZE */\n"
            "\n"
            "#ifndef PCC_ARRAYSIZE\n"
            "#define PCC_ARRAYSIZE 2\n"
            "#endif /* PCC_ARRAYSIZE */\n"
            "\n"
            "typedef struct pcc_char_array_tag {\n"
            "    char *buf;\n"
            "    int max;\n"
            "    int len;\n"
            "} pcc_char_array_t;\n"
            "\n"
            "typedef struct pcc_range_tag {\n"
            "    int start;\n"
            "    int end;\n"
            "} pcc_range_t;\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "typedef %s%spcc_value_t;\n"
            "\n",
            vt, vp ? "" : " "
        );
        fprintf(
            stream,
            "typedef %s%spcc_auxil_t;\n"
            "\n",
            at, ap ? "" : " "
        );
        fputs(
            "typedef struct pcc_value_table_tag {\n"
            "    pcc_value_t *buf;\n"
            "    int max;\n"
            "    int len;\n"
            "} pcc_value_table_t;\n"
            "\n"
            "typedef struct pcc_value_refer_table_tag {\n"
            "    pcc_value_t **buf;\n"
            "    int max;\n"
            "    int len;\n"
            "} pcc_value_refer_table_t;\n"
            "\n"
            "typedef struct pcc_capture_tag {\n"
            "    pcc_range_t range;\n"
            "    char *string; /* mutable */\n"
            "} pcc_capture_t;\n"
            "\n"
            "typedef struct pcc_capture_table_tag {\n"
            "    pcc_capture_t *buf;\n"
            "    int max;\n"
            "    int len;\n"
            "} pcc_capture_table_t;\n"
            "\n"
            "typedef struct pcc_capture_const_table_tag {\n"
            "    const pcc_capture_t **buf;\n"
            "    int max;\n"
            "    int len;\n"
            "} pcc_capture_const_table_t;\n"
            "\n"
            "typedef struct pcc_thunk_tag pcc_thunk_t;\n"
            "typedef struct pcc_thunk_array_tag pcc_thunk_array_t;\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "typedef void (*pcc_action_t)(%s_context_t *, pcc_thunk_t *, pcc_value_t *);\n"
            "\n",
            get_prefix(ctx)
        );
        fputs(
            "typedef enum pcc_thunk_type_tag {\n"
            "    PCC_THUNK_LEAF,\n"
            "    PCC_THUNK_NODE,\n"
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
            "    int max;\n"
            "    int len;\n"
            "};\n"
            "\n"
            "typedef struct pcc_thunk_chunk_tag {\n"
            "    pcc_value_table_t values;\n"
            "    pcc_capture_table_t capts;\n"
            "    pcc_thunk_array_t thunks;\n"
            "    int pos;\n"
            "} pcc_thunk_chunk_t;\n"
            "\n"
            "typedef struct pcc_lr_entry_tag pcc_lr_entry_t;\n"
            "\n"
            "typedef enum pcc_lr_answer_type_tag {\n"
            "    PCC_LR_ANSWER_LR,\n"
            "    PCC_LR_ANSWER_CHUNK,\n"
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
            "    int pos;\n"
            "    pcc_lr_answer_t *hold;\n"
            "};\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "typedef pcc_thunk_chunk_t *(*pcc_rule_t)(%s_context_t *);\n"
            "\n",
            get_prefix(ctx)
        );
        fputs(
            "typedef struct pcc_rule_set_tag {\n"
            "    pcc_rule_t *buf;\n"
            "    int max;\n"
            "    int len;\n"
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
            "    int max;\n"
            "    int len;\n"
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
            "    int max;\n"
            "    int len;\n"
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
            "    int max;\n"
            "    int len;\n"
            "} pcc_lr_stack_t;\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "struct %s_context_tag {\n"
            "    int pos;\n"
            "    pcc_char_array_t buffer;\n"
            "    pcc_lr_table_t lrtable;\n"
            "    pcc_lr_stack_t lrstack;\n"
            "    bool got_error;\n"
            "    pcc_auxil_t auxil;\n"
            "};\n"
            "\n",
            get_prefix(ctx)
        );
        fputs(
            "#ifndef PCC_ERROR\n"
            "#define PCC_ERROR(auxil) pcc_error()\n"
            "static void pcc_error(void) {\n"
            "    fprintf(stderr, \"Syntax error\\n\");\n"
            "    exit(1);\n"
            "}\n"
            "#endif /* PCC_ERROR */\n"
            "\n"
            "#ifndef PCC_GETCHAR\n"
            "#define PCC_GETCHAR(auxil) getchar()\n"
            "#endif /* PCC_GETCHAR */\n"
            "\n"
            "#ifndef PCC_MALLOC\n"
            "#define PCC_MALLOC(auxil, size) pcc_malloc_e(size)\n"
            "static void *pcc_malloc_e(size_t size) {\n"
            "    void *p = malloc(size);\n"
            "    if (p == NULL) {\n"
            "        fprintf(stderr, \"Out of memory\\n\");\n"
            "        exit(1);\n"
            "    }\n"
            "    return p;\n"
            "}\n"
            "#endif /* PCC_MALLOC */\n"
            "\n"
            "#ifndef PCC_REALLOC\n"
            "#define PCC_REALLOC(auxil, ptr, size) pcc_realloc_e(ptr, size)\n"
            "static void *pcc_realloc_e(void *ptr, size_t size) {\n"
            "    void *p = realloc(ptr, size);\n"
            "    if (p == NULL) {\n"
            "        fprintf(stderr, \"Out of memory\\n\");\n"
            "        exit(1);\n"
            "    }\n"
            "    return p;\n"
            "}\n"
            "#endif /* PCC_REALLOC */\n"
            "\n"
            "#ifndef PCC_FREE\n"
            "#define PCC_FREE(auxil, ptr) free(ptr)\n"
            "#endif /* PCC_FREE */\n"
            "\n"
            /* not used
            "static char *pcc_strdup_e(pcc_auxil_t auxil, const char *str) {\n"
            "    size_t m = strlen(str);\n"
            "    char *s = (char *)PCC_MALLOC(auxil, m + 1);\n"
            "    memcpy(s, str, m);\n"
            "    s[m] = '\\0';\n"
            "    return s;\n"
            "}\n"
            "\n"
            */
            "static char *pcc_strndup_e(pcc_auxil_t auxil, const char *str, size_t len) {\n"
            "    size_t m = strnlen(str, len);\n"
            "    char *s = (char *)PCC_MALLOC(auxil, m + 1);\n"
            "    memcpy(s, str, m);\n"
            "    s[m] = '\\0';\n"
            "    return s;\n"
            "}\n"
            "\n"
            "static void pcc_char_array__init(pcc_auxil_t auxil, pcc_char_array_t *array, int max) {\n"
            "    array->len = 0;\n"
            "    array->max = max;\n"
            "    array->buf = (char *)PCC_MALLOC(auxil, array->max);\n"
            "}\n"
            "\n"
            "static void pcc_char_array__add(pcc_auxil_t auxil, pcc_char_array_t *array, char ch) {\n"
            "    if (array->max <= 0) array->max = 1;\n"
            "    while (array->max <= array->len) array->max <<= 1;\n"
            "    array->buf = (char *)PCC_REALLOC(auxil, array->buf, array->max);\n"
            "    array->buf[array->len++] = ch;\n"
            "}\n"
            "\n"
            "static void pcc_char_array__term(pcc_auxil_t auxil, pcc_char_array_t *array) {\n"
            "    PCC_FREE(auxil, array->buf);\n"
            "}\n"
            "\n"
            "static void pcc_value_table__init(pcc_auxil_t auxil, pcc_value_table_t *table, int max) {\n"
            "    table->len = 0;\n"
            "    table->max = max;\n"
            "    table->buf = (pcc_value_t *)PCC_MALLOC(auxil, table->max * sizeof(pcc_value_t));\n"
            "}\n"
            "\n"
            "static void pcc_value_table__resize(pcc_auxil_t auxil, pcc_value_table_t *table, int len) {\n"
            "    if (table->max < len) {\n"
            "        if (table->max <= 0) table->max = 1;\n"
            "        while (table->max < len) table->max <<= 1;\n"
            "        table->buf = (pcc_value_t *)PCC_REALLOC(auxil, table->buf, table->max * sizeof(pcc_value_t));\n"
            "    }\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_value_table__term(pcc_auxil_t auxil, pcc_value_table_t *table) {\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n"
            "static void pcc_value_refer_table__init(pcc_auxil_t auxil, pcc_value_refer_table_t *table, int max) {\n"
            "    table->len = 0;\n"
            "    table->max = max;\n"
            "    table->buf = (pcc_value_t **)PCC_MALLOC(auxil, table->max * sizeof(pcc_value_t *));\n"
            "}\n"
            "\n"
            "static void pcc_value_refer_table__resize(pcc_auxil_t auxil, pcc_value_refer_table_t *table, int len) {\n"
            "    int i;\n"
            "    if (table->max < len) {\n"
            "        if (table->max <= 0) table->max = 1;\n"
            "        while (table->max < len) table->max <<= 1;\n"
            "        table->buf = (pcc_value_t **)PCC_REALLOC(auxil, table->buf, table->max * sizeof(pcc_value_t *));\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_value_refer_table__term(pcc_auxil_t auxil, pcc_value_refer_table_t *table) {\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n"
            "static void pcc_capture_table__init(pcc_auxil_t auxil, pcc_capture_table_t *table, int max) {\n"
            "    table->len = 0;\n"
            "    table->max = max;\n"
            "    table->buf = (pcc_capture_t *)PCC_MALLOC(auxil, table->max * sizeof(pcc_capture_t));\n"
            "}\n"
            "\n"
            "static void pcc_capture_table__resize(pcc_auxil_t auxil, pcc_capture_table_t *table, int len) {\n"
            "    int i;\n"
            "    for (i = table->len - 1; i >= len; i--) PCC_FREE(auxil, table->buf[i].string);\n"
            "    if (table->max < len) {\n"
            "        if (table->max <= 0) table->max = 1;\n"
            "        while (table->max < len) table->max <<= 1;\n"
            "        table->buf = (pcc_capture_t *)PCC_REALLOC(auxil, table->buf, table->max * sizeof(pcc_capture_t));\n"
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
            "    int i;\n"
            "    for (i = table->len - 1; i >= 0; i--) PCC_FREE(auxil, table->buf[i].string);\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n"
            "static void pcc_capture_const_table__init(pcc_auxil_t auxil, pcc_capture_const_table_t *table, int max) {\n"
            "    table->len = 0;\n"
            "    table->max = max;\n"
            "    table->buf = (const pcc_capture_t **)PCC_MALLOC(auxil, table->max * sizeof(const pcc_capture_t *));\n"
            "}\n"
            "\n"
            "static void pcc_capture_const_table__resize(pcc_auxil_t auxil, pcc_capture_const_table_t *table, int len) {\n"
            "    int i;\n"
            "    if (table->max < len) {\n"
            "        if (table->max <= 0) table->max = 1;\n"
            "        while (table->max < len) table->max <<= 1;\n"
            "        table->buf = (const pcc_capture_t **)PCC_REALLOC(auxil, (pcc_capture_t **)table->buf, table->max * sizeof(const pcc_capture_t *));\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_capture_const_table__term(pcc_auxil_t auxil, pcc_capture_const_table_t *table) {\n"
            "    PCC_FREE(auxil, (pcc_capture_t **)table->buf);\n"
            "}\n"
            "\n"
            "static pcc_thunk_t *pcc_thunk__create_leaf(pcc_auxil_t auxil, pcc_action_t action, int valuec, int captc) {\n"
            "    pcc_thunk_t *thunk = (pcc_thunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_t));\n"
            "    thunk->type = PCC_THUNK_LEAF;\n"
            "    pcc_value_refer_table__init(auxil, &thunk->data.leaf.values, valuec);\n"
            "    pcc_value_refer_table__resize(auxil, &thunk->data.leaf.values, valuec);\n"
            "    pcc_capture_const_table__init(auxil, &thunk->data.leaf.capts, captc);\n"
            "    pcc_capture_const_table__resize(auxil, &thunk->data.leaf.capts, captc);\n"
            "    thunk->data.leaf.capt0.range.start = 0;\n"
            "    thunk->data.leaf.capt0.range.end = 0;\n"
            "    thunk->data.leaf.capt0.string = NULL;\n"
            "    thunk->data.leaf.action = action;\n"
            "    return thunk;\n"
            "}\n"
            "\n"
            "static pcc_thunk_t *pcc_thunk__create_node(pcc_auxil_t auxil, const pcc_thunk_array_t *thunks, pcc_value_t *value) {\n"
            "    pcc_thunk_t *thunk = (pcc_thunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_t));\n"
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
            "\n"
            "static void pcc_thunk_array__init(pcc_auxil_t auxil, pcc_thunk_array_t *array, int max) {\n"
            "    array->len = 0;\n"
            "    array->max = max;\n"
            "    array->buf = (pcc_thunk_t **)PCC_MALLOC(auxil, array->max * sizeof(pcc_thunk_t *));\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__add(pcc_auxil_t auxil, pcc_thunk_array_t *array, pcc_thunk_t *thunk) {\n"
            "    if (array->max <= 0) array->max = 1;\n"
            "    while (array->max <= array->len) array->max <<= 1;\n"
            "    array->buf = (pcc_thunk_t **)PCC_REALLOC(auxil, array->buf, array->max * sizeof(pcc_thunk_t *));\n"
            "    array->buf[array->len++] = thunk;\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__revert(pcc_auxil_t auxil, pcc_thunk_array_t *array, int len) {\n"
            "    if (array->len > len) {\n"
            "        int i;\n"
            "        for (i = array->len - 1; i >= len; i--) pcc_thunk__destroy(auxil, array->buf[i]);\n"
            "        array->len = len;\n"
            "    }\n"
            "}\n"
            "\n"
            "static void pcc_thunk_array__term(pcc_auxil_t auxil, pcc_thunk_array_t *array) {\n"
            "    int i;\n"
            "    for (i = array->len - 1; i >= 0; i--) pcc_thunk__destroy(auxil, array->buf[i]);\n"
            "    PCC_FREE(auxil, array->buf);\n"
            "}\n"
            "\n"
            "static pcc_thunk_chunk_t *pcc_thunk_chunk__create(pcc_auxil_t auxil) {\n"
            "    pcc_thunk_chunk_t *chunk = (pcc_thunk_chunk_t *)PCC_MALLOC(auxil, sizeof(pcc_thunk_chunk_t));\n"
            "    pcc_value_table__init(auxil, &chunk->values, PCC_ARRAYSIZE);\n"
            "    pcc_capture_table__init(auxil, &chunk->capts, PCC_ARRAYSIZE);\n"
            "    pcc_thunk_array__init(auxil, &chunk->thunks, PCC_ARRAYSIZE);\n"
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
            "\n"
            "static void pcc_rule_set__init(pcc_auxil_t auxil, pcc_rule_set_t *set, int max) {\n"
            "    set->len = 0;\n"
            "    set->max = max;\n"
            "    set->buf = (pcc_rule_t *)PCC_MALLOC(auxil, set->max * sizeof(pcc_rule_t));\n"
            "}\n"
            "\n"
            "static int pcc_rule_set__index(pcc_auxil_t auxil, const pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    int i;\n"
            "    for (i = 0; i < set->len; i++) {\n"
            "        if (set->buf[i] == rule) return i;\n"
            "    }\n"
            "    return -1;\n"
            "}\n"
            "\n"
            "static bool pcc_rule_set__add(pcc_auxil_t auxil, pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    int i = pcc_rule_set__index(auxil, set, rule);\n"
            "    if (i >= 0) return false;\n"
            "    if (set->max <= 0) set->max = 1;\n"
            "    while (set->max <= set->len) set->max <<= 1;\n"
            "    set->buf = (pcc_rule_t *)PCC_REALLOC(auxil, set->buf, set->max * sizeof(pcc_rule_t));\n"
            "    set->buf[set->len++] = rule;\n"
            "    return true;\n"
            "}\n"
            "\n"
            "static bool pcc_rule_set__remove(pcc_auxil_t auxil, pcc_rule_set_t *set, pcc_rule_t rule) {\n"
            "    int i = pcc_rule_set__index(auxil, set, rule);\n"
            "    if (i < 0) return false;\n"
            "    memmove(set->buf + i, set->buf + (i + 1), (set->len - (i + 1)) * sizeof(pcc_rule_t));\n"
            "    return true;\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__clear(pcc_auxil_t auxil, pcc_rule_set_t *set) {\n"
            "    set->len = 0;\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__copy(pcc_auxil_t auxil, pcc_rule_set_t *set, const pcc_rule_set_t *src) {\n"
            "    int i;\n"
            "    pcc_rule_set__clear(auxil, set);\n"
            "    for (i = 0; i < src->len; i++) {\n"
            "        pcc_rule_set__add(auxil, set, src->buf[i]);\n"
            "    }\n"
            "}\n"
            "\n"
            "static void pcc_rule_set__term(pcc_auxil_t auxil, pcc_rule_set_t *set) {\n"
            "    PCC_FREE(auxil, set->buf);\n"
            "}\n"
            "\n"
            "static pcc_lr_head_t *pcc_lr_head__create(pcc_auxil_t auxil, pcc_rule_t rule) {\n"
            "    pcc_lr_head_t *head = (pcc_lr_head_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_head_t));\n"
            "    head->rule = rule;\n"
            "    pcc_rule_set__init(auxil, &head->invol, PCC_ARRAYSIZE);\n"
            "    pcc_rule_set__init(auxil, &head->eval, PCC_ARRAYSIZE);\n"
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
            "\n"
            "static void pcc_lr_entry__destroy(pcc_auxil_t auxil, pcc_lr_entry_t *lr);\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_answer__create(pcc_auxil_t auxil, pcc_lr_answer_type_t type, int pos) {\n"
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
            "    pcc_lr_answer_t *a = pcc_lr_answer__create(auxil, answer->type, answer->pos);\n"
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
            "    if (answer == NULL) return;\n"
            "    pcc_lr_answer__destroy(auxil, answer->hold);\n"
            "    switch (answer->type) {\n"
            "    case PCC_LR_ANSWER_LR:\n"
            "        pcc_lr_entry__destroy(auxil, answer->data.lr);\n"
            "        break;\n"
            "    case PCC_LR_ANSWER_CHUNK:\n"
            "        pcc_thunk_chunk__destroy(auxil, answer->data.chunk);\n"
            "        break;\n"
            "    default: /* unknown */\n"
            "        break;\n"
            "    }\n"
            "    PCC_FREE(auxil, answer);\n"
            "}\n"
            "\n"
            "static void pcc_lr_memo_map__init(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, int max) {\n"
            "    map->len = 0;\n"
            "    map->max = max;\n"
            "    map->buf = (pcc_lr_memo_t *)PCC_MALLOC(auxil, map->max * sizeof(pcc_lr_memo_t));\n"
            "}\n"
            "\n"
            "static int pcc_lr_memo_map__index(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule) {\n"
            "    int i;\n"
            "    for (i = 0; i < map->len; i++) {\n"
            "        if (map->buf[i].rule == rule) return i;\n"
            "    }\n"
            "    return -1;\n"
            "}\n"
            "\n"
            "static void pcc_lr_memo_map__put(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule, pcc_lr_answer_t *answer) {\n"
            "    int i = pcc_lr_memo_map__index(auxil, map, rule);\n"
            "    if (i >= 0) {\n"
            "        pcc_lr_answer__destroy(auxil, map->buf[i].answer);\n"
            "        map->buf[i].answer = answer;\n"
            "    }\n"
            "    else {\n"
            "        if (map->max <= 0) map->max = 1;\n"
            "        while (map->max <= map->len) map->max <<= 1;\n"
            "        map->buf = (pcc_lr_memo_t *)PCC_REALLOC(auxil, map->buf, map->max * sizeof(pcc_lr_memo_t));\n"
            "        map->buf[map->len].rule = rule;\n"
            "        map->buf[map->len].answer = answer;\n"
            "        map->len++;\n"
            "    }\n"
            "}\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_memo_map__get(pcc_auxil_t auxil, pcc_lr_memo_map_t *map, pcc_rule_t rule) {\n"
            "    int i = pcc_lr_memo_map__index(auxil, map, rule);\n"
            "    return (i >= 0) ? map->buf[i].answer : NULL;\n"
            "}\n"
            "\n"
            "static void pcc_lr_memo_map__term(pcc_auxil_t auxil, pcc_lr_memo_map_t *map) {\n"
            "    int i;\n"
            "    for (i = map->len - 1; i >= 0; i--) pcc_lr_answer__destroy(auxil, map->buf[i].answer);\n"
            "    PCC_FREE(auxil, map->buf);\n"
            "}\n"
            "\n"
            "static pcc_lr_table_entry_t *pcc_lr_table_entry__create(pcc_auxil_t auxil) {\n"
            "    pcc_lr_table_entry_t *entry = (pcc_lr_table_entry_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_table_entry_t));\n"
            "    entry->head = NULL;\n"
            "    pcc_lr_memo_map__init(auxil, &entry->memos, PCC_ARRAYSIZE);\n"
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
            "\n"
            "static void pcc_lr_table__init(pcc_auxil_t auxil, pcc_lr_table_t *table, int max) {\n"
            "    table->len = 0;\n"
            "    table->max = max;\n"
            "    table->buf = (pcc_lr_table_entry_t **)PCC_MALLOC(auxil, table->max * sizeof(pcc_lr_table_entry_t *));\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__resize(pcc_auxil_t auxil, pcc_lr_table_t *table, int len) {\n"
            "    int i;\n"
            "    for (i = table->len - 1; i >= len; i--) pcc_lr_table_entry__destroy(auxil, table->buf[i]);\n"
            "    if (table->max < len) {\n"
            "        if (table->max <= 0) table->max = 1;\n"
            "        while (table->max < len) table->max <<= 1;\n"
            "        table->buf = (pcc_lr_table_entry_t **)PCC_REALLOC(auxil, table->buf, table->max * sizeof(pcc_lr_table_entry_t *));\n"
            "    }\n"
            "    for (i = table->len; i < len; i++) table->buf[i] = NULL;\n"
            "    table->len = len;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__set_head(pcc_auxil_t auxil, pcc_lr_table_t *table, int index, pcc_lr_head_t *head) {\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    table->buf[index]->head = head;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__hold_head(pcc_auxil_t auxil, pcc_lr_table_t *table, int index, pcc_lr_head_t *head) {\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    head->hold = table->buf[index]->hold_h;\n"
            "    table->buf[index]->hold_h = head;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__set_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, int index, pcc_rule_t rule, pcc_lr_answer_t *answer) {\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    pcc_lr_memo_map__put(auxil, &table->buf[index]->memos, rule, answer);\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__hold_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, int index, pcc_lr_answer_t *answer) {\n"
            "    if (index >= table->len) pcc_lr_table__resize(auxil, table, index + 1);\n"
            "    if (table->buf[index] == NULL) table->buf[index] = pcc_lr_table_entry__create(auxil);\n"
            "    answer->hold = table->buf[index]->hold_a;\n"
            "    table->buf[index]->hold_a = answer;\n"
            "}\n"
            "\n"
            "static pcc_lr_head_t *pcc_lr_table__get_head(pcc_auxil_t auxil, pcc_lr_table_t *table, int index) {\n"
            "    if (index >= table->len || table->buf[index] == NULL) return NULL;\n"
            "    return table->buf[index]->head;\n"
            "}\n"
            "\n"
            "static pcc_lr_answer_t *pcc_lr_table__get_answer(pcc_auxil_t auxil, pcc_lr_table_t *table, int index, pcc_rule_t rule) {\n"
            "    if (index >= table->len || table->buf[index] == NULL) return NULL;\n"
            "    return pcc_lr_memo_map__get(auxil, &table->buf[index]->memos, rule);\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__shift(pcc_auxil_t auxil, pcc_lr_table_t *table, int count) {\n"
            "    int i;\n"
            "    if (count > table->len) count = table->len;\n"
            "    for (i = 0; i < count; i++) pcc_lr_table_entry__destroy(auxil, table->buf[i]);\n"
            "    memmove(table->buf, table->buf + count, (table->len - count) * sizeof(pcc_lr_table_entry_t *));\n"
            "    table->len -= count;\n"
            "}\n"
            "\n"
            "static void pcc_lr_table__term(pcc_auxil_t auxil, pcc_lr_table_t *table) {\n"
            "    int i;\n"
            "    for (i = table->len - 1; i >= 0; i--) pcc_lr_table_entry__destroy(auxil, table->buf[i]);\n"
            "    PCC_FREE(auxil, table->buf);\n"
            "}\n"
            "\n"
            "static pcc_lr_entry_t *pcc_lr_entry__create(pcc_auxil_t auxil, pcc_rule_t rule) {\n"
            "    pcc_lr_entry_t *lr = (pcc_lr_entry_t *)PCC_MALLOC(auxil, sizeof(pcc_lr_entry_t));\n"
            "    lr->rule = rule;\n"
            "    lr->seed = NULL;\n"
            "    lr->head = NULL;\n"
            "    return lr;\n"
            "}\n"
            "\n"
            "static void pcc_lr_entry__destroy(pcc_auxil_t auxil, pcc_lr_entry_t *lr) {\n"
            "    PCC_FREE(auxil, lr);\n"
            "}\n"
            "\n"
            "static void pcc_lr_stack__init(pcc_auxil_t auxil, pcc_lr_stack_t *stack, int max) {\n"
            "    stack->len = 0;\n"
            "    stack->max = max;\n"
            "    stack->buf = (pcc_lr_entry_t **)PCC_MALLOC(auxil, stack->max * sizeof(pcc_lr_entry_t *));\n"
            "}\n"
            "\n"
            "static void pcc_lr_stack__push(pcc_auxil_t auxil, pcc_lr_stack_t *stack, pcc_lr_entry_t *lr) {\n"
            "    if (stack->max <= 0) stack->max = 1;\n"
            "    while (stack->max <= stack->len) stack->max <<= 1;\n"
            "    stack->buf = (pcc_lr_entry_t **)PCC_REALLOC(auxil, stack->buf, stack->max * sizeof(pcc_lr_entry_t *));\n"
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
            stream
        );
        fprintf(
            stream,
            "static %s_context_t *pcc_context__create(pcc_auxil_t auxil) {\n"
            "    %s_context_t *ctx = (%s_context_t *)PCC_MALLOC(auxil, sizeof(%s_context_t));\n",
            get_prefix(ctx), get_prefix(ctx), get_prefix(ctx), get_prefix(ctx)
        );
        fputs(
            "    ctx->pos = 0;\n"
            "    pcc_char_array__init(auxil, &ctx->buffer, PCC_BUFFERSIZE);\n"
            "    pcc_lr_table__init(auxil, &ctx->lrtable, PCC_BUFFERSIZE);\n"
            "    pcc_lr_stack__init(auxil, &ctx->lrstack, PCC_ARRAYSIZE);\n"
            "    ctx->got_error = false;\n"
            "    ctx->auxil = auxil;\n"
            "    return ctx;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static void pcc_context__destroy(%s_context_t *ctx) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    if (ctx == NULL) return;\n"
            "    pcc_lr_stack__term(ctx->auxil, &ctx->lrstack);\n"
            "    pcc_lr_table__term(ctx->auxil, &ctx->lrtable);\n"
            "    pcc_char_array__term(ctx->auxil, &ctx->buffer);\n"
            "    PCC_FREE(ctx->auxil, ctx);\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static int pcc_refill_buffer(%s_context_t *ctx, int num) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    int n;\n"
            "    n = ctx->buffer.len - ctx->pos;\n"
            "    if (n >= num) return n;\n"
            "    while (ctx->buffer.len < ctx->pos + num) {\n"
            "        int c = PCC_GETCHAR(ctx->auxil);\n"
            "        if (c == EOF) break;\n"
            "        pcc_char_array__add(ctx->auxil, &ctx->buffer, (char)c);\n"
            "    }\n"
            "    return ctx->buffer.len - ctx->pos;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static void pcc_commit_buffer(%s_context_t *ctx) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    memmove(ctx->buffer.buf, ctx->buffer.buf + ctx->pos, ctx->buffer.len - ctx->pos);\n"
            "    ctx->buffer.len -= ctx->pos;\n"
            "    pcc_lr_table__shift(ctx->auxil, &ctx->lrtable, ctx->pos);\n"
            "    ctx->pos = 0;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static const char *pcc_get_capture_string(%s_context_t *ctx, const pcc_capture_t *capt) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    if (capt->string == NULL)\n"
            "        ((pcc_capture_t *)capt)->string =\n"
            "            pcc_strndup_e(ctx->auxil, ctx->buffer.buf + capt->range.start, capt->range.end - capt->range.start);\n"
            "    return capt->string;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static bool pcc_apply_rule(%s_context_t *ctx, pcc_rule_t rule, pcc_thunk_array_t *thunks, pcc_value_t *value) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    static pcc_value_t null;\n"
            "    pcc_thunk_chunk_t *c = NULL;\n"
            "    int p = ctx->pos;\n"
            "    bool b = true;\n"
            "    pcc_lr_answer_t *a = pcc_lr_table__get_answer(ctx->auxil, &ctx->lrtable, p, rule);\n"
            "    pcc_lr_head_t *h = pcc_lr_table__get_head(ctx->auxil, &ctx->lrtable, p);\n"
            "    if (h != NULL) {\n"
            "        if (a == NULL && rule != h->rule && pcc_rule_set__index(ctx->auxil, &h->invol, rule) < 0) {\n"
            "            b = false;\n"
            "            c = NULL;\n"
            "        }\n"
            "        else if (pcc_rule_set__remove(ctx->auxil, &h->eval, rule)) {\n"
            "            b = false;\n"
            "            c = rule(ctx);\n"
            "            a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_CHUNK, ctx->pos);\n"
            "            a->data.chunk = c;\n"
            "            pcc_lr_table__hold_answer(ctx->auxil, &ctx->lrtable, p, a);\n"
            "        }\n"
            "    }\n"
            "    if (b) {\n"
            "        if (a != NULL) {\n"
            "            ctx->pos = a->pos;\n"
            "            switch (a->type) {\n"
            "            case PCC_LR_ANSWER_LR:\n"
            "                if (a->data.lr->head == NULL) {\n"
            "                    a->data.lr->head = pcc_lr_head__create(ctx->auxil, rule);\n"
            "                    pcc_lr_table__hold_head(ctx->auxil, &ctx->lrtable, p, a->data.lr->head);\n"
            "                }\n"
            "                {\n"
            "                    int i;\n"
            "                    for (i = ctx->lrstack.len - 1; i >= 0; i--) {\n"
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
            "            pcc_lr_entry_t *e = pcc_lr_entry__create(ctx->auxil, rule);\n"
            "            pcc_lr_stack__push(ctx->auxil, &ctx->lrstack, e);\n"
            "            a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_LR, p);\n"
            "            a->data.lr = e;\n"
            "            pcc_lr_table__set_answer(ctx->auxil, &ctx->lrtable, p, rule, a);\n"
            "            c = rule(ctx);\n"
            "            pcc_lr_stack__pop(ctx->auxil, &ctx->lrstack);\n"
            "            a->pos = ctx->pos;\n"
            "            if (e->head == NULL) {\n"
            "                pcc_lr_answer__set_chunk(ctx->auxil, a, c);\n"
            "            }\n"
            "            else {\n"
            "                e->seed = c;\n"
            "                h = a->data.lr->head;\n"
            "                if (h->rule != rule) {\n"
            "                    c = a->data.lr->seed;\n"
            "                    a = pcc_lr_answer__create(ctx->auxil, PCC_LR_ANSWER_CHUNK, ctx->pos);\n"
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
            "                            ctx->pos = p;\n"
            "                            pcc_rule_set__copy(ctx->auxil, &h->eval, &h->invol);\n"
            "                            c = rule(ctx);\n"
            "                            if (c == NULL || ctx->pos <= a->pos) break;\n"
            "                            pcc_lr_answer__set_chunk(ctx->auxil, a, c);\n"
            "                            a->pos = ctx->pos;\n"
            "                        }\n"
            "                        pcc_thunk_chunk__destroy(ctx->auxil, c);\n"
            "                        pcc_lr_table__set_head(ctx->auxil, &ctx->lrtable, p, NULL);\n"
            "                        ctx->pos = a->pos;\n"
            "                        c = a->data.chunk;\n"
            "                    }\n"
            "                }\n"
            "            }\n"
            "        }\n"
            "    }\n"
            "    if (c == NULL) return false;\n"
            "    if (value == NULL) value = &null;\n"
            "    memset(value, 0, sizeof(pcc_value_t)); /* in case */\n"
            "    pcc_thunk_array__add(ctx->auxil, thunks, pcc_thunk__create_node(ctx->auxil, &c->thunks, value));\n"
            "    return true;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "static void pcc_do_action(%s_context_t *ctx, const pcc_thunk_array_t *thunks, pcc_value_t *value) {\n",
            get_prefix(ctx)
        );
        fputs(
            "    int i;\n"
            "    for (i = 0; i < thunks->len; i++) {\n"
            "        pcc_thunk_t *thunk = thunks->buf[i];\n"
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
            stream
        );
        {
            int i, j, k;
            for (i = 0; i < ctx->rules.len; i++) {
                const node_rule_t *r = &ctx->rules.buf[i]->data.rule;
                for (j = 0; j < r->codes.len; j++) {
                    const char *s;
                    int d;
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
                    fprintf(
                        stream,
                        "static void pcc_action_%s_%d(%s_context_t *__pcc_ctx, pcc_thunk_t *__pcc_in, pcc_value_t *__pcc_out) {\n",
                        r->name, d, get_prefix(ctx)
                    );
                    fputs(
                        "#define auxil (__pcc_ctx->auxil)\n"
                        "#define __ (*__pcc_out)\n",
                        stream
                    );
                    for (k = 0; k < v->len; k++) {
                        assert(v->buf[k]->type == NODE_REFERENCE);
                        fprintf(
                            stream,
                            "#define %s (*__pcc_in->data.leaf.values.buf[%d])\n",
                            v->buf[k]->data.reference.var,
                            v->buf[k]->data.reference.index
                        );
                    }
                    fputs(
                        "#define _0 pcc_get_capture_string(__pcc_ctx, &__pcc_in->data.leaf.capt0)\n"
                        "#define _0s ((const int)__pcc_in->data.leaf.capt0.range.start)\n"
                        "#define _0e ((const int)__pcc_in->data.leaf.capt0.range.end)\n",
                        stream
                    );
                    for (k = 0; k < c->len; k++) {
                        assert(c->buf[k]->type == NODE_CAPTURE);
                        fprintf(
                            stream,
                            "#define _%d pcc_get_capture_string(__pcc_ctx, __pcc_in->data.leaf.capts.buf[%d])\n",
                            c->buf[k]->data.capture.index + 1,
                            c->buf[k]->data.capture.index
                        );
                        fprintf(
                            stream,
                            "#define _%ds __pcc_in->data.leaf.capts.buf[%d]->range.start\n",
                            c->buf[k]->data.capture.index + 1,
                            c->buf[k]->data.capture.index
                        );
                        fprintf(
                            stream,
                            "#define _%de __pcc_in->data.leaf.capts.buf[%d]->range.end\n",
                            c->buf[k]->data.capture.index + 1,
                            c->buf[k]->data.capture.index
                        );
                    }
                    write_code_block(stream, s, strlen(s), 4);
                    for (k = c->len - 1; k >= 0; k--) {
                        assert(c->buf[k]->type == NODE_CAPTURE);
                        fprintf(
                            stream,
                            "#undef _%de\n",
                            c->buf[k]->data.capture.index + 1
                        );
                        fprintf(
                            stream,
                            "#undef _%ds\n",
                            c->buf[k]->data.capture.index + 1
                        );
                        fprintf(
                            stream,
                            "#undef _%d\n",
                            c->buf[k]->data.capture.index + 1
                        );
                    }
                    fputs(
                        "#undef _0e\n"
                        "#undef _0s\n"
                        "#undef _0\n",
                        stream
                    );
                    for (k = v->len - 1; k >= 0; k--) {
                        assert(v->buf[k]->type == NODE_REFERENCE);
                        fprintf(
                            stream,
                            "#undef %s\n",
                            v->buf[k]->data.reference.var
                        );
                    }
                    fputs(
                        "#undef __\n"
                        "#undef auxil\n",
                        stream
                    );
                    fputs(
                        "}\n"
                        "\n",
                        stream
                    );
                }
            }
        }
        {
            int i;
            for (i = 0; i < ctx->rules.len; i++) {
                fprintf(
                    stream,
                    "static pcc_thunk_chunk_t *pcc_evaluate_rule_%s(%s_context_t *ctx);\n",
                    ctx->rules.buf[i]->data.rule.name, get_prefix(ctx)
                );
            }
            fputs(
                "\n",
                stream
            );
            for (i = 0; i < ctx->rules.len; i++) {
                code_reach_t r;
                generate_t g;
                g.stream = stream;
                g.rule = ctx->rules.buf[i];
                g.label = 0;
                fprintf(
                    stream,
                    "static pcc_thunk_chunk_t *pcc_evaluate_rule_%s(%s_context_t *ctx) {\n",
                    ctx->rules.buf[i]->data.rule.name, get_prefix(ctx)
                );
                fputs(
                    "    pcc_thunk_chunk_t *chunk = pcc_thunk_chunk__create(ctx->auxil);\n"
                    "    chunk->pos = ctx->pos;\n",
                    stream
                );
                if (ctx->rules.buf[i]->data.rule.vars.len) fprintf(
                    stream,
                    "    pcc_value_table__resize(ctx->auxil, &chunk->values, %d);\n",
                    ctx->rules.buf[i]->data.rule.vars.len
                );
                if (ctx->rules.buf[i]->data.rule.capts.len) fprintf(
                    stream,
                    "    pcc_capture_table__resize(ctx->auxil, &chunk->capts, %d);\n",
                    ctx->rules.buf[i]->data.rule.capts.len
                );
                r = generate_code(&g, ctx->rules.buf[i]->data.rule.expr, 0, 4, false);
                fputs(
                    "    return chunk;\n",
                    stream
                );
                if (r != CODE_REACH__ALWAYS_SUCCEED) {
                    fputs(
                        "L0000:;\n"
                        "    pcc_thunk_chunk__destroy(ctx->auxil, chunk);\n"
                        "    return NULL;\n",
                        stream
                    );
                }
                fputs(
                    "}\n"
                    "\n",
                    stream
                );
            }
        }
        fprintf(
            stream,
            "%s_context_t *%s_create(%s%sauxil) {\n",
            get_prefix(ctx), get_prefix(ctx),
            at, ap ? "" : " "
        );
        fputs(
            "    return pcc_context__create(auxil);\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "int %s_parse(%s_context_t *ctx, %s%s*ret) {\n",
            get_prefix(ctx), get_prefix(ctx),
            vt, vp ? "" : " "
        );
        fputs(
            "    pcc_thunk_array_t thunks;\n"
			"    if (ctx->got_error)\n"
			"        return 0;\n"
            "    pcc_thunk_array__init(ctx->auxil, &thunks, PCC_ARRAYSIZE);\n",
            stream
        );
        if (ctx->rules.len > 0) {
            fprintf(
                stream,
                "    if (pcc_apply_rule(ctx, pcc_evaluate_rule_%s, &thunks, ret))\n",
                ctx->rules.buf[0]->data.rule.name
            );
            fputs(
                "        pcc_do_action(ctx, &thunks, ret);\n"
                "    else {\n"
                "        ctx->got_error = true;\n"
                "        PCC_ERROR(ctx->auxil);\n"
                "    }\n"
                "    if (ctx->got_error)\n"
				"        return 0;\n"
                "    pcc_commit_buffer(ctx);\n",
                stream
            );
        }
        fputs(
            "    pcc_thunk_array__term(ctx->auxil, &thunks);\n"
            "    return pcc_refill_buffer(ctx, 1) >= 1;\n"
            "}\n"
            "\n",
            stream
        );
        fprintf(
            stream,
            "void %s_destroy(%s_context_t *ctx) {\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fputs(
            "    pcc_context__destroy(ctx);\n"
            "}\n",
            stream
        );
    }
    {
        fputs(
            "#ifdef __cplusplus\n"
            "extern \"C\" {\n"
            "#endif\n"
            "\n",
            ctx->hfile
        );
        fprintf(
            ctx->hfile,
            "typedef struct %s_context_tag %s_context_t;\n"
            "\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fprintf(
            ctx->hfile,
            "%s_context_t *%s_create(%s%sauxil);\n",
            get_prefix(ctx), get_prefix(ctx),
            at, ap ? "" : " "
        );
        fprintf(
            ctx->hfile,
            "int %s_parse(%s_context_t *ctx, %s%s*ret);\n",
            get_prefix(ctx), get_prefix(ctx),
            vt, vp ? "" : " "
        );
        fprintf(
            ctx->hfile,
            "void %s_destroy(%s_context_t *ctx);\n",
            get_prefix(ctx), get_prefix(ctx)
        );
        fputs(
            "\n"
            "#ifdef __cplusplus\n"
            "}\n"
            "#endif\n",
            ctx->hfile
        );
        fprintf(
            ctx->hfile,
            "\n"
            "#endif /* PCC_INCLUDED__%s */\n",
            ctx->hid
        );
    }
    {
        match_eol(ctx);
        if (!match_eof(ctx)) fputc('\n', stream);
        commit_buffer(ctx);
        while (refill_buffer(ctx, ctx->buffer.max) > 0) {
            int n = (ctx->buffer.buf[ctx->buffer.len - 1] == '\r') ? ctx->buffer.len - 1 : ctx->buffer.len;
            write_text(stream, ctx->buffer.buf, n);
            ctx->bufpos = n;
            commit_buffer(ctx);
        }
    }
    return (ctx->errnum == 0);
}

static void print_version(FILE *output) {
    fprintf(output, "%s version %s\n", g_cmdname, VERSION);
    fprintf(output, "Copyright (c) 2014 Arihiro Yoshida. All rights reserved.\n");
}

static void print_usage(FILE *output) {
    fprintf(output, "Usage: %s [OPTIONS] [FILE]\n", g_cmdname);
    fprintf(output, "Generates a packrat parser for C.\n");
    fprintf(output, "\n");
    fprintf(output, "  -o BASENAME  specify a base name of output source and header files\n");
    fprintf(output, "  -i INCLUDE   put \"#include INCLUDE\" at the head of the source file\n");
    fprintf(output, "  -d           with debug information\n");
    fprintf(output, "  -h           print this help message and exit\n");
    fprintf(output, "  -v           print the version and exit\n");
}

int main(int argc, char **argv) {
    const char *iname = NULL;
    const char *oname = NULL;
    const char *earlyinclude = NULL;
    bool debug = false;
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
        const char *opt_i = NULL;
        bool opt_d = false;
        bool opt_h = false;
        bool opt_v = false;
        int i;
        for (i = 1; i < argc; i++) {
            if (argv[i][0] != '-') {
                break;
            }
            else if (strcmp(argv[i] + 1, "-") == 0) {
                i++; break;
            }
            else if (argv[i][1] == 'o') {
                const char *o = (argv[i][2] != '\0') ? argv[i] + 2 : (++i < argc) ?  argv[i] : NULL;
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
            else if (strcmp(argv[i] + 1, "i") == 0) {
                const char *e = (argv[i][2] != '\0') ? argv[i] + 2 : (++i < argc) ?  argv[i] : NULL;
                if (e == NULL) {
                    print_error("Argument of #include missing\n");
                    fprintf(stderr, "\n");
                    print_usage(stderr);
                    exit(1);
                }
                if (opt_i != NULL) {
                    print_error("Extra argument of #include '%s'\n", e);
                    fprintf(stderr, "\n");
                    print_usage(stderr);
                    exit(1);
                }
                size_t l = strlen(e);
                if ((e[0] != '"' && e[0] != '<') || (e[l - 1] != '"' && e[l - 1] != '>')) {
                    print_error("-i option expects \"FILENAME\" or <FILENAME>: '%s'\n", e);
                    fprintf(stderr, "\n");
                    exit(1);
                }
                opt_i = e;
            }
            else if (strcmp(argv[i] + 1, "d") == 0) {
                opt_d = true;
            }
            else if (strcmp(argv[i] + 1, "h") == 0) {
                opt_h = true;
            }
            else if (strcmp(argv[i] + 1, "v") == 0) {
                opt_v = true;
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
        earlyinclude = opt_i;
        debug = opt_d;
    }
    {
        context_t *ctx = create_context(iname, earlyinclude, oname, debug);
        int b = parse(ctx) && generate(ctx);
        destroy_context(ctx);
        if (!b) exit(10);
    }
    return 0;
}
