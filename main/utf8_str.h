/*
*
*   Copyright (c) 2023, Yinzuo Jiang
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines functions for UTF-8 string manipulation.
*   utf8_strlen is derived from parsers/rst.c
*/

#ifndef CTAGS_MAIN_UTF8_STR_H
#define CTAGS_MAIN_UTF8_STR_H

/*
*   FUNCTION PROTOTYPES
*/

extern int utf8_strlen (const char *buf, int buf_len);
extern int utf8_raw_strlen (const char *buf, int buf_len);

#endif /* CTAGS_MAIN_UTF8_STR_H */
