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

#include "general.h"

#include "utf8_str.h"

/* computes the length of an UTF-8 string
 * if the string doesn't look like UTF-8, return -1
 * FIXME consider East_Asian_Width Unicode property */
int utf8_strlen (const char *buf, int buf_len)
{
	int len = 0;
	const char *end = buf + buf_len;

	for (len = 0; buf < end; len++)
	{
		/* perform quick and naive validation (no sub-byte checking) */
		if (!(*buf & 0x80))
			buf++;
		else if ((*buf & 0xe0) == 0xc0)
			buf += 2;
		else if ((*buf & 0xf0) == 0xe0)
			buf += 3;
		else if ((*buf & 0xf8) == 0xf0)
			buf += 4;
		else	/* not a valid leading UTF-8 byte, abort */
			return -1;

		if (buf > end)	/* incomplete last byte */
			return -1;
	}

	return len;
}

/* computes the raw buf length of an UTF-8 (ascii excluded) string
 * if the string doesn't look like UTF-8, return -1 */
int utf8_raw_strlen (const char *buf, int buf_len)
{
	int raw_len = 0;
	const char *end = buf + buf_len;

	while (buf < end)
	{
		/* perform quick and naive validation (no sub-byte checking) */
		if (!(*buf & 0x80))
		{
			/* stop at ascii character */
			return raw_len;
		}
		else if ((*buf & 0xe0) == 0xc0)
		{
			buf += 2;
			raw_len += 2;
		}
		else if ((*buf & 0xf0) == 0xe0)
		{
			buf += 3;
			raw_len += 3;
		}
		else if ((*buf & 0xf8) == 0xf0)
		{
			buf += 4;
			raw_len += 4;
		}
		else	/* not a valid leading UTF-8 byte, abort */
			return -1;

		if (buf > end)	/* incomplete last byte */
			return -1;
	}

	return raw_len;
}
