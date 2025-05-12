/*
*   Copyright (c) 2025, Red Hat, Inc.
*   Copyright (c) 2025, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.

*/

/*
*   INCLUDE FILES
*/
#include "general.h"

#include "routines.h"
#include "rexprcode_p.h"

#include <regex.h>
#include <string.h>

/*
*   DATA DECLARATIONS
*/
struct rExprCode {
	regex_t *code;
	char *src;
	bool iCase;
};

/*
*   FUNCTION DECLARATIONS
*/
extern const char *rExprCodeGetSource (const struct rExprCode *rxcode)
{
	return rxcode->src;
}

extern bool rExprCodeGetICase (const struct rExprCode *rxcode)
{
	return rxcode->iCase;
}

extern vString *rExprCodeNewEncodedSource (const struct rExprCode *rxcode)
{
	vString *encoded_src = vStringNew();

	vStringPut (encoded_src, '%');

	for (const char *c = rExprCodeGetSource (rxcode); *c != '\0'; c++)
	{
		if (*c == '%')
			vStringPut (encoded_src, '\\');
		vStringPut (encoded_src, *c);
	}

	vStringPut (encoded_src, '%');
	if (rExprCodeGetICase (rxcode))
		vStringPut (encoded_src, 'i');

	return encoded_src;
}

extern struct rExprCode *rExprCodeNew(const char *rxsrc, bool iCase)
{
	regex_t *regex_code = xMalloc (1, regex_t);
	int errcode = regcomp (regex_code, rxsrc,
						   REG_EXTENDED|REG_NOSUB|(iCase? REG_ICASE: 0));
	if (errcode != 0)
	{
		char errmsg[256];
		regerror (errcode, regex_code, errmsg, sizeof(errmsg));
		error (WARNING, "regcomp: %s", errmsg);
		regfree (regex_code);
		eFree (regex_code);
		return NULL;
	}

	struct rExprCode *rxcode = xMalloc (1, struct rExprCode);

	rxcode->code = regex_code;
	rxcode->src = eStrdup (rxsrc);
	rxcode->iCase = iCase;

	return rxcode;
}

extern void rExprCodeDelete (struct rExprCode *rxcode)
{
	regfree (rxcode->code);
	eFree (rxcode->code);
	eFree (rxcode->src);
	eFree (rxcode);
}

extern bool rExprCodeMatch (struct rExprCode *rxcode, const char *fname)
{
	return (regexec (rxcode->code, fname, 0, 0, 0) == 0);
}
