/*
 * selectors.c -- routines for selecting a language
 */

#include "general.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "selectors.h"

static const char *TR_UNKNOWN = NULL;
static const char *TR_PERL5   = "Perl";
static const char *TR_PERL6   = "Perl6";

static const char *TR_OBJC    = "ObjectiveC";
static const char *TR_MATLAB  = "MatLab";

#define startsWith(line,prefix) \
  (strncmp(line, prefix, strlen(prefix)) == 0? TRUE: FALSE)

static const char *selectByLines (FILE *input,
				  const char* (* lineTaster) (const char *),
				  const char* defaultLang)
{
    char line[0x800];
    while (fgets(line, sizeof(line), input)) {
	const char *lang = lineTaster (line);
	if (lang)
	    return lang;
    }
    return defaultLang;
}

/* Returns "Perl" or "Perl6" or NULL if it does not taste like anything */
static const char *
tastePerlLine (const char *line)
{
    while (isspace(*line))
        ++line;
#define STRLEN(s) (sizeof(s) - 1)
/* Assume the first character has been checked: */
#define CHECK_PART(line, s) (    \
    0 == strncmp((line) + 1, (s) + 1, STRLEN(s) - 1) && \
    !isalnum((line)[STRLEN(s)]))
    switch (line[0]) {
        case '#':       /* TODO: taste modeline */
        case '\0':
            return TR_UNKNOWN;
        case '=':
            if (CHECK_PART(line, "=head1"))
                return TR_PERL5;
            if (CHECK_PART(line, "=head2"))
                return TR_PERL5;
            break;
        case 'c':
            if (CHECK_PART(line, "class"))
                return TR_PERL6;
            break;
        case 'g':
            if (CHECK_PART(line, "grammar"))
                return TR_PERL6;
            break;
        case 'm':
            /* TODO: my may be many things: class, role, etc. */
            if (CHECK_PART(line, "my class"))
                return TR_PERL6;
            if (CHECK_PART(line, "method"))
                return TR_PERL6;
            if (CHECK_PART(line, "multi"))
                return TR_PERL6;
            break;
        case 'n':
            if (CHECK_PART(line, "need"))
                return TR_PERL6;
            break;
        case 'p':
            if (CHECK_PART(line, "package"))
                return TR_PERL5;
            break;
        case 'r':
            if (CHECK_PART(line, "role"))
                return TR_PERL6;
            if (CHECK_PART(line, "require 5"))
                return TR_PERL5;
            break;
        case 'u':
            if (CHECK_PART(line, "unit"))
                return TR_PERL6;
            if (CHECK_PART(line, "use v6"))
                return TR_PERL6;
            if (CHECK_PART(line, "use nqp"))
                return TR_PERL5;
            if (CHECK_PART(line, "use warnings"))
                return TR_PERL5;
            break;
    }
#undef CHECK_PART
    return TR_UNKNOWN;
}

const char *
selectByPickingPerlVersion (FILE *input)
{
    /* Default to Perl 5 */
    return selectByLines (input, tastePerlLine, TR_PERL5);
}

static const char *
tasteObjectiveCOrMatLabLines (const char *line)
{
    if (startsWith (line, "% ")
	|| startsWith (line, "%{"))
	return TR_MATLAB;
    else if (startsWith (line, "// ")
	     || startsWith (line, "/* "))
	return TR_OBJC;
    else if (startsWith (line, "#include")
	     || startsWith (line, "#import")
	     || startsWith (line, "#define ")
	     || startsWith (line, "#ifdef "))
	return TR_OBJC;
    else if (startsWith (line, "@interface ")
	     || startsWith (line, "@implementation ")
	     || startsWith (line, "@protocol "))
	return TR_OBJC;
    else if (startsWith (line, "struct ")
	     || startsWith (line, "union ")
	     || startsWith (line, "typedef "))
	return TR_OBJC;
    else {
	if (startsWith (line, "function ")) {
	    const char *p = line + strlen ("function ");
	    while (isspace(*p))
		p++;
	    if (*p != '\0' && *p != '(')
		return TR_MATLAB;
	}
    }
    return NULL;
}

const char *
selectByObjectiveCAndMatLabKeywords (FILE * input)
{
    return selectByLines (input, tasteObjectiveCOrMatLabLines, NULL);
}
