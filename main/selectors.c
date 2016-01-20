/*
 * Copyright (c) 2015, Dmitri Tikhonov
 *
 * This source code is released for free distribution under the terms of the
 * GNU General Public License version 2 or (at your option) any later version.
 *
 * selectors.c -- routines for selecting a language
 */

#include "general.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "parse.h"
#include "selectors.h"
#include "vstring.h"

static const char *TR_UNKNOWN = NULL;
static const char *TR_PERL5   = "Perl";
static const char *TR_PERL6   = "Perl6";

static const char *TR_OBJC    = "ObjectiveC";
static const char *TR_MATLAB  = "MatLab";

static const char *TR_CPP     = "C++";


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

static const char *
tasteObjectiveC (const char *line)
{
    if (startsWith (line, "#import")
	|| startsWith (line, "@interface ")
	|| startsWith (line, "@implementation ")
	|| startsWith (line, "@protocol "))
	return TR_OBJC;
    return NULL;
}

const char *
selectByObjectiveCKeywords (FILE * input)
{
    /* TODO: Ideally opening input should be delayed til
       enable/disable based selection is done. */

    static langType objc = LANG_IGNORE;
    static langType cpp = LANG_IGNORE;

    if (objc == LANG_IGNORE)
	objc = getNamedLanguage (TR_OBJC, 0);

    if (cpp == LANG_IGNORE)
	cpp = getNamedLanguage (TR_CPP, 0);

    Assert (0 <= objc);
    Assert (0 <= cpp);

    if (! isLanguageEnabled (objc))
	return TR_CPP;
    else if (! isLanguageEnabled (cpp))
	return TR_OBJC;

    return selectByLines (input, tasteObjectiveC, TR_CPP);
}

#ifdef HAVE_LIBXML

#include <libxml/xpath.h>
#include <libxml/tree.h>


static xmlDocPtr
xmlParseFILE (FILE *input)
{
	vString *buf;
	xmlDocPtr doc;

	buf = vStringNewFile (input);
	if (!buf)
		return NULL;

	doc = xmlParseMemory(vStringValue(buf), vStringLength (buf));

	vStringDelete (buf);

	return doc;
}

static const char *
selectParserForXmlDoc (xmlDocPtr doc)
{
	/* These conditions should be part of parsers. */
	if (doc->children
	    && doc->intSubset
	    && doc->children->name && doc->intSubset->name
	    && (strcmp ((const char *)doc->children->name, (const char *)doc->intSubset->name) == 0)
	    && doc->intSubset->ExternalID
	    && (strcmp ((const char *)doc->intSubset->ExternalID,
			"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN") == 0)
	    && doc->intSubset->SystemID
	    && (strcmp ((const char *)doc->intSubset->SystemID,
			"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd") == 0))
	{
		/* <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
		   "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
		   <node ... */
		return "DBusIntrospect";
	}

	return NULL;
}

const char *
selectByDTD (FILE *input)
{
	xmlDocPtr doc;
	const char *r = NULL;

	doc = xmlParseFILE (input);
	if (doc == NULL)
		return NULL;

	r = selectParserForXmlDoc (doc);

	xmlFreeDoc (doc);

	return r;
}
#endif
