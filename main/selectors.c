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
#include "options.h"
#include "selectors.h"
#include "vstring.h"

static const char *TR_UNKNOWN = NULL;
static const char *TR_PERL5   = "Perl";
static const char *TR_PERL6   = "Perl6";

static const char *TR_OBJC    = "ObjectiveC";
static const char *TR_MATLAB  = "MatLab";

static const char *TR_CPP     = "C++";

static const char *TR_R       = "R";
static const char *TR_ASM     = "Asm";

static const char *TR_REXX     = "REXX";
static const char *TR_DOSBATCH = "DosBatch";

#define startsWith(line,prefix) \
  (strncmp(line, prefix, strlen(prefix)) == 0? TRUE: FALSE)

static const char *selectByLines (MIO *input,
				  const char* (* lineTaster) (const char *, void *),
				  const char* defaultLang,
				  void *userData)
{
    char line[0x800];
    while (mio_gets(input, line, sizeof(line))) {
	const char *lang = lineTaster (line, userData);
	if (lang)
	    return lang;
    }
    return defaultLang;
}

/* Returns "Perl" or "Perl6" or NULL if it does not taste like anything */
static const char *
tastePerlLine (const char *line, void *data CTAGS_ATTR_UNUSED)
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
selectByPickingPerlVersion (MIO *input)
{
    /* Default to Perl 5 */
    return selectByLines (input, tastePerlLine, TR_PERL5, NULL);
}

static const char *
tasteObjectiveCOrMatLabLines (const char *line, void *data CTAGS_ATTR_UNUSED)
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
selectByObjectiveCAndMatLabKeywords (MIO * input)
{
    return selectByLines (input, tasteObjectiveCOrMatLabLines,
			  NULL, NULL);
}

static const char *
tasteObjectiveC (const char *line, void *data CTAGS_ATTR_UNUSED)
{
    if (startsWith (line, "#import")
	|| startsWith (line, "@interface ")
	|| startsWith (line, "@implementation ")
	|| startsWith (line, "@protocol "))
	return TR_OBJC;
    return NULL;
}

const char *
selectByObjectiveCKeywords (MIO * input)
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

    return selectByLines (input, tasteObjectiveC, TR_CPP,
			  NULL);
}

static const char *
tasteR (const char *line, void *data CTAGS_ATTR_UNUSED)
{
	/* As far as reading test cases in GNU assembler,
	   assembly language for d10v and d30v processors
	   uses "<-" as part its syntax. I cannot find better
	   hint for distinguishing between the assembly
	   language and R.
	   ----
	   binutils-2.15.92.0.2/gas/testsuite/gas/d30v/mul.s */
	return strstr (line, "<-")? TR_R: NULL;
}

const char *
selectByArrowOfR (MIO *input)
{
    /* TODO: Ideally opening input should be delayed till
       enable/disable based selection is done. */

    static langType R   = LANG_IGNORE;
    static langType Asm = LANG_IGNORE;

    if (R == LANG_IGNORE)
	    R = getNamedLanguage (TR_R, 0);

    if (Asm == LANG_IGNORE)
	    Asm = getNamedLanguage (TR_ASM, 0);

    Assert (0 <= R);
    Assert (0 <= Asm);

    if (! isLanguageEnabled (R))
	    return TR_ASM;
    else if (! isLanguageEnabled (Asm))
	    return TR_R;

    return selectByLines (input, tasteR, NULL,
			  NULL);
}

static const char *
tasteREXXOrDosBatch (const char *line, void *data)
{
	boolean * in_rexx_comment = data;

	if (startsWith (line, ":"))
		return TR_DOSBATCH;
	else if (*in_rexx_comment
		 && strstr (line, "*/"))
		return TR_REXX;
	else if (strstr (line, "/*"))
	{
		*in_rexx_comment = TRUE;
		return NULL;
	}
	else
		return NULL;
}

const char *
selectByRexxCommentAndDosbatchLabelPrefix (MIO *input)
{
    /* TODO: Ideally opening input should be delayed till
       enable/disable based selection is done. */

    static langType rexx     = LANG_IGNORE;
    static langType dosbatch = LANG_IGNORE;
    boolean in_rexx_comment = FALSE;

    if (rexx == LANG_IGNORE)
	    rexx = getNamedLanguage (TR_R, 0);

    if (dosbatch == LANG_IGNORE)
	    dosbatch = getNamedLanguage (TR_DOSBATCH, 0);

    Assert (0 <= rexx);
    Assert (0 <= dosbatch);

    if (! isLanguageEnabled (rexx))
	    return TR_DOSBATCH;
    else if (! isLanguageEnabled (dosbatch))
	    return TR_REXX;

    return selectByLines (input, tasteREXXOrDosBatch,
			  NULL, &in_rexx_comment);
}

#ifdef HAVE_LIBXML

#include <libxml/xpath.h>
#include <libxml/tree.h>

static void suppressWarning (void *ctx CTAGS_ATTR_UNUSED, const char *msg CTAGS_ATTR_UNUSED, ...)
{
}

static xmlDocPtr
xmlParseMIO (MIO *input)
{
	const unsigned char *buf;
	size_t len;

	buf = mio_memory_get_data (input, &len);
	Assert (buf);

	xmlSetGenericErrorFunc (NULL, suppressWarning);
	xmlLineNumbersDefault (1);
	return xmlParseMemory((const char *)buf, len);
}

static const char *
selectParserForXmlDoc (xmlDocPtr doc)
{
	if (doc && doc->children && doc->children->name)
		verbose ("		Xml[root name]: %s\n", doc->children->name);
	if (doc && doc->intSubset && doc->intSubset->ExternalID)
		verbose ("		Xml[ExternalID]: %s\n", doc->intSubset->ExternalID);
	if (doc && doc->intSubset && doc->intSubset->SystemID)
		verbose ("		Xml[SystemID]: %s\n", doc->intSubset->SystemID);
	if (doc && doc->children && doc->children->ns && doc->children->ns->href)
		verbose ("		Xml[NS]: %s\n", doc->children->ns->href);

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
	else if (doc->children
		 && doc->children->name
		 && (strcmp ((const char*)doc->children->name, "project") == 0))
	{
		if ((doc->children->ns != NULL)
		    && doc->children->ns->href != NULL
		    && (strcmp ((const char *)doc->children->ns->href,
				"http://maven.apache.org/POM/4.0.0") == 0))
			return "Maven2";
		else
			return "Ant";
	}

	return NULL;
}

const char *
selectByDTD (MIO *input)
{
	xmlDocPtr doc;
	const char *r = NULL;

	doc = xmlParseMIO (input);
	if (doc == NULL)
		return NULL;

	r = selectParserForXmlDoc (doc);

	if (r == NULL)
		xmlFreeDoc (doc);
	else
		mio_attach_user_data (input,
				      doc,(MIODestroyNotify)xmlFreeDoc);

	return r;
}
#endif
