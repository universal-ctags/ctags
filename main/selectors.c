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
#include "parse_p.h"
#include "options.h"
#include "selectors.h"
#include "vstring.h"
#include "mio.h"

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

static const char *TR_LISP     = "Lisp";
static const char *TR_LEX      = "LEX";

#define startsWith(line,prefix) \
  (strncmp(line, prefix, strlen(prefix)) == 0? true: false)

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
selectByPickingPerlVersion (MIO *input,
							langType *candidates CTAGS_ATTR_UNUSED,
							unsigned int nCandidates CTAGS_ATTR_UNUSED)
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
selectByObjectiveCAndMatLabKeywords (MIO * input,
									 langType *candidates CTAGS_ATTR_UNUSED,
									 unsigned int nCandidates CTAGS_ATTR_UNUSED)
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
selectByObjectiveCKeywords (MIO * input,
							langType *candidates CTAGS_ATTR_UNUSED,
							unsigned int nCandidates CTAGS_ATTR_UNUSED)
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
selectByArrowOfR (MIO *input,
				  langType *candidates CTAGS_ATTR_UNUSED,
				  unsigned int nCandidates CTAGS_ATTR_UNUSED)
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
	bool * in_rexx_comment = data;

	if (startsWith (line, ":"))
		return TR_DOSBATCH;
	else if (*in_rexx_comment
		 && strstr (line, "*/"))
		return TR_REXX;
	else if (strstr (line, "/*"))
	{
		*in_rexx_comment = true;
		return NULL;
	}
	else
		return NULL;
}

const char *
selectByRexxCommentAndDosbatchLabelPrefix (MIO *input,
										   langType *candidates CTAGS_ATTR_UNUSED,
										   unsigned int nCandidates CTAGS_ATTR_UNUSED)
{
    /* TODO: Ideally opening input should be delayed till
       enable/disable based selection is done. */

    static langType rexx     = LANG_IGNORE;
    static langType dosbatch = LANG_IGNORE;
    bool in_rexx_comment = false;

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

static const char *
tasteLispOrLEXLines (const char *line, void *data CTAGS_ATTR_UNUSED)
{
	if (strcmp(line, "%{\n") == 0
		|| strcmp(line, "%top{\n") == 0
		|| strcmp(line, "%%\n") == 0)
		return TR_LEX;
	return TR_UNKNOWN;
}

const char *
selectLispOrLEXByLEXMarker (MIO *input,
							langType *candidates CTAGS_ATTR_UNUSED,
							unsigned int nCandidates CTAGS_ATTR_UNUSED)
{
	return selectByLines (input, tasteLispOrLEXLines, TR_LISP, NULL);
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

static bool
matchXpathFileSpec (xmlDocPtr doc, xpathFileSpec *spec)
{
	if (spec->rootElementName)
	{
		if (*spec->rootElementName == '\0')
		{
			/* The statement is just for keeping code symmetric.
			   Meaningless examination: a root element is
			   always there.*/
			if (doc->children && doc->children->name)
				return false;
		}
		else if (! (doc->children
					&& doc->children->name
					&& (strcmp (spec->rootElementName, (char *)doc->children->name) == 0)))
			return false;
		else
			verbose ("		Xml[rootElementName]== %s\n",
					 spec->rootElementName);
	}

	if (spec->nameInDTD)
	{
		if (*spec->nameInDTD == '\0')
		{
			if (doc->intSubset && doc->intSubset->name)
				return false;
		}
		else if (! (doc->intSubset
					&& doc->intSubset->name
					&& (strcmp (spec->nameInDTD, (char *)doc->intSubset->name) == 0)))
			return false;
		else
			verbose ("		Xml[nameInDTD]== %s\n",
					 spec->nameInDTD);
	}

	if (spec->externalID)
	{
		if (*spec->externalID == '\0')
		{
			if (doc->intSubset && doc->intSubset->ExternalID)
				return false;
		}
		else if (! (doc->intSubset
					&& doc->intSubset->ExternalID
					&& (strcmp (spec->externalID, (char *)doc->intSubset->ExternalID) == 0)))
			return false;
		else
			verbose ("		Xml[externalID]== %s\n",
					 spec->externalID);

	}

	if (spec->systemID)
	{
		if (*spec->systemID == '\0')
		{
			if (doc->intSubset && doc->intSubset->SystemID)
				return false;
		}
		else if (! (doc->intSubset
					&& doc->intSubset->SystemID
					&& (strcmp (spec->systemID, (char *)doc->intSubset->SystemID) == 0)))
			return false;
		else
			verbose ("		Xml[systemID]== %s\n",
					 spec->systemID);
	}

	if (spec->rootNSPrefix)
	{
		if (*spec->rootNSPrefix == '\0')
		{
			if (doc->children && doc->children->ns && doc->children->ns->prefix)
				return false;
		}
		else if (! (doc->children
					&& doc->children->ns
					&& doc->children->ns->prefix
					&& (strcmp (spec->rootNSPrefix, (char *)doc->children->ns->prefix))))
			return false;
		else
			verbose ("		Xml[rootNSPrefix]== %s\n",
					 spec->rootNSPrefix);
	}

	if (spec->rootNSHref)
	{
		if (*spec->rootNSHref == '\0')
		{
			if (doc->children && doc->children->ns && doc->children->ns->href)
				return false;
		}
		else if (! (doc->children
					&& doc->children->ns
					&& doc->children->ns->href
					&& (strcmp (spec->rootNSHref, (char *)doc->children->ns->href) == 0)))
			return false;
		else
			verbose ("		Xml[rootNSHref]== %s\n",
					 spec->rootNSHref);
	}
	return true;
}

static const char *
selectParserForXmlDoc (xmlDocPtr doc,
					   langType *candidates,
					   unsigned int nCandidates)
{

	unsigned int lang_index;
	bool xml_parser_is_in_candidate = false;;

	verbose ("		Xml[rootElementName]: %s\n",
			 (doc->children && doc->children->name)
			 ? ((char *)doc->children->name): "-");
	verbose ("		Xml[nameInDTD]: %s\n",
			 (doc->intSubset && doc->intSubset->name)
			 ? ((char *)doc->intSubset->name): "-");
	verbose ("		Xml[externalID]: %s\n",
			 (doc->intSubset && doc->intSubset->ExternalID)
			 ? ((char *)doc->intSubset->ExternalID): "-");
	verbose ("		Xml[systemID]: %s\n",
			 (doc->intSubset && doc->intSubset->SystemID)
			 ? ((char *)doc->intSubset->SystemID): "-");
	verbose ("		Xml[rootNSPrefix]: %s\n",
			 (doc->children && doc->children->ns && doc->children->ns->prefix)
			 ? ((char *)doc->children->ns->prefix): "-");
	verbose ("		Xml[rootNSHref]: %s\n",
			 (doc->children && doc->children->ns && doc->children->ns->href)
			 ? ((char *)doc->children->ns->href): "-");

	for (lang_index = 0; lang_index < nCandidates; lang_index++)
	{
		unsigned int spec_index;
		xpathFileSpec* spec;
		unsigned int spec_count;

		verbose ("		lxpath examines %s\n", getLanguageName (candidates[lang_index]));

		spec_count = getXpathFileSpecCount (candidates[lang_index]);
		for (spec_index = 0; spec_index < spec_count; spec_index++)
		{
			spec = getXpathFileSpec (candidates[lang_index], spec_index);
			if (matchXpathFileSpec (doc, spec))
				return getLanguageName (candidates[lang_index]);
		}

		if (strcmp (getLanguageName (candidates[lang_index]), "XML") == 0)
			xml_parser_is_in_candidate = true;
	}

	if (xml_parser_is_in_candidate)
	{
		verbose ("		Use generic XML parser as fallback\n");
		return "XML";
	}

	return NULL;
}

const char *
selectByXpathFileSpec (MIO *input,
					   langType *candidates,
					   unsigned int nCandidates)
{
	xmlDocPtr doc;
	const char *r = NULL;

	doc = xmlParseMIO (input);
	if (doc == NULL)
		return NULL;

	r = selectParserForXmlDoc (doc, candidates, nCandidates);

	if (r == NULL)
		xmlFreeDoc (doc);
	else
		mio_attach_user_data (input,
				      doc,(MIODestroyNotify)xmlFreeDoc);

	return r;
}

#else

const char *
selectByXpathFileSpec (MIO *input,
					   langType *candidates,
					   unsigned int nCandidates)
{
	return NULL;
}

#endif
