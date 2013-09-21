/*
*   $Id$
* 
*   Copyright (c) 2003, Darren Hiebert
* 
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
* 
*   This module contains functions for generating tags for the Verilog HDL
*   (Hardware Description Language).
* 
*   Language definition documents:
*       http://www.eg.bucknell.edu/~cs320/verilog/verilog-manual.html
*       http://www.sutherland-hdl.com/on-line_ref_guide/vlog_ref_top.html
*       http://www.verilog.com/VerilogBNF.html
*       http://eesun.free.fr/DOC/VERILOG/verilog_manual1.html
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "get.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

/*
*   MACROS
*/
#define NUMBER_LANGUAGES    2   /* Indicates number of defined indexes */
#define IDX_SYSTEMVERILOG   0
#define IDX_VERILOG         1

/*
 *   DATA DECLARATIONS
 */
typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

typedef enum {
	K_UNDEFINED = -1,
	K_CONSTANT,
	K_EVENT,
	K_FUNCTION,
	K_MODULE,
	K_NET,
	K_PORT,
	K_REGISTER,
	K_TASK,
	K_CLASS
} verilogKind;

typedef struct {
	const char *keyword;
	verilogKind kind;
	short isValid [NUMBER_LANGUAGES];
} keywordAssoc;

typedef struct sTokenInfo {
	verilogKind         kind;
	vString*            name;          /* the name of the token */
	struct sTokenInfo*  scope;         /* context of keyword */
} tokenInfo;

/*
 *   DATA DEFINITIONS
 */
static int Ungetc;
static int Lang_verilog;
static int Lang_systemverilog;
static jmp_buf Exception;

static kindOption VerilogKinds [] = {
 { TRUE, 'c', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" }
};

static kindOption SystemVerilogKinds [] = {
 { TRUE, 'k', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" },
 { TRUE, 'c', "class",     "classes" }
};

static const keywordAssoc KeywordTable [] = {
	/*                            SystemVerilog */
	/*                            |  Verilog    */
	/* keyword     keyword ID     |  |          */
	{ "`define",   K_CONSTANT,  { 1, 1 } },
	{ "event",     K_EVENT,     { 1, 1 } },
	{ "function",  K_FUNCTION,  { 1, 1 } },
	{ "inout",     K_PORT,      { 1, 1 } },
	{ "input",     K_PORT,      { 1, 1 } },
	{ "integer",   K_REGISTER,  { 1, 1 } },
	{ "module",    K_MODULE,    { 1, 1 } },
	{ "output",    K_PORT,      { 1, 1 } },
	{ "parameter", K_CONSTANT,  { 1, 1 } },
	{ "real",      K_REGISTER,  { 1, 1 } },
	{ "realtime",  K_REGISTER,  { 1, 1 } },
	{ "reg",       K_REGISTER,  { 1, 1 } },
	{ "specparam", K_CONSTANT,  { 1, 1 } },
	{ "supply0",   K_NET,       { 1, 1 } },
	{ "supply1",   K_NET,       { 1, 1 } },
	{ "task",      K_TASK,      { 1, 1 } },
	{ "time",      K_REGISTER,  { 1, 1 } },
	{ "tri0",      K_NET,       { 1, 1 } },
	{ "tri1",      K_NET,       { 1, 1 } },
	{ "triand",    K_NET,       { 1, 1 } },
	{ "tri",       K_NET,       { 1, 1 } },
	{ "trior",     K_NET,       { 1, 1 } },
	{ "trireg",    K_NET,       { 1, 1 } },
	{ "wand",      K_NET,       { 1, 1 } },
	{ "wire",      K_NET,       { 1, 1 } },
	{ "wor",       K_NET,       { 1, 1 } },
	{ "class",     K_CLASS,     { 1, 0 } },
	{ "logic",     K_REGISTER,  { 1, 0 } }
};

tokenInfo *currentContext = NULL;

/*
 *   FUNCTION DEFINITIONS
 */

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->kind = K_UNDEFINED;
	token->name = vStringNew ();
	token->scope = NULL;
	return token;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->name);
		eFree (token);
	}
}

static tokenInfo *pushToken (tokenInfo * const token, tokenInfo * const tokenPush)
{
	tokenPush->scope = token;
	return tokenPush;
}

static tokenInfo *popToken (tokenInfo * const token)
{
	tokenInfo *localToken;
	if (token != NULL)
	{
		localToken = token->scope;
		deleteToken (token);
		return localToken;
	}
	return NULL;
}

static const char *kindName (const verilogKind kind)
{
	if (isLanguage (Lang_verilog))
		return VerilogKinds[kind].name;
	else if (isLanguage (Lang_systemverilog))
		return SystemVerilogKinds[kind].name;
}

static char kindLetter (const verilogKind kind)
{
	if (isLanguage (Lang_verilog))
		return VerilogKinds[kind].letter;
	else if (isLanguage (Lang_systemverilog))
		return SystemVerilogKinds[kind].letter;
}

static void buildKeywordHash (const langType language, unsigned int idx)
{
	size_t i;
	const size_t count = 
			sizeof (KeywordTable) / sizeof (KeywordTable [0]);
	for (i = 0  ;  i < count  ;  ++i)
	{
		const keywordAssoc *p = &KeywordTable [i];
		if (p->isValid [idx])
			addKeyword (p->keyword, language, (int) p->kind);
	}
}

static void initializeVerilog (const langType language)
{
	Lang_verilog = language;
	buildKeywordHash (language, IDX_VERILOG);
}

static void initializeSystemVerilog (const langType language)
{
	Lang_systemverilog = language;
	buildKeywordHash (language, IDX_SYSTEMVERILOG);
}

static void vUngetc (int c)
{
	Assert (Ungetc == '\0');
	Ungetc = c;
}

static int vGetc (void)
{
	int c;
	if (Ungetc == '\0')
		c = fileGetc ();
	else
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	if (c == '/')
	{
		int c2 = fileGetc ();
		if (c2 == EOF)
			longjmp (Exception, (int) ExceptionEOF);
		else if (c2 == '/')  /* strip comment until end-of-line */
		{
			do
				c = fileGetc ();
			while (c != '\n'  &&  c != EOF);
		}
		else if (c2 == '*')  /* strip block comment */
		{
			c = skipOverCComment();
		}
		else
		{
			fileUngetc (c2);
		}
	}
	else if (c == '"')  /* strip string contents */
	{
		int c2;
		do
			c2 = fileGetc ();
		while (c2 != '"'  &&  c2 != EOF);
		c = '@';
	}
	if (c == EOF)
		longjmp (Exception, (int) ExceptionEOF);
	return c;
}

static boolean isIdentifierCharacter (const int c)
{
	return (boolean)(isalnum (c)  ||  c == '_'  ||  c == '`');
}

static int skipWhite (int c)
{
	while (isspace (c))
		c = vGetc ();
	return c;
}

static int skipPastMatch (const char *const pair)
{
	const int begin = pair [0], end = pair [1];
	int matchLevel = 1;
	int c;
	do
	{
		c = vGetc ();
		if (c == begin)
			++matchLevel;
		else if (c == end)
			--matchLevel;
	}
	while (matchLevel > 0);
	return vGetc ();
}

static boolean readIdentifier (vString *const name, int c)
{
	vStringClear (name);
	if (isIdentifierCharacter (c))
	{
		while (isIdentifierCharacter (c))
		{
			vStringPut (name, c);
			c = vGetc ();
		}
		vUngetc (c);
		vStringTerminate (name);
	}
	return (boolean)(vStringLength (name) > 0);
}

static vString *genContext ()
{
	vString *context, *catNames;
	tokenInfo *current;
	if (currentContext)
	{
		context = vStringNew ();
		vStringCopy (context, currentContext->name);
		current = currentContext->scope;
		while (current) {
			catNames = vStringNew ();

			vStringCopy (catNames, current->name);
			vStringCatS (catNames, ".");
			vStringCat (catNames, context);
			vStringCopy (context, catNames);

			current = current->scope;
			vStringDelete (catNames);
		}
		return context;
	}
	else
		return NULL;
}

static void tagNameList (const verilogKind kind, int c)
{
	vString *name = vStringNew ();
	vString *scopedName;
	verilogKind localKind;
	tagEntryInfo tag;
	boolean repeat;
	Assert (isIdentifierCharacter (c));
	do
	{ 
		repeat = FALSE;
		if (isIdentifierCharacter (c))
		{
			readIdentifier (name, c);
			/* Check if "name" is in fact a keyword */
			localKind = (verilogKind) lookupKeyword (vStringValue (name), getSourceLanguage () );
			if (kind != K_PORT || localKind == K_UNDEFINED)
			{
				initTagEntry (&tag, vStringValue (name));
				tag.kindName    = kindName (kind);
				tag.kind        = kindLetter (kind);
				if (kind != K_MODULE && currentContext)
				{
					tag.extensionFields.scope [0] = kindName (currentContext->kind);
					tag.extensionFields.scope [1] = vStringValue (currentContext->name);
				}
				makeTagEntry (&tag);
				if (kind == K_MODULE || kind == K_FUNCTION || kind == K_TASK || kind == K_CLASS)
				{
					tokenInfo *newScope = newToken ();
					vString *contextName;
					vStringCopy (newScope->name, name);
					newScope->kind = kind;
					currentContext = pushToken (currentContext, newScope);
					contextName = genContext ();
					vStringCopy (currentContext->name, contextName);
					vStringDelete (contextName);
				}
			}
			else
			{
				repeat = TRUE;
			}
		}
		else
			break;
		c = skipWhite (vGetc ());
		if (c == '[')
			c = skipPastMatch ("[]");
		c = skipWhite (c);
		if (c == '=')
		{
			c = skipWhite (vGetc ());
			if (c == '{')
				skipPastMatch ("{}");
			else
			{
				do
					c = vGetc ();
				while (c != ','  &&  c != ';');
			}
		}
		if (c == ',')
		{
			c = skipWhite (vGetc ());
			repeat = TRUE;
		}
	} while (repeat);
	vStringDelete (name);
	vUngetc (c);
}

static void findTag (vString *const name)
{
	/* Search for end of current context to drop respective context */
	if (currentContext)
	{
		vString *endTokenName = vStringNewInit("end");
		vStringCatS (endTokenName, kindName (currentContext->kind));
		if (strcmp (vStringValue (name), vStringValue (endTokenName)) == 0)
		{
			currentContext = popToken (currentContext);
		}
	}

	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), getSourceLanguage () );
	if (kind == K_CONSTANT && vStringItem (name, 0) == '`')
	{
		/* Bug #961001: Verilog compiler directives are line-based. */
		int c = skipWhite (vGetc ());
		readIdentifier (name, c);
		if (isLanguage (Lang_verilog))
			makeSimpleTag (name, VerilogKinds, kind);
		else if (isLanguage (Lang_systemverilog))
			makeSimpleTag (name, SystemVerilogKinds, kind);
		/* Skip the rest of the line. */
		do {
			c = vGetc();
		} while (c != '\n');
		vUngetc (c);
	}
	else if (kind != K_UNDEFINED)
	{
		int c = skipWhite (vGetc ());

		/* Many keywords can have bit width.
		*   reg [3:0] net_name;
		*   inout [(`DBUSWIDTH-1):0] databus;
		*/
		if (c == '(')
			c = skipPastMatch ("()");
		c = skipWhite (c);
		if (c == '[')
			c = skipPastMatch ("[]");
		c = skipWhite (c);
		if (c == '#')
		{
			c = vGetc ();
			if (c == '(')
				c = skipPastMatch ("()");
		}
		c = skipWhite (c);
		if (isIdentifierCharacter (c))
			tagNameList (kind, c);
	}
}

static void findVerilogTags (void)
{
	vString *const name = vStringNew ();
	volatile boolean newStatement = TRUE;
	volatile int c = '\0';
	exception_t exception = (exception_t) setjmp (Exception);

	if (exception == ExceptionNone) while (c != EOF)
	{
		c = vGetc ();
		switch (c)
		{
			case ';':
			case '(':
			case '\n':
				newStatement = TRUE;
				break;

			case ' ':
			case '\t':
				break;

			default:
				if (newStatement && readIdentifier (name, c))
					findTag (name);
				newStatement = FALSE;
				break;
		}
	}
	deleteToken (currentContext);
	currentContext = NULL;
}

extern parserDefinition* VerilogParser (void)
{
	static const char *const extensions [] = { "v", NULL };
	parserDefinition* def = parserNew ("Verilog");
	def->kinds      = VerilogKinds;
	def->kindCount  = KIND_COUNT (VerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initializeVerilog;
	return def;
}

extern parserDefinition* SystemVerilogParser (void)
{
	static const char *const extensions [] = { "sv", "svh", NULL };
	parserDefinition* def = parserNew ("SystemVerilog");
	def->kinds      = SystemVerilogKinds;
	def->kindCount  = KIND_COUNT (SystemVerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initializeSystemVerilog;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
