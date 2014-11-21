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
#include "options.h"
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
typedef enum {
	K_IGNORE = -2,
	K_UNDEFINED,
	K_CONSTANT,
	K_EVENT,
	K_FUNCTION,
	K_MODULE,
	K_NET,
	K_PORT,
	K_REGISTER,
	K_TASK,
	K_BLOCK,
	K_CLASS,
	K_INTERFACE,
	K_MODPORT,
	K_PROGRAM
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
	int                 nestLevel;     /* Current nest level */
	verilogKind         lastKind;      /* Kind of last found tag */
} tokenInfo;

/*
 *   DATA DEFINITIONS
 */
static int Ungetc;
static int Lang_verilog;
static int Lang_systemverilog;

static kindOption VerilogKinds [] = {
 { TRUE, 'c', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" },
 { TRUE, 'b', "block",     "blocks" }
};

static kindOption SystemVerilogKinds [] = {
 { TRUE, 'c', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" },
 { TRUE, 'b', "block",     "blocks" },
 { TRUE, 'C', "class",     "classes" },
 { TRUE, 'I', "interface", "interfaces" },
 { TRUE, 'M', "modport",   "modports" },
 { TRUE, 'P', "program",   "programs" }
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
	{ "localparam",K_CONSTANT,  { 1, 1 } },
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
	{ "uwire",     K_NET,       { 1, 1 } },
	{ "wand",      K_NET,       { 1, 1 } },
	{ "wire",      K_NET,       { 1, 1 } },
	{ "wor",       K_NET,       { 1, 1 } },
	{ "begin",     K_BLOCK,     { 1, 1 } },
	{ "end",       K_BLOCK,     { 1, 1 } },
	{ "signed",    K_IGNORE,    { 1, 1 } },
	{ "automatic", K_IGNORE,    { 1, 0 } },
	{ "bit",       K_REGISTER,  { 1, 0 } },
	{ "byte",      K_REGISTER,  { 1, 0 } },
	{ "class",     K_CLASS,     { 1, 0 } },
	{ "int",       K_REGISTER,  { 1, 0 } },
	{ "interface", K_INTERFACE, { 1, 0 } },
	{ "logic",     K_REGISTER,  { 1, 0 } },
	{ "longint",   K_REGISTER,  { 1, 0 } },
	{ "modport",   K_MODPORT,   { 1, 0 } },
	{ "program",   K_PROGRAM,   { 1, 0 } },
	{ "shortint",  K_REGISTER,  { 1, 0 } },
	{ "shortreal", K_REGISTER,  { 1, 0 } },
	{ "static",    K_IGNORE,    { 1, 0 } },
	{ "string",    K_REGISTER,  { 1, 0 } },
	{ "unsigned",  K_IGNORE,    { 1, 0 } }
};

static tokenInfo *currentContext = NULL;

/*
 *   FUNCTION DEFINITIONS
 */

static short isContainer (verilogKind kind)
{
	switch (kind)
	{
		case K_MODULE:
		case K_TASK:
		case K_FUNCTION:
		case K_BLOCK:
		case K_CLASS:
		case K_INTERFACE:
		case K_PROGRAM:
			return TRUE;
		default:
			return FALSE;
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->kind = K_UNDEFINED;
	token->name = vStringNew ();
	token->scope = NULL;
	token->nestLevel = 0;
	token->lastKind = K_UNDEFINED;
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

static void pruneTokens (tokenInfo * token)
{
	while ((token = popToken (token)));
}

static const char *kindName (const verilogKind kind)
{
	if (isLanguage (Lang_systemverilog))
		return SystemVerilogKinds[kind].name;
	else /* isLanguage (Lang_verilog) */
		return VerilogKinds[kind].name;
}

static char kindLetter (const verilogKind kind)
{
	if (isLanguage (Lang_systemverilog))
		return SystemVerilogKinds[kind].letter;
	else /* isLanguage (Lang_verilog) */
		return VerilogKinds[kind].letter;
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
			return EOF;
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
	while (c != EOF && matchLevel > 0);
	return vGetc ();
}

static void skipToEOL (void)
{
	int c;
	do
	{
		c = vGetc ();
	} while (c != EOF && c != '\n');
}

static void skipComments (int c)
{
	int p;

	if (c == '/')
	{
		c = vGetc ();
		if (c == '/')
		{
			skipToEOL ();
		}
		else if (c == '*')
		{
			do
			{
				p = c;
				c = vGetc ();
			} while (c != EOF && p != '*' && c != '/');
		}
		else
		{
			vUngetc (c);
		}
	}
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

static void createContext (tokenInfo *const scope)
{
	if (scope)
	{
		vString *contextName = vStringNew ();

		verbose ("Creating new context %s\n", vStringValue (scope->name));
		/* Determine full context name */
		if (currentContext->kind != K_UNDEFINED)
		{
			vStringCopy (contextName, currentContext->name);
			vStringCatS (contextName, ".");
		}
		vStringCat (contextName, scope->name);
		/* Create context */
		currentContext = pushToken (currentContext, scope);
		vStringCopy (currentContext->name, contextName);
		vStringDelete (contextName);
	}
}

static void createTag (const verilogKind kind, vString *name)
{
	tagEntryInfo tag;

	initTagEntry (&tag, vStringValue (name));
	tag.kindName    = kindName (kind);
	tag.kind        = kindLetter (kind);
	verbose ("Adding tag %s", vStringValue (name));
	if (currentContext->kind != K_UNDEFINED)
	{
		verbose (" to context %s\n", vStringValue (currentContext->name));
		currentContext->lastKind = kind;
		tag.extensionFields.scope [0] = kindName (currentContext->kind);
		tag.extensionFields.scope [1] = vStringValue (currentContext->name);
	}
	verbose ("\n");
	makeTagEntry (&tag);
	if (Option.include.qualifiedTags && currentContext->kind != K_UNDEFINED)
	{
		vString *const scopedName = vStringNew ();

		vStringCopy (scopedName, currentContext->name);
		vStringCatS (scopedName, ".");
		vStringCatS (scopedName, vStringValue (name));
		tag.name = vStringValue (scopedName);

		makeTagEntry (&tag);

		vStringDelete (scopedName);
	}
	if (isContainer (kind))
	{
		tokenInfo *newScope = newToken ();

		vStringCopy (newScope->name, name);
		newScope->kind = kind;
		createContext (newScope);
	}
}

static boolean findBlockName (vString *const name)
{
	int c;

	c = skipWhite (vGetc ());
	if (c == ':')
	{
		c = skipWhite (vGetc ());
		readIdentifier (name, c);
		return (boolean) (vStringLength (name) > 0);
	}
	else
		vUngetc (c);
	return FALSE;
}

static void processBlock (vString *const name, const verilogKind kind)
{
	boolean blockStart = FALSE;
	boolean blockEnd   = FALSE;

	if (strcmp (vStringValue (name), "begin") == 0)
	{
		currentContext->nestLevel++;
		blockStart = TRUE;
	}
	else if (strcmp (vStringValue (name), "end") == 0)
	{
		currentContext->nestLevel--;
		blockEnd = TRUE;
	}

	if (findBlockName (name))
	{
		verbose ("Found block: %s\n", vStringValue (name));
		if (blockStart)
		{
			createTag (kind, name);
			verbose ("Current context %s\n", vStringValue (currentContext->name));
		}
		if (blockEnd && currentContext->kind == K_BLOCK && currentContext->nestLevel <= 1)
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
		}
	}
}

static void tagNameList (const verilogKind kind, int c)
{
	vString *name = vStringNew ();
	verilogKind localKind, nameKind;
	boolean repeat;
	Assert (isIdentifierCharacter (c));
	localKind = kind;
	do
	{ 
		repeat = FALSE;
		if (isIdentifierCharacter (c))
		{
			readIdentifier (name, c);
			/* Check if "name" is in fact a keyword */
			nameKind = (verilogKind) lookupKeyword (vStringValue (name), getSourceLanguage () );
			/* Create tag in case name is not a known kind ... */
			if (nameKind == K_UNDEFINED)
			{
				createTag (localKind, name);
			}
			/* ... or else continue searching for names */
			else
			{
				/* Update local kind unless it's a port or an ignored keyword */
				if (localKind != K_PORT && nameKind != K_IGNORE)
				{
					localKind = nameKind;
				}
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
				/* Skip until end of current name, kind or parameter list definition */
				do
					c = vGetc ();
				while (c != EOF && c != ','  &&  c != ';' && c != ')');
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
	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), getSourceLanguage () );

	/* Search for end of current context to drop respective context */
	if (currentContext->kind != K_UNDEFINED)
	{
		vString *endTokenName = vStringNewInit("end");
		if (currentContext->kind == K_BLOCK && currentContext->nestLevel == 0 && strcmp (vStringValue (name), vStringValue (endTokenName)) == 0)
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
		}
		else
		{
			vStringCatS (endTokenName, kindName (currentContext->kind));
			if (strcmp (vStringValue (name), vStringValue (endTokenName)) == 0)
			{
				verbose ("Dropping context %s\n", vStringValue (currentContext->name));
				currentContext = popToken (currentContext);
			}
		}
		vStringDelete(endTokenName);
	}

	if (kind == K_CONSTANT && vStringItem (name, 0) == '`')
	{
		/* Bug #961001: Verilog compiler directives are line-based. */
		int c = skipWhite (vGetc ());
		readIdentifier (name, c);
		createTag (kind, name);
		/* Skip the rest of the line. */
		do {
			c = vGetc();
		} while (c != EOF && c != '\n');
		vUngetc (c);
	}
	else if (kind == K_BLOCK)
	{
		/* Process begin..end blocks */
		processBlock (name, kind);
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
	int c = '\0';
	currentContext = newToken ();

	while (c != EOF)
	{
		c = vGetc ();
		switch (c)
		{
			case '/':
				skipComments (c);
				break;
			/* Skip interface modport port declarations */
			case '(':
				if (currentContext && currentContext->lastKind == K_MODPORT)
				{
					skipPastMatch ("()");
				}
				break;
			default :
				c = skipWhite (c);
				if (isIdentifierCharacter (c))
				{
					readIdentifier (name, c);
					findTag (name);
				}
		}
	}

	vStringDelete (name);
	pruneTokens (currentContext);
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
	static const char *const extensions [] = { "sv", "svh", "svi", NULL };
	parserDefinition* def = parserNew ("SystemVerilog");
	def->kinds      = SystemVerilogKinds;
	def->kindCount  = KIND_COUNT (SystemVerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initializeSystemVerilog;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
