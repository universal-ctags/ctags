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
	K_ASSERTION,
	K_CLASS,
	K_INTERFACE,
	K_MODPORT,
	K_PROGRAM,
	K_PROPERTY
} verilogKind;

typedef struct {
	const char *keyword;
	verilogKind kind;
	short isValid [NUMBER_LANGUAGES];
} keywordAssoc;

typedef struct sTokenInfo {
	verilogKind         kind;
	vString*            name;          /* the name of the token */
	unsigned long       lineNumber;    /* line number where token was found */
	fpos_t              filePosition;  /* file position where token was found */
	struct sTokenInfo*  scope;         /* context of keyword */
	int                 nestLevel;     /* Current nest level */
	verilogKind         lastKind;      /* Kind of last found tag */
	vString*            blockName;     /* Current block name */
	boolean             singleStat;    /* Single statement (ends at next semi-colon) */
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
 { TRUE, 'A', "assert",    "assertions" },
 { TRUE, 'C', "class",     "classes" },
 { TRUE, 'I', "interface", "interfaces" },
 { TRUE, 'M', "modport",   "modports" },
 { TRUE, 'P', "program",   "programs" },
 { TRUE, 'R', "property",  "properties" }
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
	{ "assert",    K_ASSERTION, { 1, 0 } },
	{ "assume",    K_ASSERTION, { 1, 0 } },
	{ "bit",       K_REGISTER,  { 1, 0 } },
	{ "byte",      K_REGISTER,  { 1, 0 } },
	{ "class",     K_CLASS,     { 1, 0 } },
	{ "cover",     K_ASSERTION, { 1, 0 } },
	{ "extern",    K_IGNORE,    { 1, 0 } },
	{ "int",       K_REGISTER,  { 1, 0 } },
	{ "interface", K_INTERFACE, { 1, 0 } },
	{ "local",     K_IGNORE,    { 1, 0 } },
	{ "logic",     K_REGISTER,  { 1, 0 } },
	{ "longint",   K_REGISTER,  { 1, 0 } },
	{ "modport",   K_MODPORT,   { 1, 0 } },
	{ "program",   K_PROGRAM,   { 1, 0 } },
	{ "property",  K_PROPERTY,  { 1, 0 } },
	{ "shortint",  K_REGISTER,  { 1, 0 } },
	{ "shortreal", K_REGISTER,  { 1, 0 } },
	{ "static",    K_IGNORE,    { 1, 0 } },
	{ "string",    K_REGISTER,  { 1, 0 } },
	{ "unsigned",  K_IGNORE,    { 1, 0 } },
	{ "virtual",   K_IGNORE,    { 1, 0 } },
	{ "void",      K_IGNORE,    { 1, 0 } }
};

static tokenInfo *currentContext = NULL;

/*
 *   FUNCTION DEFINITIONS
 */

static short isContainer (tokenInfo const* token)
{
	switch (token->kind)
	{
		case K_MODULE:
		case K_TASK:
		case K_FUNCTION:
		case K_BLOCK:
		case K_CLASS:
		case K_INTERFACE:
		case K_PROGRAM:
		case K_PROPERTY:
			return TRUE;
		default:
			return FALSE;
	}
}

static short hasSimplePortList (tokenInfo const* token)
{
	switch (token->kind)
	{
		case K_TASK:
		case K_FUNCTION:
		case K_CLASS:
		case K_INTERFACE:
		case K_PROGRAM:
		case K_PROPERTY:
			return TRUE;
		default:
			return FALSE;
	}
}

static short isSingleStatement (tokenInfo const* token)
{
	if (strcmp (vStringValue (token->name), "extern") == 0 )
	{
		return TRUE;
	} else {
		return FALSE;
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->kind = K_UNDEFINED;
	token->name = vStringNew ();
	token->lineNumber = getSourceLineNumber ();
	token->filePosition = getInputFilePosition ();
	token->scope = NULL;
	token->nestLevel = 0;
	token->lastKind = K_UNDEFINED;
	token->blockName = vStringNew ();
	token->singleStat = FALSE;
	return token;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->name);
		vStringDelete (token->blockName);
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

static void skipToSemiColon (void)
{
	int c;
	do
	{
		c = vGetc ();
	} while (c != EOF && c != ';');
}

static boolean readIdentifier (tokenInfo *const token, int c)
{
	vStringClear (token->name);
	if (isIdentifierCharacter (c))
	{
		while (isIdentifierCharacter (c))
		{
			vStringPut (token->name, c);
			c = vGetc ();
		}
		vUngetc (c);
		vStringTerminate (token->name);
		token->lineNumber = getSourceLineNumber ();
		token->filePosition = getInputFilePosition ();
	}
	return (boolean)(vStringLength (token->name) > 0);
}

static verilogKind getKind (tokenInfo *const token)
{
	return (verilogKind) lookupKeyword (vStringValue (token->name), getSourceLanguage () );
}

static void updateKind (tokenInfo *const token)
{
	token->kind = getKind (token);
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

static void dropContext (tokenInfo *const token)
{
	verbose ("current context %s; context kind %0d; nest level %0d\n", vStringValue (currentContext->name), currentContext->kind, currentContext->nestLevel);
	vString *endTokenName = vStringNewInit("end");
	if (currentContext->kind == K_BLOCK && currentContext->nestLevel == 0 && strcmp (vStringValue (token->name), vStringValue (endTokenName)) == 0)
	{
		verbose ("Dropping context %s\n", vStringValue (currentContext->name));
		currentContext = popToken (currentContext);
	}
	else
	{
		vStringCatS (endTokenName, kindName (currentContext->kind));
		if (strcmp (vStringValue (token->name), vStringValue (endTokenName)) == 0)
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
		}
	}
	vStringDelete(endTokenName);
}


static void createTag (tokenInfo *const token)
{
	tagEntryInfo tag;

	initTagEntryFull(
			&tag,
			vStringValue (token->name),
			token->lineNumber,
			getSourceLanguageName (),
			token->filePosition,
			getSourceFileTagPath ()
			);
	tag.kindName    = kindName (token->kind);
	tag.kind        = kindLetter (token->kind);
	verbose ("Adding tag %s (kind %d)", vStringValue (token->name), token->kind);
	if (currentContext->kind != K_UNDEFINED)
	{
		verbose (" to context %s\n", vStringValue (currentContext->name));
		currentContext->lastKind = token->kind;
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
		vStringCatS (scopedName, vStringValue (token->name));
		tag.name = vStringValue (scopedName);

		makeTagEntry (&tag);

		vStringDelete (scopedName);
	}
	if (isContainer (token))
	{
		tokenInfo *newScope = newToken ();

		vStringCopy (newScope->name, token->name);
		newScope->kind = token->kind;
		createContext (newScope);
	}
}

static boolean findBlockName (tokenInfo *const token)
{
	int c;

	c = skipWhite (vGetc ());
	if (c == ':')
	{
		c = skipWhite (vGetc ());
		readIdentifier (token, c);
		return (boolean) (vStringLength (token->name) > 0);
	}
	else
		vUngetc (c);
	return FALSE;
}

static void processBlock (tokenInfo *const token)
{
	boolean blockStart = FALSE;
	boolean blockEnd   = FALSE;

	if (strcmp (vStringValue (token->name), "begin") == 0)
	{
		currentContext->nestLevel++;
		blockStart = TRUE;
	}
	else if (strcmp (vStringValue (token->name), "end") == 0)
	{
		currentContext->nestLevel--;
		blockEnd = TRUE;
	}

	if (findBlockName (token))
	{
		verbose ("Found block: %s\n", vStringValue (token->name));
		if (blockStart)
		{
			createTag (token);
			verbose ("Current context %s\n", vStringValue (currentContext->name));
		}
		if (blockEnd && currentContext->kind == K_BLOCK && currentContext->nestLevel <= 1)
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
		}
	}
}

static void processPortList (int c)
{
	if ((c = skipWhite (c)) == '(')
	{
		tokenInfo *token = newToken ();

		/* Get next non-whitespace character after ( */
		c = skipWhite (vGetc ());

		while (c != ';' && c != EOF)
		{
			if (c == '[')
			{
				c = skipPastMatch ("[]");
			}
			else if (c == '(')
			{
				c = skipPastMatch ("()");
			}
			else if (c == '{')
			{
				c = skipPastMatch ("{}");
			}
			else if (c == '=')
			{
				/* Search for next port or end of port declaration */
				while (c != ',' && c != ')' && c != EOF)
				{
					c = skipWhite (vGetc ());
				}
			}
			else if (isIdentifierCharacter (c))
			{
				readIdentifier (token, c);
				updateKind (token);
				if (token->kind == K_UNDEFINED)
				{
					/* Only add port name if it is the last keyword.
					 * First keyword can be a dynamic type, like a class name */
					c = skipWhite (vGetc ());
					if (! isIdentifierCharacter (c))
					{
						verbose ("Found port: %s\n", vStringValue (token->name));
						token->kind = K_PORT;
						createTag (token);
					}
				}
				else
				{
					c = skipWhite (vGetc ());
				}
			}
			else
			{
				c = skipWhite (vGetc ());
			}
		}

		if (! isIdentifierCharacter (c)) vUngetc (c);

		deleteToken (token);
	}
}

static void processFunction (tokenInfo *const token)
{
	int c;

	/* Search for function name
	 * Last identifier found before a '(' or a ';' is the function name */
	c = skipWhite (vGetc ());
	do
	{
		readIdentifier (token, c);
		c = skipWhite (vGetc ());
	} while (c != '(' && c != ';' && c != EOF);

	if ( vStringLength (token->name) > 0 )
	{
		verbose ("Found function: %s\n", vStringValue (token->name));

		/* Creat tag */
		createTag (token);

		/* Get port list from function */
		processPortList (c);
	}
}

static void tagNameList (tokenInfo* token, int c)
{
	verilogKind localKind;
	boolean repeat;
	Assert (isIdentifierCharacter (c));
	do
	{ 
		repeat = FALSE;
		if (isIdentifierCharacter (c))
		{
			readIdentifier (token, c);
			localKind = getKind (token);
			/* Create tag in case name is not a known kind ... */
			if (localKind == K_UNDEFINED)
			{
				createTag (token);
			}
			/* ... or else continue searching for names */
			else
			{
				/* Update kind unless it's a port or an ignored keyword */
				if (token->kind != K_PORT && localKind != K_IGNORE)
				{
					token->kind = localKind;
				}
				repeat = TRUE;
			}
		}
		else
			break;
		c = skipWhite (vGetc ());

		/* Get port list */
		if (getKind(token) == K_UNDEFINED && hasSimplePortList (token))
		{
			processPortList (c);
			c = vGetc ();
		}

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
	vUngetc (c);
}

static void findTag (tokenInfo *const token)
{
	if (currentContext->kind != K_UNDEFINED)
	{
		/* Drop context, but only if an end token is found */
		dropContext (token);
	}

	if (token->kind == K_CONSTANT && vStringItem (token->name, 0) == '`')
	{
		/* Bug #961001: Verilog compiler directives are line-based. */
		int c = skipWhite (vGetc ());
		readIdentifier (token, c);
		createTag (token);
		/* Skip the rest of the line. */
		do {
			c = vGetc();
		} while (c != EOF && c != '\n');
		vUngetc (c);
	}
	else if (token->kind == K_BLOCK)
	{
		/* Process begin..end blocks */
		processBlock (token);
	}
	else if (token->kind == K_FUNCTION || token->kind == K_TASK)
	{
		/* Functions are treated differently because they may also include the
		 * type of the return value.
		 * Tasks are treated in the same way, although not having a return
		 * value.*/
		processFunction (token);
	}
	else if (token->kind == K_ASSERTION)
	{
		if (vStringLength (currentContext->blockName) > 0)
		{
			vStringCopy (token->name, currentContext->blockName);
			createTag (token);
			skipToSemiColon ();
		}
	}
	else if (token->kind == K_IGNORE && isSingleStatement (token))
	{
		currentContext->singleStat = TRUE;
	}
	else if (token->kind != K_UNDEFINED)
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
			tagNameList (token, c);
	}
}

static void findVerilogTags (void)
{
	tokenInfo *const token = newToken ();
	int c = '\0';
	currentContext = newToken ();

	while (c != EOF)
	{
		c = vGetc ();
		c = skipWhite (c);
		switch (c)
		{
			case '/':
				skipComments (c);
				break;
			/* Store current block name whenever a : is found
			 * This is used later by any tag type that requires this information
			 * */
			case ':':
				vStringCopy (currentContext->blockName, token->name);
				break;
			/* Skip interface modport port declarations */
			case '(':
				if (currentContext && currentContext->lastKind == K_MODPORT)
				{
					skipPastMatch ("()");
				}
				break;
			/* Drop context on single statements, which don't have an end
			 * statement */
			case ';':
				if (currentContext->scope && currentContext->scope->singleStat)
				{
					verbose ("Dropping context %s\n", vStringValue (currentContext->name));
					currentContext = popToken (currentContext);
					currentContext->singleStat = FALSE;
				}
				break;
			default :
				if (isIdentifierCharacter (c))
				{
					readIdentifier (token, c);
					updateKind (token);
					findTag (token);
				}
		}
	}

	deleteToken (token);
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
