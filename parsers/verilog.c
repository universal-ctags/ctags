/*
*   Copyright (c) 2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
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

#include "debug.h"
#include "cpreprocessor.h"
#include "keyword.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "xtag.h"

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
	K_COVERGROUP,
	K_ENUM,
	K_INTERFACE,
	K_MODPORT,
	K_PACKAGE,
	K_PROGRAM,
	K_PROTOTYPE,
	K_PROPERTY,
	K_STRUCT,
	K_TYPEDEF
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
	MIOPos              filePosition;  /* file position where token was found */
	struct sTokenInfo*  scope;         /* context of keyword */
	int                 nestLevel;     /* Current nest level */
	verilogKind         lastKind;      /* Kind of last found tag */
	vString*            blockName;     /* Current block name */
	vString*            inheritance;   /* Class inheritance */
	bool                prototype;     /* Is only a prototype */
	bool                classScope;    /* Context is local to the current sub-context */
} tokenInfo;

/*
 *   DATA DEFINITIONS
 */
static int Ungetc;
static int Lang_verilog;
static int Lang_systemverilog;

static kindDefinition VerilogKinds [] = {
 { true, 'c', "constant",  "constants (define, parameter, specparam)" },
 { true, 'e', "event",     "events" },
 { true, 'f', "function",  "functions" },
 { true, 'm', "module",    "modules" },
 { true, 'n', "net",       "net data types" },
 { true, 'p', "port",      "ports" },
 { true, 'r', "register",  "register data types" },
 { true, 't', "task",      "tasks" },
 { true, 'b', "block",     "blocks" }
};

static kindDefinition SystemVerilogKinds [] = {
 { true, 'c', "constant",  "constants (define, parameter, specparam, enum values)" },
 { true, 'e', "event",     "events" },
 { true, 'f', "function",  "functions" },
 { true, 'm', "module",    "modules" },
 { true, 'n', "net",       "net data types" },
 { true, 'p', "port",      "ports" },
 { true, 'r', "register",  "register data types" },
 { true, 't', "task",      "tasks" },
 { true, 'b', "block",     "blocks" },
 { true, 'A', "assert",    "assertions" },
 { true, 'C', "class",     "classes" },
 { true, 'V', "covergroup","covergroups" },
 { true, 'E', "enum",      "enumerators" },
 { true, 'I', "interface", "interfaces" },
 { true, 'M', "modport",   "modports" },
 { true, 'K', "package",   "packages" },
 { true, 'P', "program",   "programs" },
 { false,'Q', "prototype", "prototypes" },
 { true, 'R', "property",  "properties" },
 { true, 'S', "struct",    "structs and unions" },
 { true, 'T', "typedef",   "type declarations" }
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
	{ "genvar",    K_REGISTER,  { 1, 1 } },
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
	{ "covergroup",K_COVERGROUP,{ 1, 0 } },
	{ "enum",      K_ENUM,      { 1, 0 } },
	{ "extern",    K_IGNORE,    { 1, 0 } },
	{ "int",       K_REGISTER,  { 1, 0 } },
	{ "interface", K_INTERFACE, { 1, 0 } },
	{ "local",     K_IGNORE,    { 1, 0 } },
	{ "logic",     K_REGISTER,  { 1, 0 } },
	{ "longint",   K_REGISTER,  { 1, 0 } },
	{ "modport",   K_MODPORT,   { 1, 0 } },
	{ "package",   K_PACKAGE,   { 1, 0 } },
	{ "program",   K_PROGRAM,   { 1, 0 } },
	{ "property",  K_PROPERTY,  { 1, 0 } },
	{ "pure",      K_IGNORE,    { 1, 0 } },
	{ "ref",       K_PORT,      { 1, 0 } },
	{ "shortint",  K_REGISTER,  { 1, 0 } },
	{ "shortreal", K_REGISTER,  { 1, 0 } },
	{ "static",    K_IGNORE,    { 1, 0 } },
	{ "string",    K_REGISTER,  { 1, 0 } },
	{ "struct",    K_STRUCT,    { 1, 0 } },
	{ "type",      K_IGNORE,    { 1, 0 } },
	{ "typedef",   K_TYPEDEF,   { 1, 0 } },
	{ "union",     K_STRUCT,    { 1, 0 } },
	{ "unsigned",  K_IGNORE,    { 1, 0 } },
	{ "virtual",   K_IGNORE,    { 1, 0 } },
	{ "void",      K_IGNORE,    { 1, 0 } }
};

static tokenInfo *currentContext = NULL;
static tokenInfo *tagContents = NULL;

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
		case K_COVERGROUP:
		case K_INTERFACE:
		case K_PACKAGE:
		case K_PROGRAM:
		case K_PROPERTY:
		case K_TYPEDEF:
		case K_ENUM:
			return true;
		default:
			return false;
	}
}

static short isTempContext (tokenInfo const* token)
{
	switch (token->kind)
	{
		case K_TYPEDEF:
		case K_ENUM:
			return true;
		default:
			return false;
	}
}

static short isVariable (tokenInfo const* token)
{
	switch (token->kind)
	{
		case K_CONSTANT:
		case K_EVENT:
		case K_NET:
		case K_PORT:
		case K_REGISTER:
			return true;
		default:
			return false;
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
			return true;
		default:
			return false;
	}
}

static short isPrototype (tokenInfo const* token)
{
	if (strcmp (vStringValue (token->name), "extern")  == 0 ||
        strcmp (vStringValue (token->name), "pure") == 0 )
	{
		return true;
	} else {
		return false;
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->kind = K_UNDEFINED;
	token->name = vStringNew ();
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	token->scope = NULL;
	token->nestLevel = 0;
	token->lastKind = K_UNDEFINED;
	token->blockName = vStringNew ();
	token->inheritance = vStringNew ();
	token->prototype = false;
	token->classScope = false;
	return token;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->name);
		vStringDelete (token->blockName);
		vStringDelete (token->inheritance);
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

static const char *getNameForKind (const verilogKind kind)
{
	if (isInputLanguage (Lang_systemverilog))
		return (SystemVerilogKinds[kind]).name;
	else /* isInputLanguage (Lang_verilog) */
		return (VerilogKinds[kind]).name;
}

static char kindEnabled (const verilogKind kind)
{
	if (isInputLanguage (Lang_systemverilog))
		return SystemVerilogKinds[kind].enabled;
	else /* isInputLanguage (Lang_verilog) */
		return VerilogKinds[kind].enabled;
}

static void buildKeywordHash (const langType language, unsigned int idx)
{
	size_t i;
	const size_t count = ARRAY_SIZE (KeywordTable);
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
		c = getcFromInputFile ();
	else
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	if (c == '/')
	{
		int c2 = getcFromInputFile ();
		if (c2 == EOF)
			return EOF;
		else if (c2 == '/')  /* strip comment until end-of-line */
		{
			do
				c = getcFromInputFile ();
			while (c != '\n'  &&  c != EOF);
		}
		else if (c2 == '*')  /* strip block comment */
		{
			c = cppSkipOverCComment();
		}
		else
		{
			ungetcToInputFile (c2);
		}
	}
	else if (c == '"')  /* strip string contents */
	{
		int c2;
		do
			c2 = getcFromInputFile ();
		while (c2 != '"'  &&  c2 != EOF);
		c = '@';
	}
	return c;
}

static bool isIdentifierCharacter (const int c)
{
	return (bool)(isalnum (c)  ||  c == '_'  ||  c == '`');
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

static void skipToSemiColon (void)
{
	int c;
	do
	{
		c = vGetc ();
	} while (c != EOF && c != ';');
}

static bool readIdentifier (tokenInfo *const token, int c)
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
		token->lineNumber = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
	}
	return (bool)(vStringLength (token->name) > 0);
}

static int skipMacro (int c)
{
	tokenInfo *token = newToken ();;

	if (c == '`')
	{
		/* Skip keyword */
		if (isIdentifierCharacter (c = vGetc ()))
		{
			readIdentifier (token, c);
			c = vGetc ();
			/* Skip next keyword if macro is `ifdef or `ifndef or `elsif*/
			if (strcmp (vStringValue (token->name), "ifdef") == 0 ||
			    strcmp (vStringValue (token->name), "ifndef") == 0 ||
				strcmp (vStringValue (token->name), "elsif") == 0)
			{
				verbose ("%c\n", c);
				c = skipWhite (c);
				readIdentifier (token, c);
				c = vGetc ();
				verbose ("Skipping conditional macro %s\n", vStringValue (token->name));
			}
			/* Skip macro functions */
			else
			{
				c = skipWhite (c);
				if (c == '(')
				{
					c = skipPastMatch ("()");
				}
			}
		}
	}
	deleteToken (token);
	return c;
}

static verilogKind getKindForToken (tokenInfo *const token)
{
	return (verilogKind) lookupKeyword (vStringValue (token->name), getInputLanguage () );
}

static void updateKind (tokenInfo *const token)
{
	token->kind = getKindForToken (token);
}

static void createContext (tokenInfo *const scope)
{
	if (scope)
	{
		vString *contextName = vStringNew ();

		/* Determine full context name */
		if (currentContext->kind != K_UNDEFINED)
		{
			vStringCopy (contextName, currentContext->name);
			vStringPut (contextName, '.');
		}
		vStringCat (contextName, scope->name);
		/* Create context */
		currentContext = pushToken (currentContext, scope);
		vStringCopy (currentContext->name, contextName);
		vStringDelete (contextName);
		verbose ("Created new context %s (kind %d)\n", vStringValue (currentContext->name), currentContext->kind);
	}
}

static void dropEndContext (tokenInfo *const token)
{
	verbose ("current context %s; context kind %0d; nest level %0d\n", vStringValue (currentContext->name), currentContext->kind, currentContext->nestLevel);
	vString *endTokenName = vStringNewInit("end");
	if ((currentContext->kind == K_COVERGROUP && strcmp (vStringValue (token->name), "endgroup") == 0) ||
	    (currentContext->kind == K_BLOCK && currentContext->nestLevel == 0 && strcmp (vStringValue (token->name), vStringValue (endTokenName)) == 0)
	    )
	{
		verbose ("Dropping context %s\n", vStringValue (currentContext->name));
		currentContext = popToken (currentContext);
	}
	else
	{
		vStringCatS (endTokenName, getNameForKind (currentContext->kind));
		if (strcmp (vStringValue (token->name), vStringValue (endTokenName)) == 0)
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
			if (currentContext->classScope)
			{
				verbose ("Dropping local context %s\n", vStringValue (currentContext->name));
				currentContext = popToken (currentContext);
			}
		}
	}
	vStringDelete(endTokenName);
}


static void createTag (tokenInfo *const token)
{
	tagEntryInfo tag;
	verilogKind kind;

	/* Determine if kind is prototype */
	if (currentContext->prototype)
	{
		kind = K_PROTOTYPE;
	}
	else
	{
		kind = token->kind;
	}

	/* Do nothing it tag name is empty or tag kind is disabled */
	if (vStringLength (token->name) == 0 || ! kindEnabled (kind))
	{
		verbose ("Unexpected empty token or kind disabled\n");
		return;
	}

	/* Create tag */
	initTagEntry (&tag,
		      vStringValue (token->name),
		      kind);
	tag.lineNumber = token->lineNumber;
	tag.filePosition = token->filePosition;

	verbose ("Adding tag %s (kind %d)", vStringValue (token->name), kind);
	if (currentContext->kind != K_UNDEFINED)
	{
		verbose (" to context %s\n", vStringValue (currentContext->name));
		currentContext->lastKind = kind;
		tag.extensionFields.scopeKindIndex = currentContext->kind;
		tag.extensionFields.scopeName = vStringValue (currentContext->name);
	}
	verbose ("\n");
	if (vStringLength (token->inheritance) > 0)
	{
		tag.extensionFields.inheritance = vStringValue (token->inheritance);
		verbose ("Class %s extends %s\n", vStringValue (token->name), tag.extensionFields.inheritance);
	}
	makeTagEntry (&tag);
	if (isXtagEnabled(XTAG_QUALIFIED_TAGS) && currentContext->kind != K_UNDEFINED)
	{
		vString *const scopedName = vStringNew ();

		vStringCopy (scopedName, currentContext->name);
		vStringPut (scopedName, '.');
		vStringCatS (scopedName, vStringValue (token->name));
		tag.name = vStringValue (scopedName);

		markTagExtraBit (&tag, XTAG_QUALIFIED_TAGS);
		makeTagEntry (&tag);

		vStringDelete (scopedName);
	}

	/* Push token as context if it is a container */
	if (isContainer (token))
	{
		tokenInfo *newScope = newToken ();

		vStringCopy (newScope->name, token->name);
		newScope->kind = kind;
		createContext (newScope);

		/* Include found contents in context */
		if (tagContents != NULL)
		{
			tokenInfo* content = tagContents;

			verbose ("Including tagContents\n");
			do
			{
				createTag (content);
				content = content->scope;
			} while (content);
		}

		/* Drop temporary contexts */
		if (isTempContext (currentContext))
		{
			verbose ("Dropping context %s\n", vStringValue (currentContext->name));
			currentContext = popToken (currentContext);
		}
	}

	/* Clear no longer required inheritance information */
	vStringClear (token->inheritance);
}

static bool findBlockName (tokenInfo *const token)
{
	int c;

	c = skipWhite (vGetc ());
	if (c == ':')
	{
		c = skipWhite (vGetc ());
		readIdentifier (token, c);
		return (bool) (vStringLength (token->name) > 0);
	}
	else
		vUngetc (c);
	return false;
}

static void processBlock (tokenInfo *const token)
{
	bool blockStart = false;
	bool blockEnd   = false;

	if (strcmp (vStringValue (token->name), "begin") == 0)
	{
		currentContext->nestLevel++;
		blockStart = true;
	}
	else if (strcmp (vStringValue (token->name), "end") == 0)
	{
		currentContext->nestLevel--;
		blockEnd = true;
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
			else if (c == '`')
			{
				c = skipMacro (c);
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
					if (! isIdentifierCharacter (c) || c == '`')
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
	else if (c != EOF)
	{
		vUngetc (c);
	}
}

static void processFunction (tokenInfo *const token)
{
	int c;
	tokenInfo *classType;

	/* Search for function name
	 * Last identifier found before a '(' or a ';' is the function name */
	c = skipWhite (vGetc ());
	do
	{
		readIdentifier (token, c);
		c = skipWhite (vGetc ());
		/* Identify class type prefixes and create respective context*/
		if (isInputLanguage (Lang_systemverilog) && c == ':')
		{
			c = vGetc ();
			if (c == ':')
			{
				verbose ("Found function declaration with class type %s\n", vStringValue (token->name));
				classType = newToken ();
				vStringCopy (classType->name, token->name);
				classType->kind = K_CLASS;
				createContext (classType);
				currentContext->classScope = true;
			}
			else
			{
				vUngetc (c);
			}
		}
	} while (c != '(' && c != ';' && c != EOF);

	if ( vStringLength (token->name) > 0 )
	{
		verbose ("Found function: %s\n", vStringValue (token->name));

		/* Create tag */
		createTag (token);

		/* Get port list from function */
		processPortList (c);
	}
}

static void tagNameList (tokenInfo* token, int c);

static void processEnum (tokenInfo *const token)
{
	int c;

	/* Read enum type */
	c = skipWhite (vGetc ());
	if (isIdentifierCharacter (c))
	{
		tokenInfo* typeQueue = NULL;
		tokenInfo* type;

		do
		{
			type = newToken ();

			readIdentifier (type, c);
			updateKind (type);
			typeQueue = pushToken (typeQueue, type);
			verbose ("Enum type %s\n", vStringValue (type->name));
			c = skipWhite (vGetc ());
		} while (isIdentifierCharacter (c));

		/* Undefined kind means that we've reached the end of the
		 * declaration without having any contents defined, which
		 * indicates that this is in fact a forward declaration */
		if (type->kind == K_UNDEFINED && (typeQueue->scope == NULL || typeQueue->scope->kind != K_UNDEFINED))
		{
			verbose ("Prototype enum found \"%s\"\n", vStringValue (type->name));
			type->kind = K_PROTOTYPE;
			createTag (type);
			pruneTokens (typeQueue);
			return;
		}

		/* Cleanup type queue */
		pruneTokens (typeQueue);
	}

	/* Skip bus width definition */
	if (c == '[')
	{
		c = skipWhite (skipPastMatch ("[]"));
	}

	/* Search enum elements */
	if (c == '{')
	{
		c = skipWhite (vGetc ());
		while (isIdentifierCharacter (c))
		{
			tokenInfo *content = newToken ();

			readIdentifier (content, c);
			content->kind = K_CONSTANT;
			tagContents = pushToken (tagContents, content);
			verbose ("Pushed enum element \"%s\"\n", vStringValue (content->name));

			c = skipWhite (vGetc ());
			/* Skip element ranges */
			/* TODO Implement element ranges */
			if (c == '[')
			{
				c = skipWhite (skipPastMatch ("[]"));
			}
			/* Skip value assignments */
			if (c == '=')
			{
				while (c != '}' && c != ',' && c != EOF)
				{
					c = skipWhite (vGetc ());

					/* Skip enum value concatenations */
					if (c == '{')
					{
						c = skipWhite (skipPastMatch ("{}"));
					}
				}
			}
			/* Skip comma */
			if (c == ',')
			{
				c = skipWhite (vGetc ());
			}
			/* End of enum elements list */
			if (c == '}')
			{
				c = skipWhite (vGetc ());
				break;
			}
		}
	}

	/* Following identifiers are tag names */
	verbose ("Find enum tags. Token %s kind %d\n", vStringValue (token->name), token->kind);
	tagNameList (token, c);
}

static void processTypedef (tokenInfo *const token)
{
	int c;

	/* Get typedef type */
	c = skipWhite (vGetc ());
	if (isIdentifierCharacter (c))
	{
		readIdentifier (token, c);
		updateKind (token);

		switch (token->kind)
		{
			case K_INTERFACE:
				/* Expecting `typedef interface class` */
				c = skipWhite (vGetc ());
				readIdentifier (token, c);
				updateKind (token);
			case K_CLASS:
				/* A typedef class is just a prototype */
				currentContext->prototype = true;
				break;
			case K_ENUM:
				/* Call enum processing function */
				token->kind = K_TYPEDEF;
				processEnum (token);
				return;
			default :
				break;
		}

		c = skipWhite (vGetc ());
	}

	/* Skip bus width definition */
	if (c == '[')
	{
		c = skipWhite (skipPastMatch ("[]"));
	}

	/* Skip remaining identifiers */
	while (isIdentifierCharacter (c))
	{
		readIdentifier (token, c);
		c = skipWhite (vGetc ());
	}

	/* Skip typedef contents */
	if (c == '{')
	{
		c = skipWhite (skipPastMatch ("{}"));
	}
	else
	{
		/* Typedefs of struct/union that have no contents are forward
		 * declarations and are considered prototypes */
		if (token->kind == K_STRUCT)
		{
			currentContext->prototype = true;
		}
	}

	/* Skip past class parameter override */
	if (c == '#')
	{
		c = skipWhite (vGetc ());
		if (c == '(')
		{
			c = skipWhite (skipPastMatch ("()"));
		}
	}

	/* Read typedef name */
	if (isIdentifierCharacter (c))
	{
		readIdentifier (token, c);
	}
	else
	{
		vUngetc (c);

		/* Empty typedefs are forward declarations and are considered
		 * prototypes */
		if (token->kind == K_UNDEFINED)
		{
			currentContext->prototype = true;
		}
	}

	/* Use last identifier to create tag, but always with kind typedef */
	token->kind = K_TYPEDEF;
	createTag (token);
}

static void processClass (tokenInfo *const token)
{
	/*Note: At the moment, only identifies typedef name and not its contents */
	int c;
	tokenInfo *extra;
	tokenInfo *parameters = NULL;

	/* Get identifiers */
	c = skipWhite (vGetc ());
	if (isIdentifierCharacter (c))
	{
		readIdentifier (token, c);
		c = skipWhite (vGetc ());
	}

	/* Find class parameters list */
	if (c == '#')
	{
		c = skipWhite (vGetc ());
		if (c == '(')
		{
			parameters = newToken ();
			do
			{
				c = skipWhite (vGetc ());
				readIdentifier (parameters, c);
				updateKind (parameters);
				verbose ("Found class parameter %s\n", vStringValue (parameters->name));
				if (parameters->kind == K_UNDEFINED)
				{
					parameters->kind = K_CONSTANT;
					parameters = pushToken (parameters, newToken ());
					c = vGetc();
					while (c != ',' && c != ')' && c != EOF)
					{
						c = vGetc();
					}
				}
			} while (c != ')' && c != EOF);
			c = skipWhite (vGetc ());
			parameters = popToken (parameters);
		}
	}

	/* Search for inheritance information */
	if (isIdentifierCharacter (c))
	{
		extra = newToken ();

		readIdentifier (extra, c);
		c = skipWhite (vGetc ());
		if (strcmp (vStringValue (extra->name), "extends") == 0)
		{
			readIdentifier (extra, c);
			vStringCopy (token->inheritance, extra->name);
			verbose ("Inheritance %s\n", vStringValue (token->inheritance));
		}
		deleteToken (extra);
	}

	/* Use last identifier to create tag */
	createTag (token);

	/* Add parameter list */
	while (parameters)
	{
		createTag (parameters);
		parameters = popToken (parameters);
	}
}

static void tagNameList (tokenInfo* token, int c)
{
	verilogKind localKind;
	bool repeat;

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

	do
	{
		repeat = false;

		while (c == '`' && c != EOF)
		{
			c = skipMacro (c);
		}
		if (isIdentifierCharacter (c))
		{
			readIdentifier (token, c);
			localKind = getKindForToken (token);
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
				repeat = true;
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
			if (c == '\'')
			{
				c = skipWhite (vGetc ());
				if (c != '{')
					vUngetc (c);
			}
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
			repeat = true;
		}
	} while (repeat);
	vUngetc (c);
}

static void findTag (tokenInfo *const token)
{
	verbose ("Checking token %s of kind %d\n", vStringValue (token->name), token->kind);

	if (currentContext->kind != K_UNDEFINED)
	{
		/* Drop context, but only if an end token is found */
		dropEndContext (token);
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
	else if (token->kind == K_TYPEDEF)
	{
		processTypedef (token);
	}
	else if (token->kind == K_ENUM)
	{
		processEnum (token);
	}
	else if (token->kind == K_CLASS)
	{
		processClass (token);
	}
	else if (token->kind == K_IGNORE && isPrototype (token))
	{
		currentContext->prototype = true;
	}
	else if (isVariable (token))
	{
		int c = skipWhite (vGetc ());

		tagNameList (token, c);
	}
	else if (token->kind != K_UNDEFINED && token->kind != K_IGNORE)
	{
		int c = skipWhite (vGetc ());

		if (isIdentifierCharacter (c))
		{
			readIdentifier (token, c);
			while (getKindForToken (token) == K_IGNORE)
			{
				c = skipWhite (vGetc ());
				readIdentifier (token, c);
			}
			createTag (token);

			/* Get port list if required */
			c = skipWhite (vGetc ());
			if (c == '(' && hasSimplePortList (token))
			{
				processPortList (c);
			}
			else
			{
				vUngetc (c);
			}
		}
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
			case ';':
				/* Drop context on prototypes because they don't have an
				 * end statement */
				if (currentContext->scope && currentContext->scope->prototype)
				{
					verbose ("Dropping context %s\n", vStringValue (currentContext->name));
					currentContext = popToken (currentContext);
					currentContext->prototype = false;
				}
				/* Prototypes end at the end of statement */
				if (currentContext->prototype)
				{
					currentContext->prototype = false;
				}
				/* Cleanup tag contents list at end of declaration */
				while (tagContents)
				{
					tagContents = popToken (tagContents);
				}
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
	def->kindTable      = VerilogKinds;
	def->kindCount  = ARRAY_SIZE (VerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initializeVerilog;
	return def;
}

extern parserDefinition* SystemVerilogParser (void)
{
	static const char *const extensions [] = { "sv", "svh", "svi", NULL };
	parserDefinition* def = parserNew ("SystemVerilog");
	def->kindTable      = SystemVerilogKinds;
	def->kindCount  = ARRAY_SIZE (SystemVerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initializeSystemVerilog;
	return def;
}
