/*
*   Copyright (c) 2008, Nicolas Vincent
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for VHDL files.
*
*   References:
*     https://rti.etf.bg.ac.rs/rti/ri5rvl/tutorial/TUTORIAL/IEEE/HTML/1076_TOC.HTM
*     https://tams.informatik.uni-hamburg.de/vhdl/tools/grammar/vhdl93-bnf.html
*     http://www.vhdl.renerta.com/mobile/index.html
*     https://www.hdlworks.com/hdl_corner/vhdl_ref/
*     https://www.ics.uci.edu/~jmoorkan/vhdlref/Synario%20VHDL%20Manual.pdf
*     http://atlas.physics.arizona.edu/~kjohns/downloads/vhdl/VHDL-xilinx-help.pdf
*     http://www.csit-sun.pub.ro/resources/xilinx/synvhdl.pdf
*     https://edg.uchicago.edu/~tang/VHDLref.pdf
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include <ctype.h>	/* to define isalpha () */
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "trace.h"

/*
 *   MACROS
 */
#define isType(token,t)     (bool) ((token)->type == (t))
#define isKeyword(token,k)  (bool) ((token)->keyword == (k))
#define isIdentChar1(c) (isalpha (c) || (c) == '_')
#define isIdentChar(c) (isalpha (c) || isdigit (c) || (c) == '_')

/*
 *   DATA DECLARATIONS
 */

/*
 * Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_ABS,
	KEYWORD_ACCESS,
	KEYWORD_AFTER,
	KEYWORD_ALIAS,
	KEYWORD_ALL,
	KEYWORD_AND,
	KEYWORD_ARCHITECTURE,
	KEYWORD_ARRAY,
	KEYWORD_ASSERT,
	KEYWORD_ATTRIBUTE,
	KEYWORD_BEGIN,
	KEYWORD_BLOCK,
	KEYWORD_BODY,
	KEYWORD_BUFFER,
	KEYWORD_BUS,
	KEYWORD_CASE,
	KEYWORD_COMPONENT,
	KEYWORD_CONFIGURATION,
	KEYWORD_CONSTANT,
	KEYWORD_DISCONNECT,
	KEYWORD_DOWNTO,
	KEYWORD_ELSE,
	KEYWORD_ELSIF,
	KEYWORD_END,
	KEYWORD_ENTITY,
	KEYWORD_EXIT,
	KEYWORD_FILE,
	KEYWORD_FOR,
	KEYWORD_FUNCTION,
	KEYWORD_GENERATE,
	KEYWORD_GENERIC,
	KEYWORD_GROUP,
	KEYWORD_GUARDED,
	KEYWORD_IF,
	KEYWORD_IMPURE,
	KEYWORD_IN,
	KEYWORD_INERTIAL,
	KEYWORD_INOUT,
	KEYWORD_IS,
	KEYWORD_LABEL,
	KEYWORD_LIBRARY,
	KEYWORD_LINKAGE,
	KEYWORD_LITERAL,
	KEYWORD_LOOP,
	KEYWORD_MAP,
	KEYWORD_MOD,
	KEYWORD_NAND,
	KEYWORD_NEW,
	KEYWORD_NEXT,
	KEYWORD_NOR,
	KEYWORD_NOT,
	KEYWORD_NULL,
	KEYWORD_OF,
	KEYWORD_ON,
	KEYWORD_OPEN,
	KEYWORD_OR,
	KEYWORD_OTHERS,
	KEYWORD_OUT,
	KEYWORD_PACKAGE,
	KEYWORD_PORT,
	KEYWORD_POSTPONED,
	KEYWORD_PROCEDURE,
	KEYWORD_PROCESS,
	KEYWORD_PURE,
	KEYWORD_RANGE,
	KEYWORD_RECORD,
	KEYWORD_REGISTER,
	KEYWORD_REJECT,
	KEYWORD_RETURN,
	KEYWORD_ROL,
	KEYWORD_ROR,
	KEYWORD_SELECT,
	KEYWORD_SEVERITY,
	KEYWORD_SIGNAL,
	KEYWORD_SHARED,
	KEYWORD_SLA,
	KEYWORD_SLI,
	KEYWORD_SRA,
	KEYWORD_SRL,
	KEYWORD_SUBTYPE,
	KEYWORD_THEN,
	KEYWORD_TO,
	KEYWORD_TRANSPORT,
	KEYWORD_TYPE,
	KEYWORD_UNAFFECTED,
	KEYWORD_UNITS,
	KEYWORD_UNTIL,
	KEYWORD_USE,
	KEYWORD_VARIABLE,
	KEYWORD_WAIT,
	KEYWORD_WHEN,
	KEYWORD_WHILE,
	KEYWORD_WITH,
	KEYWORD_XNOR,
	KEYWORD_XOR
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_NONE,		/* none */
	TOKEN_EOF,		/* end-of-file */
	TOKEN_OPEN_PAREN,	/* ( */
	TOKEN_CLOSE_PAREN,	/* ) */
	TOKEN_COMMA,		/* the comma character */
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_PERIOD,		/* . */
	TOKEN_OPERATOR,
	TOKEN_SEMICOLON,	/* the semicolon character */
	TOKEN_COLON,		/* : */
	TOKEN_STRING
} tokenType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	vString *string;		/* the name of the token */
	unsigned long lineNumber;	/* line number of tag */
	MIOPos filePosition;		/* file position of line containing name */
} tokenInfo;

/*
 *   DATA DEFINITIONS
 */
static int Lang_vhdl;

typedef enum {
	VHDL_ENTITY_DESIGNED,
} vhdlEntityRole;

static roleDefinition VhdlEntityRoles [] = {
	{ true, "desigend",
	  "designed by an architecture" },
};

/* Used to index into the VhdlKinds table. */
typedef enum {
	VHDLTAG_UNDEFINED = -1,
	VHDLTAG_CONSTANT,
	VHDLTAG_TYPE,
	VHDLTAG_SUBTYPE,
	VHDLTAG_RECORD,
	VHDLTAG_ENTITY,
	VHDLTAG_COMPONENT,
	VHDLTAG_PROTOTYPE,
	VHDLTAG_FUNCTION,
	VHDLTAG_PROCEDURE,
	VHDLTAG_PACKAGE,
	VHDLTAG_LOCAL,
	VHDLTAG_ARCHITECTURE,
	VHDLTAG_PORT,
	VHDLTAG_GENERIC,
	VHDLTAG_SIGNAL,
	VHDLTAG_PROCESS,
	VHDLTAG_VARIABLE,
	VHDLTAG_ALIAS,
} vhdlKind;

static kindDefinition VhdlKinds[] = {
	{true, 'c', "constant", "constant declarations"},
	{true, 't', "type", "type definitions"},
	{true, 'T', "subtype", "subtype definitions"},
	{true, 'r', "record", "record names"},
	{true, 'e', "entity", "entity declarations",
	 .referenceOnly = false, ATTACH_ROLES(VhdlEntityRoles)},
	{false, 'C', "component", "component declarations"},
	{false, 'd', "prototype", "prototypes"},
	{true, 'f', "function", "function prototypes and declarations"},
	{true, 'p', "procedure", "procedure prototypes and declarations"},
	{true, 'P', "package", "package definitions"},
	{false, 'l', "local", "local definitions"},
	{true, 'a', "architecture", "architectures"},
	{true, 'q', "port", "port declarations"},
	{true, 'g', "generic", "generic declarations"},
	{true , 's', "signal", "signal declarations"},
	{true, 'Q',  "process", "processes"},
	{true, 'v',  "variable", "variables"},
	{true, 'A',  "alias", "aliases"},
};

static const keywordTable VhdlKeywordTable[] = {
	{"abs", KEYWORD_ABS},
	{"access", KEYWORD_ACCESS},
	{"after", KEYWORD_AFTER},
	{"alias", KEYWORD_ALIAS},
	{"all", KEYWORD_ALL},
	{"and", KEYWORD_AND},
	{"architecture", KEYWORD_ARCHITECTURE},
	{"array", KEYWORD_ARRAY},
	{"assert", KEYWORD_ASSERT},
	{"attribute", KEYWORD_ATTRIBUTE},
	{"begin", KEYWORD_BEGIN},
	{"block", KEYWORD_BLOCK},
	{"body", KEYWORD_BODY},
	{"buffer", KEYWORD_BUFFER},
	{"bus", KEYWORD_BUS},
	{"case", KEYWORD_CASE},
	{"component", KEYWORD_COMPONENT},
	{"configuration", KEYWORD_CONFIGURATION},
	{"constant", KEYWORD_CONSTANT},
	{"disconnect", KEYWORD_DISCONNECT},
	{"downto", KEYWORD_DOWNTO},
	{"else", KEYWORD_ELSE},
	{"elsif", KEYWORD_ELSIF},
	{"end", KEYWORD_END},
	{"entity", KEYWORD_ENTITY},
	{"exit", KEYWORD_EXIT},
	{"file", KEYWORD_FILE},
	{"for", KEYWORD_FOR},
	{"function", KEYWORD_FUNCTION},
	{"generate", KEYWORD_GENERATE},
	{"generic", KEYWORD_GENERIC},
	{"group", KEYWORD_GROUP},
	{"guarded", KEYWORD_GUARDED},
	{"if", KEYWORD_IF},
	{"impure", KEYWORD_IMPURE},
	{"in", KEYWORD_IN},
	{"inertial", KEYWORD_INERTIAL},
	{"inout", KEYWORD_INOUT},
	{"is", KEYWORD_IS},
	{"label", KEYWORD_LABEL},
	{"library", KEYWORD_LIBRARY},
	{"linkage", KEYWORD_LINKAGE},
	{"literal", KEYWORD_LITERAL},
	{"loop", KEYWORD_LOOP},
	{"map", KEYWORD_MAP},
	{"mod", KEYWORD_MOD},
	{"nand", KEYWORD_NAND},
	{"new", KEYWORD_NEW},
	{"next", KEYWORD_NEXT},
	{"nor", KEYWORD_NOR},
	{"not", KEYWORD_NOT},
	{"null", KEYWORD_NULL},
	{"of", KEYWORD_OF},
	{"on", KEYWORD_ON},
	{"open", KEYWORD_OPEN},
	{"or", KEYWORD_OR},
	{"others", KEYWORD_OTHERS},
	{"out", KEYWORD_OUT},
	{"package", KEYWORD_PACKAGE},
	{"port", KEYWORD_PORT},
	{"postponed", KEYWORD_POSTPONED},
	{"procedure", KEYWORD_PROCEDURE},
	{"process", KEYWORD_PROCESS},
	{"pure", KEYWORD_PURE},
	{"range", KEYWORD_RANGE},
	{"record", KEYWORD_RECORD},
	{"register", KEYWORD_REGISTER},
	{"reject", KEYWORD_REJECT},
	{"return", KEYWORD_RETURN},
	{"rol", KEYWORD_ROL},
	{"ror", KEYWORD_ROR},
	{"select", KEYWORD_SELECT},
	{"severity", KEYWORD_SEVERITY},
	{"signal", KEYWORD_SIGNAL},
	{"shared", KEYWORD_SHARED},
	{"sla", KEYWORD_SLA},
	{"sli", KEYWORD_SLI},
	{"sra", KEYWORD_SRA},
	{"srl", KEYWORD_SRL},
	{"subtype", KEYWORD_SUBTYPE},
	{"then", KEYWORD_THEN},
	{"to", KEYWORD_TO},
	{"transport", KEYWORD_TRANSPORT},
	{"type", KEYWORD_TYPE},
	{"unaffected", KEYWORD_UNAFFECTED},
	{"units", KEYWORD_UNITS},
	{"until", KEYWORD_UNTIL},
	{"use", KEYWORD_USE},
	{"variable", KEYWORD_VARIABLE},
	{"wait", KEYWORD_WAIT},
	{"when", KEYWORD_WHEN},
	{"while", KEYWORD_WHILE},
	{"with", KEYWORD_WITH},
	{"xnor", KEYWORD_XNOR},
	{"xor", KEYWORD_XOR}
};

typedef enum {
	F_ARCHITECTURE,
} vhdlField;

static fieldDefinition VhdlFields [] = {
	{ .name = "architecture",
	  .description = "architecture designing the entity",
	  .enabled = true },
};

/*
 *   FUNCTION DECLARATIONS
 */
static void parseKeywords (tokenInfo * const token, tokenInfo * const label, int parent);

/*
 *   FUNCTION DEFINITIONS
 */
static bool isIdentifierMatch (const tokenInfo * const token,
	const char *name)
{
	return (bool) (isType (token, TOKEN_IDENTIFIER) &&
		strncasecmp (vStringValue (token->string), name,
					 vStringLength (token->string)) == 0);
}

static bool isSemicolonOrKeywordOrIdent (const tokenInfo * const token,
	const keywordId keyword, const char *name)
{
	return (bool) (isType (token, TOKEN_SEMICOLON)
				   || isKeyword (token, keyword)
				   || isIdentifierMatch (token, name));
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);
	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	token->string = vStringNew ();
	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	return token;
}

static tokenInfo *copyToken (tokenInfo * const src)
{
	tokenInfo *dst = newToken ();
	vStringCopy (dst->string, src->string);
	return dst;
}

static void deleteToken (tokenInfo * const token)
{
	if (token != NULL)
	{
		vStringDelete (token->string);
		eFree (token);
	}
}

/*
 *   Parsing functions
 */

static void parseString (vString * const string, const int delimiter)
{
	bool end = false;
	while (!end)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			end = true;
		else if (c == '\\')
		{
			c = getcFromInputFile ();	/* This maybe a ' or ". */
			vStringPut (string, c);
		}
		else if (c == delimiter)
			end = true;
		else
			vStringPut (string, c);
	}
}

/*  Read a VHDL identifier beginning with "firstChar" and place it into "name".
*/
static void parseIdentifier (vString * const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar1 (c));
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isIdentChar (c));
	if (!isspace (c))
		ungetcToInputFile (c);	/* unget non-identifier character */
}

static void readToken (tokenInfo * const token)
{
	int c;

	token->type = TOKEN_NONE;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

  getNextChar:
	do
	{
		c = getcFromInputFile ();
		token->lineNumber = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
	}
	while (c == '\t' || c == ' ' || c == '\n');

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case '(':
		token->type = TOKEN_OPEN_PAREN;
		break;
	case ')':
		token->type = TOKEN_CLOSE_PAREN;
		break;
	case ';':
		token->type = TOKEN_SEMICOLON;
		break;
	case ':':
		token->type = TOKEN_COLON;
		break;
	case '.':
		token->type = TOKEN_PERIOD;
		break;
	case ',':
		token->type = TOKEN_COMMA;
		break;
	case '\'':	/* only single char are inside simple quotes */
		break;	/* or it is for attributes so we don't care */
	case '"':
		token->type = TOKEN_STRING;
		parseString (token->string, c);
		token->lineNumber = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
		break;
	case '-':
		c = getcFromInputFile ();
		if (c == '-')	/* start of a comment */
		{
			skipToCharacterInInputFile ('\n');
			goto getNextChar;
		}
		else
		{
			if (!isspace (c))
				ungetcToInputFile (c);
			token->type = TOKEN_OPERATOR;
		}
		break;
	default:
		if (!isIdentChar1 (c))
			token->type = TOKEN_NONE;
		else
		{
			parseIdentifier (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_vhdl);
			if (isKeyword (token, KEYWORD_NONE))
				token->type = TOKEN_IDENTIFIER;
			else
				token->type = TOKEN_KEYWORD;
		}
		break;
	}
}

static bool skipToKeyword (const keywordId keyword)
{
	tokenInfo *const token = newToken ();
	do
	{
		readToken (token);
	}
	while (!isType (token, TOKEN_EOF) && !isKeyword (token, keyword));

	bool r = isKeyword (token, keyword);
	deleteToken (token);
	return r;
}

static void skipToMatched (tokenInfo * const token)
{
	int nest_level = 0;
	tokenType open_token;
	tokenType close_token;

	switch (token->type)
	{
	case TOKEN_OPEN_PAREN:
		open_token = TOKEN_OPEN_PAREN;
		close_token = TOKEN_CLOSE_PAREN;
		break;
	default:
		return;
	}

	/*
	 * This routine will skip to a matching closing token.
	 * It will also handle nested tokens like the (, ) below.
	 *   (  name varchar(30), text binary(10)  )
	 */
	if (isType (token, open_token))
	{
		nest_level++;
		while (!(isType (token, close_token) && (nest_level == 0)) && !isType (token, TOKEN_EOF))
		{
			readToken (token);
			if (isType (token, open_token))
			{
				nest_level++;
			}
			if (isType (token, close_token))
			{
				if (nest_level > 0)
				{
					nest_level--;
				}
			}
		}
		readToken (token);
	}
}

static int makeVhdlTagWithScope (tokenInfo * const token, const vhdlKind kind, int parent)
{
	const char *const name = vStringValue (token->string);
	tagEntryInfo e;
	initTagEntry (&e, name, kind);
	e.lineNumber = token->lineNumber;
	e.filePosition = token->filePosition;
	e.extensionFields.scopeIndex = parent;
	return makeTagEntry (&e);
}

static int makeVhdlTag (tokenInfo * const token, const vhdlKind kind)
{
	return makeVhdlTagWithScope (token, kind, CORK_NIL);
}

static void initialize (const langType language)
{
	Lang_vhdl = language;
}

static void parseTillEnd (tokenInfo * const token, int parent, const int end_keyword)
{
	bool ended = false;
	tagEntryInfo *e = getEntryInCorkQueue (parent);
	/* If e is NULL, the input may be broken as VHDL code
	 * or unsupported syntax in this parser. */

	do
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_END))
		{
			readToken (token);
			if (e)
				ended = isSemicolonOrKeywordOrIdent (token,
													 end_keyword, e->name);
			if (!isType (token, TOKEN_SEMICOLON))
				skipToCharacterInInputFile (';');
			if (ended)
				e->extensionFields.endLine = getInputLineNumber ();
		}
		else
		{
			if (isType (token, TOKEN_EOF))
			{
				ended = true;
			}
			else
			{
				parseKeywords (token, NULL, parent);
			}
		}
	} while (!ended);
}

static void parseTillBegin (tokenInfo * const token, int parent)
{
	bool begun = false;
	do
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_BEGIN)
			|| isType (token, TOKEN_EOF))
			begun = true;
		else
			parseKeywords (token, NULL, parent);
	} while (!begun);
}

static void parsePackage (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();
	tokenInfo *token_for_tagging = NULL;
	Assert (isKeyword (token, KEYWORD_PACKAGE));
	readToken (token);
	if (isKeyword (token, KEYWORD_BODY))
	{
		readToken (name);
		token_for_tagging = name;
	}
	else if (isType (token, TOKEN_IDENTIFIER))
		token_for_tagging = token;

	if (token_for_tagging)
	{
		int index = makeVhdlTag (token_for_tagging, VHDLTAG_PACKAGE);
		parseTillEnd (token, index, KEYWORD_PACKAGE);
	}

	deleteToken (name);
}


static void parseDeclElement (tokenInfo * const token,
							  vhdlKind kind, int parent,
							  bool ended_with_semicolon)
{
	TRACE_ENTER ();
	while (! (isType (token, TOKEN_EOF)
			  || isType (token, TOKEN_CLOSE_PAREN)
			  || (ended_with_semicolon && isType (token, TOKEN_SEMICOLON))))
	{
		if (isType (token, TOKEN_IDENTIFIER))
		{
			makeVhdlTagWithScope (token, kind, parent);
			readToken (token);
		}
		else if (isType (token, TOKEN_COMMA))
			readToken (token);
		else if (isType (token, TOKEN_COLON))
		{
			do
			{
				readToken (token);
				skipToMatched (token);
				if (isType (token, TOKEN_CLOSE_PAREN)
					|| isType (token, TOKEN_SEMICOLON))
					break;
			}
			while (!isType (token, TOKEN_EOF));
		}
		else
		{
			/* Unexpected */
			readToken (token);
		}
	}
	TRACE_LEAVE ();
}

static void parseModuleDecl (tokenInfo * const token, int parent)
{
	TRACE_ENTER ();
	while (! (isKeyword (token, KEYWORD_END)
			  || isType (token, TOKEN_EOF)))
	{
		vhdlKind kind = VHDLTAG_UNDEFINED;
		if (isKeyword (token, KEYWORD_PORT))
			kind = VHDLTAG_PORT;
		else if (isKeyword (token, KEYWORD_GENERIC))
			kind = VHDLTAG_GENERIC;

		if (kind != VHDLTAG_UNDEFINED)
		{
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				readToken (token);
				parseDeclElement (token, kind, parent, false);
			}
		}
		else
			readToken (token);
	}
	TRACE_LEAVE ();
}

static void parseModule (tokenInfo * const token, int parent)
{
	tokenInfo *const name = newToken ();
	const vhdlKind kind = isKeyword (token, KEYWORD_ENTITY) ?
		VHDLTAG_ENTITY : VHDLTAG_COMPONENT;
	Assert (isKeyword (token, KEYWORD_ENTITY) ||
		isKeyword (token, KEYWORD_COMPONENT));
	readToken (name);
	readToken (token);
	if (kind == VHDLTAG_COMPONENT || isKeyword (token, KEYWORD_IS))
	{
		int index = makeVhdlTagWithScope (name, kind, parent);
		if (isKeyword (token, KEYWORD_IS))
			readToken (token);
		parseModuleDecl (token, index);

		bool ended = isKeyword (token, KEYWORD_END);
		if (!ended)
			ended = skipToKeyword (KEYWORD_END);
		skipToCharacterInInputFile (';');

		if (ended)
		{
			tagEntryInfo *e = getEntryInCorkQueue (index);
			if (e)
				e->extensionFields.endLine = getInputLineNumber ();
		}

		if (kind == VHDLTAG_ENTITY)
			registerEntry (index);
	}
	deleteToken (name);
}

static void parseRecord (tokenInfo * const token, int parent)
{
	tokenInfo *const name = newToken ();
	Assert (isKeyword (token, KEYWORD_RECORD));
	readToken (name);
	do
	{
		readToken (token);	/* should be a colon */
		skipToCharacterInInputFile (';');
		makeVhdlTagWithScope (name, VHDLTAG_RECORD, parent);
		readToken (name);
	}
	while (!isKeyword (name, KEYWORD_END) && !isType (name, TOKEN_EOF));
	skipToCharacterInInputFile (';');

	if (isKeyword (name, KEYWORD_END))
	{
		tagEntryInfo *e = getEntryInCorkQueue (parent);
		if (e)
			e->extensionFields.endLine = getInputLineNumber ();
	}

	deleteToken (name);
}

static void parseTypes (tokenInfo * const token, int parent)
{
	tokenInfo *const name = newToken ();
	const vhdlKind kind = isKeyword (token, KEYWORD_TYPE) ?
		VHDLTAG_TYPE : VHDLTAG_SUBTYPE;
	Assert (isKeyword (token, KEYWORD_TYPE) ||
		isKeyword (token, KEYWORD_SUBTYPE));
	readToken (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_IS))
	{
		readToken (token);	/* type */
		if (isKeyword (token, KEYWORD_RECORD))
		{
			int index = makeVhdlTagWithScope (name, kind, parent);
			/*TODO: make tags of the record's names */
			parseRecord (token, index);
		}
		else
		{
			makeVhdlTagWithScope (name, kind, parent);
		}
	}
	deleteToken (name);
}

static void parseConstant (int parent)
{
	vhdlKind parent_kind = VHDLTAG_UNDEFINED;
	tagEntryInfo *e = getEntryInCorkQueue (parent);
	if (e)
		parent_kind = e->kindIndex;

	vhdlKind kind;
	switch (parent_kind)
	{
	case VHDLTAG_FUNCTION:
	case VHDLTAG_PROCEDURE:
		kind = VHDLTAG_LOCAL;
		break;
	default:
		kind = VHDLTAG_CONSTANT;
		break;
	}

	tokenInfo *const name = newToken ();
	readToken (name);
	makeVhdlTagWithScope (name, kind, parent);
	skipToCharacterInInputFile (';');
	deleteToken (name);
}

static void parseSubProgram (tokenInfo * const token, int parent)
{
	tokenInfo *const name = newToken ();
	const vhdlKind kind = isKeyword (token, KEYWORD_FUNCTION) ?
		VHDLTAG_FUNCTION : VHDLTAG_PROCEDURE;
	const int end_keyword = token->keyword;
	Assert (isKeyword (token, KEYWORD_FUNCTION) ||
		isKeyword (token, KEYWORD_PROCEDURE));
	readToken (name);	/* the name of the function or procedure */
	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipToMatched (token);
	}

	if (kind == VHDLTAG_FUNCTION)
	{
		if (isKeyword (token, KEYWORD_RETURN))
		{
			/* Read datatype */
			readToken (token);
			while (! isKeyword (token, KEYWORD_IS) &&
					! isType (token, TOKEN_SEMICOLON) &&
					! isType (token, TOKEN_EOF))
			{
				readToken (token);
			}
		}
	}

	if (isType (token, TOKEN_SEMICOLON))
	{
		makeVhdlTagWithScope (name, VHDLTAG_PROTOTYPE, parent);
	}
	else if (isKeyword (token, KEYWORD_IS))
	{
		int index = makeVhdlTagWithScope (name, kind, parent);
		parseTillEnd (token, index, end_keyword);
	}
	deleteToken (name);
}

/*  architecture behavioral of ent is*/
static void parseArchitecture (tokenInfo * const token)
{
	tokenInfo *const name = newToken ();

	readToken (name);
	if (!isType (name, TOKEN_IDENTIFIER))
	{
		skipToKeyword (KEYWORD_END);
		skipToCharacterInInputFile (';');
		deleteToken (name);
		return;
	}

	int index = makeVhdlTag (name, VHDLTAG_ARCHITECTURE);
	readToken (token);
	if (isKeyword (token, KEYWORD_OF))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
		{
			/* Filling scope field of this architecture.
			   If the definition for the entity can be found in the symbol table,
			   use its cork as the scope. If not, use the reference tag for the
			   entity as fallback. */
			int role_index = makeSimpleRefTag (token->string,
											   VHDLTAG_ENTITY, VHDL_ENTITY_DESIGNED);
			int entity_index = anyKindEntryInScope (CORK_NIL,
													vStringValue (token->string),
													VHDLTAG_ENTITY);
			tagEntryInfo *e = getEntryInCorkQueue (index);
			if (e)
			{
				e->extensionFields.scopeIndex = (
					entity_index == CORK_NIL
					? role_index
					: entity_index);

				/* TODO: append thes architecture name to
				 * architecture: field of *e*. */
			}

			attachParserFieldToCorkEntry (role_index,
										  VhdlFields[F_ARCHITECTURE].ftype,
										  vStringValue (name->string));

			readToken (token);
			if (isKeyword (token, KEYWORD_IS))
			{
				parseTillBegin (token, index);
				parseTillEnd (token, index, KEYWORD_ARCHITECTURE);
			}
		}
	}
	deleteToken (name);
}

static void parseSignal (tokenInfo * const token, int parent)
{
	readToken (token);
	parseDeclElement (token, VHDLTAG_SIGNAL, parent, true);
}

static void parseLabel (tokenInfo * const name, int parent)
{
	tokenInfo *const token = newToken ();

	readToken (token);
	if (isType (token, TOKEN_COLON))
	{
		readToken (token);
		if (isType (token, TOKEN_KEYWORD))
			parseKeywords (token, name, parent);
	}
	deleteToken (token);
}

static void parseProcess (tokenInfo * const token, tokenInfo * const label, int parent)
{
	tokenInfo *process = label? label: copyToken (token);

	if (label == NULL)
	{
		process->type = TOKEN_IDENTIFIER;
		vStringClear (process->string);
		anonGenerate (process->string, "anonProcess", VHDLTAG_PROCESS);
	}

	int index = makeVhdlTagWithScope (process, VHDLTAG_PROCESS, parent);

	if (label == NULL)
	{
		tagEntryInfo *e = getEntryInCorkQueue (index);
		if (e)
			markTagExtraBit (e, XTAG_ANONYMOUS);
		deleteToken (process);
	}

	skipToMatched (token);
	parseTillBegin (token, index);
	parseTillEnd (token, index, KEYWORD_PROCESS);
}

static void parseVariable (tokenInfo * const token, int parent)
{
	readToken (token);
	parseDeclElement (token, VHDLTAG_VARIABLE, parent, true);
}

static void parseAlias (tokenInfo * const token, int parent)
{
	readToken (token);
	parseDeclElement (token, VHDLTAG_ALIAS, parent, true);
}

/* TODO */
/* records */
static void parseKeywords (tokenInfo * const token, tokenInfo * const label, int index)
{
	switch (token->keyword)
	{
	case KEYWORD_END:
		skipToCharacterInInputFile (';');
		break;
	case KEYWORD_CONSTANT:
		parseConstant (index);
		break;
	case KEYWORD_TYPE:
		parseTypes (token, index);
		break;
	case KEYWORD_SUBTYPE:
		parseTypes (token, index);
		break;
	case KEYWORD_ENTITY:
		parseModule (token, index);
		break;
	case KEYWORD_COMPONENT:
		parseModule (token, index);
		break;
	case KEYWORD_FUNCTION:
		parseSubProgram (token, index);
		break;
	case KEYWORD_PROCEDURE:
		parseSubProgram (token, index);
		break;
	case KEYWORD_PACKAGE:
		parsePackage (token);
		break;
	case KEYWORD_ARCHITECTURE:
		parseArchitecture (token);
		break;
	case KEYWORD_SIGNAL:
		parseSignal (token, index);
		break;
	case KEYWORD_PROCESS:
		parseProcess (token, label, index);
		break;
	case KEYWORD_VARIABLE:
		parseVariable (token, index);
		break;
	case KEYWORD_ALIAS:
		parseAlias (token, index);
		break;
	default:
		if (isType (token, TOKEN_IDENTIFIER))
			parseLabel (token, index);
		break;
	}
}

static tokenType parseVhdlFile (tokenInfo * const token)
{
	do
	{
		readToken (token);
		parseKeywords (token, NULL, CORK_NIL);
	} while (!isKeyword (token, KEYWORD_END) && !isType (token, TOKEN_EOF));
	return token->type;
}

static void findVhdlTags (void)
{
	tokenInfo *const token = newToken ();

	while (parseVhdlFile (token) != TOKEN_EOF);

	deleteToken (token);
}

extern parserDefinition *VhdlParser (void)
{
	static const char *const extensions[] = { "vhdl", "vhd", NULL };
	parserDefinition *def = parserNew ("VHDL");
	def->kindTable = VhdlKinds;
	def->kindCount = ARRAY_SIZE (VhdlKinds);
	def->extensions = extensions;
	def->parser = findVhdlTags;
	def->initialize = initialize;
	def->keywordTable = VhdlKeywordTable;
	def->keywordCount = ARRAY_SIZE (VhdlKeywordTable);
	def->fieldTable = VhdlFields;
	def->fieldCount = ARRAY_SIZE (VhdlFields);
	def->useCork = CORK_QUEUE|CORK_SYMTAB;
	return def;
}
