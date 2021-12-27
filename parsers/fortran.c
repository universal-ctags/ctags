/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Fortran language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <limits.h>
#include <ctype.h>  /* to define tolower () */

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"

/*
*   MACROS
*/
#define isident(c)              (isalnum(c) || (c) == '_')
#define isBlank(c)              (bool) (c == ' ' || c == '\t')
#define isType(token,t)         (bool) ((token)->type == (t))
#define isKeyword(token,k)      (bool) ((token)->keyword == (k))
#define isSecondaryKeyword(token,k)  (bool) ((token)->secondary == NULL ? \
	false : (token)->secondary->keyword == (k))

/*
*   DATA DECLARATIONS
*/
/*  Used to designate type of line read in fixed source form.
 */
typedef enum eFortranLineType {
	LTYPE_UNDETERMINED,
	LTYPE_INVALID,
	LTYPE_COMMENT,
	LTYPE_CONTINUATION,
	LTYPE_EOF,
	LTYPE_INITIAL,
	LTYPE_SHORT
} lineType;

/*  Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_abstract,
	KEYWORD_allocatable,
	KEYWORD_assignment,
	KEYWORD_associate,
	KEYWORD_automatic,
	KEYWORD_bind,
	KEYWORD_block,
	KEYWORD_byte,
	KEYWORD_cexternal,
	KEYWORD_cglobal,
	KEYWORD_class,
	KEYWORD_character,
	KEYWORD_codimension,
	KEYWORD_common,
	KEYWORD_complex,
	KEYWORD_contains,
	KEYWORD_data,
	KEYWORD_deferred,
	KEYWORD_dimension,
	KEYWORD_dllexport,
	KEYWORD_dllimport,
	KEYWORD_do,
	KEYWORD_double,
	KEYWORD_elemental,
	KEYWORD_end,
	KEYWORD_entry,
	KEYWORD_enum,
	KEYWORD_enumerator,
	KEYWORD_equivalence,
	KEYWORD_extends,
	KEYWORD_external,
	KEYWORD_final,
	KEYWORD_forall,
	KEYWORD_format,
	KEYWORD_function,
	KEYWORD_generic,
	KEYWORD_if,
	KEYWORD_implicit,
	KEYWORD_import,
	KEYWORD_include,
	KEYWORD_inline,
	KEYWORD_integer,
	KEYWORD_intent,
	KEYWORD_interface,
	KEYWORD_intrinsic,
	KEYWORD_logical,
	KEYWORD_map,
	KEYWORD_module,
	KEYWORD_namelist,
	KEYWORD_non_overridable,
	KEYWORD_nopass,
	KEYWORD_operator,
	KEYWORD_optional,
	KEYWORD_parameter,
	KEYWORD_pascal,
	KEYWORD_pass,
	KEYWORD_pexternal,
	KEYWORD_pglobal,
	KEYWORD_pointer,
	KEYWORD_precision,
	KEYWORD_private,
	KEYWORD_procedure,
	KEYWORD_program,
	KEYWORD_protected,
	KEYWORD_public,
	KEYWORD_pure,
	KEYWORD_real,
	KEYWORD_record,
	KEYWORD_recursive,
	KEYWORD_save,
	KEYWORD_select,
	KEYWORD_sequence,
	KEYWORD_static,
	KEYWORD_stdcall,
	KEYWORD_structure,
	KEYWORD_submodule,
	KEYWORD_subroutine,
	KEYWORD_target,
	KEYWORD_then,
	KEYWORD_type,
	KEYWORD_union,
	KEYWORD_use,
	KEYWORD_value,
	KEYWORD_virtual,
	KEYWORD_volatile,
	KEYWORD_where,
	KEYWORD_while
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_COMMA,
	TOKEN_DOUBLE_COLON,
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_LABEL,
	TOKEN_NUMERIC,
	TOKEN_OPERATOR,
	TOKEN_PAREN_CLOSE,
	TOKEN_PAREN_OPEN,
	TOKEN_SQUARE_OPEN,
	TOKEN_SQUARE_CLOSE,
	TOKEN_PERCENT,
	TOKEN_STATEMENT_END,
	TOKEN_STRING,
	TOKEN_COLON,
} tokenType;

typedef enum eTagType {
	TAG_UNDEFINED = -1,
	TAG_BLOCK_DATA,
	TAG_COMMON_BLOCK,
	TAG_ENTRY_POINT,
	TAG_ENUM,
	TAG_FUNCTION,
	TAG_INTERFACE,
	TAG_COMPONENT,
	TAG_LABEL,
	TAG_LOCAL,
	TAG_MODULE,
	TAG_METHOD,
	TAG_NAMELIST,
	TAG_ENUMERATOR,
	TAG_PROGRAM,
	TAG_PROTOTYPE,
	TAG_SUBROUTINE,
	TAG_DERIVED_TYPE,
	TAG_VARIABLE,
	TAG_SUBMODULE,
	TAG_COUNT  /* must be last */
} tagType;

typedef enum eImplementation {
	IMP_DEFAULT,
	IMP_ABSTRACT,
	IMP_DEFERRED,
	IMP_NON_OVERRIDABLE,
	IMP_COUNT
} impType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	tagType tag;
	vString* string;
	vString* parentType;
	vString* signature;
	impType implementation;
	bool isMethod;
	struct sTokenInfo *secondary;
	unsigned long lineNumber;
	MIOPos filePosition;
	bool anonymous;
} tokenInfo;

/*
*   DATA DEFINITIONS
*/

static langType Lang_fortran;
static int Ungetc;
static unsigned int Column;
static bool FreeSourceForm;
static bool FreeSourceFormFound = false;
static bool ParsingString;

/* indexed by tagType */
static kindDefinition FortranKinds [] = {
	{ true,  'b', "blockData",  "block data"},
	{ true,  'c', "common",     "common blocks"},
	{ true,  'e', "entry",      "entry points"},
	{ true,  'E', "enum",       "enumerations"},
	{ true,  'f', "function",   "functions"},
	{ true,  'i', "interface",  "interface contents, generic names, and operators"},
	{ true,  'k', "component",  "type and structure components"},
	{ true,  'l', "label",      "labels"},
	{ false, 'L', "local",      "local, common block, and namelist variables"},
	{ true,  'm', "module",     "modules"},
	{ true,  'M', "method",     "type bound procedures"},
	{ true,  'n', "namelist",   "namelists"},
	{ true,  'N', "enumerator", "enumeration values"},
	{ true,  'p', "program",    "programs"},
	{ false, 'P', "prototype",  "subprogram prototypes"},
	{ true,  's', "subroutine", "subroutines"},
	{ true,  't', "type",       "derived types and structures"},
	{ true,  'v', "variable",   "program (global) and module variables"},
	{ true,  'S', "submodule",  "submodules"},
};

/* For definitions of Fortran 77 with extensions:
 * http://www.fortran.com/fortran/F77_std/rjcnf0001.html
 * http://scienide.uwaterloo.ca/MIPSpro7/007-2362-004/sgi_html/index.html
 *
 * For the Compaq Fortran Reference Manual:
 * http://h18009.www1.hp.com/fortran/docs/lrm/dflrm.htm
 */

static const keywordTable FortranKeywordTable [] = {
	/* keyword          keyword ID */
	{ "abstract",       KEYWORD_abstract     },
	{ "allocatable",    KEYWORD_allocatable  },
	{ "assignment",     KEYWORD_assignment   },
	{ "associate",      KEYWORD_associate    },
	{ "automatic",      KEYWORD_automatic    },
	{ "bind",           KEYWORD_bind         },
	{ "block",          KEYWORD_block        },
	{ "byte",           KEYWORD_byte         },
	{ "cexternal",      KEYWORD_cexternal    },
	{ "cglobal",        KEYWORD_cglobal      },
	{ "class",          KEYWORD_class        },
	{ "character",      KEYWORD_character    },
	{ "codimension",    KEYWORD_codimension  },
	{ "common",         KEYWORD_common       },
	{ "complex",        KEYWORD_complex      },
	{ "contains",       KEYWORD_contains     },
	{ "data",           KEYWORD_data         },
	{ "deferred",       KEYWORD_deferred     },
	{ "dimension",      KEYWORD_dimension    },
	{ "dll_export",     KEYWORD_dllexport    },
	{ "dll_import",     KEYWORD_dllimport    },
	{ "do",             KEYWORD_do           },
	{ "double",         KEYWORD_double       },
	{ "elemental",      KEYWORD_elemental    },
	{ "end",            KEYWORD_end          },
	{ "entry",          KEYWORD_entry        },
	{ "enum",           KEYWORD_enum         },
	{ "enumerator",     KEYWORD_enumerator   },
	{ "equivalence",    KEYWORD_equivalence  },
	{ "extends",        KEYWORD_extends      },
	{ "external",       KEYWORD_external     },
	{ "final",          KEYWORD_final        },
	{ "forall",         KEYWORD_forall       },
	{ "format",         KEYWORD_format       },
	{ "function",       KEYWORD_function     },
	{ "generic",        KEYWORD_generic      },
	{ "if",             KEYWORD_if           },
	{ "implicit",       KEYWORD_implicit     },
	{ "import",         KEYWORD_import       },
	{ "include",        KEYWORD_include      },
	{ "inline",         KEYWORD_inline       },
	{ "integer",        KEYWORD_integer      },
	{ "intent",         KEYWORD_intent       },
	{ "interface",      KEYWORD_interface    },
	{ "intrinsic",      KEYWORD_intrinsic    },
	{ "logical",        KEYWORD_logical      },
	{ "map",            KEYWORD_map          },
	{ "module",         KEYWORD_module       },
	{ "namelist",       KEYWORD_namelist     },
	{ "non_overridable", KEYWORD_non_overridable },
	{ "nopass",         KEYWORD_nopass       },
	{ "operator",       KEYWORD_operator     },
	{ "optional",       KEYWORD_optional     },
	{ "parameter",      KEYWORD_parameter    },
	{ "pascal",         KEYWORD_pascal       },
	{ "pass",           KEYWORD_pass         },
	{ "pexternal",      KEYWORD_pexternal    },
	{ "pglobal",        KEYWORD_pglobal      },
	{ "pointer",        KEYWORD_pointer      },
	{ "precision",      KEYWORD_precision    },
	{ "private",        KEYWORD_private      },
	{ "procedure",      KEYWORD_procedure    },
	{ "program",        KEYWORD_program      },
	{ "protected",      KEYWORD_protected    },
	{ "public",         KEYWORD_public       },
	{ "pure",           KEYWORD_pure         },
	{ "real",           KEYWORD_real         },
	{ "record",         KEYWORD_record       },
	{ "recursive",      KEYWORD_recursive    },
	{ "save",           KEYWORD_save         },
	{ "select",         KEYWORD_select       },
	{ "sequence",       KEYWORD_sequence     },
	{ "static",         KEYWORD_static       },
	{ "stdcall",        KEYWORD_stdcall      },
	{ "structure",      KEYWORD_structure    },
	{ "submodule",      KEYWORD_submodule   },
	{ "subroutine",     KEYWORD_subroutine   },
	{ "target",         KEYWORD_target       },
	{ "then",           KEYWORD_then         },
	{ "type",           KEYWORD_type         },
	{ "union",          KEYWORD_union        },
	{ "use",            KEYWORD_use          },
	{ "value",          KEYWORD_value        },
	{ "virtual",        KEYWORD_virtual      },
	{ "volatile",       KEYWORD_volatile     },
	{ "where",          KEYWORD_where        },
	{ "while",          KEYWORD_while        }
};

static struct {
	unsigned int count;
	unsigned int max;
	tokenInfo* list;
} Ancestors = { 0, 0, NULL };

/*
*   FUNCTION PROTOTYPES
*/
static void parseStructureStmt (tokenInfo *const token);
static void parseUnionStmt (tokenInfo *const token);
static void parseDerivedTypeDef (tokenInfo *const token);
static void parseSubprogram (tokenInfo *const token);

/*
*   FUNCTION DEFINITIONS
*/

static void ancestorPush (tokenInfo *const token)
{
	enum { incrementalIncrease = 10 };
	if (Ancestors.list == NULL)
	{
		Assert (Ancestors.max == 0);
		Ancestors.count = 0;
		Ancestors.max   = incrementalIncrease;
		Ancestors.list  = xMalloc (Ancestors.max, tokenInfo);
	}
	else if (Ancestors.count == Ancestors.max)
	{
		Ancestors.max += incrementalIncrease;
		Ancestors.list = xRealloc (Ancestors.list, Ancestors.max, tokenInfo);
	}
	Ancestors.list [Ancestors.count] = *token;
	Ancestors.list [Ancestors.count].string = vStringNewCopy (token->string);
	Ancestors.list [Ancestors.count].signature = token->signature? vStringNewCopy (token->signature): NULL;
	Ancestors.count++;
}

static void ancestorPop (void)
{
	Assert (Ancestors.count > 0);
	--Ancestors.count;
	vStringDelete (Ancestors.list [Ancestors.count].string);
	vStringDelete (Ancestors.list [Ancestors.count].signature);

	Ancestors.list [Ancestors.count].type       = TOKEN_UNDEFINED;
	Ancestors.list [Ancestors.count].keyword    = KEYWORD_NONE;
	Ancestors.list [Ancestors.count].secondary  = NULL;
	Ancestors.list [Ancestors.count].tag        = TAG_UNDEFINED;
	Ancestors.list [Ancestors.count].string     = NULL;
	Ancestors.list [Ancestors.count].lineNumber = 0L;
	Ancestors.list [Ancestors.count].implementation = IMP_DEFAULT;
	Ancestors.list [Ancestors.count].isMethod   = false;
}

static const tokenInfo* ancestorScope (void)
{
	tokenInfo *result = NULL;
	unsigned int i;
	for (i = Ancestors.count  ;  i > 0  &&  result == NULL ;  --i)
	{
		tokenInfo *const token = Ancestors.list + i - 1;
		if (token->type == TOKEN_IDENTIFIER &&
			token->tag != TAG_UNDEFINED)
			result = token;
	}
	return result;
}

static const tokenInfo* ancestorTop (void)
{
	Assert (Ancestors.count > 0);
	return &Ancestors.list [Ancestors.count - 1];
}

#define ancestorCount() (Ancestors.count)

static void ancestorClear (void)
{
	while (Ancestors.count > 0)
		ancestorPop ();
	if (Ancestors.list != NULL)
		eFree (Ancestors.list);
	Ancestors.list = NULL;
	Ancestors.count = 0;
	Ancestors.max = 0;
}

static bool insideInterface (void)
{
	bool result = false;
	unsigned int i;
	for (i = 0  ;  i < Ancestors.count && !result ;  ++i)
	{
		if (Ancestors.list [i].tag == TAG_INTERFACE)
			result = true;
	}
	return result;
}

/*
*   Tag generation functions
*/
static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type         = TOKEN_UNDEFINED;
	token->keyword      = KEYWORD_NONE;
	token->tag          = TAG_UNDEFINED;
	token->string       = vStringNew ();
	token->secondary    = NULL;
	token->parentType   = NULL;
	token->signature    = NULL;
	token->implementation = IMP_DEFAULT;
	token->isMethod     = false;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	token->anonymous    = false;

	return token;
}

static tokenInfo *newTokenFromFull (tokenInfo *const token, bool copyStr)
{
	tokenInfo *result = xMalloc (1, tokenInfo);
	*result = *token;
	result->string = copyStr? vStringNewCopy (token->string): vStringNew();
	token->secondary = NULL;
	token->parentType = NULL;
	token->signature = NULL;
	return result;
}

static tokenInfo *newTokenFrom (tokenInfo *const token)
{
	return newTokenFromFull (token, true);
}

static tokenInfo *newAnonTokenFrom (tokenInfo *const token, unsigned int uTagKind)
{
	tokenInfo *result = newTokenFromFull (token, false);
	result->anonymous = true;
	anonGenerate (result->string, "__anon", uTagKind);
	return result;
}

static void deleteToken (tokenInfo *const token)
{
	if (token != NULL)
	{
		vStringDelete (token->string);
		vStringDelete (token->parentType);
		vStringDelete (token->signature);
		deleteToken (token->secondary);
		token->secondary = NULL;
		eFree (token);
	}
}

static bool isFileScope (const tagType type)
{
	return (bool) (type == TAG_LABEL || type == TAG_LOCAL);
}

static bool includeTag (const tagType type)
{
	bool include;
	Assert (type != TAG_UNDEFINED);
	include = FortranKinds [(int) type].enabled;
	if (include && isFileScope (type))
		include = isXtagEnabled(XTAG_FILE_SCOPE);
	return include;
}

static const char *implementationString (const impType imp)
{
	static const char *const names [] ={
		"?", "abstract", "deferred", "non_overridable"
	};
	Assert (ARRAY_SIZE (names) == IMP_COUNT);
	Assert ((int) imp < IMP_COUNT);
	return names [(int) imp];
}

static void makeFortranTag (tokenInfo *const token, tagType tag)
{
	token->tag = tag;
	if (includeTag (token->tag))
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;

		initTagEntry (&e, name, token->tag);

		if (token->tag == TAG_COMMON_BLOCK)
			e.lineNumberEntry = canUseLineNumberAsLocator();

		if (token->anonymous)
			markTagExtraBit (&e, XTAG_ANONYMOUS);

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;
		e.isFileScope	= isFileScope (token->tag);
		if (e.isFileScope)
			markTagExtraBit (&e, XTAG_FILE_SCOPE);
		e.truncateLineAfterTag = (bool) (token->tag != TAG_LABEL);

		if (ancestorCount () > 0)
		{
			const tokenInfo* const scope = ancestorScope ();
			if (scope != NULL)
			{
				e.extensionFields.scopeKindIndex = scope->tag;
				e.extensionFields.scopeName = vStringValue (scope->string);
			}
		}
		if (token->parentType != NULL &&
		    vStringLength (token->parentType) > 0 &&
		    (token->tag == TAG_DERIVED_TYPE || (token->tag == TAG_SUBMODULE)))
			e.extensionFields.inheritance = vStringValue (token->parentType);
		if (token->implementation != IMP_DEFAULT)
			e.extensionFields.implementation =
				implementationString (token->implementation);
		if (token->signature &&
			vStringLength (token->signature) > 0 &&
			(token->tag == TAG_FUNCTION ||
			 token->tag == TAG_SUBROUTINE ||
			 token->tag == TAG_PROTOTYPE))
			e.extensionFields.signature = vStringValue (token->signature);
		makeTagEntry (&e);
	}
}

/*
*   Parsing functions
*/

static int skipLine (void)
{
	int c;

	do
		c = getcFromInputFile ();
	while (c != EOF  &&  c != '\n');

	return c;
}

static void makeLabelTag (vString *const label)
{
	tokenInfo *token = newToken ();
	token->type  = TOKEN_LABEL;
	vStringCopy (token->string, label);
	makeFortranTag (token, TAG_LABEL);
	deleteToken (token);
}

static lineType getLineType (void)
{
	static vString *label = NULL;
	int column = 0;
	lineType type = LTYPE_UNDETERMINED;

	label = vStringNewOrClear (label);
	do  /* read in first 6 "margin" characters */
	{
		int c = getcFromInputFile ();

		/* 3.2.1  Comment_Line.  A comment line is any line that contains
		 * a C or an asterisk in column 1, or contains only blank characters
		 * in  columns 1 through 72.  A comment line that contains a C or
		 * an asterisk in column 1 may contain any character capable  of
		 * representation in the processor in columns 2 through 72.
		 */
		/*  EXCEPTION! Some compilers permit '!' as a comment character here.
		 *
		 *  Treat # and $ in column 1 as comment to permit preprocessor directives.
		 *  Treat D and d in column 1 as comment for HP debug statements.
		 */
		if (column == 0  &&  strchr ("*Cc!#$Dd", c) != NULL)
			type = LTYPE_COMMENT;
		else if (c == '\t')  /* EXCEPTION! Some compilers permit a tab here */
		{
			column = 8;
			type = LTYPE_INITIAL;
		}
		else if (column == 5)
		{
			/* 3.2.2  Initial_Line.  An initial line is any line that is not
			 * a comment line and contains the character blank or the digit 0
			 * in column 6.  Columns 1 through 5 may contain a statement label
			 * (3.4), or each of the columns 1 through 5 must contain the
			 * character blank.
			 */
			if (c == ' '  ||  c == '0')
				type = LTYPE_INITIAL;

			/* 3.2.3  Continuation_Line.  A continuation line is any line that
			 * contains any character of the FORTRAN character set other than
			 * the character blank or the digit 0 in column 6 and contains
			 * only blank characters in columns 1 through 5.
			 */
			else if (vStringLength (label) == 0)
				type = LTYPE_CONTINUATION;
			else
				type = LTYPE_INVALID;
		}
		else if (c == ' ')
			;
		else if (c == EOF)
			type = LTYPE_EOF;
		else if (c == '\n')
			type = LTYPE_SHORT;
		else if (isdigit (c))
			vStringPut (label, c);
		else
			type = LTYPE_INVALID;

		++column;
	} while (column < 6  &&  type == LTYPE_UNDETERMINED);

	Assert (type != LTYPE_UNDETERMINED);

	if (vStringLength (label) > 0)
		makeLabelTag (label);
	return type;
}

static int getFixedFormChar (void)
{
	bool newline = false;
	lineType type;
	int c = '\0';

	if (Column > 0)
	{
#ifdef STRICT_FIXED_FORM
		/*  EXCEPTION! Some compilers permit more than 72 characters per line.
		 */
		if (Column > 71)
			c = skipLine ();
		else
#endif
		{
			c = getcFromInputFile ();
			++Column;
		}
		if (c == '\n')
		{
			newline = true;  /* need to check for continuation line */
			Column = 0;
		}
		else if (c == '!'  &&  ! ParsingString)
		{
			c = skipLine ();
			newline = true;  /* need to check for continuation line */
			Column = 0;
		}
		else if (c == '&')  /* check for free source form */
		{
			const int c2 = getcFromInputFile ();
			if (c2 == '\n')
				FreeSourceFormFound = true;
			else
				ungetcToInputFile (c2);
		}
	}
	while (Column == 0)
	{
		type = getLineType ();
		switch (type)
		{
			case LTYPE_UNDETERMINED:
			case LTYPE_INVALID:
				FreeSourceFormFound = true;
				if (! FreeSourceForm)
				    return EOF;

			case LTYPE_SHORT: break;
			case LTYPE_COMMENT: skipLine (); break;

			case LTYPE_EOF:
				Column = 6;
				if (newline)
					c = '\n';
				else
					c = EOF;
				break;

			case LTYPE_INITIAL:
				if (newline)
				{
					c = '\n';
					Column = 6;
					break;
				}
				/* fall through to next case */
			case LTYPE_CONTINUATION:
				Column = 5;
				do
				{
					c = getcFromInputFile ();
					++Column;
				} while (isBlank (c));
				if (c == '\n')
					Column = 0;
				else if (Column > 6)
				{
					ungetcToInputFile (c);
					c = ' ';
				}
				break;

			default:
				Assert ("Unexpected line type" == NULL);
		}
	}
	return c;
}

static int skipToNextLine (void)
{
	int c = skipLine ();
	if (c != EOF)
		c = getcFromInputFile ();
	return c;
}

static int getFreeFormChar (void)
{
	static bool newline = true;
	bool advanceLine = false;
	int c = getcFromInputFile ();

	/* If the last nonblank, non-comment character of a FORTRAN 90
	 * free-format text line is an ampersand then the next non-comment
	 * line is a continuation line.
	 */
	if (c == '&')
	{
		do
			c = getcFromInputFile ();
		while (isspace (c)  &&  c != '\n');
		if (c == '\n')
		{
			newline = true;
			advanceLine = true;
		}
		else if (c == '!')
			advanceLine = true;
		else
		{
			ungetcToInputFile (c);
			c = '&';
		}
	}
	else if (newline && (c == '!' || c == '#'))
		advanceLine = true;
	while (advanceLine)
	{
		while (isspace (c))
			c = getcFromInputFile ();
		if (c == '!' || (newline && c == '#'))
		{
			c = skipToNextLine ();
			newline = true;
			continue;
		}
		if (c == '&')
			c = getcFromInputFile ();
		else
			advanceLine = false;
	}
	newline = (bool) (c == '\n');
	return c;
}

static int getChar (void)
{
	int c;

	if (Ungetc != '\0')
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	else if (FreeSourceForm)
		c = getFreeFormChar ();
	else
		c = getFixedFormChar ();
	return c;
}

static void ungetChar (const int c)
{
	Ungetc = c;
}

/*  If a numeric is passed in 'c', this is used as the first digit of the
 *  numeric being parsed.
 */
static vString *parseInteger (int c)
{
	vString *string = vStringNew ();

	if (c == '-')
	{
		vStringPut (string, c);
		c = getChar ();
	}
	else if (! isdigit (c))
		c = getChar ();
	while (c != EOF  &&  isdigit (c))
	{
		vStringPut (string, c);
		c = getChar ();
	}

	if (c == '_')
	{
		do
			c = getChar ();
		while (c != EOF  &&  isalpha (c));
	}
	ungetChar (c);

	return string;
}

static vString *parseNumeric (int c)
{
	vString *string = parseInteger (c);

	c = getChar ();
	if (c == '.')
	{
		vString *integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
		c = getChar ();
	}
	if (tolower (c) == 'e')
	{
		vString *integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
	}
	else
		ungetChar (c);

	return string;
}

static void parseString (vString *const string, const int delimiter)
{
	const unsigned long inputLineNumber = getInputLineNumber ();
	int c;
	ParsingString = true;
	c = getChar ();
	while (c != delimiter  &&  c != '\n'  &&  c != EOF)
	{
		vStringPut (string, c);
		c = getChar ();
	}
	if (c == '\n'  ||  c == EOF)
	{
		verbose ("%s: unterminated character string at line %lu\n",
				getInputFileName (), inputLineNumber);
		if (c != EOF && ! FreeSourceForm)
			FreeSourceFormFound = true;
	}
	ParsingString = false;
}

/*  Read a C identifier beginning with "firstChar" and places it into "name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;

	do
	{
		vStringPut (string, c);
		c = getChar ();
	} while (isident (c));

	ungetChar (c);  /* unget non-identifier character */
}

static void checkForLabel (void)
{
	tokenInfo* token = NULL;
	int length;
	int c;

	do
		c = getChar ();
	while (isBlank (c));

	for (length = 0  ;  isdigit (c)  &&  length < 5  ;  ++length)
	{
		if (token == NULL)
		{
			token = newToken ();
			token->type = TOKEN_LABEL;
		}
		vStringPut (token->string, c);
		c = getChar ();
	}
	if (length > 0  &&  token != NULL)
	{
		makeFortranTag (token, TAG_LABEL);
		deleteToken (token);
	}
	ungetChar (c);
}

static void readIdentifier (tokenInfo *const token, const int c)
{
	parseIdentifier (token->string, c);
	token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_fortran);
	if (! isKeyword (token, KEYWORD_NONE))
		token->type = TOKEN_KEYWORD;
	else
	{
		token->type = TOKEN_IDENTIFIER;
		if (strncmp (vStringValue (token->string), "end", 3) == 0)
		{
			vString *const sub = vStringNewInit (vStringValue (token->string) + 3);
			const keywordId kw = lookupCaseKeyword (vStringValue (sub), Lang_fortran);
			vStringDelete (sub);
			if (kw != KEYWORD_NONE)
			{
				token->secondary = newToken ();
				token->secondary->type = TOKEN_KEYWORD;
				token->secondary->keyword = kw;
				token->keyword = KEYWORD_end;
			}
		}
	}
}

static void readToken (tokenInfo *const token)
{
	int c;

	deleteToken (token->secondary);
	token->type        = TOKEN_UNDEFINED;
	token->tag         = TAG_UNDEFINED;
	token->keyword     = KEYWORD_NONE;
	token->secondary   = NULL;
	token->implementation = IMP_DEFAULT;
	vStringClear (token->string);
	vStringDelete (token->parentType);
	vStringDelete (token->signature);
	token->parentType = NULL;
	token->isMethod = false;
	token->signature = NULL;

getNextChar:
	c = getChar ();

	token->lineNumber	= getInputLineNumber ();
	token->filePosition	= getInputFilePosition ();

	switch (c)
	{
		case EOF:  token->type = TOKEN_EOF;         break;
		case ' ':  goto getNextChar;
		case '\t': goto getNextChar;
		case ',':  token->type = TOKEN_COMMA;       break;
		case '(':  token->type = TOKEN_PAREN_OPEN;  break;
		case ')':  token->type = TOKEN_PAREN_CLOSE; break;
		case '[':  token->type = TOKEN_SQUARE_OPEN; break;
		case ']':  token->type = TOKEN_SQUARE_CLOSE; break;
		case '%':  token->type = TOKEN_PERCENT;     break;

		case '*':
		case '/':
		case '+':
		case '-':
		case '=':
		case '<':
		case '>':
		{
			const char *const operatorChars = "*/+=<>";
			do {
				vStringPut (token->string, c);
				c = getChar ();
			} while (strchr (operatorChars, c) != NULL);
			ungetChar (c);
			token->type = TOKEN_OPERATOR;
			break;
		}

		case '!':
			if (FreeSourceForm)
			{
				do
				   c = getChar ();
				while (c != '\n' && c != EOF);
			}
			else
			{
				skipLine ();
				Column = 0;
			}
			/* fall through to newline case */
		case '\n':
			token->type = TOKEN_STATEMENT_END;
			if (FreeSourceForm)
				checkForLabel ();
			break;

		case '.':
			parseIdentifier (token->string, c);
			c = getChar ();
			if (c == '.')
			{
				vStringPut (token->string, c);
				token->type = TOKEN_OPERATOR;
			}
			else
			{
				ungetChar (c);
				token->type = TOKEN_UNDEFINED;
			}
			break;

		case '"':
		case '\'':
			parseString (token->string, c);
			token->type = TOKEN_STRING;
			break;

		case ';':
			token->type = TOKEN_STATEMENT_END;
			break;

		case ':':
			c = getChar ();
			if (c == ':')
				token->type = TOKEN_DOUBLE_COLON;
			else
			{
				ungetChar (c);
				token->type = TOKEN_COLON;
			}
			break;

		default:
			if (isalpha (c))
				readIdentifier (token, c);
			else if (isdigit (c))
			{
				vString *numeric = parseNumeric (c);
				vStringCat (token->string, numeric);
				vStringDelete (numeric);
				token->type = TOKEN_NUMERIC;
			}
			else
				token->type = TOKEN_UNDEFINED;
			break;
	}
}

static void readSubToken (tokenInfo *const token)
{
	if (token->secondary == NULL)
	{
		token->secondary = newToken ();
		readToken (token->secondary);
	}
}

/*
*   Scanning functions
*/

static void skipToToken (tokenInfo *const token, tokenType type)
{
	while (! isType (token, type) && ! isType (token, TOKEN_STATEMENT_END) &&
			!(token->secondary != NULL && isType (token->secondary, TOKEN_STATEMENT_END)) &&
			! isType (token, TOKEN_EOF))
		readToken (token);
}

static void skipPast (tokenInfo *const token, tokenType type)
{
	skipToToken (token, type);
	if (! isType (token, TOKEN_STATEMENT_END))
		readToken (token);
}

static void skipToNextStatement (tokenInfo *const token)
{
	do
	{
		skipToToken (token, TOKEN_STATEMENT_END);
		readToken (token);
	} while (isType (token, TOKEN_STATEMENT_END));
}

/* skip over paired tokens, managing nested pairs and stopping at statement end
 * or right after closing token, whatever comes first.
 */
static void skipOverPairsFull (tokenInfo *const token,
							   tokenType topen,
							   tokenType tclose,
							   void (* token_cb) (tokenInfo *const, void *),
							   void *user_data)
{
	int level = 0;
	do {
		if (isType (token, TOKEN_STATEMENT_END))
			break;
		else if (isType (token, topen))
			++level;
		else if (isType (token, tclose))
			--level;
		else if (token_cb)
			token_cb (token, user_data);
		readToken (token);
	} while (level > 0 && !isType (token, TOKEN_EOF));
}

static void skipOverParensFull (tokenInfo *const token,
								void (* token_cb) (tokenInfo *const, void *),
								void *user_data)
{
	skipOverPairsFull (token, TOKEN_PAREN_OPEN,
					   TOKEN_PAREN_CLOSE,
					   token_cb, user_data);
}

static void skipOverSquaresFull (tokenInfo *const token,
								 void (* token_cb) (tokenInfo *const, void *),
								 void *user_data)
{
	skipOverPairsFull (token, TOKEN_SQUARE_OPEN,
					   TOKEN_SQUARE_CLOSE,
					   token_cb, user_data);
}

static void skipOverParens (tokenInfo *const token)
{
	skipOverParensFull (token, NULL, NULL);
}

static void skipOverSquares (tokenInfo *const token)
{
	skipOverSquaresFull (token, NULL, NULL);
}

static bool isTypeSpec (tokenInfo *const token)
{
	bool result;
	switch (token->keyword)
	{
		case KEYWORD_byte:
		case KEYWORD_integer:
		case KEYWORD_real:
		case KEYWORD_double:
		case KEYWORD_complex:
		case KEYWORD_character:
		case KEYWORD_logical:
		case KEYWORD_record:
		case KEYWORD_type:
		case KEYWORD_procedure:
		case KEYWORD_final:
		case KEYWORD_generic:
		case KEYWORD_class:
		case KEYWORD_enumerator:
			result = true;
			break;
		default:
			result = false;
			break;
	}
	return result;
}

static bool isSubprogramPrefix (tokenInfo *const token)
{
	bool result;
	switch (token->keyword)
	{
		case KEYWORD_elemental:
		case KEYWORD_pure:
		case KEYWORD_recursive:
		case KEYWORD_stdcall:
			result = true;
			break;
		default:
			result = false;
			break;
	}
	return result;
}

static void parseKindSelector (tokenInfo *const token)
{
	if (isType (token, TOKEN_PAREN_OPEN))
		skipOverParens (token);  /* skip kind-selector */
	if (isType (token, TOKEN_OPERATOR) &&
		strcmp (vStringValue (token->string), "*") == 0)
	{
		readToken (token);
		if (isType (token, TOKEN_PAREN_OPEN))
			skipOverParens (token);
		else
			readToken (token);
	}
}

/*  type-spec
 *      is INTEGER [kind-selector]
 *      or REAL [kind-selector] is ( etc. )
 *      or DOUBLE PRECISION
 *      or COMPLEX [kind-selector]
 *      or CHARACTER [kind-selector]
 *      or LOGICAL [kind-selector]
 *      or TYPE ( type-name )
 *
 *  Note that INTEGER and REAL may be followed by "*N" where "N" is an integer
 */
static void parseTypeSpec (tokenInfo *const token)
{
	/* parse type-spec, leaving `token' at first token following type-spec */
	Assert (isTypeSpec (token));
	switch (token->keyword)
	{
		case KEYWORD_character:
			/* skip char-selector */
			readToken (token);
			if (isType (token, TOKEN_OPERATOR) &&
					 strcmp (vStringValue (token->string), "*") == 0)
				readToken (token);
			if (isType (token, TOKEN_PAREN_OPEN))
				skipOverParens (token);
			else if (isType (token, TOKEN_NUMERIC))
				readToken (token);
			break;


		case KEYWORD_byte:
		case KEYWORD_complex:
		case KEYWORD_integer:
		case KEYWORD_logical:
		case KEYWORD_real:
		case KEYWORD_procedure:
		case KEYWORD_class:
			readToken (token);
			parseKindSelector (token);
			break;

		case KEYWORD_double:
			readToken (token);
			if (isKeyword (token, KEYWORD_complex) ||
				isKeyword (token, KEYWORD_precision))
					readToken (token);
			else
				skipToToken (token, TOKEN_STATEMENT_END);
			break;

		case KEYWORD_record:
			readToken (token);
			if (isType (token, TOKEN_OPERATOR) &&
				strcmp (vStringValue (token->string), "/") == 0)
			{
				readToken (token);  /* skip to structure name */
				readToken (token);  /* skip to '/' */
				readToken (token);  /* skip to variable name */
			}
			break;

		case KEYWORD_type:
			readToken (token);
			if (isType (token, TOKEN_PAREN_OPEN))
				skipOverParens (token);  /* skip type-name */
			else
				parseDerivedTypeDef (token);
			break;

		case KEYWORD_final:
		case KEYWORD_generic:
		case KEYWORD_enumerator:
			readToken (token);
			break;

		default:
			skipToToken (token, TOKEN_STATEMENT_END);
			break;
	}
}

static bool skipStatementIfKeyword (tokenInfo *const token, keywordId keyword)
{
	bool result = false;
	if (isKeyword (token, keyword))
	{
		result = true;
		skipToNextStatement (token);
	}
	return result;
}

/* parse extends qualifier, leaving token at first token following close
 * parenthesis.
 */

static void attachParentType (tokenInfo *const token, vString* parentType)
{
	if (token->parentType)
		vStringDelete (token->parentType);
	token->parentType = parentType;
}

static void makeParentType (tokenInfo *const token, void *userData)
{
	attachParentType ((tokenInfo *const)userData,
					  vStringNewCopy (token->string));
}

static void parseExtendsQualifier (tokenInfo *const token,
								   tokenInfo *const qualifierToken)
{
	skipOverParensFull (token, makeParentType, qualifierToken);
}

static void parseAbstractQualifier (tokenInfo *const token,
									tokenInfo *const qualifierToken)
{
	Assert (isKeyword (token, KEYWORD_abstract));
	qualifierToken->implementation = IMP_ABSTRACT;
	readToken (token);
}

static void parseDeferredQualifier (tokenInfo *const token,
									tokenInfo *const qualifierToken)
{
	Assert (isKeyword (token, KEYWORD_deferred));
	qualifierToken->implementation = IMP_DEFERRED;
	readToken (token);
}

static void parseNonOverridableQualifier (tokenInfo *const token,
										  tokenInfo *const qualifierToken)
{
	Assert (isKeyword (token, KEYWORD_non_overridable));
	qualifierToken->implementation = IMP_NON_OVERRIDABLE;
	readToken (token);
}

/* parse a list of qualifying specifiers, leaving `token' at first token
 * following list. Examples of such specifiers are:
 *      [[, attr-spec] ::]
 *      [[, component-attr-spec-list] ::]
 *
 *  attr-spec
 *      is PARAMETER
 *      or access-spec (is PUBLIC or PRIVATE)
 *      or ALLOCATABLE
 *      or DIMENSION ( array-spec )
 *      or EXTENDS ( extends-spec )
 *      or EXTERNAL
 *      or INTENT ( intent-spec )
 *      or INTRINSIC
 *      or OPTIONAL
 *      or POINTER
 *      or SAVE
 *      or TARGET
 *      or PASS
 *      or NOPASS
 *      or DEFERRED
 *      or NON_OVERRIDABLE
 *      or ABSTRACT
 *
 *  component-attr-spec
 *      is POINTER
 *      or DIMENSION ( component-array-spec )
 */
static tokenInfo *parseQualifierSpecList (tokenInfo *const token)
{
	tokenInfo *qualifierToken = newToken ();

	do
	{
		readToken (token);  /* should be an attr-spec */
		switch (token->keyword)
		{
			case KEYWORD_parameter:
			case KEYWORD_allocatable:
			case KEYWORD_external:
			case KEYWORD_intrinsic:
			case KEYWORD_optional:
			case KEYWORD_private:
			case KEYWORD_pointer:
			case KEYWORD_protected:
			case KEYWORD_public:
			case KEYWORD_save:
			case KEYWORD_target:
			case KEYWORD_nopass:
				readToken (token);
				break;

			case KEYWORD_dimension:
			case KEYWORD_intent:
			case KEYWORD_bind:
				readToken (token);
				skipOverParens (token);
				break;

			case KEYWORD_extends:
				readToken (token);
				parseExtendsQualifier (token, qualifierToken);
				break;

			case KEYWORD_pass:
				readToken (token);
				if (isType (token, TOKEN_PAREN_OPEN))
					skipOverParens (token);
				break;

			case KEYWORD_abstract:
				parseAbstractQualifier (token, qualifierToken);
				break;

			case KEYWORD_deferred:
				parseDeferredQualifier (token, qualifierToken);
				break;

			case KEYWORD_non_overridable:
				parseNonOverridableQualifier (token, qualifierToken);
				break;

			case KEYWORD_codimension:
				readToken (token);
				skipOverSquares (token);
				break;

			default: skipToToken (token, TOKEN_STATEMENT_END); break;
		}
	} while (isType (token, TOKEN_COMMA));
	if (! isType (token, TOKEN_DOUBLE_COLON))
		skipToToken (token, TOKEN_STATEMENT_END);

	return qualifierToken;
}

static tagType variableTagType (tokenInfo *const st)
{
	tagType result = TAG_VARIABLE;
	if (ancestorCount () > 0)
	{
		const tokenInfo* const parent = ancestorTop ();
		switch (parent->tag)
		{
			case TAG_SUBMODULE:	/* Fall through */
			case TAG_MODULE:       result = TAG_VARIABLE;  break;
			case TAG_DERIVED_TYPE:
				if (st && st->isMethod)
					result = TAG_METHOD;
				else
					result = TAG_COMPONENT;
				break;
			case TAG_FUNCTION:     result = TAG_LOCAL;     break;
			case TAG_SUBROUTINE:   result = TAG_LOCAL;     break;
			case TAG_PROTOTYPE:    result = TAG_LOCAL;     break;
			case TAG_ENUM:         result = TAG_ENUMERATOR; break;
			default:               result = TAG_VARIABLE;  break;
		}
	}
	return result;
}

static void parseEntityDecl (tokenInfo *const token,
							 tokenInfo *const st)
{
	Assert (isType (token, TOKEN_IDENTIFIER));
	if (st && st->implementation != IMP_DEFAULT)
		token->implementation = st->implementation;
	makeFortranTag (token, variableTagType (st));
	readToken (token);
	/* we check for both '()' and '[]'
	 * coarray syntax permits variable(), variable[], or variable()[]
	 */
	if (isType (token, TOKEN_PAREN_OPEN))
		skipOverParens (token);
	if (isType (token, TOKEN_SQUARE_OPEN))
		skipOverSquares (token);
	if (isType (token, TOKEN_OPERATOR) &&
			strcmp (vStringValue (token->string), "*") == 0)
	{
		readToken (token);  /* read char-length */
		if (isType (token, TOKEN_PAREN_OPEN))
			skipOverParens (token);
		else
			readToken (token);
	}
	if (isType (token, TOKEN_OPERATOR))
	{
		if (strcmp (vStringValue (token->string), "/") == 0)
		{  /* skip over initializations of structure field */
			readToken (token);
			skipPast (token, TOKEN_OPERATOR);
		}
		else if (strcmp (vStringValue (token->string), "=") == 0 ||
				 strcmp (vStringValue (token->string), "=>") == 0)
		{
			while (! isType (token, TOKEN_COMMA) &&
					! isType (token, TOKEN_STATEMENT_END) &&
					! isType (token, TOKEN_EOF))
			{
				readToken (token);
				/* another coarray check, for () and [] */
				if (isType (token, TOKEN_PAREN_OPEN))
					skipOverParens (token);
				if (isType (token, TOKEN_SQUARE_OPEN))
					skipOverSquares (token);
			}
		}
	}
	/* token left at either comma or statement end */
}

static void parseEntityDeclList (tokenInfo *const token,
								 tokenInfo *const st)
{
	if (isType (token, TOKEN_PERCENT))
		skipToNextStatement (token);
	else while (isType (token, TOKEN_IDENTIFIER) ||
				(isType (token, TOKEN_KEYWORD) &&
				 !isKeyword (token, KEYWORD_function) &&
				 !isKeyword (token, KEYWORD_subroutine)))
	{
		/* compilers accept keywords as identifiers */
		if (isType (token, TOKEN_KEYWORD))
			token->type = TOKEN_IDENTIFIER;
		parseEntityDecl (token, st);
		if (isType (token, TOKEN_COMMA))
			readToken (token);
		else if (isType (token, TOKEN_STATEMENT_END))
		{
			skipToNextStatement (token);
			break;
		}
	}
}

/*  type-declaration-stmt is
 *      type-spec [[, attr-spec] ... ::] entity-decl-list
 */
static void parseTypeDeclarationStmt (tokenInfo *const token)
{
	Assert (isTypeSpec (token));
	parseTypeSpec (token);
	if (!isType (token, TOKEN_STATEMENT_END))  /* if not end of derived type... */
	{
		if (isType (token, TOKEN_COMMA))
		{
			tokenInfo* qualifierToken = parseQualifierSpecList (token);
			deleteToken (qualifierToken);
		}
		if (isType (token, TOKEN_DOUBLE_COLON))
			readToken (token);
		parseEntityDeclList (token, NULL);
	}
	if (isType (token, TOKEN_STATEMENT_END))
		skipToNextStatement (token);
}

/*  namelist-stmt is
 *      NAMELIST /namelist-group-name/ namelist-group-object-list
 *			[[,]/[namelist-group-name]/ namelist-block-object-list] ...
 *
 *  namelist-group-object is
 *      variable-name
 *
 *  common-stmt is
 *      COMMON [/[common-block-name]/] common-block-object-list
 *			[[,]/[common-block-name]/ common-block-object-list] ...
 *
 *  common-block-object is
 *      variable-name [ ( explicit-shape-spec-list ) ]
 */
static void parseCommonNamelistStmt (tokenInfo *const token, tagType type)
{
	Assert (isKeyword (token, KEYWORD_common) ||
			isKeyword (token, KEYWORD_namelist));
	readToken (token);
	do
	{
		if (isType (token, TOKEN_OPERATOR) &&
			strcmp (vStringValue (token->string), "/") == 0)
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				makeFortranTag (token, type);
				readToken (token);
			}
			skipPast (token, TOKEN_OPERATOR);
		}
		if (isType (token, TOKEN_IDENTIFIER))
			makeFortranTag (token, TAG_LOCAL);
		readToken (token);
		if (isType (token, TOKEN_PAREN_OPEN))
			skipOverParens (token);  /* skip explicit-shape-spec-list */
		if (isType (token, TOKEN_COMMA))
			readToken (token);
	} while (! isType (token, TOKEN_STATEMENT_END) &&
			 ! isType (token, TOKEN_EOF));
	skipToNextStatement (token);
}

static void parseFieldDefinition (tokenInfo *const token)
{
	if (isTypeSpec (token))
		parseTypeDeclarationStmt (token);
	else if (isKeyword (token, KEYWORD_structure))
		parseStructureStmt (token);
	else if (isKeyword (token, KEYWORD_union))
		parseUnionStmt (token);
	else
		skipToNextStatement (token);
}

static void parseMap (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_map));
	skipToNextStatement (token);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
		parseFieldDefinition (token);
	readSubToken (token);
	/* should be at KEYWORD_map token */
	skipToNextStatement (token);
}

/* UNION
 *      MAP
 *          [field-definition] [field-definition] ...
 *      END MAP
 *      MAP
 *          [field-definition] [field-definition] ...
 *      END MAP
 *      [MAP
 *          [field-definition]
 *          [field-definition] ...
 *      END MAP] ...
 *  END UNION
 *      *
 *
 *  Typed data declarations (variables or arrays) in structure declarations
 *  have the form of normal Fortran typed data declarations. Data items with
 *  different types can be freely intermixed within a structure declaration.
 *
 *  Unnamed fields can be declared in a structure by specifying the pseudo
 *  name %FILL in place of an actual field name. You can use this mechanism to
 *  generate empty space in a record for purposes such as alignment.
 *
 *  All mapped field declarations that are made within a UNION declaration
 *  share a common location within the containing structure. When initializing
 *  the fields within a UNION, the final initialization value assigned
 *  overlays any value previously assigned to a field definition that shares
 *  that field.
 */
static void parseUnionStmt (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_union));
	skipToNextStatement (token);
	while (isKeyword (token, KEYWORD_map))
		parseMap (token);
	/* should be at KEYWORD_end token */
	readSubToken (token);
	/* secondary token should be KEYWORD_end token */
	skipToNextStatement (token);
}

/*  STRUCTURE [/structure-name/] [field-names]
 *      [field-definition]
 *      [field-definition] ...
 *  END STRUCTURE
 *
 *  structure-name
 *		identifies the structure in a subsequent RECORD statement.
 *		Substructures can be established within a structure by means of either
 *		a nested STRUCTURE declaration or a RECORD statement.
 *
 *   field-names
 *		(for substructure declarations only) one or more names having the
 *		structure of the substructure being defined.
 *
 *   field-definition
 *		can be one or more of the following:
 *
 *			Typed data declarations, which can optionally include one or more
 *			data initialization values.
 *
 *			Substructure declarations (defined by either RECORD statements or
 *			subsequent STRUCTURE statements).
 *
 *			UNION declarations, which are mapped fields defined by a block of
 *			statements. The syntax of a UNION declaration is described below.
 *
 *			PARAMETER statements, which do not affect the form of the
 *			structure.
 */
static void parseStructureStmt (tokenInfo *const token)
{
	tokenInfo *name = NULL;
	Assert (isKeyword (token, KEYWORD_structure));
	readToken (token);
	if (isType (token, TOKEN_OPERATOR) &&
		strcmp (vStringValue (token->string), "/") == 0)
	{  /* read structure name */
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
		{
			name = newTokenFrom (token);
			name->type = TOKEN_IDENTIFIER;
		}
		skipPast (token, TOKEN_OPERATOR);
	}
	if (name == NULL)
	{  /* fake out anonymous structure */
		name = newAnonTokenFrom (token, TAG_COMPONENT);
		name->type = TOKEN_IDENTIFIER;
		name->tag = TAG_DERIVED_TYPE;
	}
	makeFortranTag (name, TAG_DERIVED_TYPE);
	while (isType (token, TOKEN_IDENTIFIER))
	{  /* read field names */
		makeFortranTag (token, TAG_COMPONENT);
		readToken (token);
		if (isType (token, TOKEN_COMMA))
			readToken (token);
	}
	skipToNextStatement (token);
	ancestorPush (name);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
		parseFieldDefinition (token);
	readSubToken (token);
	/* secondary token should be KEYWORD_structure token */
	skipToNextStatement (token);
	ancestorPop ();
	deleteToken (name);
}

/*  specification-stmt
 *      is access-stmt      (is access-spec [[::] access-id-list)
 *      or allocatable-stmt (is ALLOCATABLE [::] array-name etc.)
 *      or common-stmt      (is COMMON [ / [common-block-name] /] etc.)
 *      or data-stmt        (is DATA data-stmt-list [[,] data-stmt-set] ...)
 *      or dimension-stmt   (is DIMENSION [::] array-name etc.)
 *      or equivalence-stmt (is EQUIVALENCE equivalence-set-list)
 *      or external-stmt    (is EXTERNAL etc.)
 *      or intent-stmt      (is INTENT ( intent-spec ) [::] etc.)
 *      or intrinsic-stmt   (is INTRINSIC etc.)
 *      or namelist-stmt    (is NAMELIST / namelist-group-name / etc.)
 *      or optional-stmt    (is OPTIONAL [::] etc.)
 *      or pointer-stmt     (is POINTER [::] object-name etc.)
 *      or save-stmt        (is SAVE etc.)
 *      or target-stmt      (is TARGET [::] object-name etc.)
 *
 *  access-spec is PUBLIC or PRIVATE
 */
static bool parseSpecificationStmt (tokenInfo *const token)
{
	bool result = true;
	switch (token->keyword)
	{
		case KEYWORD_common:
			parseCommonNamelistStmt (token, TAG_COMMON_BLOCK);
			break;

		case KEYWORD_namelist:
			parseCommonNamelistStmt (token, TAG_NAMELIST);
			break;

		case KEYWORD_structure:
			parseStructureStmt (token);
			break;

		case KEYWORD_allocatable:
		case KEYWORD_data:
		case KEYWORD_dimension:
		case KEYWORD_equivalence:
		case KEYWORD_external:
		case KEYWORD_intent:
		case KEYWORD_intrinsic:
		case KEYWORD_optional:
		case KEYWORD_pointer:
		case KEYWORD_private:
		case KEYWORD_protected:
		case KEYWORD_public:
		case KEYWORD_save:
		case KEYWORD_target:
			skipToNextStatement (token);
			break;

		default:
			result = false;
			break;
	}
	return result;
}

/* Type bound generic procedure is:
 *   GENERIC [, access-spec ] :: generic-spec => binding-name1 [, binding-name2]...
 *     access-spec: PUBLIC or PRIVATE
 *     generic-spec: 1. generic name; 2. OPERATOR(op); 3. ASSIGNMENT(=)
 *     binding-name: type bound procedure
 */
static void parseGenericMethod (tokenInfo *const token)
{
	if (isKeyword (token, KEYWORD_assignment) ||
		isKeyword (token, KEYWORD_operator))
	{
		readToken (token);
		if (isType (token, TOKEN_PAREN_OPEN))
			readToken (token);
		if (isType (token, TOKEN_OPERATOR))
			makeFortranTag (token, TAG_METHOD);
	}
	else
	{
		if (isType (token, TOKEN_KEYWORD))
			token->type = TOKEN_IDENTIFIER;
		makeFortranTag (token, TAG_METHOD);
	}
	skipToNextStatement (token);
}

/*  component-def-stmt is
 *      type-spec [[, component-attr-spec-list] ::] component-decl-list
 *
 *  component-decl is
 *      component-name [ ( component-array-spec ) ] [ * char-length ]
 */
static void parseComponentDefStmt (tokenInfo *const token)
{
	tokenInfo* st = newToken ();
	tokenInfo* qt = NULL;
	bool isGeneric = false;

	Assert (isTypeSpec (token));
	if (isKeyword (token, KEYWORD_procedure) ||
		isKeyword (token, KEYWORD_final) ||
		isKeyword (token, KEYWORD_generic))
		st->isMethod = true;
	if (isKeyword (token, KEYWORD_generic))
		isGeneric = true;
	parseTypeSpec (token);
	if (isType (token, TOKEN_COMMA))
	{
		qt = parseQualifierSpecList (token);
		if (qt->implementation != IMP_DEFAULT)
			st->implementation = qt->implementation;
		deleteToken (qt);
	}
	if (isType (token, TOKEN_DOUBLE_COLON))
		readToken (token);
	if (isGeneric)
		parseGenericMethod (token);
	else
		parseEntityDeclList (token, st);
	deleteToken (st);
}

/*  derived-type-def is
 *      derived-type-stmt is (TYPE [[, access-spec] ::] type-name
 *          [private-sequence-stmt] ... (is PRIVATE or SEQUENCE)
 *          component-def-stmt
 *          [component-def-stmt] ...
 *          end-type-stmt
 */
static void parseDerivedTypeDef (tokenInfo *const token)
{
	tokenInfo *qualifierToken = NULL;

	if (isType (token, TOKEN_COMMA))
		qualifierToken = parseQualifierSpecList (token);
	if (isType (token, TOKEN_DOUBLE_COLON))
		readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
	{
		token->type = TOKEN_IDENTIFIER;
		if (qualifierToken)
		{
			if (qualifierToken->parentType)
				token->parentType = vStringNewCopy (qualifierToken->parentType);
			if (qualifierToken->implementation != IMP_DEFAULT)
				token->implementation = qualifierToken->implementation;
		}
		makeFortranTag (token, TAG_DERIVED_TYPE);
	}
	deleteToken (qualifierToken);
	ancestorPush (token);
	skipToNextStatement (token);
	if (isKeyword (token, KEYWORD_private) ||
		isKeyword (token, KEYWORD_sequence))
	{
		skipToNextStatement (token);
	}
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
	{
		if (isTypeSpec (token))
			parseComponentDefStmt (token);
		else
			skipToNextStatement (token);
	}
	readSubToken (token);
	/* secondary token should be KEYWORD_type token */
	skipToToken (token, TOKEN_STATEMENT_END);
	ancestorPop ();
}

/*  interface-block
 *      interface-stmt (is INTERFACE [generic-spec])
 *          [interface-body]
 *          [module-procedure-stmt] ...
 *          end-interface-stmt (is END INTERFACE)
 *
 *  generic-spec
 *      is generic-name
 *      or OPERATOR ( defined-operator )
 *      or ASSIGNMENT ( = )
 *
 *  interface-body
 *      is function-stmt
 *          [specification-part]
 *          end-function-stmt
 *      or subroutine-stmt
 *          [specification-part]
 *          end-subroutine-stmt
 *
 *  module-procedure-stmt is
 *      MODULE PROCEDURE procedure-name-list
 */
static void parseInterfaceBlock (tokenInfo *const token)
{
	tokenInfo *name = NULL;
	Assert (isKeyword (token, KEYWORD_interface));
	readToken (token);
	if (isKeyword (token, KEYWORD_assignment) ||
			 isKeyword (token, KEYWORD_operator))
	{
		readToken (token);
		if (isType (token, TOKEN_PAREN_OPEN))
			readToken (token);
		if (isType (token, TOKEN_OPERATOR))
			name = newTokenFrom (token);
	}
	else if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
	{
		name = newTokenFrom (token);
		name->type = TOKEN_IDENTIFIER;
	}
	if (name == NULL)
	{
		name = newAnonTokenFrom (token, TAG_INTERFACE);
		name->type = TOKEN_IDENTIFIER;
		name->tag = TAG_INTERFACE;
	}
	makeFortranTag (name, TAG_INTERFACE);
	ancestorPush (name);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
	{
		switch (token->keyword)
		{
			case KEYWORD_function:
			case KEYWORD_subroutine: parseSubprogram (token); break;

			default:
				if (isSubprogramPrefix (token))
					readToken (token);
				else if (isTypeSpec (token))
					parseTypeSpec (token);
				else
					skipToNextStatement (token);
				break;
		}
	}
	readSubToken (token);
	/* secondary token should be KEYWORD_interface token */
	skipToNextStatement (token);
	ancestorPop ();
	deleteToken (name);
}

/* enum-block
 *      enum-stmt (is ENUM, BIND(C) [ :: type-alias-name ]
 *                 or ENUM [ kind-selector ] [ :: ] [ type-alias-name ])
 *          [ enum-body (is ENUMERATOR [ :: ] enumerator-list) ]
 *      end-enum-stmt (is END ENUM)
 */
static void parseEnumBlock (tokenInfo *const token)
{
	tokenInfo *name = NULL;
	Assert (isKeyword (token, KEYWORD_enum));
	readToken (token);
	if (isType (token, TOKEN_COMMA))
	{
		readToken (token);
		if (isType (token, TOKEN_KEYWORD))
			readToken (token);
		if (isType (token, TOKEN_PAREN_OPEN))
			skipOverParens (token);
	}
	parseKindSelector (token);
	if (isType (token, TOKEN_DOUBLE_COLON))
		readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
	{
		name = newTokenFrom (token);
		name->type = TOKEN_IDENTIFIER;
	}
	if (name == NULL)
	{
		name = newAnonTokenFrom (token, TAG_ENUM);
		name->type = TOKEN_IDENTIFIER;
		name->tag = TAG_ENUM;
	}
	makeFortranTag (name, TAG_ENUM);
	skipToNextStatement (token);
	ancestorPush (name);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType(token, TOKEN_EOF))
	{
		if (isTypeSpec (token))
			parseTypeDeclarationStmt (token);
		else
			skipToNextStatement (token);
	}
	readSubToken (token);
	/* secondary token should be KEYWORD_enum token */
	skipToNextStatement (token);
	ancestorPop ();
	deleteToken (name);
}

/*  entry-stmt is
 *      ENTRY entry-name [ ( dummy-arg-list ) ]
 */
static void parseEntryStmt (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_entry));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
		makeFortranTag (token, TAG_ENTRY_POINT);
	skipToNextStatement (token);
}

/*  stmt-function-stmt is
 *      function-name ([dummy-arg-name-list]) = scalar-expr
 */
static bool parseStmtFunctionStmt (tokenInfo *const token)
{
	bool result = false;
	Assert (isType (token, TOKEN_IDENTIFIER));
#if 0  /* cannot reliably parse this yet */
	makeFortranTag (token, TAG_FUNCTION);
#endif
	readToken (token);
	if (isType (token, TOKEN_PAREN_OPEN))
	{
		skipOverParens (token);
		result = (bool) (isType (token, TOKEN_OPERATOR) &&
			strcmp (vStringValue (token->string), "=") == 0);
	}
	skipToNextStatement (token);
	return result;
}

static bool isIgnoredDeclaration (tokenInfo *const token)
{
	bool result;
	switch (token->keyword)
	{
		case KEYWORD_cexternal:
		case KEYWORD_cglobal:
		case KEYWORD_dllexport:
		case KEYWORD_dllimport:
		case KEYWORD_external:
		case KEYWORD_format:
		case KEYWORD_include:
		case KEYWORD_inline:
		case KEYWORD_parameter:
		case KEYWORD_pascal:
		case KEYWORD_pexternal:
		case KEYWORD_pglobal:
		case KEYWORD_static:
		case KEYWORD_value:
		case KEYWORD_virtual:
		case KEYWORD_volatile:
			result = true;
			break;

		default:
			result = false;
			break;
	}
	return result;
}

/*  declaration-construct
 *      [derived-type-def]
 *      [interface-block]
 *      [type-declaration-stmt]
 *      [specification-stmt]
 *      [parameter-stmt] (is PARAMETER ( named-constant-def-list )
 *      [format-stmt]    (is FORMAT format-specification)
 *      [entry-stmt]
 *      [stmt-function-stmt]
 */
static bool parseDeclarationConstruct (tokenInfo *const token)
{
	bool result = true;
	switch (token->keyword)
	{
		case KEYWORD_entry:		parseEntryStmt (token);      break;
		case KEYWORD_interface:	parseInterfaceBlock (token); break;
		case KEYWORD_enum:      parseEnumBlock (token);      break;
		case KEYWORD_stdcall:   readToken (token);           break;
		/* derived type handled by parseTypeDeclarationStmt(); */

		case KEYWORD_abstract:
			readToken (token);
			if (isKeyword (token, KEYWORD_interface))
				parseInterfaceBlock (token);
			else
				skipToNextStatement (token);
			result = true;
			break;

		case KEYWORD_automatic:
			readToken (token);
			if (isTypeSpec (token))
				parseTypeDeclarationStmt (token);
			else
				skipToNextStatement (token);
			result = true;
			break;

		default:
			if (isIgnoredDeclaration (token))
				skipToNextStatement (token);
			else if (isTypeSpec (token))
			{
				parseTypeDeclarationStmt (token);
				result = true;
			}
			else if (isType (token, TOKEN_IDENTIFIER))
				result = parseStmtFunctionStmt (token);
			else
				result = parseSpecificationStmt (token);
			break;
	}
	return result;
}

/*  implicit-part-stmt
 *      is [implicit-stmt] (is IMPLICIT etc.)
 *      or [parameter-stmt] (is PARAMETER etc.)
 *      or [format-stmt] (is FORMAT etc.)
 *      or [entry-stmt] (is ENTRY entry-name etc.)
 */
static bool parseImplicitPartStmt (tokenInfo *const token)
{
	bool result = true;
	switch (token->keyword)
	{
		case KEYWORD_entry: parseEntryStmt (token); break;

		case KEYWORD_implicit:
		case KEYWORD_include:
		case KEYWORD_parameter:
		case KEYWORD_format:
			skipToNextStatement (token);
			break;

		default: result = false; break;
	}
	return result;
}

/*  specification-part is
 *      [use-stmt] ... (is USE module-name etc.)
 *      [implicit-part] (is [implicit-part-stmt] ... [implicit-stmt])
 *      [declaration-construct] ...
 */
static bool parseSpecificationPart (tokenInfo *const token)
{
	bool result = false;
	while (skipStatementIfKeyword (token, KEYWORD_use))
		result = true;
	while (skipStatementIfKeyword (token, KEYWORD_import))
		result = true;
	while (parseImplicitPartStmt (token))
		result = true;
	while (parseDeclarationConstruct (token))
		result = true;
	return result;
}

/*  block-data is
 *      block-data-stmt (is BLOCK DATA [block-data-name]
 *          [specification-part]
 *          end-block-data-stmt (is END [BLOCK DATA [block-data-name]])
 */
static void parseBlockData (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_block));
	readToken (token);
	if (isKeyword (token, KEYWORD_data))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			makeFortranTag (token, TAG_BLOCK_DATA);
	}
	ancestorPush (token);
	skipToNextStatement (token);
	parseSpecificationPart (token);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
		skipToNextStatement (token);
	readSubToken (token);
	/* secondary token should be KEYWORD_NONE or KEYWORD_block token */
	skipToNextStatement (token);
	ancestorPop ();
}

/*  internal-subprogram-part is
 *      contains-stmt (is CONTAINS)
 *          internal-subprogram
 *          [internal-subprogram] ...
 *
 *  internal-subprogram
 *      is function-subprogram
 *      or subroutine-subprogram
 */
static void parseInternalSubprogramPart (tokenInfo *const token)
{
	bool done = false;
	if (isKeyword (token, KEYWORD_contains))
		skipToNextStatement (token);
	do
	{
		switch (token->keyword)
		{
			case KEYWORD_function:
			case KEYWORD_subroutine: parseSubprogram (token); break;
			case KEYWORD_end:        done = true;             break;

			default:
				if (isSubprogramPrefix (token))
					readToken (token);
				else if (isTypeSpec (token))
					parseTypeSpec (token);
				else
					readToken (token);
				break;
		}
	} while (! done && ! isType (token, TOKEN_EOF));
}

/* submodule is
 *     submodule-stmt (is SUBMODULE ( parent-identifier ) submodule-name)
 *          [specification-part]
 *          [module-subprogram-part]
 *          end-submodule-stmt (is END [SUBMODULE [submodule-name]])
 *
 * parent-identifier is
 *     ancestor_module_name [ : parent_submodule_name ]*
 *
 * ------------------------------------------------------------------
 * XL Fortran for AIX, V15.1.3
 * Language Reference
 * Program units and procedures
 *   https://www.ibm.com/support/knowledgecenter/en/SSGH4D_15.1.3/com.ibm.xlf1513.aix.doc/language_ref/submodules.html
 * -------------------------------------------------------------------
 */
static vString *parserParentIdentifierOfSubmoduleStatement (tokenInfo *const token)
{
	vString *parentId;

	if (!isType (token, TOKEN_PAREN_OPEN))
		return NULL;

	parentId = vStringNew();

	while (1)
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER))
			vStringCat (parentId, token->string);
		else if (isType (token, TOKEN_COLON))
			vStringPut (parentId, ':');
		else if (isType (token, TOKEN_PAREN_CLOSE))
			break;
		else
		{
			/* Unexpected token (including EOF) */
			vStringClear (parentId);
			break;
		}
	}

	if (vStringLength (parentId) == 0)
	{
		vStringDelete (parentId);
		parentId = NULL;
	}
	return parentId;
}

/*  module is
 *      module-stmt (is MODULE module-name)
 *          [specification-part]
 *          [module-subprogram-part]
 *          end-module-stmt (is END [MODULE [module-name]])
 *
 *  module-subprogram-part
 *      contains-stmt (is CONTAINS)
 *          module-subprogram
 *          [module-subprogram] ...
 *
 *  module-subprogram
 *      is function-subprogram
 *      or subroutine-subprogram
 */
static void parseModule (tokenInfo *const token, bool isSubmodule)
{
	vString *parentIdentifier = NULL;

	Assert (((!isSubmodule) && isKeyword (token, KEYWORD_module))
			|| (isSubmodule && isKeyword (token, KEYWORD_submodule)));


	if (isSubmodule)
	{
		readToken (token);
		parentIdentifier = parserParentIdentifierOfSubmoduleStatement (token);
		if (parentIdentifier == NULL)
		{
			/* Unexpected syntax */
			skipToNextStatement (token);
			return;
		}
	}

	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
	{
		token->type = TOKEN_IDENTIFIER;
		if (isSubmodule)
		{
			attachParentType (token, parentIdentifier);
			parentIdentifier = NULL;
		}
		makeFortranTag (token, isSubmodule? TAG_SUBMODULE: TAG_MODULE);
	}
	ancestorPush (token);
	skipToNextStatement (token);
	parseSpecificationPart (token);
	if (isKeyword (token, KEYWORD_contains))
		parseInternalSubprogramPart (token);
	while (! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
		skipToNextStatement (token);
	readSubToken (token);
	/* secondary token should be KEYWORD_NONE or KEYWORD_module token */
	skipToNextStatement (token);
	ancestorPop ();

	if (parentIdentifier)
		vStringDelete (parentIdentifier);
}

/*  execution-part
 *      executable-construct
 *
 *  executable-construct is
 *      execution-part-construct [execution-part-construct]
 *
 *  execution-part-construct
 *      is executable-construct
 *      or format-stmt
 *      or data-stmt
 *      or entry-stmt
 */
static bool parseExecutionPart (tokenInfo *const token)
{
	bool result = false;
	bool done = false;
	while (! done && ! isType (token, TOKEN_EOF))
	{
		switch (token->keyword)
		{
			default:
				if (isSubprogramPrefix (token))
					readToken (token);
				else
					skipToNextStatement (token);
				result = true;
				break;

			case KEYWORD_entry:
				parseEntryStmt (token);
				result = true;
				break;

			case KEYWORD_contains:
			case KEYWORD_function:
			case KEYWORD_subroutine:
				done = true;
				break;

			case KEYWORD_end:
				readSubToken (token);
				if (isSecondaryKeyword (token, KEYWORD_do) ||
					isSecondaryKeyword (token, KEYWORD_enum) ||
					isSecondaryKeyword (token, KEYWORD_if) ||
					isSecondaryKeyword (token, KEYWORD_select) ||
					isSecondaryKeyword (token, KEYWORD_where) ||
					isSecondaryKeyword (token, KEYWORD_forall) ||
					isSecondaryKeyword (token, KEYWORD_associate) ||
					isSecondaryKeyword (token, KEYWORD_block))
				{
					skipToNextStatement (token);
					result = true;
				}
				else
					done = true;
				break;
		}
	}
	return result;
}

static void makeSignature (tokenInfo *const token, void* signature)
{
	if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
		vStringCat ((vString *)signature, token->string);
	else if (isType (token, TOKEN_COMMA))
		vStringCatS ((vString *)signature, ", ");
}

static vString* parseSignature (tokenInfo *const token)
{
	vString* signature = vStringNew ();

	readToken (token);
	if (isType (token, TOKEN_PAREN_OPEN))
	{
		vStringPut (signature, '(');
		skipOverParensFull (token, makeSignature, signature);
		vStringPut (signature, ')');
	}
	return signature;
}

static void parseSubprogramFull (tokenInfo *const token, const tagType tag)
{
	Assert (isKeyword (token, KEYWORD_program) ||
			isKeyword (token, KEYWORD_function) ||
			isKeyword (token, KEYWORD_subroutine));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
	{
		tokenInfo* name = newTokenFrom (token);
		token->type = TOKEN_IDENTIFIER;
		if (tag == TAG_SUBROUTINE ||
			tag == TAG_PROTOTYPE)
			name->signature = parseSignature (token);
		makeFortranTag (name, tag);
		ancestorPush (name);
		deleteToken (name);
	}
	else
		ancestorPush (token);
	skipToNextStatement (token);
	parseSpecificationPart (token);
	parseExecutionPart (token);
	if (isKeyword (token, KEYWORD_contains))
		parseInternalSubprogramPart (token);
	/* should be at KEYWORD_end token */
	readSubToken (token);
	/* secondary token should be one of KEYWORD_NONE, KEYWORD_program,
	 * KEYWORD_function, KEYWORD_function
	 */
	skipToNextStatement (token);
	ancestorPop ();
}

static tagType subprogramTagType (tokenInfo *const token)
{
	tagType result = TAG_UNDEFINED;

	if (insideInterface ())
		result = TAG_PROTOTYPE;
	else if (isKeyword (token, KEYWORD_subroutine))
		result = TAG_SUBROUTINE;
	else if (isKeyword (token, KEYWORD_function))
		result = TAG_FUNCTION;

	Assert (result != TAG_UNDEFINED);

	return result;
}

/*  function-subprogram is
 *      function-stmt (is [prefix] FUNCTION function-name etc.)
 *          [specification-part]
 *          [execution-part]
 *          [internal-subprogram-part]
 *          end-function-stmt (is END [FUNCTION [function-name]])
 *
 *  prefix
 *      is type-spec [RECURSIVE]
 *      or [RECURSIVE] type-spec
 */
/*  subroutine-subprogram is
 *      subroutine-stmt (is [RECURSIVE] SUBROUTINE subroutine-name etc.)
 *          [specification-part]
 *          [execution-part]
 *          [internal-subprogram-part]
 *          end-subroutine-stmt (is END [SUBROUTINE [function-name]])
 */
static void parseSubprogram (tokenInfo *const token)
{
	parseSubprogramFull (token, subprogramTagType (token));
}

/*  main-program is
 *      [program-stmt] (is PROGRAM program-name)
 *          [specification-part]
 *          [execution-part]
 *          [internal-subprogram-part ]
 *          end-program-stmt
 */
static void parseMainProgram (tokenInfo *const token)
{
	parseSubprogramFull (token, TAG_PROGRAM);
}

/*  program-unit
 *      is main-program
 *      or external-subprogram (is function-subprogram or subroutine-subprogram)
 *      or module
 *      or block-data
 */
static void parseProgramUnit (tokenInfo *const token)
{
	readToken (token);
	do
	{
		if (isType (token, TOKEN_STATEMENT_END))
			readToken (token);
		else switch (token->keyword)
		{
			case KEYWORD_block:      parseBlockData (token);            break;
			case KEYWORD_end:        skipToNextStatement (token);       break;
			case KEYWORD_function:
			case KEYWORD_subroutine: parseSubprogram (token);           break;
			case KEYWORD_submodule:  parseModule (token, true);         break;
			case KEYWORD_module:     parseModule (token, false);        break;
			case KEYWORD_program:    parseMainProgram (token);          break;

			default:
				if (isSubprogramPrefix (token))
					readToken (token);
				else
				{
					bool one = parseSpecificationPart (token);
					bool two = parseExecutionPart (token);
					if (! (one || two))
						readToken (token);
				}
				break;
		}
	} while (! isType (token, TOKEN_EOF));
}

static rescanReason findFortranTags (const unsigned int passCount)
{
	tokenInfo *token;
	rescanReason rescan;

	Assert (passCount < 3);
	token = newToken ();

	FreeSourceForm = (bool) (passCount > 1);
	Column = 0;
	parseProgramUnit (token);
	if (FreeSourceFormFound  &&  ! FreeSourceForm)
	{
		verbose ("%s: not fixed source form; retry as free source form\n",
				getInputFileName ());
		rescan = RESCAN_FAILED;
	}
	else
	{
		rescan = RESCAN_NONE;
	}
	ancestorClear ();
	deleteToken (token);

	return rescan;
}

static void initialize (const langType language)
{
	Lang_fortran = language;
}

extern parserDefinition* FortranParser (void)
{
	static const char *const extensions [] = {
		"f", "for", "ftn", "f77", "f90", "f95", "f03", "f08", "f15",
#ifndef CASE_INSENSITIVE_FILENAMES
		"F", "FOR", "FTN", "F77", "F90", "F95", "F03", "F08", "F15",
#endif
		NULL
	};
	parserDefinition* def = parserNew ("Fortran");
	def->kindTable      = FortranKinds;
	def->kindCount  = ARRAY_SIZE (FortranKinds);
	def->extensions = extensions;
	def->parser2    = findFortranTags;
	def->initialize = initialize;
	def->keywordTable = FortranKeywordTable;
	def->keywordCount = ARRAY_SIZE (FortranKeywordTable);
	return def;
}
