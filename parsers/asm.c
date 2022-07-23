/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for assembly language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "cpreprocessor.h"
#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "param.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "trace.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_PSUEDO_MACRO_END = -2,
	K_NONE = -1, K_DEFINE, K_LABEL, K_MACRO, K_TYPE,
	K_SECTION,
	K_PARAM,
} AsmKind;

typedef enum {
	OP_UNDEFINED = -1,
	OP_ALIGN,
	OP_COLON_EQUAL,
	OP_END,
	OP_ENDM,
	OP_ENDMACRO,
	OP_ENDP,
	OP_ENDS,
	OP_EQU,
	OP_EQUAL,
	OP_LABEL,
	OP_MACRO,
	OP_PROC,
	OP_RECORD,
	OP_SECTIONS,
	OP_SECTION,
	OP_SET,
	OP_STRUCT,
	OP_LAST
} opKeyword;

typedef enum {
	ASM_SECTION_PLACEMENT,
} asmSectionRole;

typedef struct {
	opKeyword keyword;
	AsmKind kind;
} opKind;

typedef enum {
	F_PROPERTIES,
} asmField;

static fieldDefinition AsmFields[] = {
	{ .name = "properties",
	  .description = "properties (req, vararg for parameters)",
	  .enabled = true },
};

/*
*   DATA DEFINITIONS
*/
static langType Lang_asm;

static roleDefinition asmSectionRoles [] = {
	{ true, "placement", "placement where the assembled code goes" },
};

static kindDefinition AsmKinds [] = {
	{ true, 'd', "define", "defines" },
	{ true, 'l', "label",  "labels"  },
	{ true, 'm', "macro",  "macros"  },
	{ true, 't', "type",   "types (structs and records)"   },
	{ true, 's', "section",   "sections",
	  .referenceOnly = true, ATTACH_ROLES(asmSectionRoles)},
	{ false,'z', "parameter", "parameters for a macro" },
};

static const keywordTable AsmKeywords [] = {
	{ "align",    OP_ALIGN       },
	{ "endmacro", OP_ENDMACRO    },
	{ "endm",     OP_ENDM        },
	{ "end",      OP_END         },
	{ "endp",     OP_ENDP        },
	{ "ends",     OP_ENDS        },
	{ "equ",      OP_EQU         },
	{ "label",    OP_LABEL       },
	{ "macro",    OP_MACRO       },
	{ ":=",       OP_COLON_EQUAL },
	{ "=",        OP_EQUAL       },
	{ "proc",     OP_PROC        },
	{ "record",   OP_RECORD      },
	{ "sections", OP_SECTIONS    },

	/* These are used in GNU as. */
	{ "section",  OP_SECTION     },
	{ "equiv",    OP_EQU         },
	{ "eqv",      OP_EQU         },

	{ "set",      OP_SET         },
	{ "struct",   OP_STRUCT      }
};

static const opKind OpKinds [] = {
	/* must be ordered same as opKeyword enumeration */
	{ OP_ALIGN,       K_NONE   },
	{ OP_COLON_EQUAL, K_DEFINE },
	{ OP_END,         K_NONE   },
	{ OP_ENDM,        K_PSUEDO_MACRO_END },
	{ OP_ENDMACRO,    K_NONE   },
	{ OP_ENDP,        K_NONE   },
	{ OP_ENDS,        K_NONE   },
	{ OP_EQU,         K_DEFINE },
	{ OP_EQUAL,       K_DEFINE },
	{ OP_LABEL,       K_LABEL  },
	{ OP_MACRO,       K_MACRO  },
	{ OP_PROC,        K_LABEL  },
	{ OP_RECORD,      K_TYPE   },
	{ OP_SECTIONS,    K_NONE   },
	{ OP_SECTION,     K_SECTION },
	{ OP_SET,         K_DEFINE },
	{ OP_STRUCT,      K_TYPE   }
};

#define DEFAULT_COMMENT_CHARS_BOL ";*@"
static const char defaultCommentCharAtBOL [] = DEFAULT_COMMENT_CHARS_BOL;
static const char *commentCharsAtBOL = defaultCommentCharAtBOL;

#define DEFAULT_COMMENT_CHARS_MOL ""
static const char defaultCommentCharInMOL [] = DEFAULT_COMMENT_CHARS_MOL;
static const char *commentCharsInMOL = defaultCommentCharInMOL;

#define DEFAULT_EXTRA_LINESEP_CHARS ""
static const char defaultExtraLinesepChars [] = DEFAULT_EXTRA_LINESEP_CHARS;
static const char *extraLinesepChars = defaultExtraLinesepChars;

static bool useCPreProcessor = true;

/*
*   FUNCTION DEFINITIONS
*/
static opKeyword analyzeOperator (const vString *const op)
{
	vString *keyword = vStringNew ();
	opKeyword result;

	vStringCopyToLower (keyword, op);
	result = (opKeyword) lookupKeyword (vStringValue (keyword), Lang_asm);
	vStringDelete (keyword);
	return result;
}

static bool isInitialSymbolCharacter (int c)
{
	return (bool) (c != '\0' && (isalpha (c) || strchr ("_$", c) != NULL));
}

static bool isSymbolCharacter (int c)
{
	/* '?' character is allowed in AMD 29K family */
	return (bool) (c != '\0' && (isalnum (c) || strchr ("_$?", c) != NULL));
}

static AsmKind operatorKind (
		const vString *const operator,
		bool *const found)
{
	AsmKind result = K_NONE;
	const opKeyword kw = analyzeOperator (operator);
	*found = (bool) (kw != OP_UNDEFINED);
	if (*found)
	{
		result = OpKinds [kw].kind;
		Assert (OpKinds [kw].keyword == kw);
	}
	return result;
}

/*  We must check for "DB", "DB.L", "DCB.W" (68000)
 */
static bool isDefineOperator (const vString *const operator)
{
	const unsigned char *const op =
		(unsigned char*) vStringValue (operator);
	const size_t length = vStringLength (operator);
	const bool result = (bool) (length > 0  &&
		toupper ((int) *op) == 'D'  &&
		(length == 2 ||
		 (length == 4  &&  (int) op [2] == '.') ||
		 (length == 5  &&  (int) op [3] == '.')));
	return result;
}

static int makeAsmTag (
		const vString *const name,
		const vString *const operator,
		const bool labelCandidate,
		const bool nameFollows,
		const bool directive,
		int *scope)
{
	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		bool found = false;
		AsmKind kind = directive? K_NONE: operatorKind (operator, &found);

		if (found)
		{
			if (kind > K_NONE)
				r = makeSimpleTag (name, kind);
		}
		else if (isDefineOperator (operator))
		{
			if (! nameFollows)
				r = makeSimpleTag (name, K_DEFINE);
		}
		else if (labelCandidate)
		{
			operatorKind (name, &found);
			if (! found)
				r = makeSimpleTag (name, K_LABEL);
		}
		else if (directive)
		{
			bool found_dummy;
			const AsmKind kind_for_directive = operatorKind (name, &found_dummy);
			tagEntryInfo *macro_tag;

			switch (kind_for_directive)
			{
			case K_NONE:
				break;
			case K_MACRO:
				r = makeSimpleTag (operator, kind_for_directive);
				macro_tag = getEntryInCorkQueue (r);
				if (macro_tag)
				{
					macro_tag->extensionFields.scopeIndex = *scope;
					registerEntry (r);
					*scope = r;
				}
				break;
			case K_PSUEDO_MACRO_END:
				macro_tag = getEntryInCorkQueue (*scope);
				if (macro_tag)
				{
					macro_tag->extensionFields.endLine = getInputLineNumber ();
					*scope = macro_tag->extensionFields.scopeIndex;
				}
				break;
			case K_SECTION:
				r = makeSimpleRefTag (operator,
									  kind_for_directive,
									  ASM_SECTION_PLACEMENT);
				break;
			default:
				r = makeSimpleTag (operator, kind_for_directive);
			}
		}
	}
	return r;
}

static const unsigned char *readSymbol (
		const unsigned char *const start,
		vString *const sym)
{
	const unsigned char *cp = start;
	vStringClear (sym);
	if (isInitialSymbolCharacter ((int) *cp))
	{
		while (isSymbolCharacter ((int) *cp))
		{
			vStringPut (sym, *cp);
			++cp;
		}
	}
	return cp;
}

static const unsigned char *readOperator (
		const unsigned char *const start,
		vString *const operator)
{
	const unsigned char *cp = start;
	vStringClear (operator);
	while (*cp != '\0'  &&  ! isspace ((int) *cp) && *cp != ',')
	{
		vStringPut (operator, *cp);
		++cp;
	}
	return cp;
}

// We stop applying macro replacements if the unget buffer gets too big
// as it is a sign of recursive macro expansion
#define ASM_SCRIPT_PARSER_MAXIMUM_UNGET_BUFFER_SIZE_FOR_MACRO_REPLACEMENTS 65536

// We stop applying macro replacements if a macro is used so many
// times in a recursive macro expansion.
#define ASM_SCRIPT_PARSER_MAXIMUM_MACRO_USE_COUNT 8

static bool collectCppMacroArguments (ptrArray *args)
{
	vString *s = vStringNew ();
	int c;
	int depth = 1;

	do
	{
		c = cppGetc ();
		if (c == EOF || c == '\n')
			break;
		else if (c == ')')
		{
			depth--;
			if (depth == 0)
			{
				char *cstr = vStringDeleteUnwrap (s);
				ptrArrayAdd (args, cstr);
				s = NULL;
			}
			else
				vStringPut (s, c);
		}
		else if (c == '(')
		{
			depth++;
			vStringPut (s, c);
		}
		else if (c == ',')
		{
			char *cstr = vStringDeleteUnwrap (s);
			ptrArrayAdd (args, cstr);
			s = vStringNew ();
		}
		else if (c == STRING_SYMBOL || c == CHAR_SYMBOL)
			vStringPut (s, ' ');
		else
			vStringPut (s, c);
	}
	while (depth > 0);

	vStringDelete (s);			/* NULL is acceptable. */

	if (depth > 0)
		TRACE_PRINT("unbalanced argument list");

	return (depth > 0)? false: true;
}

static bool expandCppMacro (cppMacroInfo *macroInfo)
{
	ptrArray *args = NULL;

	if (macroInfo->hasParameterList)
	{
		int c;

		while (1)
		{
			c = cppGetc ();
			if (c == STRING_SYMBOL || c == CHAR_SYMBOL || !isspace (c))
				break;
		}

		if (c != '(')
		{
			cppUngetc (c);
			return false;
		}

		args = ptrArrayNew (eFree);
		if (!collectCppMacroArguments (args))
		{
			/* The input stream is already corrupted.
			 * It is hard to recover. */
			ptrArrayDelete (args);
			return false;
		}
	}

	cppBuildMacroReplacementWithPtrArrayAndUngetResult(macroInfo, args);

	ptrArrayDelete (args);		/* NULL is acceptable. */
	return true;
}

static void truncateLastIdetifier (vString *line, vString *identifier)
{
	Assert (vStringLength (line) >= vStringLength (identifier));
	size_t len = vStringLength (line) - vStringLength (identifier);
	Assert (strcmp (vStringValue (line) + len,
					vStringValue (identifier)) == 0);
	vStringTruncate (line, len);
}

static bool processCppMacroX (vString *identifier, int lastChar, vString *line)
{
	TRACE_ENTER();

	bool r = false;
	cppMacroInfo *macroInfo = cppFindMacro (vStringValue (identifier));

	if (!macroInfo)
		goto out;

	if (lastChar != EOF)
		cppUngetc (lastChar);

	TRACE_PRINT("Macro expansion: %s<%p>%s", macroInfo->name,
				macroInfo, macroInfo->hasParameterList? "(...)": "");

	r = expandCppMacro (macroInfo);

 out:
	if (r)
		truncateLastIdetifier (line, identifier);

	vStringClear (identifier);

	TRACE_LEAVE();
	return r;
}

static const unsigned char *readLineViaCpp (const char *commentChars)
{
	static vString *line;
	int c;
	bool truncation = false;

	line = vStringNewOrClear (line);

	vString *identifier = vStringNew ();

 cont:
	while ((c = cppGetc()) != EOF)
	{
		if (c == STRING_SYMBOL || c == CHAR_SYMBOL)
		{
			/* c == CHAR_SYMBOL is subtle condition.
			 * If the last char of IDENTIFIER is [0-9a-f],
			 * cppGetc() never returns CHAR_SYMBOL to
			 * Handle c++14 digit separator.
			 */
			if (!vStringIsEmpty (identifier)
				&& processCppMacroX (identifier, ' ', line))
				continue;

			/* We cannot store these values to vString
			 * Store a whitespace as a dummy value for them.
			 */
			if (!truncation)
				vStringPut (line, ' ');
		}
		else if (c == '\n' || (extraLinesepChars[0] != '\0'
							   && strchr (extraLinesepChars, c) != NULL))
		{
			if (!vStringIsEmpty (identifier)
				&& processCppMacroX (identifier, c, line))
				continue;
			break;
		}
		else if ((vStringIsEmpty (identifier) && (isalpha (c) || c == '_'))
				|| (!vStringIsEmpty (identifier) && (isalnum (c) || c == '_')))
		{
			vStringPut (identifier, c);
			if (!truncation)
				vStringPut (line, c);
		}
		else
		{
			if (!vStringIsEmpty (identifier)
				&& processCppMacroX (identifier, c, line))
				continue;

			if (truncation == false && commentChars[0] && strchr (commentChars, c))
				truncation = true;

			if (!truncation)
				vStringPut (line, c);
		}
	}

	if (c == EOF
		&& !vStringIsEmpty(identifier)
		&& processCppMacroX (identifier, EOF, line))
		goto cont;

	vStringDelete (identifier);

	TRACE_PRINT("line: %s\n", vStringValue (line));

	if ((vStringLength (line) == 0) && (c == EOF))
		return NULL;
	else
		return (unsigned char *)vStringValue (line);
}

static const unsigned char *readLineNoCpp (const char *commentChars)
{
	static vString *line;
	int c;
	bool truncation = false;

	line = vStringNewOrClear (line);

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c == '\n' || (extraLinesepChars[0] != '\0'
						  && strchr (extraLinesepChars, c) != NULL))
			break;
		else
		{
			if (truncation == false && commentChars[0] && strchr (commentChars, c))
				truncation = true;

			if (!truncation)
				vStringPut (line, c);
		}
	}
	if ((vStringLength (line) == 0) && (c == EOF))
		return NULL;
	else
		return (unsigned char *)vStringValue (line);
}

static const unsigned char *asmReadLineFromInputFile (const char *commentChars, bool useCpp)
{
	if (useCpp)
		return readLineViaCpp (commentChars);
	else
		return readLineNoCpp (commentChars);
}

static void  readMacroParameters (int index, tagEntryInfo *e, const unsigned char *cp)
{
	vString *name = vStringNew ();
	vString *signature = vStringNew ();
	int nth = 0;

	if (*cp == ',')
		++cp;

	while (*cp)
	{
		const unsigned char *tmp;
		tagEntryInfo *e = NULL;

		while (isspace ((int) *cp))
			++cp;

		tmp = cp;
		cp = readSymbol (cp, name);
		if (cp == tmp)
			break;

		{
			int r = makeSimpleTag (name, K_PARAM);
			e = getEntryInCorkQueue (r);
			if (e)
			{
				e->extensionFields.scopeIndex = index;
				e->extensionFields.nth = nth++;
			}
			if (vStringLength (signature) > 0 && vStringLast (signature) != ' ')
				vStringPut (signature, ' ');
			vStringCat (signature, name);
		}

		if (*cp == ':')
		{
			cp++;
			if (strncmp((const char *)cp, "req" ,3) == 0)
			{
				cp += 3;
				if (e)
					attachParserField (e, true, AsmFields[F_PROPERTIES].ftype,
									   "req");
				vStringCatS (signature, ":req");
			}
			else if (strncmp((const char *)cp, "vararg", 6) == 0)
			{
				cp += 6;
				if (e)
					attachParserField (e, true, AsmFields[F_PROPERTIES].ftype,
									   "vararg");
				vStringCatS (signature, ":vararg");
			}
			cp = (const unsigned char *)strpbrk ((const char *)cp , " \t,=");
			if (cp == NULL)
				break;
		}
		if (*cp == '=')
		{
			const unsigned char *start = cp;
			cp = (const unsigned char *)strpbrk ((const char *)cp , " \t,");

			if (cp)
				vStringNCatS (signature, (const char *)start, cp - start);
			else
			{
				vStringCatS (signature, (const char *)start);
				break;
			}
		}

		while (isspace ((int) *cp))
			++cp;

		if (*cp == ',')
			cp++;
	}

	if (vStringLength (signature) > 0)
	{
		e->extensionFields.signature = vStringDeleteUnwrap (signature);
		signature = NULL;
	}
	vStringDelete (signature);	/* NULL is acceptable. */
	vStringDelete (name);
}

static void findAsmTagsCommon (bool useCpp)
{
	vString *name = vStringNew ();
	vString *operator = vStringNew ();
	const unsigned char *line;

	if (useCpp)
		cppInit (false, false, false, false,
				 KIND_GHOST_INDEX, 0, 0, KIND_GHOST_INDEX, KIND_GHOST_INDEX, 0, 0,
				 FIELD_UNKNOWN);

	int scope = CORK_NIL;

	 while ((line = asmReadLineFromInputFile (commentCharsInMOL, useCpp)) != NULL)
	 {
		const unsigned char *cp = line;
		bool labelCandidate = (bool) (! isspace ((int) *cp));
		bool nameFollows = false;
		bool directive = false;
		const bool isComment = (bool)
				(*cp != '\0' && strchr (commentCharsAtBOL, *cp) != NULL);

		/* skip comments */
		if (isComment)
			continue;

		/* skip white space */
		while (isspace ((int) *cp))
			++cp;

		/* read symbol */
		if (*cp == '.')
		{
			directive = true;
			labelCandidate = false;
			++cp;
		}

		cp = readSymbol (cp, name);
		if (vStringLength (name) > 0)
		{
			if (*cp == ':')
			{
				labelCandidate = true;
				++cp;
			}
			else if (anyKindEntryInScope (CORK_NIL,
										  vStringValue (name),
										  K_MACRO, true))
				labelCandidate = false;
		}

		if (! isspace ((int) *cp)  &&  *cp != '\0')
			continue;

		/* skip white space */
		while (isspace ((int) *cp))
			++cp;

		/* skip leading dot */
#if 0
		if (*cp == '.')
			++cp;
#endif

		cp = readOperator (cp, operator);

		/* attempt second read of symbol */
		if (vStringLength (name) == 0)
		{
			while (isspace ((int) *cp))
				++cp;
			cp = readSymbol (cp, name);
			nameFollows = true;
		}
		int r = makeAsmTag (name, operator, labelCandidate, nameFollows, directive, &scope);
		tagEntryInfo *e = getEntryInCorkQueue (r);
		if (e && e->kindIndex == K_MACRO && isRoleAssigned(e, ROLE_DEFINITION_INDEX))
			readMacroParameters (r, e, cp);
	}

	if (useCpp)
		cppTerminate ();

	vStringDelete (name);
	vStringDelete (operator);
}

static void findAsmTags (void)
{
	findAsmTagsCommon (useCPreProcessor);
}

static void initialize (const langType language)
{
	Lang_asm = language;
}

#define defineCommentCharSetter(PREPOS, POS)							\
	static void asmSetCommentChars##PREPOS##POS (const langType language CTAGS_ATTR_UNUSED, \
												 const char *optname CTAGS_ATTR_UNUSED, const char *arg) \
	{																	\
		if (commentChars##PREPOS##POS != defaultCommentChar##PREPOS##POS) \
			eFree ((void *)commentChars##PREPOS##POS);					\
																		\
		if (arg && (arg[0] != '\0'))									\
			commentChars##PREPOS##POS = eStrdup (arg);					\
		else															\
			commentChars##PREPOS##POS = defaultCommentChar##PREPOS##POS; \
	}

defineCommentCharSetter(At, BOL);
defineCommentCharSetter(In, MOL);

static void asmSetExtraLinesepChars(const langType language CTAGS_ATTR_UNUSED,
									const char *optname CTAGS_ATTR_UNUSED, const char *arg)
{
	if (extraLinesepChars != defaultExtraLinesepChars)
		eFree ((void *)extraLinesepChars);

	if (arg && (arg[0] != '\0'))
		extraLinesepChars = eStrdup (arg);
	else
		extraLinesepChars = defaultExtraLinesepChars;
}

static void setUseCPreProcessor(const langType language CTAGS_ATTR_UNUSED,
								const char *name, const char *arg)
{
	useCPreProcessor = paramParserBool (arg, useCPreProcessor,
										name, "parameter");
}

static parameterHandlerTable AsmParameterHandlerTable [] = {
	{
		.name = "commentCharsAtBOL",
		.desc = "line comment chraracters at the begining of line ([" DEFAULT_COMMENT_CHARS_BOL "])",
		.handleParameter = asmSetCommentCharsAtBOL,
	},
	{
		.name = "commentCharsInMOL",
		.desc = "line comment chraracters in the begining of line ([" DEFAULT_COMMENT_CHARS_MOL "])",
		.handleParameter = asmSetCommentCharsInMOL,
	},
	{
		.name = "extraLinesepChars",
		.desc = "extra characters used as a line separator ([])",
		.handleParameter = asmSetExtraLinesepChars,
	},
	{
		.name = "useCPreProcessor",
		.desc = "run CPreProcessor parser for extracting macro definitions ([true] or false)",
		.handleParameter = setUseCPreProcessor,
	},
};

extern parserDefinition* AsmParser (void)
{
	static const char *const extensions [] = {
		"asm", "ASM", "s", "S", NULL
	};
	static const char *const patterns [] = {
		"*.A51",
		"*.29[kK]",
		"*.[68][68][kKsSxX]",
		"*.[xX][68][68]",
		NULL
	};
	static selectLanguage selectors[] = { selectByArrowOfR, NULL };

	parserDefinition* def = parserNew ("Asm");
	def->kindTable      = AsmKinds;
	def->kindCount  = ARRAY_SIZE (AsmKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->parser     = findAsmTags;
	def->initialize = initialize;
	def->keywordTable = AsmKeywords;
	def->keywordCount = ARRAY_SIZE (AsmKeywords);
	def->selectLanguage = selectors;
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	def->fieldTable = AsmFields;
	def->fieldCount = ARRAY_SIZE (AsmFields);

	def->parameterHandlerTable = AsmParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(AsmParameterHandlerTable);

	return def;
}
