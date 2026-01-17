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

#include "x-cpreprocessor.h"
#include "debug.h"
#include "dependency.h"
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
	K_PSUEDO_FOREIGN_LD_SCRIPT_SYMBOL = -4,
	K_PSUEDO_FOREIGN_LD_SCRIPT_SECTION = -3,
	K_PSUEDO_MACRO_END = -2,
	K_NONE = -1, K_DEFINE, K_LABEL, K_MACRO, K_TYPE,
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
	OP_GLOBAL,
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
static langType Lang_ldscript;

static kindDefinition AsmKinds [] = {
	{ true, 'd', "define", "defines" },
	{ true, 'l', "label",  "labels"  },
	{ true, 'm', "macro",  "macros"  },
	{ true, 't', "type",   "types (structs and records)"   },
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
	{ "global",   OP_GLOBAL      },
	{ "globl",    OP_GLOBAL      },
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
	{ OP_GLOBAL,      K_PSUEDO_FOREIGN_LD_SCRIPT_SYMBOL },
	{ OP_LABEL,       K_LABEL  },
	{ OP_MACRO,       K_MACRO  },
	{ OP_PROC,        K_LABEL  },
	{ OP_RECORD,      K_TYPE   },
	{ OP_SECTIONS,    K_NONE   },
	{ OP_SECTION,     K_PSUEDO_FOREIGN_LD_SCRIPT_SECTION },
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
		toupper (*op) == 'D'  &&
		(length == 2 ||
		 (length == 4  &&  (int) op [2] == '.') ||
		 (length == 5  &&  (int) op [3] == '.')));
	return result;
}

static int makeTagForLdScript (const char * name, int kind, int *scope, bool useCpp)
{
	tagEntryInfo e;

	if (kind == K_PSUEDO_FOREIGN_LD_SCRIPT_SYMBOL)
	{
		static kindDefinition * kdef = NULL;
		if(kdef == NULL)
			kdef = getLanguageKindForName (Lang_ldscript, "symbol");
		if(kdef == NULL)
			return CORK_NIL;

		initForeignTagEntry(&e, name, Lang_ldscript, kdef->id);
		e.extensionFields.scopeIndex = *scope;
		if (useCpp)
			updateTagLine (&e, cppGetInputLineNumber (), cppGetInputFilePosition());
		return makeTagEntry (&e);
	}
	else
	{
		static kindDefinition * kdef = NULL;
		if(kdef == NULL)
			kdef = getLanguageKindForName (Lang_ldscript, "inputSection");
		if(kdef == NULL)
			return CORK_NIL;

		static roleDefinition *rdef = NULL;
		if(rdef == NULL)
			rdef = getLanguageRoleForName (Lang_ldscript, kdef->id, "destination");
		if (rdef == NULL)
			return CORK_NIL;

		initForeignRefTagEntry(&e, name, Lang_ldscript, kdef->id, rdef->id);
		if (useCpp)
			updateTagLine (&e, cppGetInputLineNumber (), cppGetInputFilePosition());
		*scope = makeTagEntry (&e);
		return *scope;
	}
}

static int makeAsmSimpleTag (const vString *const name,
							 AsmKind kind,
							 bool useCpp)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kind);
	if (useCpp)
		updateTagLine (&e, cppGetInputLineNumber (), cppGetInputFilePosition());
	return makeTagEntry (&e);
}

static int makeAsmTag (
		const vString *const name,
		const vString *const operator,
		const bool labelCandidate,
		const bool nameFollows,
		const bool directive,
		int *sectionScope,
		int *macroScope,
		bool useCpp)
{
	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		bool found = false;
		AsmKind kind = directive? K_NONE: operatorKind (operator, &found);

		if (found)
		{
			if (kind > K_NONE)
				r = makeAsmSimpleTag (name, kind, useCpp);
		}
		else if (isDefineOperator (operator))
		{
			if (! nameFollows)
				r = makeAsmSimpleTag (name, K_DEFINE, useCpp);
		}
		else if (labelCandidate)
		{
			operatorKind (name, &found);
			if (! found)
			{
				r = makeAsmSimpleTag (name, K_LABEL, useCpp);
			}
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
				if (!vStringIsEmpty (operator))
				{
					r = makeAsmSimpleTag (operator, kind_for_directive, useCpp);
					macro_tag = getEntryInCorkQueue (r);
					if (macro_tag)
					{
						macro_tag->extensionFields.scopeIndex = *macroScope;
						registerEntry (r);
						*macroScope = r;
					}
				}
				break;
			case K_PSUEDO_MACRO_END:
				macro_tag = getEntryInCorkQueue (*macroScope);
				if (macro_tag)
				{
					setTagEndLine (macro_tag,
								   useCpp? cppGetInputLineNumber (): getInputLineNumber ());
					*macroScope = macro_tag->extensionFields.scopeIndex;
				}
				break;
			case K_PSUEDO_FOREIGN_LD_SCRIPT_SYMBOL:
			case K_PSUEDO_FOREIGN_LD_SCRIPT_SECTION:
				if (!vStringIsEmpty (operator))
					r = makeTagForLdScript (vStringValue (operator),
											kind_for_directive, sectionScope, useCpp);
				break;
			default:
				if (!vStringIsEmpty (operator))
					r = makeAsmSimpleTag (operator, kind_for_directive, useCpp);
				break;
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
	if (isInitialSymbolCharacter (*cp))
	{
		while (isSymbolCharacter (*cp))
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
	while (*cp != '\0'  &&  ! isspace (*cp) && *cp != ',')
	{
		vStringPut (operator, *cp);
		++cp;
	}
	return cp;
}

static bool collectCppMacroArguments (ptrArray *args)
{
	vString *s = vStringNew ();
	int c;
	unsigned long ln = cppGetInputLineNumber ();
	MIOPos pos = cppGetInputFilePosition ();
	int depth = 1;

	do
	{
		if (s && vStringLength (s) == 1)
		{
			ln = cppGetInputLineNumber ();
			pos = cppGetInputFilePosition ();
		}
		c = cppGetc ();

		if (c == EOF)
			break;
		else if (c == ')')
		{
			depth--;
			if (depth == 0)
			{
				vStringStripTrailing(s);
				cppMacroArg *a = cppMacroArgNew (vStringDeleteUnwrap (s), true,
												 ln, pos);
				ptrArrayAdd (args, a);
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
			vStringStripTrailing(s);
			cppMacroArg *a = cppMacroArgNew (vStringDeleteUnwrap (s), true,
											 ln, pos);
			ptrArrayAdd (args, a);
			s = vStringNew ();
		}
		else if (c == CPP_STRING_SYMBOL || c == CPP_CHAR_SYMBOL)
			vStringPut (s, ' ');
		else if (isspace(c))
		{
			if (!vStringIsEmpty (s) &&
				!isspace ((unsigned char)vStringLast (s)))
				vStringPut (s, ' ');
		}
		else
			vStringPut (s, c);
	}
	while (depth > 0);

	vStringDelete (s);			/* NULL is acceptable. */

	if (depth > 0)
		TRACE_PRINT("unbalanced argument list");

	return (depth > 0)? false: true;
}

static bool expandCppMacro (cppMacroInfo *macroInfo,
							unsigned long lineNumber, MIOPos filePosition)
{
	ptrArray *args = NULL;

	if (macroInfo->hasParameterList)
	{
		int c;

		while (1)
		{
			c = cppGetc ();
			if (c == CPP_STRING_SYMBOL || c == CPP_CHAR_SYMBOL || !isspace (c))
				break;
		}

		if (c != '(')
		{
			cppUngetc (c);
			return false;
		}

		args = ptrArrayNew (cppMacroArgDelete);
		if (!collectCppMacroArguments (args))
		{
			/* The input stream is already corrupted.
			 * It is hard to recover. */
			ptrArrayDelete (args);
			return false;
		}
	}

	{
		cppMacroTokens *tokens = cppExpandMacro (macroInfo, args,
												 lineNumber, filePosition);
		cppUngetMacroTokens (tokens);
	}

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

	if (cppUngetBufferSize() >= CPP_MAXIMUM_UNGET_BUFFER_SIZE_FOR_MACRO_REPLACEMENTS)
	{
		TRACE_LEAVE_TEXT ("Ungetbuffer overflow when processing \"%s\": %d",
						  vStringValue (identifier), cppUngetBufferSize());
		return false;
	}

	bool r = false;
	cppMacroInfo *macroInfo = cppFindMacro (vStringValue (identifier));

	if (!macroInfo)
		goto out;

	if (macroInfo->useCount >= CPP_MAXIMUM_MACRO_USE_COUNT)
	{
		TRACE_PRINT ("Overly uesd macro %s<%p> useCount: %d (> %d)",
					 vStringValue (identifier), macroInfo, macroInfo->useCount,
					 CPP_MAXIMUM_MACRO_USE_COUNT);
		goto out;
	}

	if (lastChar != EOF)
		cppUngetc (lastChar);

	TRACE_PRINT("Macro expansion: %s<%p>%s", macroInfo->name,
				macroInfo, macroInfo->hasParameterList? "(...)": "");

	if (!macroInfo->replacements)
		goto out;

	r = expandCppMacro (macroInfo,
						cppGetInputLineNumber (), cppGetInputFilePosition ());

 out:
	if (r)
		truncateLastIdetifier (line, identifier);

	vStringClear (identifier);

	TRACE_LEAVE();
	return r;
}

/* If a section name is built with a macro expansion, the following
 * strings may appear in parts of the string.
 * - \param
 * - \()
 * - \@
 */
static bool isCharInMarcoParamref(char c)
{
	return (c == '\\' || c == '(' || c == ')'  || c == '@')? true: false;
}

static bool isEligibleAsSectionName (const vString *str)
{
	char *c = vStringValue(str);
	while (*c)
	{
		if (!(isalnum(((unsigned char)*c))
			  || (*c == '.')
			  || (*c == '-')
			  || (*c == '_')
			  || isCharInMarcoParamref(*c)))
			return false;
		c++;
	}
	return true;
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
		if (c == CPP_STRING_SYMBOL || c == CPP_CHAR_SYMBOL)
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
			 * Store a whitespace as a dummy value for them, but...
			 */
			if (!truncation)
			{
				vStringPut (line, ' ');

				/* Quoted from the info document of Gas:
				   -------------------------------------
				   For ELF targets, the assembler supports another type of '.section'
				   directive for compatibility with the Solaris assembler:

				   .section "NAME"[, FLAGS...]
				   -------------------------------------

				   If we replace "..." with ' ' here, we can lost the name
				   of the section. */
				const vString *str = cppGetLastCharOrStringContents();
				if (str)
				{
					const char *section = strrstr (vStringValue (line), ".section");
					if (section && isEligibleAsSectionName(str))
					{
						section += strlen(".section");
						while (isspace((unsigned char)*section))
							section++;
						if (*section == '\0')
						{
							vStringCat (line, str);
							vStringPut (line, ' ');
						}
					}
				}
			}
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

static void readMacroParameters (int index, tagEntryInfo *e, const unsigned char *cp,
								 bool useCpp)
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

		while (isspace (*cp))
			++cp;

		tmp = cp;
		cp = readSymbol (cp, name);
		if (cp == tmp)
			break;

		{
			int r = makeAsmSimpleTag (name, K_PARAM, useCpp);
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
					attachParserField (e, AsmFields[F_PROPERTIES].ftype,
									   "req");
				vStringCatS (signature, ":req");
			}
			else if (strncmp((const char *)cp, "vararg", 6) == 0)
			{
				cp += 6;
				if (e)
					attachParserField (e, AsmFields[F_PROPERTIES].ftype,
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

		while (isspace (*cp))
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

	int sectionScope = CORK_NIL;
	int macroScope = CORK_NIL;

	 while ((line = asmReadLineFromInputFile (commentCharsInMOL, useCpp)) != NULL)
	 {
		const unsigned char *cp = line;
		bool labelCandidate = (bool) (! isspace (*cp));
		bool nameFollows = false;
		bool directive = false;
		const bool isComment = (bool)
				(*cp != '\0' && strchr (commentCharsAtBOL, *cp) != NULL);

		/* skip comments */
		if (isComment)
			continue;

		/* skip white space */
		while (isspace (*cp))
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

		if (! isspace (*cp)  &&  *cp != '\0')
			continue;

		/* skip white space */
		while (isspace (*cp))
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
			while (isspace (*cp))
				++cp;
			cp = readSymbol (cp, name);
			nameFollows = true;
		}
		int r = makeAsmTag (name, operator, labelCandidate, nameFollows, directive,
							&sectionScope, &macroScope, useCpp);
		tagEntryInfo *e = getEntryInCorkQueue (r);
		if (e && e->langType == Lang_asm
			&& e->kindIndex == K_MACRO && isRoleAssigned(e, ROLE_DEFINITION_INDEX))
			readMacroParameters (r, e, cp, useCpp);
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

/* dummy definition to allow/require an extra semicolon */
#define END_DEF(sfx) typedef int ctags_dummy_int_type_ignore_me_##sfx

#define defineCommentCharSetter(PREPOS, POS)							\
	static bool asmSetCommentChars##PREPOS##POS (const langType language CTAGS_ATTR_UNUSED, \
												 const char *optname CTAGS_ATTR_UNUSED, const char *arg) \
	{																	\
		if (commentChars##PREPOS##POS != defaultCommentChar##PREPOS##POS) \
			eFree ((void *)commentChars##PREPOS##POS);					\
																		\
		if (arg && (arg[0] != '\0'))									\
			commentChars##PREPOS##POS = eStrdup (arg);					\
		else															\
			commentChars##PREPOS##POS = defaultCommentChar##PREPOS##POS; \
		return true;													\
	} END_DEF(asmSetCommentChars##PREPOS##POS)

defineCommentCharSetter(At, BOL);
defineCommentCharSetter(In, MOL);

static bool asmSetExtraLinesepChars(const langType language CTAGS_ATTR_UNUSED,
									const char *optname CTAGS_ATTR_UNUSED, const char *arg)
{
	if (extraLinesepChars != defaultExtraLinesepChars)
		eFree ((void *)extraLinesepChars);

	if (arg && (arg[0] != '\0'))
		extraLinesepChars = eStrdup (arg);
	else
		extraLinesepChars = defaultExtraLinesepChars;

	return true;
}

static bool setUseCPreProcessor(const langType language CTAGS_ATTR_UNUSED,
								const char *name, const char *arg)
{
	useCPreProcessor = paramParserBool (arg, useCPreProcessor,
										name, "parameter");
	return true;
}

static paramDefinition AsmParams [] = {
	{
		.name = "commentCharsAtBOL",
		.desc = "line comment chraracters at the beginning of line ([" DEFAULT_COMMENT_CHARS_BOL "])",
		.handleParam = asmSetCommentCharsAtBOL,
	},
	{
		.name = "commentCharsInMOL",
		.desc = "line comment chraracters in the beginning of line ([" DEFAULT_COMMENT_CHARS_MOL "])",
		.handleParam = asmSetCommentCharsInMOL,
	},
	{
		.name = "extraLinesepChars",
		.desc = "extra characters used as a line separator ([])",
		.handleParam = asmSetExtraLinesepChars,
	},
	{
		.name = "useCPreProcessor",
		.desc = "run CPreProcessor parser for extracting macro definitions ([true] or false)",
		.handleParam = setUseCPreProcessor,
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

	static parserDependency dependencies [] = {
		{ DEPTYPE_FOREIGNER, "LdScript", &Lang_ldscript },
	};

	parserDefinition* def = parserNew ("Asm");
	def->versionCurrent = 1;
	def->versionAge = 0;
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
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

	def->paramTable = AsmParams;
	def->paramCount = ARRAY_SIZE(AsmParams);

	return def;
}
