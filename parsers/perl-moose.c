/*
 *   Copyright (c) 2019, Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for Moose perl extension.
 *   https://metacpan.org/pod/Moose
 *
 *   This module can gather tags for Moo perl extension, too.
 *   https://metacpan.org/pod/Moo
 *
 */

/* NOTE about kind/role design:
 *
 * sub foo { ... }
 * after 'foo' => sub { ...}
 *
 * There were two ideas to capture 'foo':
 *
 * A: capturing 'foo' as a reference tag with 'method' kind and 'wrapped' role, and
 * B: capturing 'foo' as a definition tag with 'wrapper' kind.
 *
 * This implementation takes the idea B. */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "parse.h"
#include "perl.h"
#include "read.h"
#include "routines.h"
#include "trace.h"

#include <string.h>

/*
 *   DATA DECLARATIONS
 */

enum MooseKind {
	K_CLASS,
	K_METHOD,
	K_ATTR,
	K_WRAPPER,
	K_ROLE,
};

static kindDefinition MooseKinds[] = {
	{ true, 'c', "class", "classes"  },
	{ true, 'm', "method", "methods" },
	{ true, 'a', "attribute", "attributes" },
	{ true, 'w', "wrapper", "wrappers" },
	{ true, 'r', "role", "roles" },
};

typedef enum {
	F_WRAPPING,
} MooseField;

static fieldDefinition MooseFields [] = {
	{
		.name = "wrapping",
		.description = "how a wrapper wrapping the method (around, after, or before)",
		.enabled = true,
	},
};

enum Wrapping {
	W_UNKNOWN,
	W_AROUND,
	W_BEFORE,
	W_AFTER,
};

static char * WrappingStrings[] = {
	"unknown",
	"around",
	"before",
	"after",
};

struct mooseSubparser {
	perlSubparser perl;
	bool notInMoose;
	bool inPod;
	int packageCork;
	int classCork;
	bool notContinuousExtendsLines;

	int indexForFunctionParameters;

	/* functionParametersModifiersStateCounter is for tracking the conditions
	 * that both "use Moose;" and "use Functions::Parameters qw/:modifiers/"
	 * are specified.
	 * functionParametersModifiersStateCounter is initialized to 2.
	 * Decrement functionParametersModifiersStateCounter when finding one of two.
	 * When functionParametersModifiersStateCounter is 0, the state is
	 * propagated to notInFunctionParametersModifiers. */
	bool notInFunctionParametersModifiers;
	int  functionParametersModifiersStateCounter;
#define RESET_FunctionParameters_STATE(MOOSE)				\
	do {													\
		MOOSE->notInFunctionParametersModifiers = true;		\
		MOOSE->functionParametersModifiersStateCounter = 2;	\
	} while (0)
#define DEC_FunctionParameters_STATE(MOOSE)							\
	do {															\
		MOOSE->functionParametersModifiersStateCounter--;			\
		if (MOOSE->functionParametersModifiersStateCounter == 0)	\
			MOOSE->notInFunctionParametersModifiers = false;		\
		if (MOOSE->functionParametersModifiersStateCounter < 0)		\
			MOOSE->functionParametersModifiersStateCounter = 0; 	\
	} while (0)
#define INC_FunctionParameters_STATE(MOOSE)						\
	do {														\
		MOOSE->functionParametersModifiersStateCounter++;		\
		if (MOOSE->functionParametersModifiersStateCounter > 0)	\
			MOOSE->notInFunctionParametersModifiers = true;		\
		if (MOOSE->functionParametersModifiersStateCounter > 2)	\
			MOOSE->functionParametersModifiersStateCounter = 2;	\
	} while (0)

	vString *supersOrRoles;
};

/*
 *   FUNCTION PROTOTYPES
 */
static void inputStart (subparser *s);
static void inputEnd (subparser *s);
static void makeTagEntryNotify (subparser *s, const tagEntryInfo *tag, int corkIndex);
static void enterMoose (struct mooseSubparser *moose, bool role);
static void leaveMoose (struct mooseSubparser *moose);
static void enteringPodNotify (perlSubparser *perl);
static void leavingPodNotify  (perlSubparser *perl);
static void findingQuotedWordNotify (perlSubparser *perl, int moduleIndex, const char *qwd);

/*
 *   DATA DEFINITIONS
 */

static struct mooseSubparser mooseSubparser = {
	.perl = {
		.subparser = {
			.direction  = SUBPARSER_BI_DIRECTION,
			.inputStart = inputStart,
			.inputEnd   = inputEnd,
			.makeTagEntryNotify = makeTagEntryNotify,
		},
		.enteringPodNotify = enteringPodNotify,
		.leavingPodNotify  = leavingPodNotify,
		.findingQuotedWordNotify = findingQuotedWordNotify,
	},
};


/*
 *   FUNCTION DEFINITIONS
 */

static void inputStart (subparser *s)
{
	struct mooseSubparser *moose = (struct mooseSubparser *)s;

	moose->notInMoose = true;
	moose->packageCork = CORK_NIL;
	moose->classCork = CORK_NIL;
	moose->inPod = false;
	moose->supersOrRoles = vStringNew ();
	moose->notContinuousExtendsLines = true;
	moose->indexForFunctionParameters = CORK_NIL;
	RESET_FunctionParameters_STATE (moose);
}

static void inputEnd (subparser *s)
{
	struct mooseSubparser *moose = (struct mooseSubparser *)s;
	tagEntryInfo *e = getEntryInCorkQueue (moose->classCork);
	if (e)
		e->extensionFields.endLine = getInputLineNumber ();

	vStringDelete (moose->supersOrRoles);
	moose->supersOrRoles = NULL;
}

static void makeTagEntryNotify (subparser *s, const tagEntryInfo *tag, int corkIndex)
{
	perlSubparser *perl = (perlSubparser *)s;
	struct mooseSubparser *moose = (struct mooseSubparser *)perl;

	if (isTagExtraBitMarked(tag, XTAG_QUALIFIED_TAGS))
		return;

	if (tag->kindIndex == KIND_PERL_PACKAGE)
		moose->packageCork = corkIndex;
	else if ((!moose->notInMoose) && tag->kindIndex == KIND_PERL_SUBROUTINE)
	{
		tagEntryInfo moose_e;
		initTagEntry (&moose_e, tag->name, K_METHOD);
		setTagPositionFromTag (&moose_e, tag);
		moose_e.extensionFields.scopeIndex = moose->classCork;
		makeTagEntry (&moose_e);
	}
	else if (tag->kindIndex == KIND_PERL_MODULE)
	{
		if (isRoleAssigned(tag, ROLE_PERL_MODULE_USED))
		{
			if (strcmp (tag->name, "Moose") == 0
				|| strcmp (tag->name, "Moo") == 0)
				enterMoose (moose, false);
			else if (strcmp (tag->name, "Moose::Role") == 0)
				enterMoose (moose, true);
			else if (strcmp (tag->name, "Function::Parameters") == 0)
				moose->indexForFunctionParameters = corkIndex;
		}
		else if (isRoleAssigned(tag, ROLE_PERL_MODULE_UNUSED))
		{
			if (strcmp (tag->name, "Moose") == 0
				|| strcmp (tag->name, "Moo") == 0)
				leaveMoose (moose);
			else if (strcmp (tag->name, "Function::Parameters") == 0)
			{
				moose->indexForFunctionParameters = CORK_NIL;
				INC_FunctionParameters_STATE(moose);
			}
		}
	}
}

static void enteringPodNotify (perlSubparser *perl)
{
	struct mooseSubparser *moose = (struct mooseSubparser *)perl;
	moose->inPod = true;
}

static void leavingPodNotify  (perlSubparser *perl)
{
	struct mooseSubparser *moose = (struct mooseSubparser *)perl;
	moose->inPod = false;
}

static void findingQuotedWordNotify (perlSubparser *perl,
									 int moduleIndex, const char *qwd)
{
	struct mooseSubparser *moose = (struct mooseSubparser *)perl;
	if (moose->indexForFunctionParameters != moduleIndex)
		return;

	if (strcmp (qwd, ":modifiers") == 0)
		DEC_FunctionParameters_STATE(moose);
}

static void leaveMoose (struct mooseSubparser *moose)
{
	moose->notContinuousExtendsLines = true;

	tagEntryInfo *e = getEntryInCorkQueue (moose->classCork);
	if (!e)
		return;

	e->extensionFields.endLine = getInputLineNumber ();

	moose->classCork = CORK_NIL;
	moose->notInMoose = true;
	moose->packageCork = CORK_NIL;
	INC_FunctionParameters_STATE(moose);
}

static void enterMoose (struct mooseSubparser *moose, bool role)
{
	moose->notContinuousExtendsLines = true;

	tagEntryInfo *perl_e = getEntryInCorkQueue (moose->packageCork);
	if (!perl_e)
		return;

	moose->notInMoose = false;

	tagEntryInfo moose_e;
	initTagEntry (&moose_e, perl_e->name, role? K_ROLE: K_CLASS);
	moose_e.lineNumber = perl_e->lineNumber;
	moose_e.filePosition = perl_e->filePosition;
	moose->classCork = makeTagEntry (&moose_e);
	vStringClear (moose->supersOrRoles);

	DEC_FunctionParameters_STATE(moose);

	return;
}

static void parseExtendsClass (const char *input,
							   size_t input_len,
							   vString *inherits,
							   bool *notContinuousLine)
{
	int i = 0;
	do
	{
		if (input [i] == ',')
			i++;

		for (; (i < input_len) && input[i] != '\n' && input[i] != '\0'; i++)
		{
			if (input[i] == '\'' || input[i] == ' '  || input[i] == '\t')
				continue;
			else if (input[i] == ',')
			{
				vStringPut (inherits, ',');
				*notContinuousLine = false;
				break;
			}
			else if (input[i] == ';')
			{
				*notContinuousLine = true;
				break;
			}
			else
				vStringPut (inherits, input[i]);
		}
	}
	while (input[i] == ',');
}

static bool findExtendsClass (const char *line,
							  const regexMatch *matches,
							  unsigned int count,
							  void *data)
{
	struct mooseSubparser *moose = data;
	moose->notContinuousExtendsLines = true;

	if (moose->inPod)
		return true;

	tagEntryInfo *e = getEntryInCorkQueue (moose->classCork);
	if (!e)
		return true;

	const char *input = line + matches[2].start;
	vString *str = moose->supersOrRoles;

	if (vStringLength (str) > 0)
		vStringPut (str, ',');
	parseExtendsClass (input, matches[2].length, str,
					   &moose->notContinuousExtendsLines);

	if (moose->notContinuousExtendsLines == true
		&& vStringLength (str) > 0)
	{
		if (e->extensionFields.inheritance)
			eFree ((void *)e->extensionFields.inheritance);
		e->extensionFields.inheritance = vStringStrdup (str);
	}

	return true;
}

static bool findExtendsClassContinuation (const char *line,
										  const regexMatch *matches,
										  unsigned int count,
										  void *data)
{
	struct mooseSubparser *moose = data;
	moose->notContinuousExtendsLines = true;

	tagEntryInfo *e = getEntryInCorkQueue (moose->classCork);
	if (!e)
		return true;

	const char *input = line + matches[1].start;
	vString *str = moose->supersOrRoles;

	parseExtendsClass (input, matches[1].length, str,
					   &moose->notContinuousExtendsLines);

	if (moose->notContinuousExtendsLines == true
		&& vStringLength (str) > 0)
	{
		if (e->extensionFields.inheritance)
			eFree ((void *)e->extensionFields.inheritance);
		e->extensionFields.inheritance = vStringStrdup (str);
	}

	return true;
}

static const char *parseAttributeOrWrapper (const char *str, int parentCorkIndex, int extraTerminator,
											int kind, enum Wrapping wrapping)
{
	int i;

	for (i = 0;
		 str[i]
			 && str[i] != extraTerminator
			 && (isalnum ((unsigned char)str[i]) || str[i] == '_');
		 i++)
		;						/* Just advancing `i' */

	if (i == 0)
		return NULL;

	tagEntryInfo e;
	char *buf = eStrndup (str, i);

	initTagEntry (&e, buf, kind);
	if (parentCorkIndex != CORK_NIL)
		e.extensionFields.scopeIndex = parentCorkIndex;

	int corkIndex = makeTagEntry (&e);
	if (kind == K_WRAPPER)
		attachParserFieldToCorkEntry (corkIndex, MooseFields[F_WRAPPING].ftype,
									  WrappingStrings [wrapping]);
	eFree (buf);

	return str[i] == '\0'? NULL: str + i;
}

static void solveKindAndWrapping (const char *str, int *kind, enum Wrapping *wrapping)
{
	*kind = K_WRAPPER;
	*wrapping = W_UNKNOWN;
	switch (str[0])
	{
	case 'h':					/* has */
		*kind = K_ATTR;
		break;
	case 'a':					/* around or after */
		if (str[1] == 'r')
			*wrapping = W_AROUND;
		else if (str[1] == 'f')
			*wrapping = W_AFTER;
		break;
	case 'b':					/* before */
		*wrapping = W_BEFORE;
		break;
	case 'o':					/* override */
		*kind = K_METHOD;
	}
}

static bool findAttributeOrWrapperOne (const char *line,
									   const regexMatch *matches,
									   unsigned int count,
									   void *data)
{
	struct mooseSubparser *moose = data;
	int kind;
	enum Wrapping wrapping;

	if (moose->inPod)
		return true;

	moose->notContinuousExtendsLines = true;

	if (count < 3)
		return true;

	solveKindAndWrapping (line + matches[1].start, &kind, &wrapping);
	parseAttributeOrWrapper (line + matches[2].start, moose->classCork, -1,
							 kind, wrapping);
	return true;
}

static bool findAttributeOrWrapperMulti (const char *line,
										 const regexMatch *matches,
										 unsigned int count,
										 void *data)
{

	struct mooseSubparser *moose = data;
	int kind;
	enum Wrapping wrapping;
	int terminator;

	if (moose->inPod)
		return true;

	moose->notContinuousExtendsLines = true;

	if (count < 4)
		return true;

	solveKindAndWrapping (line + matches[1].start, &kind, &wrapping);
	terminator = line[matches[2].start];


	const char *str = line + matches[3].start;
	while ((str = parseAttributeOrWrapper (str, moose->classCork, terminator,
										   kind, wrapping)))
	{
		int i;
		for (i = 0; (str[i] == ' ') || (str[i] == '\t'); i++)
			;
		if (str[i] == '\0' || str[i] == terminator)
			break;
		str = str + i;
	}

	return true;
}

static void findMooseTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void initializeMooseParser (langType language)
{
	addLanguageCallbackRegex (language, "^[ \t]*(extends|with) *(.+)",
							  "{exclusive}",
							  findExtendsClass, &mooseSubparser.notInMoose,
							  &mooseSubparser);
	addLanguageCallbackRegex (language, "^[ \t]*(has|after|before|around|override) +"
							  /* foo => ()
							   * 'foo' => ()
							   * "foo" => ()
							   * ( foo => ())
							   * ( "foo" => ())
							   * ( 'foo' => ()) */
							  "\\(?[ \t]*[\"']?"
							  ""
							  "([a-zA-Z_][a-zA-Z0-9_]*([ \t][a-zA-Z_][a-zA-Z0-9_]*)*).*=>",
							  "{exclusive}",
							  findAttributeOrWrapperOne, &mooseSubparser.notInMoose,
							  &mooseSubparser);
	addLanguageCallbackRegex (language, "^[ \t]*(has|after|before|around) +\\(?\\[qw([^ \t])[ \t]*"
							  /* [qw/foo bar/] => ()
							   * ([qw/foo bar/] => ()) */
							  "([a-zA-Z_][a-zA-Z0-9_]*([ \t][a-zA-Z_][a-zA-Z0-9_]*)*).*=>",
							  "{exclusive}",
							  findAttributeOrWrapperMulti, &mooseSubparser.notInMoose,
							  &mooseSubparser);
	addLanguageCallbackRegex (language, "^[ \t]*(has|after|before|around|override) +"
							  "([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*\\(",
							  "{exclusive}",
							  findAttributeOrWrapperOne, &mooseSubparser.notInFunctionParametersModifiers,
							  &mooseSubparser);
	addLanguageCallbackRegex (language, "^[ \t]*(.+)",
							  "{exclusive}",
							  findExtendsClassContinuation, &mooseSubparser.notContinuousExtendsLines,
							  &mooseSubparser);
}

extern parserDefinition* MooseParser (void)
{
	parserDefinition* const def = parserNew("Moose");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "Perl", &mooseSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable = MooseKinds;
	def->kindCount = ARRAY_SIZE(MooseKinds);

	def->fieldTable = MooseFields;
	def->fieldCount = ARRAY_SIZE (MooseFields);

	def->initialize = initializeMooseParser;
	def->parser = findMooseTags;
	def->useCork = CORK_QUEUE;

	return def;
}
