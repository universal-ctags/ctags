/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for parsing and scanning C, C++ and Java
*   source files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "entry.h"
#include "get.h"
#include "keyword.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

/*
*   MACROS
*/
#define MaxFields   6

#define stringValue(a)		#a
#define activeToken(st)		((st)->token [(int) (st)->tokenIndex])
#define parentDecl(st)		((st)->parent == NULL ? \
				DECL_NONE : (st)->parent->declaration)
#define isType(token,t)		(boolean) ((token)->type == (t))
#define insideEnumBody(st)	((st)->parent == NULL ? FALSE : \
			    (boolean) ((st)->parent->declaration == DECL_ENUM))
#define isExternCDecl(st,c)	(boolean) ((c) == STRING_SYMBOL  && \
		    ! (st)->haveQualifyingName  && (st)->scope == SCOPE_EXTERN)

#define isOneOf(c,s)		(boolean) (strchr ((s), (c)) != NULL)

/*
*   DATA DECLARATIONS
*/

enum { NumTokens = 3 };

typedef enum eException {
    ExceptionNone, ExceptionEOF, ExceptionFormattingError,
    ExceptionBraceFormattingError
} exception_t;

/*  Used to specify type of keyword.
 */
typedef enum eKeywordId {
    KEYWORD_NONE,
    KEYWORD_ATTRIBUTE, KEYWORD_ABSTRACT,
    KEYWORD_BOOLEAN, KEYWORD_BYTE,
    KEYWORD_CATCH, KEYWORD_CHAR, KEYWORD_CLASS, KEYWORD_CONST,
    KEYWORD_DOUBLE,
    KEYWORD_ENUM, KEYWORD_EXPLICIT, KEYWORD_EXTERN, KEYWORD_EXTENDS,
    KEYWORD_FINAL, KEYWORD_FLOAT, KEYWORD_FRIEND,
    KEYWORD_IMPLEMENTS, KEYWORD_IMPORT, KEYWORD_INLINE, KEYWORD_INT,
    KEYWORD_INTERFACE,
    KEYWORD_LONG,
    KEYWORD_MUTABLE,
    KEYWORD_NAMESPACE, KEYWORD_NEW, KEYWORD_NATIVE,
    KEYWORD_OPERATOR, KEYWORD_OVERLOAD,
    KEYWORD_PACKAGE, KEYWORD_PRIVATE, KEYWORD_PROTECTED, KEYWORD_PUBLIC,
    KEYWORD_REGISTER,
    KEYWORD_SHORT, KEYWORD_SIGNED, KEYWORD_STATIC, KEYWORD_STRUCT,
    KEYWORD_SYNCHRONIZED,
    KEYWORD_TEMPLATE, KEYWORD_THROW, KEYWORD_THROWS, KEYWORD_TRANSIENT,
    KEYWORD_TRY, KEYWORD_TYPEDEF, KEYWORD_TYPENAME,
    KEYWORD_UNION, KEYWORD_UNSIGNED, KEYWORD_USING,
    KEYWORD_VIRTUAL, KEYWORD_VOID, KEYWORD_VOLATILE,
    KEYWORD_WCHAR_T
} keywordId;

/*  Used to determine whether keyword is valid for the current language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
    const char *name;
    keywordId id;
    short isValid [3]; /* indicates languages for which kw is valid */
} keywordDesc;

/*  Used for reporting the type of object parsed by nextToken ().
 */
typedef enum eTokenType {
    TOKEN_NONE,		/* none */
    TOKEN_ARGS,		/* a parenthetical pair and its contents */
    TOKEN_BRACE_CLOSE,
    TOKEN_BRACE_OPEN,
    TOKEN_COMMA,		/* the comma character */
    TOKEN_DOUBLE_COLON,	/* double colon indicates nested-name-specifier */
    TOKEN_KEYWORD,
    TOKEN_NAME,		/* an unknown name */
    TOKEN_PACKAGE,	/* a Java package name */
    TOKEN_PAREN_NAME,	/* a single name in parentheses */
    TOKEN_SEMICOLON,	/* the semicolon character */
    TOKEN_SPEC,		/* a storage class specifier, qualifier, type, etc. */
    TOKEN_COUNT
} tokenType;

/*  This describes the scoping of the current statement.
 */
typedef enum eTagScope {
    SCOPE_GLOBAL,		/* no storage class specified */
    SCOPE_STATIC,		/* static storage class */
    SCOPE_EXTERN,		/* external storage class */
    SCOPE_FRIEND,		/* declares access only */
    SCOPE_TYPEDEF,		/* scoping depends upon context */
    SCOPE_COUNT
} tagScope;

typedef enum eDeclaration {
    DECL_NONE,
    DECL_BASE,			/* base type (default) */
    DECL_CLASS,
    DECL_ENUM,
    DECL_FUNCTION,
    DECL_IGNORE,		/* non-taggable "declaration" */
    DECL_INTERFACE,
    DECL_NAMESPACE,
    DECL_NOMANGLE,		/* C++ name demangling block */
    DECL_PACKAGE,
    DECL_STRUCT,
    DECL_UNION,
    DECL_COUNT
} declType;

typedef enum eVisibilityType {
    ACCESS_UNDEFINED,
    ACCESS_PRIVATE,
    ACCESS_PROTECTED,
    ACCESS_PUBLIC,
    ACCESS_DEFAULT,		/* Java-specific */
    ACCESS_COUNT
} accessType;

/*  Information about the parent class of a member (if any).
 */
typedef struct sMemberInfo {
    accessType	access;		/* access of current statement */
    accessType	accessDefault;	/* access default for current statement */
} memberInfo;

typedef struct sTokenInfo {
    tokenType     type;
    keywordId     keyword;
    vString*      name;		/* the name of the token */
    unsigned long lineNumber;	/* line number of tag */
    fpos_t        filePosition;	/* file position of line containing name */
} tokenInfo;

typedef enum eImplementation {
    IMP_DEFAULT,
    IMP_ABSTRACT,
    IMP_VIRTUAL,
    IMP_PURE_VIRTUAL,
    IMP_COUNT
} impType;

/*  Describes the statement currently undergoing analysis.
 */
typedef struct sStatementInfo {
    tagScope	scope;
    declType	declaration;	/* specifier associated with TOKEN_SPEC */
    boolean	gotName;	/* was a name parsed yet? */
    boolean	haveQualifyingName;  /* do we have a name we are considering? */
    boolean	gotParenName;	/* was a name inside parentheses parsed yet? */
    boolean	gotArgs;	/* was a list of parameters parsed yet? */
    boolean	isPointer;	/* is 'name' a pointer? */
    impType	implementation;	/* abstract or concrete implementation? */
    unsigned int tokenIndex;	/* currently active token */
    tokenInfo*	token [(int) NumTokens];
    tokenInfo*	context;	/* accumulated scope of current statement */
    tokenInfo*	blockName;	/* name of current block */
    memberInfo	member;		/* information regarding parent class/struct */
    vString*	parentClasses;	/* parent classes */
    struct sStatementInfo *parent;  /* statement we are nested within */
} statementInfo;

/*  Describes the type of tag being generated.
 */
typedef enum eTagType {
    TAG_UNDEFINED,
    TAG_CLASS,			/* class name */
    TAG_ENUM,			/* enumeration name */
    TAG_ENUMERATOR,		/* enumerator (enumeration value) */
    TAG_FIELD,			/* field (Java) */
    TAG_FUNCTION,		/* function definition */
    TAG_INTERFACE,		/* interface declaration */
    TAG_MEMBER,			/* structure, class or interface member */
    TAG_METHOD,			/* method declaration */
    TAG_NAMESPACE,		/* namespace name */
    TAG_PACKAGE,		/* package name */
    TAG_PROTOTYPE,		/* function prototype or declaration */
    TAG_STRUCT,			/* structure name */
    TAG_TYPEDEF,		/* typedef name */
    TAG_UNION,			/* union name */
    TAG_VARIABLE,		/* variable definition */
    TAG_EXTERN_VAR,		/* external variable declaration */
    TAG_COUNT			/* must be last */
} tagType;

typedef struct sParenInfo {
    boolean isPointer;
    boolean isParamList;
    boolean isKnrParamList;
    boolean isNameCandidate;
    boolean invalidContents;
    boolean nestedArgs;
    unsigned int parameterCount;
} parenInfo;

/*
*   DATA DEFINITIONS
*/

static jmp_buf Exception;

static langType Lang_c;
static langType Lang_cpp;
static langType Lang_java;

/* Used to index into the CKinds table. */
typedef enum {
    CK_UNDEFINED = -1,
    CK_CLASS, CK_DEFINE, CK_ENUMERATOR, CK_FUNCTION,
    CK_ENUMERATION, CK_MEMBER, CK_NAMESPACE, CK_PROTOTYPE,
    CK_STRUCT, CK_TYPEDEF, CK_UNION, CK_VARIABLE,
    CK_EXTERN_VARIABLE
} cKind;

static kindOption CKinds [] = {
    { TRUE,  'c', "class",      "classes"},
    { TRUE,  'd', "macro",      "macro definitions"},
    { TRUE,  'e', "enumerator", "enumerators (values inside an enumeration)"},
    { TRUE,  'f', "function",   "function definitions"},
    { TRUE,  'g', "enum",       "enumeration names"},
    { TRUE,  'm', "member",     "class, struct, and union members"},
    { TRUE,  'n', "namespace",  "namespaces"},
    { FALSE, 'p', "prototype",  "function prototypes"},
    { TRUE,  's', "struct",     "structure names"},
    { TRUE,  't', "typedef",    "typedefs"},
    { TRUE,  'u', "union",      "union names"},
    { TRUE,  'v', "variable",   "variable definitions"},
    { FALSE, 'x', "externvar",  "external variable declarations"},
};

/* Used to index into the JavaKinds table. */
typedef enum {
    JK_UNDEFINED = -1,
    JK_CLASS, JK_FIELD, JK_INTERFACE, JK_METHOD,
    JK_PACKAGE, JK_ACCESS, JK_CLASS_PREFIX
} javaKind;

static kindOption JavaKinds [] = {
    { TRUE,  'c', "class", "classes"},
    { TRUE,  'f', "field", "fields"},
    { TRUE,  'i', "interface", "interfaces"},
    { TRUE,  'm', "method", "methods"},
    { TRUE,  'p', "package", "packages"},
};

static const keywordDesc KeywordTable [] = {
    /* 						    C++		*/
    /* 					     ANSI C  |  Java	*/
    /* keyword		keyword ID		 \   |   /  	*/
    { "__attribute__",	KEYWORD_ATTRIBUTE,	{ 1, 1, 0 } },
    { "abstract",	KEYWORD_ABSTRACT,	{ 0, 0, 1 } },
    { "boolean",	KEYWORD_BOOLEAN,	{ 0, 0, 1 } },
    { "byte",		KEYWORD_BYTE,		{ 0, 0, 1 } },
    { "catch",		KEYWORD_CATCH,		{ 0, 1, 0 } },
    { "char",		KEYWORD_CHAR,		{ 1, 1, 1 } },
    { "class",		KEYWORD_CLASS,		{ 0, 1, 1 } },
    { "const",		KEYWORD_CONST,		{ 1, 1, 1 } },
    { "double",		KEYWORD_DOUBLE,		{ 1, 1, 1 } },
    { "enum",		KEYWORD_ENUM,		{ 1, 1, 0 } },
    { "explicit",	KEYWORD_EXPLICIT,	{ 0, 1, 0 } },
    { "extends",	KEYWORD_EXTENDS,	{ 0, 0, 1 } },
    { "extern",		KEYWORD_EXTERN,		{ 1, 1, 0 } },
    { "final",		KEYWORD_FINAL,		{ 0, 0, 1 } },
    { "float",		KEYWORD_FLOAT,		{ 1, 1, 1 } },
    { "friend",		KEYWORD_FRIEND,		{ 0, 1, 0 } },
    { "implements",	KEYWORD_IMPLEMENTS,	{ 0, 0, 1 } },
    { "import",		KEYWORD_IMPORT,		{ 0, 0, 1 } },
    { "inline",		KEYWORD_INLINE,		{ 0, 1, 0 } },
    { "int",		KEYWORD_INT,		{ 1, 1, 1 } },
    { "interface",	KEYWORD_INTERFACE,	{ 0, 0, 1 } },
    { "long",		KEYWORD_LONG,		{ 1, 1, 1 } },
    { "mutable",	KEYWORD_MUTABLE,	{ 0, 1, 0 } },
    { "namespace",	KEYWORD_NAMESPACE,	{ 0, 1, 0 } },
    { "native",		KEYWORD_NATIVE,		{ 0, 0, 1 } },
    { "new",		KEYWORD_NEW,		{ 0, 1, 1 } },
    { "operator",	KEYWORD_OPERATOR,	{ 0, 1, 0 } },
    { "overload",	KEYWORD_OVERLOAD,	{ 0, 1, 0 } },
    { "package",	KEYWORD_PACKAGE,	{ 0, 0, 1 } },
    { "private",	KEYWORD_PRIVATE,	{ 0, 1, 1 } },
    { "protected",	KEYWORD_PROTECTED,	{ 0, 1, 1 } },
    { "public",		KEYWORD_PUBLIC,		{ 0, 1, 1 } },
    { "register",	KEYWORD_REGISTER,	{ 1, 1, 0 } },
    { "short",		KEYWORD_SHORT,		{ 1, 1, 1 } },
    { "signed",		KEYWORD_SIGNED,		{ 1, 1, 0 } },
    { "static",		KEYWORD_STATIC,		{ 1, 1, 1 } },
    { "struct",		KEYWORD_STRUCT,		{ 1, 1, 0 } },
    { "synchronized",	KEYWORD_SYNCHRONIZED,	{ 0, 0, 1 } },
    { "template",	KEYWORD_TEMPLATE,	{ 0, 1, 0 } },
    { "throw",		KEYWORD_THROW,		{ 0, 1, 1 } },
    { "throws",		KEYWORD_THROWS,		{ 0, 0, 1 } },
    { "transient",	KEYWORD_TRANSIENT,	{ 0, 0, 1 } },
    { "try",		KEYWORD_TRY,		{ 0, 1, 0 } },
    { "typedef",	KEYWORD_TYPEDEF,	{ 1, 1, 0 } },
    { "typename",	KEYWORD_TYPENAME,	{ 0, 1, 0 } },
    { "union",		KEYWORD_UNION,		{ 1, 1, 0 } },
    { "unsigned",	KEYWORD_UNSIGNED,	{ 1, 1, 0 } },
    { "using",		KEYWORD_USING,		{ 0, 1, 0 } },
    { "virtual",	KEYWORD_VIRTUAL,	{ 0, 1, 0 } },
    { "void",		KEYWORD_VOID,		{ 1, 1, 1 } },
    { "volatile",	KEYWORD_VOLATILE,	{ 1, 1, 1 } },
    { "wchar_t",	KEYWORD_WCHAR_T,	{ 1, 1, 0 } }
};

/*
*   FUNCTION PROTOTYPES
*/
static void createTags (const unsigned int nestLevel, statementInfo *const parent);

/*
*   FUNCTION DEFINITIONS
*/

extern boolean includingDefineTags (void)
{
    return CKinds [CK_DEFINE].enabled;
}

/*
*   Token management
*/

static void initToken (tokenInfo* const token)
{
    token->type		= TOKEN_NONE;
    token->keyword	= KEYWORD_NONE;
    token->lineNumber	= getSourceLineNumber ();
    token->filePosition	= getInputFilePosition ();
    vStringClear (token->name);
}

static void advanceToken (statementInfo* const st)
{
    if (st->tokenIndex >= (unsigned int) NumTokens - 1)
	st->tokenIndex = 0;
    else
	++st->tokenIndex;
    initToken (st->token [st->tokenIndex]);
}

static tokenInfo *prevToken (const statementInfo *const st, unsigned int n)
{
    unsigned int tokenIndex;
    unsigned int num = (unsigned int) NumTokens;
    Assert (n < num);
    tokenIndex = (st->tokenIndex + num - n) % num;
    return st->token [tokenIndex];
}

static void setToken (statementInfo *const st, const tokenType type)
{
    tokenInfo *token;
    token = activeToken (st);
    initToken (token);
    token->type = type;
}

static void retardToken (statementInfo *const st)
{
    if (st->tokenIndex == 0)
	st->tokenIndex = (unsigned int) NumTokens - 1;
    else
	--st->tokenIndex;
    setToken (st, TOKEN_NONE);
}

static tokenInfo *newToken (void)
{
    tokenInfo *const token = xMalloc (1, tokenInfo);
    token->name = vStringNew ();
    initToken (token);
    return token;
}

static void deleteToken (tokenInfo *const token)
{
    if (token != NULL)
    {
	vStringDelete (token->name);
	eFree (token);
    }
}

static const char *accessString (const accessType access)
{
    static const char *const names [] ={
	"?", "private", "protected", "public", "default"
    };
    Assert (sizeof (names) / sizeof (names [0]) == ACCESS_COUNT);
    Assert ((int) access < ACCESS_COUNT);
    return names [(int) access];
}

static const char *implementationString (const impType imp)
{
    static const char *const names [] ={
	"?", "abstract", "virtual", "pure virtual"
    };
    Assert (sizeof (names) / sizeof (names [0]) == IMP_COUNT);
    Assert ((int) imp < IMP_COUNT);
    return names [(int) imp];
}

/*
*   Debugging functions
*/

#ifdef DEBUG

#define boolString(c)   ((c) ? "TRUE" : "FALSE")

static const char *tokenString (const tokenType type)
{
    static const char *const names [] = {
	"none", "args", "}", "{", "comma", "double colon", "keyword", "name",
	"package", "paren-name", "semicolon", "specifier"
    };
    Assert (sizeof (names) / sizeof (names [0]) == TOKEN_COUNT);
    Assert ((int) type < TOKEN_COUNT);
    return names [(int) type];
}

static const char *scopeString (const tagScope scope)
{
    static const char *const names [] = {
	"global", "static", "extern", "friend", "typedef"
    };
    Assert (sizeof (names) / sizeof (names [0]) == SCOPE_COUNT);
    Assert ((int) scope < SCOPE_COUNT);
    return names [(int) scope];
}

static const char *declString (const declType declaration)
{
    static const char *const names [] = {
	"?", "base", "class", "enum", "function", "ignore", "interface",
	"namespace", "no mangle", "package", "struct", "union",
    };
    Assert (sizeof (names) / sizeof (names [0]) == DECL_COUNT);
    Assert ((int) declaration < DECL_COUNT);
    return names [(int) declaration];
}

static const char *keywordString (const keywordId keyword)
{
    const size_t count = sizeof (KeywordTable) / sizeof (KeywordTable [0]);
    const char *name = "none";
    size_t i;
    for (i = 0  ;  i < count  ;  ++i)
    {
	const keywordDesc *p = &KeywordTable [i];

	if (p->id == keyword)
	{
	    name = p->name;
	    break;
	}
    }
    return name;
}

static void __unused__ pt (tokenInfo *const token)
{
    if (isType (token, TOKEN_NAME))
	printf ("type: %-12s: %-13s   line: %lu\n",
	       tokenString (token->type), vStringValue (token->name),
	       token->lineNumber);
    else if (isType (token, TOKEN_KEYWORD))
	printf ("type: %-12s: %-13s   line: %lu\n",
	       tokenString (token->type), keywordString (token->keyword),
	       token->lineNumber);
    else
	printf ("type: %-12s                  line: %lu\n",
	       tokenString (token->type), token->lineNumber);
}

static void __unused__ ps (statementInfo *const st)
{
    unsigned int i;
    printf ("scope: %s   decl: %s   gotName: %s   gotParenName: %s\n",
	   scopeString (st->scope), declString (st->declaration),
	   boolString (st->gotName), boolString (st->gotParenName));
    printf ("haveQualifyingName: %s\n", boolString (st->haveQualifyingName));
    printf ("access: %s   default: %s\n", accessString (st->member.access),
	   accessString (st->member.accessDefault));
    printf ("token  : ");
    pt (activeToken (st));
    for (i = 1  ;  i < (unsigned int) NumTokens  ;  ++i)
    {
	printf ("prev %u : ", i);
	pt (prevToken (st, i));
    }
    printf ("context: ");
    pt (st->context);
}

#endif

/*
*   Statement management
*/

static boolean isContextualKeyword (const tokenInfo *const token)
{
    boolean result;
    switch (token->keyword)
    {
	case KEYWORD_CLASS:
	case KEYWORD_ENUM:
	case KEYWORD_INTERFACE:
	case KEYWORD_NAMESPACE:
	case KEYWORD_STRUCT:
	case KEYWORD_UNION:
	    result = TRUE;
	    break;

	default: result = FALSE; break;
    }
    return result;
}

static boolean isContextualStatement (const statementInfo *const st)
{
    boolean result = FALSE;
    if (st != NULL) switch (st->declaration)
    {
	case DECL_CLASS:
	case DECL_ENUM:
	case DECL_INTERFACE:
	case DECL_NAMESPACE:
	case DECL_STRUCT:
	case DECL_UNION:
	    result = TRUE;
	    break;

	default: result = FALSE; break;
    }
    return result;
}

static boolean isMember (const statementInfo *const st)
{
    boolean result;
    if (isType (st->context, TOKEN_NAME))
	result = TRUE;
    else
	result = isContextualStatement (st->parent);
    return result;
}

static void initMemberInfo (statementInfo *const st)
{
    accessType accessDefault = ACCESS_UNDEFINED;

    if (st->parent != NULL) switch (st->parent->declaration)
    {
	case DECL_ENUM:
	case DECL_NAMESPACE:
	case DECL_UNION:
	    accessDefault = ACCESS_UNDEFINED;
	    break;

	case DECL_CLASS:
	    if (isLanguage (Lang_java))
		accessDefault = ACCESS_DEFAULT;
	    else
		accessDefault = ACCESS_PRIVATE;
	    break;

	case DECL_INTERFACE:
	case DECL_STRUCT:
	    accessDefault = ACCESS_PUBLIC;
	    break;

	default: break;
    }
    st->member.accessDefault = accessDefault;
    st->member.access	     = accessDefault;
}

static void reinitStatement (statementInfo *const st, const boolean partial)
{
    unsigned int i;

    if (! partial)
    {
	st->scope = SCOPE_GLOBAL;
	if (isContextualStatement (st->parent))
	    st->declaration = DECL_BASE;
	else
	    st->declaration = DECL_NONE;
    }
    st->gotParenName	= FALSE;
    st->isPointer	= FALSE;
    st->implementation	= IMP_DEFAULT;
    st->gotArgs		= FALSE;
    st->gotName		= FALSE;
    st->haveQualifyingName = FALSE;
    st->tokenIndex	= 0;

    for (i = 0  ;  i < (unsigned int) NumTokens  ;  ++i)
	initToken (st->token [i]);

    initToken (st->context);
    initToken (st->blockName);
    vStringClear (st->parentClasses);

    /*  Init member info.
     */
    if (! partial)
	st->member.access = st->member.accessDefault;
}

static void initStatement (statementInfo *const st, statementInfo *const parent)
{
    st->parent = parent;
    initMemberInfo (st);
    reinitStatement (st, FALSE);
}

/*
*   Tag generation functions
*/
static cKind cTagKind (const tagType type)
{
    cKind result = CK_UNDEFINED;
    switch (type)
    {
	case TAG_CLASS:      result = CK_CLASS;           break;
	case TAG_ENUM:       result = CK_ENUMERATION;     break;
	case TAG_ENUMERATOR: result = CK_ENUMERATOR;      break;
	case TAG_FUNCTION:   result = CK_FUNCTION;        break;
	case TAG_MEMBER:     result = CK_MEMBER;          break;
	case TAG_NAMESPACE:  result = CK_NAMESPACE;       break;
	case TAG_PROTOTYPE:  result = CK_PROTOTYPE;       break;
	case TAG_STRUCT:     result = CK_STRUCT;          break;
	case TAG_TYPEDEF:    result = CK_TYPEDEF;         break;
	case TAG_UNION:      result = CK_UNION;           break;
	case TAG_VARIABLE:   result = CK_VARIABLE;        break;
	case TAG_EXTERN_VAR: result = CK_EXTERN_VARIABLE; break;

	default: Assert ("Bad C tag type" == NULL); break;
    }
    return result;
}

static javaKind javaTagKind (const tagType type)
{
    javaKind result = JK_UNDEFINED;
    switch (type)
    {
	case TAG_CLASS:     result = JK_CLASS;     break;
	case TAG_FIELD:     result = JK_FIELD;     break;
	case TAG_INTERFACE: result = JK_INTERFACE; break;
	case TAG_METHOD:    result = JK_METHOD;    break;
	case TAG_PACKAGE:   result = JK_PACKAGE;   break;

	default: Assert ("Bad Java tag type" == NULL); break;
    }
    return result;
}

static const char *tagName (const tagType type)
{
    const char* result;
    if (isLanguage (Lang_java))
	result = JavaKinds [javaTagKind (type)].name;
    else
	result = CKinds [cTagKind (type)].name;
    return result;
}

static int tagLetter (const tagType type)
{
    int result;
    if (isLanguage (Lang_java))
	result = JavaKinds [javaTagKind (type)].letter;
    else
	result = CKinds [cTagKind (type)].letter;
    return result;
}

static boolean includeTag (const tagType type, const boolean isFileScope)
{
    boolean result;
    if (isFileScope  &&  ! Option.include.fileScope)
	result = FALSE;
    else if (isLanguage (Lang_java))
	result = JavaKinds [javaTagKind (type)].enabled;
    else
	result = CKinds [cTagKind (type)].enabled;
    return result;
}

static tagType declToTagType (const declType declaration)
{
    tagType type = TAG_UNDEFINED;

    switch (declaration)
    {
	case DECL_CLASS:	type = TAG_CLASS;	break;
	case DECL_ENUM:		type = TAG_ENUM;	break;
	case DECL_FUNCTION:	type = TAG_FUNCTION;	break;
	case DECL_INTERFACE:	type = TAG_INTERFACE;	break;
	case DECL_NAMESPACE:	type = TAG_NAMESPACE;	break;
	case DECL_STRUCT:	type = TAG_STRUCT;	break;
	case DECL_UNION:	type = TAG_UNION;	break;

	default: Assert ("Unexpected declaration" == NULL); break;
    }
    return type;
}

static const char* accessField (const statementInfo *const st)
{
    const char* result = NULL;
    if (isLanguage (Lang_cpp)  &&  st->scope == SCOPE_FRIEND)
	result = "friend";
    else if (st->member.access != ACCESS_UNDEFINED)
	result = accessString (st->member.access);
    return result;
}

static void addOtherFields (tagEntryInfo* const tag, const tagType type,
			    const statementInfo *const st, vString *const scope)
{
    /*  For selected tag types, append an extension flag designating the
     *  parent object in which the tag is defined.
     */
    switch (type)
    {
	default: break;

	case TAG_CLASS:
	case TAG_ENUM:
	case TAG_ENUMERATOR:
	case TAG_FIELD:
	case TAG_FUNCTION:
	case TAG_INTERFACE:
	case TAG_MEMBER:
	case TAG_METHOD:
	case TAG_PROTOTYPE:
	case TAG_STRUCT:
	case TAG_TYPEDEF:
	case TAG_UNION:
	    if (isMember (st) &&
		! (type == TAG_ENUMERATOR  &&  vStringLength (scope) == 0))
	    {
		if (isType (st->context, TOKEN_NAME))
		    tag->extensionFields.scope [0] = tagName (TAG_CLASS);
		else
		    tag->extensionFields.scope [0] =
			tagName (declToTagType (parentDecl (st)));
		tag->extensionFields.scope [1] = vStringValue (scope);
	    }
	    if ((type == TAG_CLASS  ||  type == TAG_INTERFACE  ||
		 type == TAG_STRUCT) && vStringLength (st->parentClasses) > 0)
	    {

		tag->extensionFields.inheritance =
			vStringValue (st->parentClasses);
	    }
	    if (st->implementation != IMP_DEFAULT &&
		(isLanguage (Lang_cpp) || isLanguage (Lang_java)))
	    {
		tag->extensionFields.implementation =
			implementationString (st->implementation);
	    }
	    if (isMember (st))
	    {
		tag->extensionFields.access = accessField (st);
	    }
	    break;
    }
}

static void addContextSeparator (vString *const scope)
{
    if (isLanguage (Lang_c)  ||  isLanguage (Lang_cpp))
	vStringCatS (scope, "::");
    else if (isLanguage (Lang_java))
	vStringCatS (scope, ".");
}

static void findScopeHierarchy (vString *const string,
				const statementInfo *const st)
{
    const char* const anon = "<anonymous>";
    boolean nonAnonPresent = FALSE;

    vStringClear (string);
    if (isType (st->context, TOKEN_NAME))
    {
	vStringCopy (string, st->context->name);
	nonAnonPresent = TRUE;
    }
    if (st->parent != NULL)
    {
	vString *temp = vStringNew ();
	const statementInfo *s;

	for (s = st->parent  ;  s != NULL  ;  s = s->parent)
	{
	    if (isContextualStatement (s))
	    {
		vStringCopy (temp, string);
		vStringClear (string);
		if (isType (s->blockName, TOKEN_NAME))
		{
		    if (isType (s->context, TOKEN_NAME) &&
			vStringLength (s->context->name) > 0)
		    {
			vStringCat (string, s->context->name);
			addContextSeparator (string);
		    }
		    vStringCat (string, s->blockName->name);
		    nonAnonPresent = TRUE;
		}
		else
		    vStringCopyS (string, anon);
		if (vStringLength (temp) > 0)
		    addContextSeparator (string);
		vStringCat (string, temp);
	    }
	}
	vStringDelete (temp);

	if (! nonAnonPresent)
	    vStringClear (string);
    }
}

static void makeExtraTagEntry (const tagType type, tagEntryInfo *const e,
			       vString *const scope)
{
    if (Option.include.qualifiedTags  &&
	scope != NULL  &&  vStringLength (scope) > 0)
    {
	vString *const scopedName = vStringNew ();

	if (type != TAG_ENUMERATOR)
	    vStringCopy (scopedName, scope);
	else
	{
	    /* remove last component (i.e. enumeration name) from scope */
	    const char* const sc = vStringValue (scope);
	    const char* colon = strrchr (sc, ':');
	    if (colon != NULL)
	    {
		while (*colon == ':'  &&  colon > sc)
		    --colon;
		vStringNCopy (scopedName, scope, colon + 1 - sc);
	    }
	}
	if (vStringLength (scopedName) > 0)
	{
	    addContextSeparator (scopedName);
	    vStringCatS (scopedName, e->name);
	    e->name = vStringValue (scopedName);
	    makeTagEntry (e);
	}
	vStringDelete (scopedName);
    }
}

static void makeTag (const tokenInfo *const token,
		     const statementInfo *const st,
		     boolean isFileScope, const tagType type)
{
    /*  Nothing is really of file scope when it appears in a header file.
     */
    isFileScope = (boolean) (isFileScope && ! isHeaderFile ());

    if (isType (token, TOKEN_NAME)  &&  vStringLength (token->name) > 0  &&
	includeTag (type, isFileScope))
    {
	vString *scope = vStringNew ();
	tagEntryInfo e;

	initTagEntry (&e, vStringValue (token->name));

	e.lineNumber	= token->lineNumber;
	e.filePosition	= token->filePosition;
	e.isFileScope	= isFileScope;
	e.kindName	= tagName (type);
	e.kind		= tagLetter (type);

	findScopeHierarchy (scope, st);
	addOtherFields (&e, type, st, scope);

	makeTagEntry (&e);
	makeExtraTagEntry (type, &e, scope);
	vStringDelete (scope);
    }
}

static boolean isValidTypeSpecifier (const declType declaration)
{
    boolean result;
    switch (declaration)
    {
	case DECL_BASE:
	case DECL_CLASS:
	case DECL_ENUM:
	case DECL_STRUCT:
	case DECL_UNION:
	    result = TRUE;
	    break;

	default:
	    result = FALSE;
	    break;
    }
    return result;
}

static void qualifyEnumeratorTag (const statementInfo *const st,
				  const tokenInfo *const nameToken)
{
    if (isType (nameToken, TOKEN_NAME))
	makeTag (nameToken, st, TRUE, TAG_ENUMERATOR);
}

static void qualifyFunctionTag (const statementInfo *const st,
				const tokenInfo *const nameToken)
{
    if (isType (nameToken, TOKEN_NAME))
    {
	const tagType type = isLanguage (Lang_java) ? TAG_METHOD : TAG_FUNCTION;
	const boolean isFileScope =
			(boolean) (st->member.access == ACCESS_PRIVATE ||
			(!isMember (st)  &&  st->scope == SCOPE_STATIC));

	makeTag (nameToken, st, isFileScope, type);
    }
}

static void qualifyFunctionDeclTag (const statementInfo *const st,
				    const tokenInfo *const nameToken)
{
    if (! isType (nameToken, TOKEN_NAME))
	;
    else if (isLanguage (Lang_java))
	qualifyFunctionTag (st, nameToken);
    else if (st->scope == SCOPE_TYPEDEF)
	makeTag (nameToken, st, TRUE, TAG_TYPEDEF);
    else if (isValidTypeSpecifier (st->declaration))
	makeTag (nameToken, st, TRUE, TAG_PROTOTYPE);
}

static void qualifyCompoundTag (const statementInfo *const st,
				const tokenInfo *const nameToken)
{
    if (isType (nameToken, TOKEN_NAME))
    {
	const tagType type = declToTagType (st->declaration);

	if (type != TAG_UNDEFINED)
	    makeTag (nameToken, st, (boolean) (! isLanguage (Lang_java)), type);
    }
}

static void qualifyBlockTag (statementInfo *const st,
			     const tokenInfo *const nameToken)
{
    switch (st->declaration)
    {
	case DECL_CLASS:
	case DECL_ENUM:
	case DECL_INTERFACE:
	case DECL_NAMESPACE:
	case DECL_STRUCT:
	case DECL_UNION:
	    qualifyCompoundTag (st, nameToken);
	    break;
	default: break;
    }
}

static void qualifyVariableTag (const statementInfo *const st,
				const tokenInfo *const nameToken)
{
    /*	We have to watch that we do not interpret a declaration of the
     *	form "struct tag;" as a variable definition. In such a case, the
     *	token preceding the name will be a keyword.
     */
    if (! isType (nameToken, TOKEN_NAME))
	;
    else if (st->declaration == DECL_IGNORE)
	;
    else if (st->scope == SCOPE_TYPEDEF)
	makeTag (nameToken, st, TRUE, TAG_TYPEDEF);
    else if (st->declaration == DECL_PACKAGE)
	makeTag (nameToken, st, FALSE, TAG_PACKAGE);
    else if (isValidTypeSpecifier (st->declaration))
    {
	if (isMember (st))
	{
	    if (isLanguage (Lang_java))
		makeTag (nameToken, st,
			(boolean) (st->member.access == ACCESS_PRIVATE),
			TAG_FIELD);
	    else if (st->scope == SCOPE_GLOBAL  ||  st->scope == SCOPE_STATIC)
		makeTag (nameToken, st, TRUE, TAG_MEMBER);
	}
	else
	{
	    if (st->scope == SCOPE_EXTERN  ||  ! st->haveQualifyingName)
		makeTag (nameToken, st, FALSE, TAG_EXTERN_VAR);
	    else
		makeTag (nameToken, st, (boolean) (st->scope == SCOPE_STATIC),
			TAG_VARIABLE);
	}
    }
}

/*
*   Parsing functions
*/

/*  Skip to the next non-white character.
 */
static int skipToNonWhite (void)
{
    int c;

    do
	c = cppGetc ();
    while (isspace (c));

    return c;
}

/*  Skips to the next brace in column 1. This is intended for cases where
 *  preprocessor constructs result in unbalanced braces.
 */
static void skipToFormattedBraceMatch (void)
{
    int c, next;

    c = cppGetc ();
    next = cppGetc ();
    while (c != EOF  &&  (c != '\n'  ||  next != '}'))
    {
	c = next;
	next = cppGetc ();
    }
}

/*  Skip to the matching character indicated by the pair string. If skipping
 *  to a matching brace and any brace is found within a different level of a
 *  #if conditional statement while brace formatting is in effect, we skip to
 *  the brace matched by its formatting. It is assumed that we have already
 *  read the character which starts the group (i.e. the first character of
 *  "pair").
 */
static void skipToMatch (const char *const pair)
{
    const boolean braceMatching = (boolean) (strcmp ("{}", pair) == 0);
    const boolean braceFormatting = (boolean) (isBraceFormat () && braceMatching);
    const unsigned int initialLevel = getDirectiveNestLevel ();
    const int begin = pair [0], end = pair [1];
    const unsigned long inputLineNumber = getInputLineNumber ();
    int matchLevel = 1;
    int c = '\0';

    while (matchLevel > 0  &&  (c = cppGetc ()) != EOF)
    {
	if (c == begin)
	{
	    ++matchLevel;
	    if (braceFormatting  &&  getDirectiveNestLevel () != initialLevel)
	    {
		skipToFormattedBraceMatch ();
		break;
	    }
	}
	else if (c == end)
	{
	    --matchLevel;
	    if (braceFormatting  &&  getDirectiveNestLevel () != initialLevel)
	    {
		skipToFormattedBraceMatch ();
		break;
	    }
	}
    }
    if (c == EOF)
    {
	verbose ("%s: failed to find match for '%c' at line %lu\n",
		getInputFileName (), begin, inputLineNumber);
	if (braceMatching)
	    longjmp (Exception, (int) ExceptionBraceFormattingError);
	else
	    longjmp (Exception, (int) ExceptionFormattingError);
    }
}

static void skipParens (void)
{
    const int c = skipToNonWhite ();

    if (c == '(')
	skipToMatch ("()");
    else
	cppUngetc (c);
}

static void skipBraces (void)
{
    const int c = skipToNonWhite ();

    if (c == '{')
	skipToMatch ("{}");
    else
	cppUngetc (c);
}

static keywordId analyzeKeyword (const char *const name)
{
    const keywordId id = (keywordId) lookupKeyword (name, getSourceLanguage ());
    return id;
}

static void analyzeIdentifier (tokenInfo *const token)
{
    char *const name = vStringValue (token->name);
    const char *replacement = NULL;
    boolean parensToo = FALSE;

    if (isLanguage (Lang_java)  ||
	! isIgnoreToken (name, &parensToo, &replacement))
    {
	if (replacement != NULL)
	    token->keyword = analyzeKeyword (replacement);
	else
	    token->keyword = analyzeKeyword (vStringValue (token->name));

	if (token->keyword == KEYWORD_NONE)
	    token->type = TOKEN_NAME;
	else
	    token->type = TOKEN_KEYWORD;
    }
    else
    {
	initToken (token);
	if (parensToo)
	{
	    int c = skipToNonWhite ();

	    if (c == '(')
		skipToMatch ("()");
	}
    }
}

static void readIdentifier (tokenInfo *const token, const int firstChar)
{
    vString *const name = token->name;
    int c = firstChar;

    initToken (token);

    do
    {
	vStringPut (name, c);
	c = cppGetc ();
    } while (isident (c));
    vStringTerminate (name);
    cppUngetc (c);		/* unget non-identifier character */

    analyzeIdentifier (token);
}

static void readPackageName (tokenInfo *const token, const int firstChar)
{
    vString *const name = token->name;
    int c = firstChar;

    initToken (token);

    while (isident (c)  ||  c == '.')
    {
	vStringPut (name, c);
	c = cppGetc ();
    }
    vStringTerminate (name);
    cppUngetc (c);		/* unget non-package character */
}

static void readPackage (statementInfo *const st)
{
    tokenInfo *const token = activeToken (st);
    Assert (isType (token, TOKEN_KEYWORD));
    readPackageName (token, skipToNonWhite ());
    token->type = TOKEN_NAME;
    st->declaration = DECL_PACKAGE;
    st->gotName = TRUE;
    st->haveQualifyingName = TRUE;
}

static void processName (statementInfo *const st)
{
    Assert (isType (activeToken (st), TOKEN_NAME));
    if (st->gotName  &&  st->declaration == DECL_NONE)
	st->declaration = DECL_BASE;
    st->gotName = TRUE;
    st->haveQualifyingName = TRUE;
}

static void readOperator (statementInfo *const st)
{
    const char *const acceptable = "+-*/%^&|~!=<>,[]";
    const tokenInfo* const prev = prevToken (st,1);
    tokenInfo *const token = activeToken (st);
    vString *const name = token->name;
    int c = skipToNonWhite ();

    /*  When we arrive here, we have the keyword "operator" in 'name'.
     */
    if (isType (prev, TOKEN_KEYWORD) && (prev->keyword == KEYWORD_ENUM ||
	 prev->keyword == KEYWORD_STRUCT || prev->keyword == KEYWORD_UNION))
	;	/* ignore "operator" keyword if preceded by these keywords */
    else if (c == '(')
    {
	/*  Verify whether this is a valid function call (i.e. "()") operator.
	 */
	if (cppGetc () == ')')
	{
	    vStringPut (name, ' ');  /* always separate operator from keyword */
	    c = skipToNonWhite ();
	    if (c == '(')
		vStringCatS (name, "()");
	}
	else
	{
	    skipToMatch ("()");
	    c = cppGetc ();
	}
    }
    else if (isident1 (c))
    {
	/*  Handle "new" and "delete" operators, and conversion functions
	 *  (per 13.3.1.1.2 [2] of the C++ spec).
	 */
	boolean whiteSpace = TRUE;	/* default causes insertion of space */
	do
	{
	    if (isspace (c))
		whiteSpace = TRUE;
	    else
	    {
		if (whiteSpace)
		{
		    vStringPut (name, ' ');
		    whiteSpace = FALSE;
		}
		vStringPut (name, c);
	    }
	    c = cppGetc ();
	} while (! isOneOf (c, "(;")  &&  c != EOF);
	vStringTerminate (name);
    }
    else if (isOneOf (c, acceptable))
    {
	vStringPut (name, ' ');	/* always separate operator from keyword */
	do
	{
	    vStringPut (name, c);
	    c = cppGetc ();
	} while (isOneOf (c, acceptable));
	vStringTerminate (name);
    }

    cppUngetc (c);

    token->type	= TOKEN_NAME;
    token->keyword = KEYWORD_NONE;
    processName (st);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src)
{
    dest->type         = src->type;
    dest->keyword      = src->keyword;
    dest->filePosition = src->filePosition;
    dest->lineNumber   = src->lineNumber;
    vStringCopy (dest->name, src->name);
}

static void setAccess (statementInfo *const st, const accessType access)
{
    if (isMember (st))
    {
	if (isLanguage (Lang_cpp))
	{
	    int c = skipToNonWhite ();

	    if (c == ':')
		reinitStatement (st, FALSE);
	    else
		cppUngetc (c);

	    st->member.accessDefault = access;
	}
	st->member.access = access;
    }
}

static void discardTypeList (tokenInfo *const token)
{
    int c = skipToNonWhite ();
    while (isident1 (c))
    {
	readIdentifier (token, c);
	c = skipToNonWhite ();
	if (c == '.'  ||  c == ',')
	    c = skipToNonWhite ();
    }
    cppUngetc (c);
}

static void addParentClass (statementInfo *const st, tokenInfo *const token)
{
    if (vStringLength (token->name) > 0  &&
	vStringLength (st->parentClasses) > 0)
    {
	vStringPut (st->parentClasses, ',');
    }
    vStringCat (st->parentClasses, token->name);
}

static void readParents (statementInfo *const st, const int qualifier)
{
    tokenInfo *const token = newToken ();
    tokenInfo *const parent = newToken ();
    int c;

    do
    {
	c = skipToNonWhite ();
	if (isident1 (c))
	{
	    readIdentifier (token, c);
	    if (isType (token, TOKEN_NAME))
		vStringCat (parent->name, token->name);
	    else
	    {
		addParentClass (st, parent);
		initToken (parent);
	    }
	}
	else if (c == qualifier)
	    vStringPut (parent->name, c);
	else if (c == '<')
	    skipToMatch ("<>");
	else if (isType (token, TOKEN_NAME))
	{
	    addParentClass (st, parent);
	    initToken (parent);
	}
    } while (c != '{'  &&  c != EOF);
    cppUngetc (c);
    deleteToken (parent);
    deleteToken (token);
}

static void processToken (tokenInfo *const token, statementInfo *const st)
{
    switch (token->keyword)		/* is it a reserved word? */
    {
	default: break;

	case KEYWORD_NONE:	processName (st);			break;
	case KEYWORD_ABSTRACT:	st->implementation = IMP_ABSTRACT;	break;
	case KEYWORD_ATTRIBUTE:	skipParens (); initToken (token);	break;
	case KEYWORD_CATCH:	skipParens (); skipBraces ();		break;
	case KEYWORD_CHAR:	st->declaration = DECL_BASE;		break;
	case KEYWORD_CLASS:	st->declaration = DECL_CLASS;		break;
	case KEYWORD_CONST:	st->declaration = DECL_BASE;		break;
	case KEYWORD_DOUBLE:	st->declaration = DECL_BASE;		break;
	case KEYWORD_ENUM:	st->declaration = DECL_ENUM;		break;
	case KEYWORD_EXTENDS:	readParents (st, '.');
				setToken (st, TOKEN_NONE);		break;
	case KEYWORD_FLOAT:	st->declaration = DECL_BASE;		break;
	case KEYWORD_FRIEND:	st->scope	= SCOPE_FRIEND;		break;
	case KEYWORD_IMPLEMENTS:readParents (st, '.');
				setToken (st, TOKEN_NONE);		break;
	case KEYWORD_IMPORT:	st->declaration = DECL_IGNORE;		break;
	case KEYWORD_INT:	st->declaration = DECL_BASE;		break;
	case KEYWORD_INTERFACE: st->declaration = DECL_INTERFACE;	break;
	case KEYWORD_LONG:	st->declaration = DECL_BASE;		break;
	case KEYWORD_NAMESPACE: st->declaration = DECL_NAMESPACE;	break;
	case KEYWORD_OPERATOR:	readOperator (st);			break;
	case KEYWORD_PACKAGE:	readPackage (st);			break;
	case KEYWORD_PRIVATE:	setAccess (st, ACCESS_PRIVATE);		break;
	case KEYWORD_PROTECTED:	setAccess (st, ACCESS_PROTECTED);	break;
	case KEYWORD_PUBLIC:	setAccess (st, ACCESS_PUBLIC);		break;
	case KEYWORD_SHORT:	st->declaration = DECL_BASE;		break;
	case KEYWORD_SIGNED:	st->declaration = DECL_BASE;		break;
	case KEYWORD_STRUCT:	st->declaration = DECL_STRUCT;		break;
	case KEYWORD_THROWS:	discardTypeList (token);		break;
	case KEYWORD_UNION:	st->declaration = DECL_UNION;		break;
	case KEYWORD_UNSIGNED:	st->declaration = DECL_BASE;		break;
	case KEYWORD_USING:	st->declaration = DECL_IGNORE;		break;
	case KEYWORD_VOID:	st->declaration = DECL_BASE;		break;
	case KEYWORD_VOLATILE:	st->declaration = DECL_BASE;		break;
	case KEYWORD_VIRTUAL:	st->implementation = IMP_VIRTUAL;	break;

	case KEYWORD_TYPEDEF:
	    reinitStatement (st, FALSE);
	    st->scope = SCOPE_TYPEDEF;
	    break;

	case KEYWORD_EXTERN:
	    reinitStatement (st, FALSE);
	    st->scope = SCOPE_EXTERN;
	    st->declaration = DECL_BASE;
	    break;

	case KEYWORD_STATIC:
	    reinitStatement (st, FALSE);
	    if (! isLanguage (Lang_java))
		st->scope = SCOPE_STATIC;
	    st->declaration = DECL_BASE;
	    break;
    }
}

/*
*   Parenthesis handling functions
*/

static void restartStatement (statementInfo *const st)
{
    tokenInfo *const save = newToken ();
    tokenInfo *token = activeToken (st);

    copyToken (save, token);
    DebugStatement ( if (debug (DEBUG_PARSE)) printf ("<ES>");)
    reinitStatement (st, FALSE);
    token = activeToken (st);
    copyToken (token, save);
    deleteToken (save);
    processToken (token, st);
}

/*  Skips over a the mem-initializer-list of a ctor-initializer, defined as:
 *
 *  mem-initializer-list:
 *    mem-initializer, mem-initializer-list
 *
 *  mem-initializer:
 *    [::] [nested-name-spec] class-name (...)
 *    identifier
 */
static void skipMemIntializerList (tokenInfo *const token)
{
    int c;

    do
    {
	c = skipToNonWhite ();
	while (isident1 (c)  ||  c == ':')
	{
	    if (c != ':')
		readIdentifier (token, c);
	    c = skipToNonWhite ();
	}
	if (c == '<')
	{
	    skipToMatch ("<>");
	    c = skipToNonWhite ();
	}
	if (c == '(')
	{
	    skipToMatch ("()");
	    c = skipToNonWhite ();
	}
    } while (c == ',');
    cppUngetc (c);
}

static void skipMacro (statementInfo *const st)
{
    tokenInfo *const prev2 = prevToken (st, 2);

    if (isType (prev2, TOKEN_NAME))
	retardToken (st);
    skipToMatch ("()");
}

/*  Skips over characters following the parameter list. This will be either
 *  non-ANSI style function declarations or C++ stuff. Our choices:
 *
 *  C (K&R):
 *    int func ();
 *    int func (one, two) int one; float two; {...}
 *  C (ANSI):
 *    int func (int one, float two);
 *    int func (int one, float two) {...}
 *  C++:
 *    int foo (...) [const|volatile] [throw (...)];
 *    int foo (...) [const|volatile] [throw (...)] [ctor-initializer] {...}
 *    int foo (...) [const|volatile] [throw (...)] try [ctor-initializer] {...}
 *        catch (...) {...}
 */
static boolean skipPostArgumentStuff (statementInfo *const st,
				      parenInfo *const info)
{
    tokenInfo *const token = activeToken (st);
    unsigned int parameters = info->parameterCount;
    unsigned int elementCount = 0;
    boolean restart = FALSE;
    boolean end = FALSE;
    int c = skipToNonWhite ();

    do
    {
	switch (c)
	{
	case ')':				break;
	case ':': skipMemIntializerList (token);break;	/* ctor-initializer */
	case '[': skipToMatch ("[]");		break;
	case '=': cppUngetc (c); end = TRUE;	break;
	case '{': cppUngetc (c); end = TRUE;	break;
	case '}': cppUngetc (c); end = TRUE;	break;

	case '(':
	    if (elementCount > 0)
		++elementCount;
	    skipToMatch ("()");
	    break;

	case ';':
	    if (parameters == 0  ||  elementCount < 2)
	    {
		cppUngetc (c);
		end = TRUE;
	    }
	    else if (--parameters == 0)
		end = TRUE;
	    break;

	default:
	    if (isident1 (c))
	    {
		readIdentifier (token, c);
		switch (token->keyword)
		{
		case KEYWORD_ATTRIBUTE:	skipParens ();	break;
		case KEYWORD_THROW:	skipParens ();	break;
		case KEYWORD_CONST:			break;
		case KEYWORD_TRY:			break;
		case KEYWORD_VOLATILE:			break;

		case KEYWORD_CATCH:	case KEYWORD_CLASS:
		case KEYWORD_EXPLICIT:	case KEYWORD_EXTERN:
		case KEYWORD_FRIEND:	case KEYWORD_INLINE:
		case KEYWORD_MUTABLE:	case KEYWORD_NAMESPACE:
		case KEYWORD_NEW:	case KEYWORD_OPERATOR:
		case KEYWORD_OVERLOAD:	case KEYWORD_PRIVATE:
		case KEYWORD_PROTECTED:	case KEYWORD_PUBLIC:
		case KEYWORD_STATIC:	case KEYWORD_TEMPLATE:
		case KEYWORD_TYPEDEF:	case KEYWORD_TYPENAME:
		case KEYWORD_USING:	case KEYWORD_VIRTUAL:
		    /*  Never allowed within parameter declarations.
		     */
		    restart = TRUE;
		    end = TRUE;
		    break;

		default:
		    if (isType (token, TOKEN_NONE))
			;
		    else if (info->isKnrParamList  &&  info->parameterCount > 0)
			++elementCount;
		    else
		    {
			/*  If we encounter any other identifier immediately
			 *  following an empty parameter list, this is almost
			 *  certainly one of those Microsoft macro "thingies"
			 *  that the automatic source code generation sticks
			 *  in. Terminate the current statement.
			 */
			restart = TRUE;
			end = TRUE;
		    }
		    break;
		}
	    }
	}
	if (! end)
	{
	    c = skipToNonWhite ();
	    if (c == EOF)
		end = TRUE;
	}
    } while (! end);

    if (restart)
	restartStatement (st);
    else
	setToken (st, TOKEN_NONE);

    return (boolean) (c != EOF);
}

static void skipJavaThrows (statementInfo *const st)
{
    tokenInfo *const token = activeToken (st);
    int c = skipToNonWhite ();

    if (isident1 (c))
    {
	readIdentifier (token, c);
	if (token->keyword == KEYWORD_THROWS)
	{
	    do
	    {
		c = skipToNonWhite ();
		if (isident1 (c))
		{
		    readIdentifier (token, c);
		    c = skipToNonWhite ();
		}
	    } while (c == '.'  ||  c == ',');
	}
    }
    cppUngetc (c);
    setToken (st, TOKEN_NONE);
}

static void analyzePostParens (statementInfo *const st, parenInfo *const info)
{
    const unsigned long inputLineNumber = getInputLineNumber ();
    int c = skipToNonWhite ();

    cppUngetc (c);
    if (isOneOf (c, "{;,="))
	;
    else if (isLanguage (Lang_java))
	skipJavaThrows (st);
    else
    {
	if (! skipPostArgumentStuff (st, info))
	{
	    verbose (
		"%s: confusing argument declarations beginning at line %lu\n",
		getInputFileName (), inputLineNumber);
	    longjmp (Exception, (int) ExceptionFormattingError);
	}
    }
}

static int parseParens (statementInfo *const st, parenInfo *const info)
{
    tokenInfo *const token = activeToken (st);
    unsigned int identifierCount = 0;
    unsigned int depth = 1;
    boolean firstChar = TRUE;
    int nextChar = '\0';

    info->parameterCount = 1;
    do
    {
	int c = skipToNonWhite ();

	switch (c)
	{
	    case '&':
	    case '*':
		info->isPointer = TRUE;
		info->isKnrParamList = FALSE;
		if (identifierCount == 0)
		    info->isParamList = FALSE;
		initToken (token);
		break;

	    case ':':
		info->isKnrParamList = FALSE;
		break;

	    case '.':
		info->isNameCandidate = FALSE;
		info->isKnrParamList = FALSE;
		break;

	    case ',':
		info->isNameCandidate = FALSE;
		if (info->isKnrParamList)
		{
		    ++info->parameterCount;
		    identifierCount = 0;
		}
		break;

	    case '=':
		info->isKnrParamList = FALSE;
		info->isNameCandidate = FALSE;
		if (firstChar)
		{
		    info->isParamList = FALSE;
		    skipMacro (st);
		    depth = 0;
		}
		break;

	    case '[':
		info->isKnrParamList = FALSE;
		skipToMatch ("[]");
		break;

	    case '<':
		info->isKnrParamList = FALSE;
		skipToMatch ("<>");
		break;

	    case ')':
		if (firstChar)
		    info->parameterCount = 0;
		--depth;
		break;

	    case '(':
		info->isKnrParamList = FALSE;
		if (firstChar)
		{
		    info->isNameCandidate = FALSE;
		    cppUngetc (c);
		    skipMacro (st);
		    depth = 0;
		}
		else if (isType (token, TOKEN_PAREN_NAME))
		{
		    c = skipToNonWhite ();
		    if (c == '*')	/* check for function pointer */
		    {
			skipToMatch ("()");
			c = skipToNonWhite ();
			if (c == '(')
			    skipToMatch ("()");
		    }
		    else
		    {
			cppUngetc (c);
			cppUngetc ('(');
			info->nestedArgs = TRUE;
		    }
		}
		else
		    ++depth;
		break;

	    default:
		if (isident1 (c))
		{
		    if (++identifierCount > 1)
			info->isKnrParamList = FALSE;
		    readIdentifier (token, c);
		    if (isType (token, TOKEN_NAME)  &&  info->isNameCandidate)
			token->type = TOKEN_PAREN_NAME;
		    else if (isType (token, TOKEN_KEYWORD))
		    {
			if (token->keyword != KEYWORD_CONST &&
			    token->keyword != KEYWORD_VOLATILE)
			{
			    info->isKnrParamList = FALSE;
			    info->isNameCandidate = FALSE;
			}
		    }
		}
		else
		{
		    info->isParamList     = FALSE;
		    info->isKnrParamList  = FALSE;
		    info->isNameCandidate = FALSE;
		    info->invalidContents = TRUE;
		}
		break;
	}
	firstChar = FALSE;
    } while (! info->nestedArgs  &&  depth > 0  &&
	     (info->isKnrParamList  ||  info->isNameCandidate));

    if (! info->nestedArgs) while (depth > 0)
    {
	skipToMatch ("()");
	--depth;
    }

    if (! info->isNameCandidate)
	initToken (token);

    return nextChar;
}

static void initParenInfo (parenInfo *const info)
{
    info->isPointer		= FALSE;
    info->isParamList		= TRUE;
    info->isKnrParamList	= TRUE;
    info->isNameCandidate	= TRUE;
    info->invalidContents	= FALSE;
    info->nestedArgs		= FALSE;
    info->parameterCount	= 0;
}

static void analyzeParens (statementInfo *const st)
{
    tokenInfo *const prev = prevToken (st, 1);

    if (! isType (prev, TOKEN_NONE))    /* in case of ignored enclosing macros */
    {
	tokenInfo *const token = activeToken (st);
	parenInfo info;
	int c;

	initParenInfo (&info);
	parseParens (st, &info);
	c = skipToNonWhite ();
	cppUngetc (c);
	if (info.invalidContents)
	    reinitStatement (st, FALSE);
	else if (info.isNameCandidate  &&  isType (token, TOKEN_PAREN_NAME)  &&
		 ! st->gotParenName  &&
		 (! info.isParamList || ! st->haveQualifyingName  ||
		  c == '('  ||
		  (c == '='  &&  st->implementation != IMP_VIRTUAL) ||
		  (st->declaration == DECL_NONE  &&  isOneOf (c, ",;"))))
	{
	    token->type = TOKEN_NAME;
	    processName (st);
	    st->gotParenName = TRUE;
	    if (! (c == '('  &&  info.nestedArgs))
		st->isPointer = info.isPointer;
	}
	else if (! st->gotArgs  &&  info.isParamList)
	{
	    st->gotArgs = TRUE;
	    setToken (st, TOKEN_ARGS);
	    advanceToken (st);
	    analyzePostParens (st, &info);
	}
	else
	    setToken (st, TOKEN_NONE);
    }
}

/*
*   Token parsing functions
*/

static void addContext (statementInfo *const st, const tokenInfo* const token)
{
    if (isType (token, TOKEN_NAME))
    {
	if (vStringLength (st->context->name) > 0)
	{
	    if (isLanguage (Lang_c)  ||  isLanguage (Lang_cpp))
		vStringCatS (st->context->name, "::");
	    else if (isLanguage (Lang_java))
		vStringCatS (st->context->name, ".");
	}
	vStringCat (st->context->name, token->name);
	st->context->type = TOKEN_NAME;
    }
}

static void processColon (statementInfo *const st)
{
    const int c = skipToNonWhite ();
    const boolean doubleColon = (boolean) (c == ':');

    if (doubleColon)
    {
	setToken (st, TOKEN_DOUBLE_COLON);
	st->haveQualifyingName = FALSE;
    }
    else
    {
	cppUngetc (c);
	if (isLanguage (Lang_cpp)  && (
	    st->declaration == DECL_CLASS  ||  st->declaration == DECL_STRUCT))
	{
	    readParents (st, ':');
	}
    }
}

/*  Skips over any initializing value which may follow an '=' character in a
 *  variable definition.
 */
static int skipInitializer (statementInfo *const st)
{
    boolean done = FALSE;
    int c;

    while (! done)
    {
	c = skipToNonWhite ();

	if (c == EOF)
	    longjmp (Exception, (int) ExceptionFormattingError);
	else switch (c)
	{
	    case ',':
	    case ';': done = TRUE; break;

	    case '0':
		if (st->implementation == IMP_VIRTUAL)
		    st->implementation = IMP_PURE_VIRTUAL;
		break;

	    case '[': skipToMatch ("[]"); break;
	    case '(': skipToMatch ("()"); break;
	    case '{': skipToMatch ("{}"); break;

	    case '}':
		if (insideEnumBody (st))
		    done = TRUE;
		else if (! isBraceFormat ())
		{
		    verbose ("%s: unexpected closing brace at line %lu\n",
			    getInputFileName (), getInputLineNumber ());
		    longjmp (Exception, (int) ExceptionBraceFormattingError);
		}
		break;

	    default: break;
	}
    }
    return c;
}

static void processInitializer (statementInfo *const st)
{
    const boolean inEnumBody = insideEnumBody (st);
    const int c = skipInitializer (st);

    if (c == ';')
	setToken (st, TOKEN_SEMICOLON);
    else if (c == ',')
	setToken (st, TOKEN_COMMA);
    else if ('}'  &&  inEnumBody)
    {
	cppUngetc (c);
	setToken (st, TOKEN_COMMA);
    }
    if (st->scope == SCOPE_EXTERN)
	st->scope = SCOPE_GLOBAL;
}

static void parseIdentifier (statementInfo *const st, const int c)
{
    tokenInfo *const token = activeToken (st);

    readIdentifier (token, c);
    if (! isType (token, TOKEN_NONE))
	processToken (token, st);
}

static void parseGeneralToken (statementInfo *const st, const int c)
{
    const tokenInfo *const prev = prevToken (st, 1);

    if (isident1 (c))
    {
	parseIdentifier (st, c);
	if (isType (st->context, TOKEN_NAME) &&
	    isType (activeToken (st), TOKEN_NAME) && isType (prev, TOKEN_NAME))
	{
	    initToken (st->context);
	}
    }
    else if (isExternCDecl (st, c))
    {
	st->declaration = DECL_NOMANGLE;
	st->scope = SCOPE_GLOBAL;
    }
}

/*  Reads characters from the pre-processor and assembles tokens, setting
 *  the current statement state.
 */
static void nextToken (statementInfo *const st)
{
    tokenInfo *token;
    do
    {
	int c = skipToNonWhite ();
	switch (c)
	{
	    case EOF: longjmp (Exception, (int) ExceptionEOF);	break;
	    case '(': analyzeParens (st);			break;
	    case '*': st->haveQualifyingName = FALSE;		break;
	    case ',': setToken (st, TOKEN_COMMA);		break;
	    case ':': processColon (st);			break;
	    case ';': setToken (st, TOKEN_SEMICOLON);		break;
	    case '<': skipToMatch ("<>");			break;
	    case '=': processInitializer (st);			break;
	    case '[': skipToMatch ("[]");			break;
	    case '{': setToken (st, TOKEN_BRACE_OPEN);		break;
	    case '}': setToken (st, TOKEN_BRACE_CLOSE);		break;
	    default:  parseGeneralToken (st, c);		break;
	}
	token = activeToken (st);
    } while (isType (token, TOKEN_NONE));
}

/*
*   Scanning support functions
*/

static statementInfo *CurrentStatement = NULL;

static statementInfo *newStatement (statementInfo *const parent)
{
    statementInfo *const st = xMalloc (1, statementInfo);
    unsigned int i;

    for (i = 0  ;  i < (unsigned int) NumTokens  ;  ++i)
	st->token [i] = newToken ();

    st->context = newToken ();
    st->blockName = newToken ();
    st->parentClasses = vStringNew ();

    initStatement (st, parent);
    CurrentStatement = st;

    return st;
}

static void deleteStatement (void)
{
    statementInfo *const st = CurrentStatement;
    statementInfo *const parent = st->parent;
    unsigned int i;

    for (i = 0  ;  i < (unsigned int) NumTokens  ;  ++i)
    {
	deleteToken (st->token [i]);        st->token [i] = NULL;
    }
    deleteToken (st->blockName);           st->blockName = NULL;
    deleteToken (st->context);             st->context = NULL;
    vStringDelete (st->parentClasses);     st->parentClasses = NULL;
    eFree (st);
    CurrentStatement = parent;
}

static void deleteAllStatements (void)
{
    while (CurrentStatement != NULL)
	deleteStatement ();
}

static boolean isStatementEnd (const statementInfo *const st)
{
    const tokenInfo *const token = activeToken (st);
    boolean isEnd;

    if (isType (token, TOKEN_SEMICOLON))
	isEnd = TRUE;
    else if (isType (token, TOKEN_BRACE_CLOSE))
	isEnd = (boolean) (isLanguage (Lang_java) ||
			  ! isContextualStatement (st));
    else
	isEnd = FALSE;

    return isEnd;
}

static void checkStatementEnd (statementInfo *const st)
{
    const tokenInfo *const token = activeToken (st);

    if (isType (token, TOKEN_COMMA))
	reinitStatement (st, TRUE);
    else if (isStatementEnd (st))
    {
	DebugStatement ( if (debug (DEBUG_PARSE)) printf ("<ES>"); )
	reinitStatement (st, FALSE);
	cppEndStatement ();
    }
    else
    {
	cppBeginStatement ();
	advanceToken (st);
    }
}

static void nest (statementInfo *const st, const unsigned int nestLevel)
{
    switch (st->declaration)
    {
	case DECL_CLASS:
	case DECL_ENUM:
	case DECL_INTERFACE:
	case DECL_NAMESPACE:
	case DECL_NOMANGLE:
	case DECL_STRUCT:
	case DECL_UNION:
	    createTags (nestLevel, st);
	    break;

	default:
	    skipToMatch ("{}");
	    break;
    }
    advanceToken (st);
    setToken (st, TOKEN_BRACE_CLOSE);
}

static void tagCheck (statementInfo *const st)
{
    const tokenInfo *const token = activeToken (st);
    const tokenInfo *const prev  = prevToken (st, 1);
    const tokenInfo *const prev2 = prevToken (st, 2);

    switch (token->type)
    {
	case TOKEN_NAME:
	    if (insideEnumBody (st))
		qualifyEnumeratorTag (st, token);
	    break;
#if 0
	case TOKEN_PACKAGE:
	    if (st->haveQualifyingName)
		makeTag (token, st, FALSE, TAG_PACKAGE);
	    break;
#endif
	case TOKEN_BRACE_OPEN:
	    if (isType (prev, TOKEN_ARGS))
	    {
		if (st->haveQualifyingName)
		{
		    st->declaration = DECL_FUNCTION;
		    if (isType (prev2, TOKEN_NAME))
			copyToken (st->blockName, prev2);
		    qualifyFunctionTag (st, prev2);
		}
	    }
	    else if (isContextualStatement (st))
	    {
		if (isType (prev, TOKEN_NAME))
		    copyToken (st->blockName, prev);
		qualifyBlockTag (st, prev);
	    }
	    break;

	case TOKEN_SEMICOLON:
	case TOKEN_COMMA:
	    if (insideEnumBody (st))
		;
	    else if (isType (prev, TOKEN_NAME))
	    {
		if (isContextualKeyword (prev2))
		    st->scope = SCOPE_EXTERN;
		else
		    qualifyVariableTag (st, prev);
	    }
	    else if (isType (prev, TOKEN_ARGS)  &&  isType (prev2, TOKEN_NAME))
	    {
		if (st->isPointer)
		    qualifyVariableTag (st, prev2);
		else
		    qualifyFunctionDeclTag (st, prev2);
	    }
	    break;

	default: break;
    }
}

/*  Parses the current file and decides whether to write out and tags that
 *  are discovered.
 */
static void createTags (const unsigned int nestLevel,
			statementInfo *const parent)
{
    statementInfo *const st = newStatement (parent);

    DebugStatement ( if (nestLevel > 0) debugParseNest (TRUE, nestLevel); )
    while (TRUE)
    {
	tokenInfo *token;

	nextToken (st);
	token = activeToken (st);
	if (isType (token, TOKEN_BRACE_CLOSE))
	{
	    if (nestLevel > 0)
		break;
	    else
	    {
		verbose ("%s: unexpected closing brace at line %lu\n",
			getInputFileName (), getInputLineNumber ());
		longjmp (Exception, (int) ExceptionBraceFormattingError);
	    }
	}
	else if (isType (token, TOKEN_DOUBLE_COLON))
	{
	    addContext (st, prevToken (st, 1));
	    advanceToken (st);
	}
	else
	{
	    tagCheck (st);
	    if (isType (token, TOKEN_BRACE_OPEN))
		nest (st, nestLevel + 1);
	    checkStatementEnd (st);
	}
    }
    deleteStatement ();
    DebugStatement ( if (nestLevel > 0) debugParseNest (FALSE, nestLevel - 1); )
}

static boolean findCTags (const unsigned int passCount)
{
    exception_t exception;
    boolean retry;

    Assert (passCount < 3);
    cppInit ((boolean) (passCount > 1));

    exception = (exception_t) setjmp (Exception);
    retry = FALSE;
    if (exception == ExceptionNone)
	createTags (0, NULL);
    else
    {
	deleteAllStatements ();
	if (exception == ExceptionBraceFormattingError  &&  passCount == 1)
	{
	    retry = TRUE;
	   verbose ("%s: retrying file with fallback brace matching algorithm\n",
		    getInputFileName ());
	}
    }
    cppTerminate ();
    return retry;
}

static void buildKeywordHash (const langType language, unsigned int idx)
{
    const size_t count = sizeof (KeywordTable) / sizeof (KeywordTable [0]);
    size_t i;
    for (i = 0  ;  i < count  ;  ++i)
    {
	const keywordDesc* const p = &KeywordTable [i];
	if (p->isValid [idx])
	    addKeyword (p->name, language, (int) p->id);
    }
}

static void initializeCParser (const langType language)
{
    Lang_c = language;
    buildKeywordHash (language, 0);
}

static void initializeCppParser (const langType language)
{
    Lang_cpp = language;
    buildKeywordHash (language, 1);
}

static void initializeJavaParser (const langType language)
{
    Lang_java = language;
    buildKeywordHash (language, 2);
}

extern parserDefinition* CParser (void)
{
    static const char *const extensions [] = { "c", NULL };
    parserDefinition* def = parserNew ("C");
    def->kinds      = CKinds;
    def->kindCount  = KIND_COUNT (CKinds);
    def->extensions = extensions;
    def->parser2    = findCTags;
    def->initialize = initializeCParser;
    return def;
}

extern parserDefinition* CppParser (void)
{
    static const char *const extensions [] = {
	"c++", "cc", "cp", "cpp", "cxx", "h", "h++", "hh", "hp", "hpp", "hxx",
#ifndef CASE_INSENSITIVE_FILENAMES
	"C", "H",
#endif
	NULL
    };
    parserDefinition* def = parserNew ("C++");
    def->kinds      = CKinds;
    def->kindCount  = KIND_COUNT (CKinds);
    def->extensions = extensions;
    def->parser2    = findCTags;
    def->initialize = initializeCppParser;
    return def;
}

extern parserDefinition* JavaParser (void)
{
    static const char *const extensions [] = { "java", NULL };
    parserDefinition* def = parserNew ("Java");
    def->kinds      = JavaKinds;
    def->kindCount  = KIND_COUNT (JavaKinds);
    def->extensions = extensions;
    def->parser2    = findCTags;
    def->initialize = initializeJavaParser;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
