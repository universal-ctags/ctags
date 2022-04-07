/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C, C++, C#, D and Java
*   source files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"        /* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "entry.h"
#include "cpreprocessor.h"
#include "keyword.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "xtag.h"

/*
*   MACROS
*/

#define activeToken(st)     ((st)->token [(int) (st)->tokenIndex])
#define parentDecl(st)      ((st)->parent == NULL ? \
                            DECL_NONE : (st)->parent->declaration)
#define isType(token,t)     (bool) ((token)->type == (t))
#define insideEnumBody(st)  ((st)->parent == NULL ? false : \
                            (bool) ((st)->parent->declaration == DECL_ENUM))
#define insideAnnotationBody(st)  ((st)->parent == NULL ? false : \
								  (bool) ((st)->parent->declaration == DECL_ANNOTATION))
#define insideInterfaceBody(st) ((st)->parent == NULL ? false : \
                            (bool) ((st)->parent->declaration == DECL_INTERFACE))
#define isSignalDirection(token) (bool)(( (token)->keyword == KEYWORD_INPUT  ) ||\
					   ( (token)->keyword == KEYWORD_OUTPUT ) ||\
					   ( (token)->keyword == KEYWORD_INOUT  )  )
#define isExternCDecl(st,c) (bool) ((c) == STRING_SYMBOL  && \
                    ! (st)->haveQualifyingName  && (st)->scope == SCOPE_EXTERN)

#define isOneOf(c,s)        (bool) (strchr ((s), (c)) != NULL)

#define isHighChar(c)       ((c) != EOF && (unsigned int)(c) >= 0xc0 && \
							               (unsigned int)(c) <= 0xff)

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
enum eKeywordId {
	KEYWORD_ALIAS, KEYWORD_ATTRIBUTE, KEYWORD_ABSTRACT,
	KEYWORD_BOOLEAN, KEYWORD_BYTE, KEYWORD_BAD_STATE, KEYWORD_BAD_TRANS,
	KEYWORD_BIND, KEYWORD_BIND_VAR, KEYWORD_BIT,
	KEYWORD_CASE, KEYWORD_CATCH, KEYWORD_CHAR, KEYWORD_CLASS, KEYWORD_CLOCK, KEYWORD_CONST,
	KEYWORD_CONSTRAINT, KEYWORD_COVERAGE_BLOCK, KEYWORD_COVERAGE_DEF,
	KEYWORD_DEFAULT, KEYWORD_DELEGATE, KEYWORD_DELETE, KEYWORD_DO,
	KEYWORD_DOUBLE,
	KEYWORD_ELSE, KEYWORD_ENUM, KEYWORD_EXPLICIT, KEYWORD_EXTERN,
	KEYWORD_EXTENDS, KEYWORD_EVENT,
	KEYWORD_FINAL, KEYWORD_FLOAT, KEYWORD_FOR, KEYWORD_FOREACH,
	KEYWORD_FRIEND, KEYWORD_FUNCTION,
	KEYWORD_GOTO,
	KEYWORD_HDL_NODE,
	KEYWORD_IF, KEYWORD_IMPLEMENTS, KEYWORD_IMPORT, KEYWORD_INLINE, KEYWORD_INT,
	KEYWORD_INOUT, KEYWORD_INPUT, KEYWORD_INTEGER, KEYWORD_INTERFACE,
	KEYWORD_INTERNAL,
	KEYWORD_LOCAL, KEYWORD_LONG,
	KEYWORD_M_BAD_STATE, KEYWORD_M_BAD_TRANS, KEYWORD_M_STATE, KEYWORD_M_TRANS,
	KEYWORD_MUTABLE,
	KEYWORD_NAMESPACE, KEYWORD_NEW, KEYWORD_NEWCOV, KEYWORD_NATIVE,
	KEYWORD_NHOLD, KEYWORD_NOEXCEPT, KEYWORD_NSAMPLE,
	KEYWORD_OPERATOR, KEYWORD_OUTPUT, KEYWORD_OVERLOAD, KEYWORD_OVERRIDE,
	KEYWORD_PACKED, KEYWORD_PORT, KEYWORD_PACKAGE, KEYWORD_PHOLD, KEYWORD_PRIVATE,
	KEYWORD_PROGRAM, KEYWORD_PROTECTED, KEYWORD_PSAMPLE, KEYWORD_PUBLIC,
	KEYWORD_REGISTER, KEYWORD_RETURN,
	KEYWORD_SHADOW, KEYWORD_STATE,
	KEYWORD_SHORT, KEYWORD_SIGNED, KEYWORD_STATIC, KEYWORD_STRING,
	KEYWORD_STRUCT, KEYWORD_SWITCH, KEYWORD_SYNCHRONIZED,
	KEYWORD_TASK, KEYWORD_TEMPLATE, KEYWORD_THIS, KEYWORD_THROW,
	KEYWORD_THROWS, KEYWORD_TRANSIENT, KEYWORD_TRANS, KEYWORD_TRANSITION,
	KEYWORD_TRY, KEYWORD_TYPEDEF, KEYWORD_TYPENAME,
	KEYWORD_UINT, KEYWORD_ULONG, KEYWORD_UNION, KEYWORD_UNSIGNED, KEYWORD_USHORT,
	KEYWORD_USING,
	KEYWORD_VIRTUAL, KEYWORD_VOID, KEYWORD_VOLATILE,
	KEYWORD_WCHAR_T, KEYWORD_WHILE,
	KEYWORD_ALIGN, KEYWORD_ASM, KEYWORD_ASSERT, KEYWORD_AUTO,
	KEYWORD_BODY, KEYWORD_BOOL, KEYWORD_BREAK, KEYWORD_CAST,
	KEYWORD_CDOUBLE, KEYWORD_CENT, KEYWORD_CFLOAT, KEYWORD_CONTINUE,
	KEYWORD_CREAL, KEYWORD_DCHAR, KEYWORD_DEBUG,
	KEYWORD_DEPRECATED, KEYWORD_EXPORT, KEYWORD_FALSE, KEYWORD_FINALLY,
	KEYWORD_FOREACH_REVERSE, KEYWORD_IDOUBLE, KEYWORD_IFLOAT,
	KEYWORD_IN, KEYWORD_INVARIANT, KEYWORD_IREAL, KEYWORD_IS,
	KEYWORD_LAZY, KEYWORD_MIXIN, KEYWORD_MODULE, KEYWORD_NULL,
	KEYWORD_OUT, KEYWORD_PRAGMA, KEYWORD_REAL, KEYWORD_SCOPE,
	KEYWORD_SUPER, KEYWORD_TRUE, KEYWORD_TYPEID, KEYWORD_TYPEOF,
	KEYWORD_UBYTE, KEYWORD_UCENT, KEYWORD_UNITTEST, KEYWORD_VERSION,
	KEYWORD_WCHAR, KEYWORD_WITH
};
typedef int keywordId; /* to allow KEYWORD_NONE */

/*  Used to determine whether keyword is valid for the current language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
	short isValid [6]; /* indicates languages for which kw is valid */
} keywordDesc;

/*  Used for reporting the type of object parsed by nextToken ().
 */
typedef enum eTokenType {
	TOKEN_NONE,          /* none */
	TOKEN_ARGS,          /* a parenthetical pair and its contents */
	TOKEN_BRACE_CLOSE,
	TOKEN_BRACE_OPEN,
	TOKEN_COLON,         /* the colon character */
	TOKEN_COMMA,         /* the comma character */
	TOKEN_DOUBLE_COLON,  /* double colon indicates nested-name-specifier */
	TOKEN_KEYWORD,
	TOKEN_NAME,          /* an unknown name */
	TOKEN_PACKAGE,       /* a Java package name */
	TOKEN_PAREN_NAME,    /* a single name in parentheses */
	TOKEN_SEMICOLON,     /* the semicolon character */
	TOKEN_SPEC,          /* a storage class specifier, qualifier, type, etc. */
	TOKEN_COUNT
} tokenType;

/*  This describes the scoping of the current statement.
 */
typedef enum eTagScope {
	SCOPE_GLOBAL,        /* no storage class specified */
	SCOPE_STATIC,        /* static storage class */
	SCOPE_EXTERN,        /* external storage class */
	SCOPE_FRIEND,        /* declares access only */
	SCOPE_TYPEDEF,       /* scoping depends upon context */
	SCOPE_COUNT
} tagScope;

typedef enum eDeclaration {
	DECL_NONE,
	DECL_BASE,           /* base type (default) */
	DECL_CLASS,
	DECL_ENUM,
	DECL_EVENT,
	DECL_FUNCTION,
	DECL_FUNCTION_TEMPLATE, /* D-only */
	DECL_IGNORE,         /* non-taggable "declaration" */
	DECL_INTERFACE,
	DECL_MIXIN,
	DECL_NAMESPACE,
	DECL_NOMANGLE,       /* C++ name demangling block */
	DECL_PACKAGE,
	DECL_PACKAGEREF,
	DECL_PRIVATE,
	DECL_PROGRAM,        /* Vera program */
	DECL_PROTECTED,
	DECL_PUBLIC,
	DECL_STRUCT,
	DECL_TASK,           /* Vera task */
	DECL_TEMPLATE,       /* D-only */
	DECL_UNION,
	DECL_USING,
	DECL_VERSION,        /* D conditional compile */
	DECL_ANNOTATION,     /* Java annotation */
	DECL_COUNT
} declType;

typedef enum eVisibilityType {
	ACCESS_UNDEFINED,
	ACCESS_LOCAL,
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC,
	ACCESS_DEFAULT,      /* Java-specific */
	ACCESS_COUNT
} accessType;

/*  Information about the parent class of a member (if any).
 */
typedef struct sMemberInfo {
	accessType access;           /* access of current statement */
	accessType accessDefault;    /* access default for current statement */
} memberInfo;

typedef struct sTokenInfo {
	tokenType     type;
	keywordId     keyword;
	vString*      name;          /* the name of the token */
	unsigned long lineNumber;    /* line number of tag */
	MIOPos        filePosition;  /* file position of line containing name */
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
	declType	declaration;    /* specifier associated with TOKEN_SPEC */
	bool		gotName;        /* was a name parsed yet? */
	bool		haveQualifyingName;  /* do we have a name we are considering? */
	bool		gotParenName;   /* was a name inside parentheses parsed yet? */
	bool		gotArgs;        /* was a list of parameters parsed yet? */
	bool		isPointer;      /* is 'name' a pointer? */
	bool     inFunction;     /* are we inside of a function? */
	bool		assignment;     /* have we handled an '='? */
	bool		notVariable;    /* has a variable declaration been disqualified ? */
	impType		implementation; /* abstract or concrete implementation? */
	unsigned int tokenIndex;    /* currently active token */
	tokenInfo*	token [(int) NumTokens];
	tokenInfo*	context;        /* accumulated scope of current statement */
	tokenInfo*	blockName;      /* name of current block */
	memberInfo	member;         /* information regarding parent class/struct */
	vString*	parentClasses;  /* parent classes */
	struct sStatementInfo *parent;  /* statement we are nested within */
} statementInfo;

/*  Describes the type of tag being generated.
 */
typedef enum eTagType {
	TAG_UNDEFINED,
	TAG_CLASS,       /* class name */
	TAG_ENUM,        /* enumeration name */
	TAG_ENUMERATOR,  /* enumerator (enumeration value) */
	TAG_EVENT,       /* event */
	TAG_FIELD,       /* field (Java) */
	TAG_FUNCTION,    /* function definition */
	TAG_INTERFACE,   /* interface declaration */
	TAG_LOCAL,       /* local variable definition */
	TAG_MEMBER,      /* structure, class or interface member */
	TAG_METHOD,      /* method declaration */
	TAG_MIXIN, 		 /* D mixin */
	TAG_NAMESPACE,   /* namespace name */
	TAG_PACKAGE,     /* package name / D module name */
	TAG_PACKAGEREF,	 /* referenced package name */
	TAG_PROGRAM,     /* program name */
	TAG_PROPERTY,    /* property name */
	TAG_PROTOTYPE,   /* function prototype or declaration */
	TAG_SIGNAL,	 /* VERA signal name */
	TAG_STRUCT,      /* structure name */
	TAG_TASK,        /* task name */
	TAG_TYPEDEF,     /* typedef name / D alias name */
	TAG_TEMPLATE,    /* D template name */
	TAG_UNION,       /* union name */
	TAG_VARIABLE,    /* variable definition */
	TAG_EXTERN_VAR,  /* external variable declaration */
	TAG_VERSION, 	 /* conditional template compilation */
	TAG_LABEL,	 /* goto label */
	TAG_ANNOTATION,  /* Java annotation definition */
	TAG_COUNT        /* must be last */
} tagType;

typedef struct sParenInfo {
	bool isPointer;
	bool isParamList;
	bool isKnrParamList;
	bool isNameCandidate;
	bool invalidContents;
	bool nestedArgs;
	unsigned int parameterCount;
} parenInfo;

/*
*   DATA DEFINITIONS
*/

static jmp_buf Exception;

static langType Lang_c;
static langType Lang_cpp;
static langType Lang_csharp;
static langType Lang_d;
static langType Lang_java;
static langType Lang_vera;
static vString *Signature;
static bool CollectingSignature;

/* Number used to uniquely identify anonymous structs and unions. */
static int AnonymousID = 0;

#define COMMONK_UNDEFINED -1


/* Used to index into the CKinds table. */
typedef enum {
	CR_MACRO_UNDEF,
	CR_MACRO_CONDITION,
} cMacroRole;

static roleDefinition CMacroRoles [] = {
	RoleTemplateUndef,
	RoleTemplateCondition,
};

typedef enum {
	CR_HEADER_SYSTEM,
	CR_HEADER_LOCAL,
} cHeaderRole;

static roleDefinition CHeaderRoles [] = {
	RoleTemplateSystem,
	RoleTemplateLocal,
};

typedef enum {
	CK_UNDEFINED = COMMONK_UNDEFINED,
	CK_CLASS, CK_DEFINE, CK_ENUMERATOR, CK_FUNCTION,
	CK_ENUMERATION, CK_HEADER, CK_LOCAL, CK_MEMBER, CK_NAMESPACE, CK_PROTOTYPE,
	CK_STRUCT, CK_TYPEDEF, CK_UNION, CK_VARIABLE,
	CK_EXTERN_VARIABLE, CK_LABEL, CK_MACRO_PARAM,
} cKind;

static kindDefinition CKinds [] = {
	{ true,  'c', "class",      "classes"},
	{ true,  'd', "macro",      "macro definitions",
	  .referenceOnly = false, ATTACH_ROLES(CMacroRoles)},
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)"},
	{ true,  'f', "function",   "function definitions"},
	{ true,  'g', "enum",       "enumeration names"},
	{ true,  'h', "header",     "included header files",
	  .referenceOnly = true,  ATTACH_ROLES(CHeaderRoles)},
	{ false, 'l', "local",      "local variables"},
	{ true,  'm', "member",     "class, struct, and union members"},
	{ true,  'n', "namespace",  "namespaces"},
	{ false, 'p', "prototype",  "function prototypes"},
	{ true,  's', "struct",     "structure names"},
	{ true,  't', "typedef",    "typedefs"},
	{ true,  'u', "union",      "union names"},
	{ true,  'v', "variable",   "variable definitions"},
	{ false, 'x', "externvar",  "external and forward variable declarations"},
	{ false, 'L', "label",      "goto label"},
	{ false, 'D', "macroparam", "cpp macro parameters"},
};

typedef enum {
	CSK_UNDEFINED = COMMONK_UNDEFINED,
	CSK_CLASS, CSK_DEFINE, CSK_ENUMERATOR, CSK_EVENT, CSK_FIELD,
	CSK_ENUMERATION, CSK_INTERFACE, CSK_LOCAL, CSK_METHOD,
	CSK_NAMESPACE, CSK_PROPERTY, CSK_STRUCT, CSK_TYPEDEF
} csharpKind;

static kindDefinition CsharpKinds [] = {
	{ true,  'c', "class",      "classes"},
	{ true,  'd', "macro",      "macro definitions"},
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)"},
	{ true,  'E', "event",      "events"},
	{ true,  'f', "field",      "fields"},
	{ true,  'g', "enum",       "enumeration names"},
	{ true,  'i', "interface",  "interfaces"},
	{ false, 'l', "local",      "local variables"},
	{ true,  'm', "method",     "methods"},
	{ true,  'n', "namespace",  "namespaces"},
	{ true,  'p', "property",   "properties"},
	{ true,  's', "struct",     "structure names"},
	{ true,  't', "typedef",    "typedefs"},
};

typedef enum
{
	DK_UNDEFINED = COMMONK_UNDEFINED,
	DK_ALIAS, DK_CLASS, DK_ENUMERATION, DK_ENUMERATOR, DK_EXTERN_VARIABLE, DK_FUNCTION,
	DK_INTERFACE, DK_LOCAL, DK_MEMBER, DK_MIXIN, DK_MODULE, DK_NAMESPACE,
	DK_PROTOTYPE, DK_STRUCT, DK_TEMPLATE, DK_UNION,
	DK_VARIABLE, DK_VERSION
} dKind;

static kindDefinition DKinds [] = {
	{ true,  'a', "alias",      "aliases"},
	{ true,  'c', "class",      "classes"},
	{ true,  'g', "enum",       "enumeration names"},
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)"},
	{ false, 'x', "externvar",  "external variable declarations"},
	{ true,  'f', "function",   "function definitions"},
	{ true,  'i', "interface",  "interfaces"},
	{ false, 'l', "local",      "local variables"},
	{ true,  'm', "member",     "class, struct, and union members"},
	{ true,  'X', "mixin",      "mixins"},
	{ true,  'M', "module",     "modules"},
	{ true,  'n', "namespace",  "namespaces"},
	{ false, 'p', "prototype",  "function prototypes"},
	{ true,  's', "struct",     "structure names"},
	{ true,  'T', "template",   "templates"},
	{ true,  'u', "union",      "union names"},
	{ true,  'v', "variable",   "variable definitions"},
	{ true,  'V', "version",    "version statements"}
};

/* Used to index into the JavaKinds table. */
typedef enum {
	JAVAR_PACKAGE_IMPORTED,
} javaPackageRole;

static roleDefinition JavaPackageRoles [] = {
	{ true, "imported", "imported package"},
};

typedef enum {
	JK_UNDEFINED = COMMONK_UNDEFINED,
	JK_ANNOTATION, JK_CLASS, JK_ENUM_CONSTANT, JK_FIELD, JK_ENUM, JK_INTERFACE,
	JK_LOCAL, JK_METHOD, JK_PACKAGE, JK_ACCESS, JK_CLASS_PREFIX
} javaKind;

static kindDefinition JavaKinds [] = {
	{ true,  'a', "annotation",    "annotation declarations" },
	{ true,  'c', "class",         "classes"},
	{ true,  'e', "enumConstant",  "enum constants"},
	{ true,  'f', "field",         "fields"},
	{ true,  'g', "enum",          "enum types"},
	{ true,  'i', "interface",     "interfaces"},
	{ false, 'l', "local",         "local variables"},
	{ true,  'm', "method",        "methods"},
	{ true,  'p', "package",       "packages",
	  .referenceOnly = false, ATTACH_ROLES(JavaPackageRoles)},
};

/* Used to index into the VeraKinds table. */
typedef enum {
	VR_MACRO_UNDEF,
	VR_MACRO_CONDITION,
} veraMacroRole;

static roleDefinition VeraMacroRoles [] = {
	RoleTemplateUndef,
	RoleTemplateCondition,
};


typedef enum {
	VR_HEADER_SYSTEM,
	VR_HEADER_LOCAL,
} veraHeaderRole;

static roleDefinition VeraHeaderRoles [] = {
	RoleTemplateSystem,
	RoleTemplateLocal,
};

typedef enum {
	VK_UNDEFINED = COMMONK_UNDEFINED,
	VK_CLASS, VK_DEFINE, VK_ENUMERATOR, VK_FUNCTION,
	VK_ENUMERATION, VK_INTERFACE, VK_LOCAL, VK_MEMBER, VK_PROGRAM, VK_PROTOTYPE,
	VK_SIGNAL, VK_TASK, VK_TYPEDEF, VK_VARIABLE,
	VK_EXTERN_VARIABLE, VK_HEADER, VK_MACRO_PARAM,
} veraKind;

static kindDefinition VeraKinds [] = {
	{ true,  'c', "class",      "classes"},
	{ true,  'd', "macro",      "macro definitions",
	  .referenceOnly = false, ATTACH_ROLES(VeraMacroRoles)},
	{ true,  'e', "enumerator", "enumerators (values inside an enumeration)"},
	{ true,  'f', "function",   "function definitions"},
	{ true,  'g', "enum",       "enumeration names"},
	{ true,  'i', "interface",  "interfaces"},
	{ false, 'l', "local",      "local variables"},
	{ true,  'm', "member",     "class, struct, and union members"},
	{ true,  'p', "program",    "programs"},
	{ false, 'P', "prototype",  "function prototypes"},
	{ true,  's', "signal",     "signals"},
	{ true,  't', "task",       "tasks"},
	{ true,  'T', "typedef",    "typedefs"},
	{ true,  'v', "variable",   "variable definitions"},
	{ false, 'x', "externvar",  "external variable declarations"},
	{ true,  'h', "header",     "included header files",
	  .referenceOnly = true, ATTACH_ROLES(VeraHeaderRoles)},
	{ false, 'D', "macroParameter", "cpp macro parameters"},
};

static const keywordDesc KeywordTable [] = {
     /*                                                C++    D          */
     /*                                         ANSI C  |  C# | Java     */
     /*                                              |  |  |  |  |  Vera */
     /* keyword           keyword ID                 |  |  |  |  |  |    */
     { "__attribute__",   KEYWORD_ATTRIBUTE,       { 1, 1, 1, 1, 0, 0 } },
     { "abstract",        KEYWORD_ABSTRACT,        { 0, 0, 1, 1, 1, 0 } },
     { "alias",           KEYWORD_ALIAS,           { 0, 0, 0, 1, 0, 0 } },
     { "align",           KEYWORD_ALIGN,           { 0, 0, 0, 1, 0, 0 } },
     { "asm",             KEYWORD_ASM,             { 0, 0, 0, 1, 0, 0 } },
     { "assert",          KEYWORD_ASSERT,          { 0, 0, 0, 1, 0, 0 } },
     { "auto",            KEYWORD_AUTO,            { 0, 0, 0, 1, 0, 0 } },
     { "bad_state",       KEYWORD_BAD_STATE,       { 0, 0, 0, 0, 0, 1 } },
     { "bad_trans",       KEYWORD_BAD_TRANS,       { 0, 0, 0, 0, 0, 1 } },
     { "bind",            KEYWORD_BIND,            { 0, 0, 0, 0, 0, 1 } },
     { "bind_var",        KEYWORD_BIND_VAR,        { 0, 0, 0, 0, 0, 1 } },
     { "bit",             KEYWORD_BIT,             { 0, 0, 0, 0, 0, 1 } },
     { "body",            KEYWORD_BODY,            { 0, 0, 0, 1, 0, 0 } },
     { "bool",            KEYWORD_BOOL,            { 0, 0, 0, 1, 0, 0 } },
     { "boolean",         KEYWORD_BOOLEAN,         { 0, 0, 0, 0, 1, 0 } },
     { "break",           KEYWORD_BREAK,           { 0, 0, 0, 1, 0, 0 } },
     { "byte",            KEYWORD_BYTE,            { 0, 0, 0, 1, 1, 0 } },
     { "case",            KEYWORD_CASE,            { 1, 1, 1, 1, 1, 0 } },
     { "cast",            KEYWORD_CAST,            { 0, 0, 0, 1, 0, 0 } },
     { "catch",           KEYWORD_CATCH,           { 0, 1, 1, 1, 1, 0 } },
     { "cdouble",         KEYWORD_CDOUBLE,         { 0, 0, 0, 1, 0, 0 } },
     { "cent",            KEYWORD_CENT,            { 0, 0, 0, 1, 0, 0 } },
     { "cfloat",          KEYWORD_CFLOAT,          { 0, 0, 0, 1, 0, 0 } },
     { "char",            KEYWORD_CHAR,            { 1, 1, 1, 1, 1, 0 } },
     { "class",           KEYWORD_CLASS,           { 0, 1, 1, 1, 1, 1 } },
     { "CLOCK",           KEYWORD_CLOCK,           { 0, 0, 0, 0, 0, 1 } },
     { "const",           KEYWORD_CONST,           { 1, 1, 1, 1, 1, 0 } },
     { "constraint",      KEYWORD_CONSTRAINT,      { 0, 0, 0, 0, 0, 1 } },
     { "continue",        KEYWORD_CONTINUE,        { 0, 0, 0, 1, 0, 0 } },
     { "coverage_block",  KEYWORD_COVERAGE_BLOCK,  { 0, 0, 0, 0, 0, 1 } },
     { "coverage_def",    KEYWORD_COVERAGE_DEF,    { 0, 0, 0, 0, 0, 1 } },
     { "creal",           KEYWORD_CREAL,           { 0, 0, 0, 1, 0, 0 } },
     { "dchar",           KEYWORD_DCHAR,           { 0, 0, 0, 1, 0, 0 } },
     { "debug",           KEYWORD_DEBUG,           { 0, 0, 0, 1, 0, 0 } },
     { "default",         KEYWORD_DEFAULT,         { 1, 1, 1, 1, 1, 0 } },
     { "delegate",        KEYWORD_DELEGATE,        { 0, 0, 1, 1, 0, 0 } },
     { "delete",          KEYWORD_DELETE,          { 0, 1, 0, 1, 0, 0 } },
     { "deprecated",      KEYWORD_DEPRECATED,      { 0, 0, 0, 1, 0, 0 } },
     { "do",              KEYWORD_DO,              { 1, 1, 1, 1, 1, 0 } },
     { "double",          KEYWORD_DOUBLE,          { 1, 1, 1, 1, 1, 0 } },
     { "else",            KEYWORD_ELSE,            { 1, 1, 1, 1, 1, 0 } },
     { "enum",            KEYWORD_ENUM,            { 1, 1, 1, 1, 1, 1 } },
     { "event",           KEYWORD_EVENT,           { 0, 0, 1, 0, 0, 1 } },
     { "explicit",        KEYWORD_EXPLICIT,        { 0, 1, 1, 1, 0, 0 } },
     { "export",          KEYWORD_EXPORT,          { 0, 0, 0, 1, 0, 0 } },
     { "extends",         KEYWORD_EXTENDS,         { 0, 0, 0, 0, 1, 1 } },
     { "extern",          KEYWORD_EXTERN,          { 1, 1, 1, 1, 0, 1 } },
     { "false",           KEYWORD_FALSE,           { 0, 0, 0, 1, 0, 0 } },
     { "final",           KEYWORD_FINAL,           { 0, 0, 0, 1, 1, 0 } },
     { "finally",         KEYWORD_FINALLY,         { 0, 0, 0, 1, 0, 0 } },
     { "float",           KEYWORD_FLOAT,           { 1, 1, 1, 1, 1, 0 } },
     { "for",             KEYWORD_FOR,             { 1, 1, 1, 1, 1, 0 } },
     { "foreach",         KEYWORD_FOREACH,         { 0, 0, 1, 1, 0, 0 } },
     { "foreach_reverse", KEYWORD_FOREACH_REVERSE, { 0, 0, 0, 1, 0, 0 } },
     { "friend",          KEYWORD_FRIEND,          { 0, 1, 0, 1, 0, 0 } },
     { "function",        KEYWORD_FUNCTION,        { 0, 0, 0, 1, 0, 1 } },
     { "goto",            KEYWORD_GOTO,            { 1, 1, 1, 1, 1, 0 } },
     { "hdl_node",        KEYWORD_HDL_NODE,        { 0, 0, 0, 0, 0, 1 } },
     { "idouble",         KEYWORD_IDOUBLE,         { 0, 0, 0, 1, 0, 0 } },
     { "if",              KEYWORD_IF,              { 1, 1, 1, 1, 1, 0 } },
     { "ifloat",          KEYWORD_IFLOAT,          { 0, 0, 0, 1, 0, 0 } },
     { "implements",      KEYWORD_IMPLEMENTS,      { 0, 0, 0, 0, 1, 0 } },
     { "import",          KEYWORD_IMPORT,          { 0, 0, 0, 1, 1, 0 } },
     { "in",              KEYWORD_IN,              { 0, 0, 0, 1, 0, 0 } },
     { "inline",          KEYWORD_INLINE,          { 0, 1, 0, 1, 0, 0 } },
     { "inout",           KEYWORD_INOUT,           { 0, 0, 0, 1, 0, 1 } },
     { "input",           KEYWORD_INPUT,           { 0, 0, 0, 0, 0, 1 } },
     { "int",             KEYWORD_INT,             { 1, 1, 1, 1, 1, 0 } },
     { "integer",         KEYWORD_INTEGER,         { 0, 0, 0, 0, 0, 1 } },
     { "interface",       KEYWORD_INTERFACE,       { 0, 0, 1, 1, 1, 1 } },
     { "internal",        KEYWORD_INTERNAL,        { 0, 0, 1, 0, 0, 0 } },
     { "invariant",       KEYWORD_INVARIANT,       { 0, 0, 0, 1, 0, 0 } },
     { "ireal",           KEYWORD_IREAL,           { 0, 0, 0, 1, 0, 0 } },
     { "is",              KEYWORD_IS,              { 0, 0, 0, 1, 0, 0 } },
     { "lazy",            KEYWORD_LAZY,            { 0, 0, 0, 1, 0, 0 } },
     { "local",           KEYWORD_LOCAL,           { 0, 0, 0, 0, 0, 1 } },
     { "long",            KEYWORD_LONG,            { 1, 1, 1, 1, 1, 0 } },
     { "m_bad_state",     KEYWORD_M_BAD_STATE,     { 0, 0, 0, 0, 0, 1 } },
     { "m_bad_trans",     KEYWORD_M_BAD_TRANS,     { 0, 0, 0, 0, 0, 1 } },
     { "m_state",         KEYWORD_M_STATE,         { 0, 0, 0, 0, 0, 1 } },
     { "m_trans",         KEYWORD_M_TRANS,         { 0, 0, 0, 0, 0, 1 } },
     { "mixin",           KEYWORD_MIXIN,           { 0, 0, 0, 1, 0, 0 } },
     { "module",          KEYWORD_MODULE,          { 0, 0, 0, 1, 0, 0 } },
     { "mutable",         KEYWORD_MUTABLE,         { 0, 1, 0, 1, 0, 0 } },
     { "namespace",       KEYWORD_NAMESPACE,       { 0, 1, 1, 1, 0, 0 } },
     { "native",          KEYWORD_NATIVE,          { 0, 0, 0, 0, 1, 0 } },
     { "new",             KEYWORD_NEW,             { 0, 1, 1, 1, 1, 0 } },
     { "newcov",          KEYWORD_NEWCOV,          { 0, 0, 0, 0, 0, 1 } },
     { "NHOLD",           KEYWORD_NHOLD,           { 0, 0, 0, 0, 0, 1 } },
     { "noexcept",        KEYWORD_NOEXCEPT,        { 0, 1, 0, 0, 0, 0 } },
     { "NSAMPLE",         KEYWORD_NSAMPLE,         { 0, 0, 0, 0, 0, 1 } },
     { "null",            KEYWORD_NULL,            { 0, 0, 0, 1, 0, 0 } },
     { "operator",        KEYWORD_OPERATOR,        { 0, 1, 1, 1, 0, 0 } },
     { "out",             KEYWORD_OUT,             { 0, 0, 0, 1, 0, 0 } },
     { "output",          KEYWORD_OUTPUT,          { 0, 0, 0, 0, 0, 1 } },
     { "overload",        KEYWORD_OVERLOAD,        { 0, 1, 0, 1, 0, 0 } },
     { "override",        KEYWORD_OVERRIDE,        { 0, 0, 1, 1, 0, 0 } },
     { "package",         KEYWORD_PACKAGE,         { 0, 0, 0, 1, 1, 0 } },
     { "packed",          KEYWORD_PACKED,          { 0, 0, 0, 0, 0, 1 } },
     { "PHOLD",           KEYWORD_PHOLD,           { 0, 0, 0, 0, 0, 1 } },
     { "port",            KEYWORD_PORT,            { 0, 0, 0, 0, 0, 1 } },
     { "pragma",          KEYWORD_PRAGMA,          { 0, 0, 0, 1, 0, 0 } },
     { "private",         KEYWORD_PRIVATE,         { 0, 1, 1, 1, 1, 0 } },
     { "program",         KEYWORD_PROGRAM,         { 0, 0, 0, 0, 0, 1 } },
     { "protected",       KEYWORD_PROTECTED,       { 0, 1, 1, 1, 1, 1 } },
     { "PSAMPLE",         KEYWORD_PSAMPLE,         { 0, 0, 0, 0, 0, 1 } },
     { "public",          KEYWORD_PUBLIC,          { 0, 1, 1, 1, 1, 1 } },
     { "real",            KEYWORD_REAL,            { 0, 0, 0, 1, 0, 0 } },
     { "register",        KEYWORD_REGISTER,        { 1, 1, 0, 1, 0, 0 } },
     { "return",          KEYWORD_RETURN,          { 1, 1, 1, 1, 1, 0 } },
     { "scope",           KEYWORD_SCOPE,           { 0, 0, 0, 1, 0, 0 } },
     { "shadow",          KEYWORD_SHADOW,          { 0, 0, 0, 0, 0, 1 } },
     { "short",           KEYWORD_SHORT,           { 1, 1, 1, 1, 1, 0 } },
     { "signed",          KEYWORD_SIGNED,          { 1, 1, 0, 1, 0, 0 } },
     { "state",           KEYWORD_STATE,           { 0, 0, 0, 0, 0, 1 } },
     { "static",          KEYWORD_STATIC,          { 1, 1, 1, 1, 1, 1 } },
     { "string",          KEYWORD_STRING,          { 0, 0, 1, 0, 0, 1 } },
     { "struct",          KEYWORD_STRUCT,          { 1, 1, 1, 1, 0, 0 } },
     { "super",           KEYWORD_SUPER,           { 0, 0, 0, 1, 0, 0 } },
     { "switch",          KEYWORD_SWITCH,          { 1, 1, 1, 1, 1, 0 } },
     { "synchronized",    KEYWORD_SYNCHRONIZED,    { 0, 0, 0, 1, 1, 0 } },
     { "task",            KEYWORD_TASK,            { 0, 0, 0, 0, 0, 1 } },
     { "template",        KEYWORD_TEMPLATE,        { 0, 1, 0, 1, 0, 0 } },
     { "this",            KEYWORD_THIS,            { 0, 1, 1, 0, 1, 0 } },
     { "throw",           KEYWORD_THROW,           { 0, 1, 1, 1, 1, 0 } },
     { "throws",          KEYWORD_THROWS,          { 0, 0, 0, 0, 1, 0 } },
     { "trans",           KEYWORD_TRANS,           { 0, 0, 0, 0, 0, 1 } },
     { "transient",       KEYWORD_TRANSIENT,       { 0, 0, 0, 0, 1, 0 } },
     { "transition",      KEYWORD_TRANSITION,      { 0, 0, 0, 0, 0, 1 } },
     { "true",            KEYWORD_TRUE,            { 0, 0, 0, 1, 0, 0 } },
     { "try",             KEYWORD_TRY,             { 0, 1, 1, 1, 0, 0 } },
     { "typedef",         KEYWORD_TYPEDEF,         { 1, 1, 1, 1, 0, 1 } },
     { "typeid",          KEYWORD_TYPEID,          { 0, 0, 0, 1, 0, 0 } },
     { "typename",        KEYWORD_TYPENAME,        { 0, 1, 0, 1, 0, 0 } },
     { "typeof",          KEYWORD_TYPEOF,          { 0, 0, 0, 1, 0, 0 } },
     { "ubyte",           KEYWORD_UBYTE,           { 0, 0, 0, 1, 0, 0 } },
     { "ucent",           KEYWORD_UCENT,           { 0, 0, 0, 1, 0, 0 } },
     { "uint",            KEYWORD_UINT,            { 0, 0, 1, 1, 0, 0 } },
     { "ulong",           KEYWORD_ULONG,           { 0, 0, 1, 1, 0, 0 } },
     { "union",           KEYWORD_UNION,           { 1, 1, 0, 1, 0, 0 } },
     { "unittest",        KEYWORD_UNITTEST,        { 0, 0, 0, 1, 0, 0 } },
     { "unsigned",        KEYWORD_UNSIGNED,        { 1, 1, 1, 1, 0, 0 } },
     { "ushort",          KEYWORD_USHORT,          { 0, 0, 1, 1, 0, 0 } },
     { "using",           KEYWORD_USING,           { 0, 1, 1, 1, 0, 0 } },
     { "version",         KEYWORD_VERSION,         { 0, 0, 0, 1, 0, 0 } },
     { "virtual",         KEYWORD_VIRTUAL,         { 0, 1, 1, 1, 0, 1 } },
     { "void",            KEYWORD_VOID,            { 1, 1, 1, 1, 1, 1 } },
     { "volatile",        KEYWORD_VOLATILE,        { 1, 1, 1, 1, 1, 0 } },
     { "wchar",           KEYWORD_WCHAR,           { 0, 0, 0, 1, 0, 0 } },
     { "wchar_t",         KEYWORD_WCHAR_T,         { 0, 1, 1, 0, 0, 0 } },
     { "while",           KEYWORD_WHILE,           { 1, 1, 1, 1, 1, 0 } },
     { "with",            KEYWORD_WITH,            { 0, 0, 0, 1, 0, 0 } },
};

/*
*   FUNCTION PROTOTYPES
*/
static void createTags (const unsigned int nestLevel, statementInfo *const parent);

/*
*   FUNCTION DEFINITIONS
*/

/*
*   Token management
*/

static void initToken (tokenInfo* const token)
{
	token->type			= TOKEN_NONE;
	token->keyword		= KEYWORD_NONE;
	token->lineNumber	= getInputLineNumber ();
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
	static const char *const names [] = {
		"?", "local", "private", "protected", "public", "default"
	};
	Assert (ARRAY_SIZE (names) == ACCESS_COUNT);
	Assert ((int) access < ACCESS_COUNT);
	return names [(int) access];
}

static const char *implementationString (const impType imp)
{
	static const char *const names [] ={
		"?", "abstract", "virtual", "pure virtual"
	};
	Assert (ARRAY_SIZE (names) == IMP_COUNT);
	Assert ((int) imp < IMP_COUNT);
	return names [(int) imp];
}

/*
*   Debugging functions
*/

#ifdef DEBUG

#define boolString(c)   ((c) ? "true" : "false")

static const char *tokenString (const tokenType type)
{
	static const char *const names [] = {
		"none", "args", "}", "{", "colon", "comma", "double colon", "keyword",
		"name", "package", "paren-name", "semicolon", "specifier"
	};
	Assert (ARRAY_SIZE (names) == TOKEN_COUNT);
	Assert ((int) type < TOKEN_COUNT);
	return names [(int) type];
}

static const char *scopeString (const tagScope scope)
{
	static const char *const names [] = {
		"global", "static", "extern", "friend", "typedef"
	};
	Assert (ARRAY_SIZE (names) == SCOPE_COUNT);
	Assert ((int) scope < SCOPE_COUNT);
	return names [(int) scope];
}

static const char *declString (const declType declaration)
{
	static const char *const names [] = {
		"?", "base", "class", "enum", "event", "function", "function template",
		"ignore", "interface", "mixin", "namespace", "no mangle", "package", "package ref",
		"private", "program", "protected", "public", "struct", "task", "template",
		"union", "using", "version", "annotation"
	};
	Assert (ARRAY_SIZE (names) == DECL_COUNT);
	Assert ((int) declaration < DECL_COUNT);
	return names [(int) declaration];
}

static const char *keywordString (const keywordId keyword)
{
	const size_t count = ARRAY_SIZE (KeywordTable);
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

static void CTAGS_ATTR_UNUSED pt (tokenInfo *const token)
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

static void CTAGS_ATTR_UNUSED ps (statementInfo *const st)
{
#define P	"[%-7u]"
	static unsigned int id = 0;
	unsigned int i;
	printf (P"scope: %s   decl: %s   gotName: %s   gotParenName: %s\n", id,
		scopeString (st->scope), declString (st->declaration),
		boolString (st->gotName), boolString (st->gotParenName));
	printf (P"haveQualifyingName: %s\n", id, boolString (st->haveQualifyingName));
	printf (P"access: %s   default: %s\n", id, accessString (st->member.access),
		accessString (st->member.accessDefault));
	printf (P"token  : ", id);
	pt (activeToken (st));
	for (i = 1  ;  i < (unsigned int) NumTokens  ;  ++i)
	{
		printf (P"prev %u : ", id, i);
		pt (prevToken (st, i));
	}
	printf (P"context: ", id);
	pt (st->context);
	id++;
#undef P
}

#endif

/*
*   Statement management
*/

static bool isContextualKeyword (const tokenInfo *const token)
{
	bool result;
	switch (token->keyword)
	{
		case KEYWORD_CLASS:
		case KEYWORD_ENUM:
		case KEYWORD_INTERFACE:
		case KEYWORD_NAMESPACE:
		case KEYWORD_STRUCT:
		case KEYWORD_UNION:
		case KEYWORD_VERSION:
		case KEYWORD_TEMPLATE:
			result = true;
			break;

		default: result = false; break;
	}
	return result;
}

static bool isContextualStatement (const statementInfo *const st)
{
	bool result = false;
	if (st != NULL) switch (st->declaration)
	{
		case DECL_CLASS:
		case DECL_ENUM:
		case DECL_INTERFACE:
		case DECL_NAMESPACE:
		case DECL_PRIVATE:
		case DECL_PROTECTED:
		case DECL_PUBLIC:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TEMPLATE:
		case DECL_ANNOTATION:
			result = true;
			break;

		default: result = false; break;
	}
	return result;
}

static bool isMember (const statementInfo *const st)
{
	bool result;
	if (isType (st->context, TOKEN_NAME))
		result = true;
	else
		result = (bool)
			(st->parent != NULL && isContextualStatement (st->parent));
	return result;
}

static void initMemberInfo (statementInfo *const st)
{
	accessType accessDefault = ACCESS_UNDEFINED;
	if (st->parent != NULL) switch (st->parent->declaration)
	{
		case DECL_PRIVATE:
			accessDefault = ACCESS_PRIVATE;
			break;
		case DECL_PROTECTED:
			accessDefault = ACCESS_PROTECTED;
			break;
		case DECL_PUBLIC:
			accessDefault = ACCESS_PUBLIC;
			break;
		case DECL_ENUM:
			accessDefault = (isInputLanguage (Lang_java) ? ACCESS_PUBLIC : ACCESS_UNDEFINED);
			break;
		case DECL_NAMESPACE:
			accessDefault = ACCESS_UNDEFINED;
			break;

		case DECL_CLASS:
			if (isInputLanguage (Lang_java))
				accessDefault = ACCESS_DEFAULT;
			else if (isInputLanguage (Lang_d))
				accessDefault = ACCESS_PUBLIC;
			else
				accessDefault = ACCESS_PRIVATE;
			break;

		case DECL_INTERFACE:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ANNOTATION:
			accessDefault = ACCESS_PUBLIC;
			break;

		default: break;
	}
	st->member.accessDefault = accessDefault;
	st->member.access		 = accessDefault;
}

static void reinitStatement (statementInfo *const st, const bool partial)
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
	st->gotParenName	= false;
	st->isPointer		= false;
	st->inFunction		= false;
	st->assignment		= false;
	st->notVariable		= false;
	st->implementation	= IMP_DEFAULT;
	st->gotArgs			= false;
	st->gotName			= false;
	st->haveQualifyingName = false;
	st->tokenIndex		= 0;

	if (st->parent != NULL)
		st->inFunction = st->parent->inFunction;

	for (i = 0  ;  i < (unsigned int) NumTokens  ;  ++i)
		initToken (st->token [i]);

	initToken (st->context);

	/*	Keep the block name, so that a variable following after a comma will
	 *	still have the structure name.
	 */
	if (! partial)
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
	reinitStatement (st, false);
}

/*
*   Tag generation functions
*/
#define cTagKind(type) cTagKindFull(type, true)
#define cTagKindNoAssert(type) cTagKindFull(type, false)
static cKind cTagKindFull (const tagType type, const bool with_assert)
{
	cKind result = CK_UNDEFINED;
	switch (type)
	{
		case TAG_CLASS:      result = CK_CLASS;       break;
		case TAG_ENUM:       result = CK_ENUMERATION; break;
		case TAG_ENUMERATOR: result = CK_ENUMERATOR;  break;
		case TAG_FUNCTION:   result = CK_FUNCTION;    break;
		case TAG_LOCAL:      result = CK_LOCAL;       break;
		case TAG_MEMBER:     result = CK_MEMBER;      break;
		case TAG_NAMESPACE:  result = CK_NAMESPACE;   break;
		case TAG_PROTOTYPE:  result = CK_PROTOTYPE;   break;
		case TAG_STRUCT:     result = CK_STRUCT;      break;
		case TAG_TYPEDEF:    result = CK_TYPEDEF;     break;
		case TAG_UNION:      result = CK_UNION;       break;
		case TAG_VARIABLE:   result = CK_VARIABLE;    break;
		case TAG_EXTERN_VAR: result = CK_EXTERN_VARIABLE; break;
		case TAG_LABEL:      result = CK_LABEL; break;

		default: if (with_assert) Assert ("Bad C tag type" == NULL); break;
	}
	return result;
}

#define csharpTagKind(type) csharpTagKindFull(type, true)
#define csharpTagKindNoAssert(type) csharpTagKindFull(type, false)
static csharpKind csharpTagKindFull (const tagType type, const bool with_assert)
{
	csharpKind result = CSK_UNDEFINED;
	switch (type)
	{
		case TAG_CLASS:      result = CSK_CLASS;           break;
		case TAG_ENUM:       result = CSK_ENUMERATION;     break;
		case TAG_ENUMERATOR: result = CSK_ENUMERATOR;      break;
		case TAG_EVENT:      result = CSK_EVENT;           break;
		case TAG_FIELD:      result = CSK_FIELD ;          break;
		case TAG_INTERFACE:  result = CSK_INTERFACE;       break;
		case TAG_LOCAL:      result = CSK_LOCAL;           break;
		case TAG_METHOD:     result = CSK_METHOD;          break;
		case TAG_NAMESPACE:  result = CSK_NAMESPACE;       break;
		case TAG_PROPERTY:   result = CSK_PROPERTY;        break;
		case TAG_STRUCT:     result = CSK_STRUCT;          break;
		case TAG_TYPEDEF:    result = CSK_TYPEDEF;         break;

		default: if (with_assert) Assert ("Bad C# tag type" == NULL); break;
	}
	return result;
}

#define javaTagKind(type) javaTagKindFull(type, true)
#define javaTagKindNoAssert(type) javaTagKindFull(type, false)
static javaKind javaTagKindFull (const tagType type, bool with_assert)
{
	javaKind result = JK_UNDEFINED;
	switch (type)
	{
		case TAG_CLASS:      result = JK_CLASS;         break;
		case TAG_ENUM:       result = JK_ENUM;          break;
		case TAG_ENUMERATOR: result = JK_ENUM_CONSTANT; break;
		case TAG_FIELD:      result = JK_FIELD;         break;
		case TAG_INTERFACE:  result = JK_INTERFACE;     break;
		case TAG_LOCAL:      result = JK_LOCAL;         break;
		case TAG_METHOD:     result = JK_METHOD;        break;
		case TAG_PACKAGE:    /* Fall through */
		case TAG_PACKAGEREF: result = JK_PACKAGE;       break;
		case TAG_ANNOTATION: result = JK_ANNOTATION;     break;

		default: if (with_assert) Assert ("Bad Java tag type" == NULL); break;
	}
	return result;
}

#define dTagKind(type) dTagKindFull(type, true)
#define dTagKindNoAssert(type) dTagKindFull(type, false)
static dKind dTagKindFull (const tagType type, bool with_assert)
{
	dKind result = DK_UNDEFINED;
	switch (type)
	{
		case TAG_TYPEDEF:    result = DK_ALIAS;           break;
		case TAG_CLASS:      result = DK_CLASS;           break;
		case TAG_ENUM:       result = DK_ENUMERATION;     break;
		case TAG_ENUMERATOR: result = DK_ENUMERATOR;      break;
		case TAG_EXTERN_VAR: result = DK_EXTERN_VARIABLE; break;
		case TAG_FUNCTION:   result = DK_FUNCTION;        break;
		case TAG_INTERFACE:  result = DK_INTERFACE;       break;
		case TAG_LOCAL:      result = DK_LOCAL;           break;
		case TAG_MEMBER:     result = DK_MEMBER;          break;
		case TAG_MIXIN:      result = DK_MIXIN;           break;
		case TAG_PACKAGE:    result = DK_MODULE;          break;
		case TAG_NAMESPACE:  result = DK_NAMESPACE;       break;
		case TAG_PROTOTYPE:  result = DK_PROTOTYPE;       break;
		case TAG_STRUCT:     result = DK_STRUCT;          break;
		case TAG_TEMPLATE:   result = DK_TEMPLATE;        break;
		case TAG_UNION:      result = DK_UNION;           break;
		case TAG_VARIABLE:   result = DK_VARIABLE;        break;
		case TAG_VERSION:    result = DK_VERSION;         break;

		default: if (with_assert) Assert ("Bad D tag type" == NULL); break;
	}
	return result;
}

#define veraTagKind(type) veraTagKindFull(type, true)
#define veraTagKindNoAssert(type) veraTagKindFull(type, false)
static veraKind veraTagKindFull (const tagType type, bool with_assert) {
	veraKind result = VK_UNDEFINED;
	switch (type)
	{
		case TAG_CLASS:      result = VK_CLASS;           break;
		case TAG_ENUM:       result = VK_ENUMERATION;     break;
		case TAG_ENUMERATOR: result = VK_ENUMERATOR;      break;
		case TAG_FUNCTION:   result = VK_FUNCTION;        break;
		case TAG_INTERFACE:  result = VK_INTERFACE;       break;
		case TAG_LOCAL:      result = VK_LOCAL;           break;
		case TAG_MEMBER:     result = VK_MEMBER;          break;
		case TAG_PROGRAM:    result = VK_PROGRAM;         break;
		case TAG_PROTOTYPE:  result = VK_PROTOTYPE;       break;
		case TAG_SIGNAL:     result = VK_SIGNAL;          break;
		case TAG_TASK:       result = VK_TASK;            break;
		case TAG_TYPEDEF:    result = VK_TYPEDEF;         break;
		case TAG_VARIABLE:   result = VK_VARIABLE;        break;
		case TAG_EXTERN_VAR: result = VK_EXTERN_VARIABLE; break;

		default: if (with_assert) Assert ("Bad Vera tag type" == NULL); break;
	}
	return result;
}

static int kindIndexForType (const tagType type)
{
	int result;
	if (isInputLanguage (Lang_csharp))
		result = csharpTagKind (type);
	else if (isInputLanguage (Lang_java))
		result = javaTagKind (type);
	else if (isInputLanguage (Lang_d))
		result = dTagKind (type);
	else if (isInputLanguage (Lang_vera))
		result = veraTagKind (type);
	else
		result = cTagKind (type);
	return result;
}

static int roleForType (const tagType type)
{
	int result;

	result = ROLE_DEFINITION_INDEX;
	if (isInputLanguage (Lang_java))
	{
		if (type == TAG_PACKAGEREF)
			result = JAVAR_PACKAGE_IMPORTED;
	}

	return result;
}

static const char *tagName (const tagType type)
{
	const char* result;
	if (isInputLanguage (Lang_csharp))
		result = CsharpKinds [csharpTagKind (type)].name;
	else if (isInputLanguage (Lang_java))
		result = JavaKinds [javaTagKind (type)].name;
	else if (isInputLanguage (Lang_d))
		result = DKinds [dTagKind (type)].name;
	else if (isInputLanguage (Lang_vera))
		result = VeraKinds [veraTagKind (type)].name;
	else
		result = CKinds [cTagKind (type)].name;
	return result;
}

static bool includeTag (const tagType type, const bool isFileScope)
{
	bool result;
	int k;

	if (isFileScope && !isXtagEnabled(XTAG_FILE_SCOPE))
		return false;
	else if (isInputLanguage (Lang_csharp))
		k = csharpTagKindNoAssert (type);
	else if (isInputLanguage (Lang_java))
		k = javaTagKindNoAssert (type);
	else if (isInputLanguage (Lang_d))
		k = dTagKindNoAssert (type);
	else if (isInputLanguage (Lang_vera))
		k = veraTagKindNoAssert (type);
	else
		k = cTagKindNoAssert (type);

	if (k == COMMONK_UNDEFINED)
		result = false;
	else
		result = isInputLanguageKindEnabled (k);

	return result;
}

static tagType declToTagType (const declType declaration)
{
	tagType type = TAG_UNDEFINED;

	switch (declaration)
	{
		case DECL_CLASS:        type = TAG_CLASS;       break;
		case DECL_ENUM:         type = TAG_ENUM;        break;
		case DECL_EVENT:        type = TAG_EVENT;       break;
		case DECL_FUNCTION:     type = TAG_FUNCTION;    break;
		case DECL_FUNCTION_TEMPLATE: type = TAG_FUNCTION; break;
		case DECL_INTERFACE:    type = TAG_INTERFACE;   break;
		case DECL_NAMESPACE:    type = TAG_NAMESPACE;   break;
		case DECL_PROGRAM:      type = TAG_PROGRAM;     break;
		case DECL_PRIVATE:      type = TAG_CLASS;       break;
		case DECL_PROTECTED:    type = TAG_CLASS;       break;
		case DECL_PUBLIC:       type = TAG_CLASS;       break;
		case DECL_TASK:         type = TAG_TASK;        break;
		case DECL_TEMPLATE: 	type = TAG_TEMPLATE; 	break;
		case DECL_STRUCT:       type = TAG_STRUCT;      break;
		case DECL_UNION:        type = TAG_UNION;       break;
		case DECL_VERSION: 		type = TAG_VERSION; 	break;
		case DECL_ANNOTATION:   type = TAG_ANNOTATION;  break;

		default: Assert ("Unexpected declaration" == NULL); break;
	}
	return type;
}

static const char* accessField (const statementInfo *const st)
{
	const char* result = NULL;
	if (isInputLanguage (Lang_cpp)  &&  st->scope == SCOPE_FRIEND)
		result = "friend";
	else if (st->member.access != ACCESS_UNDEFINED)
		result = accessString (st->member.access);
	return result;
}

static void addContextSeparator (vString *const scope)
{
	if (isInputLanguage (Lang_c)  ||  isInputLanguage (Lang_cpp))
		vStringCatS (scope, "::");
	else if (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp) || isInputLanguage(Lang_d))
		vStringPut (scope, '.');
}

static void addOtherFields (tagEntryInfo* const tag, const tagType type,
							const statementInfo *const st,
							vString *const scope, vString *const typeRef)
{
	/*  For selected tag types, append an extension flag designating the
	 *  parent object in which the tag is defined.
	 */
	switch (type)
	{
		default: break;

		case TAG_FUNCTION:
		case TAG_TEMPLATE:
		case TAG_METHOD:
		case TAG_PROTOTYPE:
			if (vStringLength (Signature) > 0)
				tag->extensionFields.signature = vStringValue (Signature);
		case TAG_CLASS:
		case TAG_ENUM:
		case TAG_ENUMERATOR:
		case TAG_EVENT:
		case TAG_FIELD:
		case TAG_INTERFACE:
		case TAG_MEMBER:
		case TAG_NAMESPACE:
		case TAG_PROPERTY:
		case TAG_SIGNAL:
		case TAG_STRUCT:
		case TAG_TASK:
		case TAG_TYPEDEF:
		case TAG_UNION:
		case TAG_ANNOTATION:
			if (vStringLength (scope) > 0  &&
				(isMember (st) || st->parent->declaration == DECL_NAMESPACE))
			{
				tagType ptype;

				if (isType (st->context, TOKEN_NAME))
				{
					tag->extensionFields.scopeKindIndex = kindIndexForType (TAG_CLASS);
					tag->extensionFields.scopeName = vStringValue (scope);
				}
				else if ((ptype = declToTagType (parentDecl (st))) &&
					 includeTag (ptype, isXtagEnabled(XTAG_FILE_SCOPE)))
				{
					tag->extensionFields.scopeKindIndex = kindIndexForType (ptype);
					tag->extensionFields.scopeName = vStringValue (scope);
				}
			}
			if ((type == TAG_CLASS  ||  type == TAG_INTERFACE  ||
				 type == TAG_STRUCT || type == TAG_ANNOTATION) && vStringLength (st->parentClasses) > 0)
			{

				tag->extensionFields.inheritance =
						vStringValue (st->parentClasses);
			}
			if (st->implementation != IMP_DEFAULT &&
				(isInputLanguage (Lang_cpp) || isInputLanguage (Lang_csharp) ||
				 isInputLanguage (Lang_d) || isInputLanguage (Lang_java)))
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

	/* Add typename info, type of the tag and name of struct/union/etc. */
	if ((type == TAG_TYPEDEF || type == TAG_VARIABLE || type == TAG_MEMBER)
			&& isContextualStatement(st))
	{
		char *p;

		tag->extensionFields.typeRef [0] =
						tagName (declToTagType (st->declaration));
		p = vStringValue (st->blockName->name);

		/*  If there was no {} block get the name from the token before the
		 *  name (current token is ';' or ',', previous token is the name).
		 */
		if (p == NULL || *p == '\0')
		{
			tokenInfo *const prev2 = prevToken (st, 2);
			if (isType (prev2, TOKEN_NAME))
				p = vStringValue (prev2->name);
		}

		/* Prepend the scope name if there is one. */
		if (vStringLength (scope) > 0)
		{
			vStringCopy(typeRef, scope);
			addContextSeparator (typeRef);
			vStringCatS(typeRef, p);
			p = vStringValue (typeRef);
		}
		tag->extensionFields.typeRef [1] = p;
	}
}

static bool findScopeHierarchy (vString *const string, const statementInfo *const st)
{
	bool found = false;

	vStringClear (string);

	if (isType (st->context, TOKEN_NAME))
	{
		vStringCopy (string, st->context->name);
		found = true;
	}

	if (st->parent != NULL)
	{
		vString *temp = vStringNew ();
		const statementInfo *s;
		for (s = st->parent  ;  s != NULL  ;  s = s->parent)
		{
			if (isContextualStatement (s) ||
				s->declaration == DECL_NAMESPACE ||
				s->declaration == DECL_PROGRAM)
			{
				if (s->declaration == DECL_PRIVATE ||
					s->declaration == DECL_PROTECTED ||
					s->declaration == DECL_PUBLIC) {
					continue;
				}

				found = true;
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
					if (vStringLength (temp) > 0)
						addContextSeparator (string);
					vStringCat (string, temp);
				}
				else
				{
					/* Information for building scope string
					   is lacking. Maybe input is broken. */
					found = false;
				}
			}
		}
		vStringDelete (temp);
	}
	return found;
}

static void makeExtraTagEntry (const tagType type, tagEntryInfo *const e,
							   vString *const scope)
{
	if (isXtagEnabled(XTAG_QUALIFIED_TAGS)  &&
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
			markTagExtraBit (e, XTAG_QUALIFIED_TAGS);
			makeTagEntry (e);
		}
		vStringDelete (scopedName);
	}
}

static int makeTag (const tokenInfo *const token,
					 const statementInfo *const st,
					 bool isFileScope, const tagType type)
{
	int corkIndex = CORK_NIL;
	/*  Nothing is really of file scope when it appears in a header file.
	 */
	isFileScope = (bool) (isFileScope && ! isInputHeaderFile ());

	if (isType (token, TOKEN_NAME)  &&  vStringLength (token->name) > 0  &&
		includeTag (type, isFileScope))
	{
		vString *scope;
		vString *typeRef;
		bool isScopeBuilt;
		/* Use "typeRef" to store the typename from addOtherFields() until
		 * it's used in makeTagEntry().
		 */
		tagEntryInfo e;
		int kind;
		int role;

		role = roleForType (type);
		if (! (role == ROLE_DEFINITION_INDEX || isXtagEnabled (XTAG_REFERENCE_TAGS)))
			return CORK_NIL;

		scope  = vStringNew ();
		typeRef = vStringNew ();

		kind  = kindIndexForType(type);
		if (role == ROLE_DEFINITION_INDEX)
			initTagEntry (&e, vStringValue (token->name), kind);
		else
			initRefTagEntry (&e, vStringValue (token->name), kind, role);

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;
		e.isFileScope	= isFileScope;
		if (e.isFileScope)
			markTagExtraBit (&e, XTAG_FILE_SCOPE);

		isScopeBuilt = findScopeHierarchy (scope, st);
		addOtherFields (&e, type, st, scope, typeRef);

		corkIndex = makeTagEntry (&e);
		if (isScopeBuilt)
			makeExtraTagEntry (type, &e, scope);
		vStringDelete (scope);
		vStringDelete (typeRef);
	}
	return corkIndex;
}

static bool isValidTypeSpecifier (const declType declaration)
{
	bool result;
	switch (declaration)
	{
		case DECL_BASE:
		case DECL_CLASS:
		case DECL_ENUM:
		case DECL_EVENT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ANNOTATION:
			result = true;
			break;

		default:
			result = false;
			break;
	}
	return result;
}

static int qualifyEnumeratorTag (const statementInfo *const st,
								 const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	if (isType (nameToken, TOKEN_NAME))
		corkIndex = makeTag (nameToken, st, true, TAG_ENUMERATOR);
	return corkIndex;
}

static int qualifyFunctionTag (const statementInfo *const st,
								const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	if (isType (nameToken, TOKEN_NAME))
	{
		tagType type;
		const bool isFileScope =
						(bool) (st->member.access == ACCESS_PRIVATE ||
						(!isMember (st)  &&  st->scope == SCOPE_STATIC));
		if (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp))
			type = TAG_METHOD;
		else if (isInputLanguage (Lang_vera)  &&  st->declaration == DECL_TASK)
			type = TAG_TASK;
		else
			type = TAG_FUNCTION;
		corkIndex = makeTag (nameToken, st, isFileScope, type);
	}
	return corkIndex;
}

static int qualifyFunctionDeclTag (const statementInfo *const st,
									const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	if (! isType (nameToken, TOKEN_NAME))
		;
	else if (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp))
		corkIndex = qualifyFunctionTag (st, nameToken);
	else if (st->scope == SCOPE_TYPEDEF)
		corkIndex = makeTag (nameToken, st, true, TAG_TYPEDEF);
	else if (isValidTypeSpecifier (st->declaration) && ! isInputLanguage (Lang_csharp))
		corkIndex = makeTag (nameToken, st, true, TAG_PROTOTYPE);
	return corkIndex;
}

static int qualifyCompoundTag (const statementInfo *const st,
								const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	if (isType (nameToken, TOKEN_NAME))
	{
		const tagType type = declToTagType (st->declaration);
		const bool fileScoped = (bool)
				(!(isInputLanguage (Lang_java) ||
				   isInputLanguage (Lang_csharp) ||
				   isInputLanguage (Lang_vera)));

		if (type != TAG_UNDEFINED)
			corkIndex = makeTag (nameToken, st, fileScoped, type);
	}
	return corkIndex;
}

static int qualifyBlockTag (statementInfo *const st,
							 const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	switch (st->declaration)
	{

		case DECL_CLASS:
		case DECL_ENUM:
		case DECL_INTERFACE:
		case DECL_NAMESPACE:
		case DECL_PROGRAM:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TEMPLATE:
		case DECL_VERSION:
		case DECL_ANNOTATION:
			corkIndex = qualifyCompoundTag (st, nameToken);
			break;
		default: break;
	}
	return corkIndex;
}

static int qualifyVariableTag (const statementInfo *const st,
								const tokenInfo *const nameToken)
{
	int corkIndex = CORK_NIL;
	/*	We have to watch that we do not interpret a declaration of the
	 *	form "struct tag;" as a variable definition. In such a case, the
	 *	token preceding the name will be a keyword.
	 */
	if (! isType (nameToken, TOKEN_NAME))
		;
	else if (st->scope == SCOPE_TYPEDEF)
		corkIndex = makeTag (nameToken, st, true, TAG_TYPEDEF);
	else if (st->declaration == DECL_EVENT)
		corkIndex = makeTag (nameToken, st, (bool) (st->member.access == ACCESS_PRIVATE),
							 TAG_EVENT);
	else if (st->declaration == DECL_PACKAGE)
		corkIndex = makeTag (nameToken, st, false, TAG_PACKAGE);
	else if (st->declaration == DECL_PACKAGEREF)
		corkIndex = makeTag (nameToken, st, false, TAG_PACKAGEREF);
	else if (st->declaration == DECL_USING && st->assignment)
		corkIndex = makeTag (nameToken, st, true, TAG_TYPEDEF);
	else if (isValidTypeSpecifier (st->declaration))
	{
		if (st->notVariable)
			;
		else if (isMember (st))
		{
			if (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp))
				corkIndex = makeTag (nameToken, st,
									 (bool) (st->member.access == ACCESS_PRIVATE), TAG_FIELD);
			else if (st->scope == SCOPE_GLOBAL  ||  st->scope == SCOPE_STATIC)
				corkIndex = makeTag (nameToken, st, true, TAG_MEMBER);
		}
		else
		{
			if (st->scope == SCOPE_EXTERN  ||  ! st->haveQualifyingName)
				corkIndex = makeTag (nameToken, st, false, TAG_EXTERN_VAR);
			else if (st->inFunction)
				corkIndex = makeTag (nameToken, st, (bool) (st->scope == SCOPE_STATIC),
									 TAG_LOCAL);
			else
				corkIndex = makeTag (nameToken, st, (bool) (st->scope == SCOPE_STATIC),
									 TAG_VARIABLE);
		}
	}
	return corkIndex;
}

/*
*   Parsing functions
*/

static int skipToOneOf (const char *const chars)
{
	int c;
	do
		c = cppGetc ();
	while (c != EOF  &&  c != '\0'  &&  strchr (chars, c) == NULL);
	return c;
}

/*  Skip to the next non-white character.
 */
static int skipToNonWhite (void)
{
	bool found = false;
	int c;

#if 0
	do
		c = cppGetc ();
	while (cppIsspace (c));
#else
	while (1)
	{
		c = cppGetc ();
		if (cppIsspace (c))
			found = true;
		else
			break;
	}
	if (CollectingSignature && found)
		vStringPut (Signature, ' ');
#endif

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
	const bool braceMatching = (bool) (strcmp ("{}", pair) == 0);
	const bool braceFormatting = (bool) (cppIsBraceFormat () && braceMatching);
	const unsigned int initialLevel = cppGetDirectiveNestLevel ();
	const int begin = pair [0], end = pair [1];
	const unsigned long inputLineNumber = getInputLineNumber ();
	int matchLevel = 1;
	int c = '\0';

	while (matchLevel > 0  &&  (c = skipToNonWhite ()) != EOF)
	{
		if (CollectingSignature)
			vStringPut (Signature, c);
		if (c == begin)
		{
			++matchLevel;
			if (braceFormatting  &&  cppGetDirectiveNestLevel () != initialLevel)
			{
				skipToFormattedBraceMatch ();
				break;
			}
		}
		else if (c == end)
		{
			--matchLevel;
			if (braceFormatting  &&  cppGetDirectiveNestLevel () != initialLevel)
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

static void skipCppTemplateParameterList (void)
{
	const unsigned long inputLineNumber = getInputLineNumber ();
	int angleBracketsLevel = 1;
	int c = '\0';

	int roundBracketsLevel = 0;
	bool defaultValueExpected = false;

	while (angleBracketsLevel > 0  &&  (c = skipToNonWhite ()) != EOF)
	{
		if (CollectingSignature)
			vStringPut (Signature, c);

		if (c == '<')
		{
			int x = cppGetc ();
			if(x != '<')
			{
				cppUngetc (x);
				if (roundBracketsLevel == 0)
				{
					if (defaultValueExpected == false)
						++angleBracketsLevel;
				}
			}
			else if(CollectingSignature)
				vStringPut (Signature, x);
		}
		else if (c == '>')
		{
			int x = cppGetc ();
			if( x != '>')
			{
				cppUngetc (x);
				if (roundBracketsLevel == 0)
				{
					--angleBracketsLevel;
					defaultValueExpected = false;
				}
			}
			else if(CollectingSignature)
				vStringPut (Signature, x);
		}
		else if (c == '(')
			roundBracketsLevel ++;
		else if (c == ')')
			roundBracketsLevel --;
		else if (c == '=' && (roundBracketsLevel == 0))
			defaultValueExpected = true;
		else if (c == ',' && (roundBracketsLevel == 0))
			defaultValueExpected = false;
	}

	if (c == EOF)
	{
		verbose ("%s: failed to find match for '%c' at line %lu\n",
				getInputFileName (), '<', inputLineNumber);
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
	const keywordId id = (keywordId) lookupKeyword (name, getInputLanguage ());
	return id;
}

static void analyzeIdentifier (tokenInfo *const token)
{
	const char * name = vStringValue (token->name);

	vString * replacement = NULL;

	if(!isInputLanguage(Lang_java))
	{
		// C: check for ignored token
		// (FIXME: java doesn't support -I... but maybe it should?)
		const cppMacroInfo * macro = cppFindMacro(name);

		if(macro)
		{
			if(macro->hasParameterList)
			{
				// This old parser does not support macro parameters: we simply assume them to be empty
				int c = skipToNonWhite ();

				if (c == '(')
					skipToMatch ("()");
			}

			if(macro->replacements)
			{
				// There is a replacement: analyze it
				replacement = cppBuildMacroReplacement(macro,NULL,0);
				name = replacement ? vStringValue(replacement) : NULL;
			} else {
				// There is no replacement: just ignore
				name = NULL;
			}
		}
	}

	if(!name)
	{
		initToken(token);
		if(replacement)
			vStringDelete(replacement);
		return;
	}

	token->keyword = analyzeKeyword (name);

	if (token->keyword == KEYWORD_NONE)
		token->type = TOKEN_NAME;
	else
		token->type = TOKEN_KEYWORD;

	if(replacement)
		vStringDelete(replacement);
}

static void readIdentifier (tokenInfo *const token, const int firstChar)
{
	vString *const name = token->name;
	int c = firstChar;
	bool first = true;

	initToken (token);

	/* Bug #1585745: strangely, C++ destructors allow whitespace between
	 * the ~ and the class name. */
	if (isInputLanguage (Lang_cpp) && firstChar == '~')
	{
		vStringPut (name, c);
		c = skipToNonWhite ();
	}

	do
	{
		vStringPut (name, c);
		if (CollectingSignature)
		{
			if (!first)
				vStringPut (Signature, c);
			first = false;
		}
		c = cppGetc ();
	} while (cppIsident (c) || ((isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp)) && (isHighChar (c) || c == '.')));
	cppUngetc (c);        /* unget non-identifier character */

	analyzeIdentifier (token);
}

static void readPackageName (tokenInfo *const token, const int firstChar, bool allowWildCard)
{
	vString *const name = token->name;
	int c = firstChar;

	initToken (token);

	while (cppIsident (c)  || (allowWildCard && (c == '*')) ||  c == '.')
	{
		vStringPut (name, c);
		c = cppGetc ();
	}
	cppUngetc (c);        /* unget non-package character */
}

static void readPackageOrNamespace (statementInfo *const st, const declType declaration, bool allowWildCard)
{
	st->declaration = declaration;

	if (declaration == DECL_NAMESPACE && !isInputLanguage (Lang_csharp))
	{
		/* In C++ a namespace is specified one level at a time. */
		return;
	}
	else
	{
		/* In C#, a namespace can also be specified like a Java package name. */
		tokenInfo *const token = activeToken (st);
		Assert (isType (token, TOKEN_KEYWORD));
		readPackageName (token, skipToNonWhite (), allowWildCard);
		token->type = TOKEN_NAME;
		st->gotName = true;
		st->haveQualifyingName = true;
	}
}

static void readVersionName (tokenInfo *const token, const int firstChar)
{
	vString *const name = token->name;
	int c = firstChar;

	initToken (token);

	while (cppIsident (c))
	{
		vStringPut (name, c);
		c = cppGetc ();
	}
    cppGetc ();
}

static void readVersion (statementInfo *const st)
{
    tokenInfo *const token = activeToken (st);
	Assert (isType (token, TOKEN_KEYWORD));
    skipToNonWhite ();
	readVersionName (token, cppGetc ());
	token->type = TOKEN_NAME;
	st->declaration = DECL_VERSION;
	st->gotName = true;
	st->haveQualifyingName = true;
}

static void processName (statementInfo *const st)
{
	Assert (isType (activeToken (st), TOKEN_NAME));
	if (st->gotName  &&  st->declaration == DECL_NONE)
		st->declaration = DECL_BASE;
	st->gotName = true;
	st->haveQualifyingName = true;
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
		;        /* ignore "operator" keyword if preceded by these keywords */
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
	else if (cppIsident1 (c))
	{
		/*  Handle "new" and "delete" operators, and conversion functions
		 *  (per 13.3.1.1.2 [2] of the C++ spec).
		 */
		bool whiteSpace = true;  /* default causes insertion of space */
		do
		{
			if (cppIsspace (c))
				whiteSpace = true;
			else
			{
				if (whiteSpace)
				{
					vStringPut (name, ' ');
					whiteSpace = false;
				}
				vStringPut (name, c);
			}
			c = cppGetc ();
		} while (! isOneOf (c, "(;")  &&  c != EOF);
	}
	else if (isOneOf (c, acceptable))
	{
		vStringPut (name, ' ');  /* always separate operator from keyword */
		do
		{
			vStringPut (name, c);
			c = cppGetc ();
		} while (isOneOf (c, acceptable));
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
	if (isInputLanguage (Lang_d))
	{
		int c = skipToNonWhite ();

		if (c == '{')
		{
			switch(access)
			{
				case ACCESS_PRIVATE:
					st->declaration = DECL_PRIVATE;
					break;
				case ACCESS_PUBLIC:
					st->declaration = DECL_PUBLIC;
					break;
				case ACCESS_PROTECTED:
					st->declaration = DECL_PROTECTED;
					break;
				default:
					break;
			}
			st->member.access = access;
			cppUngetc (c);
		}
		else if (c == ':') {
			reinitStatement (st, false);
			st->member.accessDefault = access;
		}
		else {
			cppUngetc (c);
		}
	}

	if (isMember (st))
	{
		if (isInputLanguage (Lang_cpp))
		{
			int c = skipToNonWhite ();

			if (c == ':')
				reinitStatement (st, false);
			else
				cppUngetc (c);

			st->member.accessDefault = access;
		}
		else if (isInputLanguage (Lang_d))
		{
			if (st->parent != NULL &&
				(st->parent->declaration == DECL_PRIVATE ||
				st->parent->declaration == DECL_PROTECTED ||
				st->parent->declaration == DECL_PUBLIC))
			{
				st->member.access = st->parent->member.access;
				return;
			}
		}
		st->member.access = access;
	}
}

static void discardTypeList (tokenInfo *const token)
{
	int c = skipToNonWhite ();
	while (cppIsident1 (c))
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
		if (cppIsident1 (c))
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

static void skipStatement (statementInfo *const st)
{
	st->declaration = DECL_IGNORE;
	skipToOneOf (";");
}

static void processAnnotation (statementInfo *const st)
{
	st->declaration = DECL_ANNOTATION;
}

static void processInterface (statementInfo *const st)
{
	st->declaration = DECL_INTERFACE;
}

static void checkIsClassEnum (statementInfo *const st, const declType decl)
{
	if (! isInputLanguage (Lang_cpp) || st->declaration != DECL_ENUM)
		st->declaration = decl;
}

static void processToken (tokenInfo *const token, statementInfo *const st)
{
	switch ((int)token->keyword)        /* is it a reserved word? */
	{
		default: break;

		case KEYWORD_NONE:      processName (st);                       break;
		case KEYWORD_ABSTRACT:  st->implementation = IMP_ABSTRACT;      break;
		case KEYWORD_ATTRIBUTE: skipParens (); initToken (token);       break;
		case KEYWORD_BIND:      st->declaration = DECL_BASE;            break;
		case KEYWORD_BIT:       st->declaration = DECL_BASE;            break;
		case KEYWORD_CATCH:     skipParens (); skipBraces ();           break;
		case KEYWORD_CHAR:      st->declaration = DECL_BASE;            break;
		case KEYWORD_CLASS:     checkIsClassEnum (st, DECL_CLASS);      break;
		case KEYWORD_CONST:     st->declaration = DECL_BASE;            break;
		case KEYWORD_DOUBLE:    st->declaration = DECL_BASE;            break;
		case KEYWORD_ENUM:      st->declaration = DECL_ENUM;            break;
		case KEYWORD_EXTENDS:   readParents (st, '.');
		                        setToken (st, TOKEN_NONE);              break;
		case KEYWORD_FLOAT:     st->declaration = DECL_BASE;            break;
		case KEYWORD_FUNCTION:  st->declaration = DECL_BASE;            break;
		case KEYWORD_FRIEND:    st->scope       = SCOPE_FRIEND;         break;
		case KEYWORD_GOTO:      skipStatement (st);                     break;
		case KEYWORD_IMPLEMENTS:readParents (st, '.');
		                        setToken (st, TOKEN_NONE);              break;
		case KEYWORD_IMPORT:
			if (isInputLanguage (Lang_java))
				readPackageOrNamespace (st, DECL_PACKAGEREF, true);
			else
				skipStatement (st);
			break;
		case KEYWORD_INT:       st->declaration = DECL_BASE;            break;
		case KEYWORD_INTEGER:   st->declaration = DECL_BASE;            break;
		case KEYWORD_INTERFACE: processInterface (st);                  break;
		case KEYWORD_LOCAL:     setAccess (st, ACCESS_LOCAL);           break;
		case KEYWORD_LONG:      st->declaration = DECL_BASE;            break;
		case KEYWORD_OPERATOR:  readOperator (st);                      break;
		case KEYWORD_MIXIN:     st->declaration = DECL_MIXIN;           break;
		case KEYWORD_PRIVATE:   setAccess (st, ACCESS_PRIVATE);         break;
		case KEYWORD_PROGRAM:   st->declaration = DECL_PROGRAM;         break;
		case KEYWORD_PROTECTED: setAccess (st, ACCESS_PROTECTED);       break;
		case KEYWORD_PUBLIC:    setAccess (st, ACCESS_PUBLIC);          break;
		case KEYWORD_RETURN:    skipStatement (st);                     break;
		case KEYWORD_SHORT:     st->declaration = DECL_BASE;            break;
		case KEYWORD_SIGNED:    st->declaration = DECL_BASE;            break;
		case KEYWORD_STRING:    st->declaration = DECL_BASE;            break;
		case KEYWORD_STRUCT:    checkIsClassEnum (st, DECL_STRUCT);     break;
		case KEYWORD_TASK:      st->declaration = DECL_TASK;            break;
		case KEYWORD_THROWS:    discardTypeList (token);                break;
		case KEYWORD_UNION:     st->declaration = DECL_UNION;           break;
		case KEYWORD_UNSIGNED:  st->declaration = DECL_BASE;            break;
		case KEYWORD_USING:     st->declaration = DECL_USING;           break;
		case KEYWORD_VOID:      st->declaration = DECL_BASE;            break;
		case KEYWORD_VOLATILE:  st->declaration = DECL_BASE;            break;
		case KEYWORD_VERSION:   readVersion(st);                        break;
		case KEYWORD_VIRTUAL:   st->implementation = IMP_VIRTUAL;       break;
		case KEYWORD_WCHAR_T:   st->declaration = DECL_BASE;            break;
		case KEYWORD_TEMPLATE:
			if (isInputLanguage (Lang_d))
				st->declaration = DECL_TEMPLATE;
			break;
		case KEYWORD_NAMESPACE: readPackageOrNamespace (st, DECL_NAMESPACE, false); break;
		case KEYWORD_MODULE:
		case KEYWORD_PACKAGE:   readPackageOrNamespace (st, DECL_PACKAGE, false);   break;

		case KEYWORD_EVENT:
			if (isInputLanguage (Lang_csharp))
				st->declaration = DECL_EVENT;
			break;

		case KEYWORD_ALIAS:
		case KEYWORD_TYPEDEF:
			reinitStatement (st, false);
			st->scope = SCOPE_TYPEDEF;
			break;

		case KEYWORD_EXTERN:
			if (! isInputLanguage (Lang_csharp) || !st->gotName)
			{
				reinitStatement (st, false);
				st->scope = SCOPE_EXTERN;
				st->declaration = DECL_BASE;
			}
			break;

		case KEYWORD_STATIC:
			if (! (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp)))
			{
				reinitStatement (st, false);
				st->scope = SCOPE_STATIC;
				st->declaration = DECL_BASE;
			}
			break;

		case KEYWORD_FOR:
		case KEYWORD_FOREACH:
		case KEYWORD_IF:
		case KEYWORD_SWITCH:
		case KEYWORD_WHILE:
		{
			int c = skipToNonWhite ();
			if (c == '(')
				skipToMatch ("()");
			break;
		}
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
	reinitStatement (st, false);
	token = activeToken (st);
	copyToken (token, save);
	deleteToken (save);
	processToken (token, st);
}

/*  Skips over a mem-initializer-list of a ctor-initializer, defined as:
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
		while (cppIsident1 (c)  ||  c == ':')
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
static bool skipPostArgumentStuff (
		statementInfo *const st, parenInfo *const info)
{
	tokenInfo *const token = activeToken (st);
	unsigned int parameters = info->parameterCount;
	unsigned int elementCount = 0;
	bool restart = false;
	bool end = false;
	int c = skipToNonWhite ();

	do
	{
		switch (c)
		{
		case ')':                               break;
		case ':': skipMemIntializerList (token);break;  /* ctor-initializer */
		case '[': skipToMatch ("[]");           break;
		case '=': cppUngetc (c); end = true;    break;
		case '{': cppUngetc (c); end = true;    break;
		case '}': cppUngetc (c); end = true;    break;

		case '(':
			if (elementCount > 0)
				++elementCount;
			skipToMatch ("()");
			break;

		case ';':
			if (parameters == 0  ||  elementCount < 2)
			{
				cppUngetc (c);
				end = true;
			}
			else if (--parameters == 0)
				end = true;
			break;

		default:
			if (cppIsident1 (c))
			{
				readIdentifier (token, c);
				switch (token->keyword)
				{
				case KEYWORD_ATTRIBUTE: skipParens ();  break;
				case KEYWORD_THROW:     skipParens ();  break;
				case KEYWORD_IF:        if (isInputLanguage (Lang_d)) skipParens ();  break;
				case KEYWORD_TRY:                       break;
				case KEYWORD_NOEXCEPT:                  break;

				case KEYWORD_CONST:
				case KEYWORD_VOLATILE:
					if (vStringLength (Signature) > 0)
					{
						vStringPut (Signature, ' ');
						vStringCat (Signature, token->name);
					}
					break;
				case KEYWORD_ALIAS:
				case KEYWORD_CATCH:
				case KEYWORD_CLASS:
				case KEYWORD_EXPLICIT:
				case KEYWORD_EXTERN:
				case KEYWORD_FRIEND:
				case KEYWORD_INLINE:
				case KEYWORD_MUTABLE:
				case KEYWORD_NAMESPACE:
				case KEYWORD_NEW:
				case KEYWORD_NEWCOV:
				case KEYWORD_OPERATOR:
				case KEYWORD_OVERLOAD:
				case KEYWORD_PRIVATE:
				case KEYWORD_PROTECTED:
				case KEYWORD_PUBLIC:
				case KEYWORD_STATIC:
				case KEYWORD_TEMPLATE:
				case KEYWORD_TYPEDEF:
				case KEYWORD_TYPENAME:
				case KEYWORD_USING:
				case KEYWORD_VIRTUAL:
					/* Never allowed within parameter declarations. */
					restart = true;
					end = true;
					break;

				default:
					/* "override" and "final" are only keywords in the declaration of a virtual
					 * member function, so need to be handled specially, not as keywords */
					if (isInputLanguage(Lang_cpp) && isType (token, TOKEN_NAME) &&
						(strcmp ("override", vStringValue (token->name)) == 0 ||
						 strcmp ("final", vStringValue (token->name)) == 0))
						;
					else if (isType (token, TOKEN_NONE))
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
						restart = true;
						end = true;
					}
					break;
				}
			}
		}
		if (! end)
		{
			c = skipToNonWhite ();
			if (c == EOF)
				end = true;
		}
	} while (! end);

	if (restart)
		restartStatement (st);
	else
		setToken (st, TOKEN_NONE);

	return (bool) (c != EOF);
}

static void skipJavaThrows (statementInfo *const st)
{
	tokenInfo *const token = activeToken (st);
	int c = skipToNonWhite ();

	if (cppIsident1 (c))
	{
		readIdentifier (token, c);
		if (token->keyword == KEYWORD_THROWS)
		{
			do
			{
				c = skipToNonWhite ();
				if (cppIsident1 (c))
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
	else if (isInputLanguage (Lang_java)) {

		if (!insideAnnotationBody(st)) {
			skipJavaThrows (st);
		}
	} else {
		if (! skipPostArgumentStuff (st, info))
		{
			verbose (
				"%s: confusing argument declarations beginning at line %lu\n",
				getInputFileName (), inputLineNumber);
			longjmp (Exception, (int) ExceptionFormattingError);
		}
	}
}

static bool languageSupportsGenerics (void)
{
	return (bool) (isInputLanguage (Lang_cpp) || isInputLanguage (Lang_csharp) ||
		isInputLanguage (Lang_java));
}

static void processAngleBracket (void)
{
	int c = cppGetc ();
	if (c == '>') {
		/* already found match for template */
	} else if (languageSupportsGenerics () && c != '<' && c != '=') {
		/* this is a template */
		cppUngetc (c);
		if (isInputLanguage (Lang_cpp))
			skipCppTemplateParameterList ();
		else
			skipToMatch ("<>");
	} else if (c == '<') {
		/* skip "<<" or "<<=". */
		c = cppGetc ();
		if (c != '=') {
			cppUngetc (c);
		}
	} else {
		cppUngetc (c);
	}
}

static void parseJavaAnnotation (statementInfo *const st)
{
	/*
	 * @Override
	 * @Target(ElementType.METHOD)
	 * @SuppressWarnings(value = "unchecked")
	 *
	 * But watch out for "@interface"!
	 */
	tokenInfo *const token = activeToken (st);

	int c = skipToNonWhite ();
	readIdentifier (token, c);
	if (token->keyword == KEYWORD_INTERFACE)
	{
		/* Oops. This was actually "@interface" defining a new annotation. */
		processAnnotation(st);
	}
	else
	{
		/* Bug #1691412: skip any annotation arguments. */
		skipParens ();
	}
}

static int parseParens (statementInfo *const st, parenInfo *const info)
{
	tokenInfo *const token = activeToken (st);
	unsigned int identifierCount = 0;
	unsigned int depth = 1;
	bool firstChar = true;
	int nextChar = '\0';

	CollectingSignature = true;
	vStringClear (Signature);
	vStringPut (Signature, '(');
	info->parameterCount = 1;
	do
	{
		int c = skipToNonWhite ();
		vStringPut (Signature, c);

		switch (c)
		{
			case '^':
				break;

			case '&':
			case '*':
				info->isPointer = true;
				info->isKnrParamList = false;
				if (identifierCount == 0)
					info->isParamList = false;
				initToken (token);
				break;

			case ':':
				info->isKnrParamList = false;
				break;

			case '.':
				info->isNameCandidate = false;
				c = cppGetc ();
				if (c != '.')
				{
					cppUngetc (c);
					info->isKnrParamList = false;
				}
				else
				{
					c = cppGetc ();
					if (c != '.')
					{
						cppUngetc (c);
						info->isKnrParamList = false;
					}
					else
						vStringCatS (Signature, "..."); /* variable arg list */
				}
				break;

			case ',':
				info->isNameCandidate = false;
				if (info->isKnrParamList)
				{
					++info->parameterCount;
					identifierCount = 0;
				}
				break;

			case '=':
				info->isKnrParamList = false;
				info->isNameCandidate = false;
				if (firstChar)
				{
					info->isParamList = false;
					skipMacro (st);
					depth = 0;
				}
				break;

			case '[':
				info->isKnrParamList = false;
				skipToMatch ("[]");
				break;

			case '<':
				info->isKnrParamList = false;
				processAngleBracket ();
				break;

			case ')':
				if (firstChar)
					info->parameterCount = 0;
				--depth;
				break;

			case '(':
				info->isKnrParamList = false;
				if (firstChar)
				{
					info->isNameCandidate = false;
					cppUngetc (c);
					vStringClear (Signature);
					skipMacro (st);
					depth = 0;
					vStringChop (Signature);
				}
				else if (isType (token, TOKEN_PAREN_NAME))
				{
					c = skipToNonWhite ();
					if (c == '*')        /* check for function pointer */
					{
						skipToMatch ("()");
						c = skipToNonWhite ();
						if (c == '(')
							skipToMatch ("()");
						else
							cppUngetc (c);
					}
					else
					{
						cppUngetc (c);
						cppUngetc ('(');
						info->nestedArgs = true;
					}
				}
				else
					++depth;
				break;

			default:
				if (c == '@' && isInputLanguage (Lang_java))
				{
					parseJavaAnnotation(st);
				}
				else if (cppIsident1 (c))
				{
					if (++identifierCount > 1)
						info->isKnrParamList = false;
					readIdentifier (token, c);
					if (isType (token, TOKEN_NAME)  &&  info->isNameCandidate)
						token->type = TOKEN_PAREN_NAME;
					else if (isType (token, TOKEN_KEYWORD))
					{
						if (token->keyword != KEYWORD_CONST &&
							token->keyword != KEYWORD_VOLATILE)
						{
							info->isKnrParamList = false;
							info->isNameCandidate = false;
						}
					}
				}
				else
				{
					info->isParamList     = false;
					info->isKnrParamList  = false;
					info->isNameCandidate = false;
					info->invalidContents = true;
				}
				break;
		}
		firstChar = false;
	} while (! info->nestedArgs  &&  depth > 0  &&
			 (info->isKnrParamList  ||  info->isNameCandidate));

	if (! info->nestedArgs) while (depth > 0)
	{
		skipToMatch ("()");
		--depth;
	}

	if (! info->isNameCandidate)
		initToken (token);

	if (info->isKnrParamList)
		vStringClear (Signature);
	CollectingSignature = false;
	return nextChar;
}

static void initParenInfo (parenInfo *const info)
{
	info->isPointer				= false;
	info->isParamList			= true;
	info->isKnrParamList		= isInputLanguage (Lang_c);
	info->isNameCandidate		= true;
	info->invalidContents		= false;
	info->nestedArgs			= false;
	info->parameterCount		= 0;
}

static void analyzeParens (statementInfo *const st)
{
	tokenInfo *const prev = prevToken (st, 1);
	const tokenInfo *const prev2 = prevToken (st, 2);

	if (
			st->inFunction &&
			!st->assignment &&
			!(
				/* C++: Accept Type var(...) as variable; */
				isInputLanguage(Lang_cpp) &&
				isType(prev,TOKEN_NAME) &&
				isType(prev2,TOKEN_NAME)
			)
		)
	{
		st->notVariable = true;
	}

	if (! isType (prev, TOKEN_NONE))  /* in case of ignored enclosing macros */
	{
		tokenInfo *const token = activeToken (st);
		parenInfo info;
		int c;

		initParenInfo (&info);
		parseParens (st, &info);
		c = skipToNonWhite ();
		cppUngetc (c);
		if (info.invalidContents)
		{
			/* FIXME: This breaks parsing of variable instantiations that have
			   constants as parameters: Type var(0) or Type var("..."). */
			reinitStatement (st, false);
		}
		else if (info.isNameCandidate  &&  isType (token, TOKEN_PAREN_NAME)  &&
				 ! st->gotParenName  &&
				 (! info.isParamList || ! st->haveQualifyingName  ||
				  c == '('  ||
				  (c == '='  &&  st->implementation != IMP_VIRTUAL && !isInputLanguage (Lang_cpp)) ||
				  (st->declaration == DECL_NONE  &&  isOneOf (c, ",;"))))
		{
			token->type = TOKEN_NAME;
			processName (st);
			st->gotParenName = true;
			if (! (c == '('  &&  info.nestedArgs))
				st->isPointer = info.isPointer;
			if (isInputLanguage(Lang_d) && c == '(' && isType (prev, TOKEN_NAME))
			{
				st->declaration = DECL_FUNCTION_TEMPLATE;
				copyToken (st->blockName, prev);
			}
		}
		else if (! st->gotArgs  &&  info.isParamList)
		{
			st->gotArgs = true;
			setToken (st, TOKEN_ARGS);
			advanceToken (st);
			if (st->scope != SCOPE_TYPEDEF)
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
			if (isInputLanguage (Lang_c)  ||  isInputLanguage (Lang_cpp))
				vStringCatS (st->context->name, "::");
			else if (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp) ||
				isInputLanguage (Lang_d))
				vStringPut (st->context->name, '.');
		}
		vStringCat (st->context->name, token->name);
		st->context->type = TOKEN_NAME;
	}
}

static bool inheritingDeclaration (declType decl)
{
	/* enum base types */
	if (decl == DECL_ENUM)
	{
		return (bool) (isInputLanguage (Lang_cpp) || isInputLanguage (Lang_csharp) ||
			isInputLanguage (Lang_d));
	}
	return (bool) (
		decl == DECL_CLASS ||
		decl == DECL_STRUCT ||
		decl == DECL_INTERFACE);
}

static void processColon (statementInfo *const st)
{
	int c = (isInputLanguage (Lang_cpp) ? cppGetc () : skipToNonWhite ());
	const bool doubleColon = (bool) (c == ':');

	if (doubleColon)
	{
		setToken (st, TOKEN_DOUBLE_COLON);
		st->haveQualifyingName = false;
	}
	else
	{
		cppUngetc (c);
		if ((isInputLanguage (Lang_cpp) || isInputLanguage (Lang_csharp) || isInputLanguage (Lang_d))  &&
			inheritingDeclaration (st->declaration))
		{
			readParents (st, ':');
		}
		else if (parentDecl (st) == DECL_STRUCT)
		{
			c = skipToOneOf (",;");
			if (c == ',')
				setToken (st, TOKEN_COMMA);
			else if (c == ';')
				setToken (st, TOKEN_SEMICOLON);
		}
		else
		{
			const tokenInfo *const prev  = prevToken (st, 1);
			const tokenInfo *const prev2 = prevToken (st, 2);
			if (prev->keyword == KEYWORD_DEFAULT ||
				prev2->keyword == KEYWORD_CASE)
			{
				reinitStatement (st, false);
			}
			else if (st->parent != NULL)
			{
				if (prevToken (st->parent, 1)->keyword != KEYWORD_SWITCH)
					makeTag (prev, st, false, TAG_LABEL);
				reinitStatement (st, false);
			}
		}
	}
}

/*  Skips over any initializing value which may follow an '=' character in a
 *  variable definition.
 */
static int skipInitializer (statementInfo *const st)
{
	bool done = false;
	int c;

	while (! done)
	{
		c = skipToNonWhite ();

		if (c == EOF)
			longjmp (Exception, (int) ExceptionFormattingError);
		else switch (c)
		{
			case ',':
			case ';': done = true; break;

			case '0':
				if (st->implementation == IMP_VIRTUAL)
					st->implementation = IMP_PURE_VIRTUAL;
				break;

			case '[': skipToMatch ("[]"); break;
			case '(': skipToMatch ("()"); break;
			case '{': skipToMatch ("{}"); break;
			case '<': processAngleBracket(); break;

			case '}':
				if (insideEnumBody (st))
					done = true;
				else if (! cppIsBraceFormat ())
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
	const bool inEnumBody = insideEnumBody (st);
	int c = cppGetc ();

	if (c != '=')
	{
		cppUngetc (c);
		c = skipInitializer (st);
		st->assignment = true;
		if (c == ';')
			setToken (st, TOKEN_SEMICOLON);
		else if (c == ',')
			setToken (st, TOKEN_COMMA);
		else if (c == '}'  &&  inEnumBody)
		{
			cppUngetc (c);
			setToken (st, TOKEN_COMMA);
		}
		if (st->scope == SCOPE_EXTERN)
			st->scope = SCOPE_GLOBAL;
	}
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

	if (cppIsident1 (c) || (isInputLanguage (Lang_java) && isHighChar (c)))
	{

		parseIdentifier (st, c);
		if (isType (st->context, TOKEN_NAME) &&
			isType (activeToken (st), TOKEN_NAME) && isType (prev, TOKEN_NAME))
		{
			initToken (st->context);
		}
	}
	else if (c == '.' || c == '-')
	{
		if (! st->assignment)
			st->notVariable = true;
		if (c == '-')
		{
			int c2 = cppGetc ();
			if (c2 != '>')
				cppUngetc (c2);
		}
	}
	else if (c == '!' || c == '>')
	{
		int c2 = cppGetc ();
		if (c2 != '=')
			cppUngetc (c2);
	}
	else if (c == '@' && isInputLanguage (Lang_java))
	{
		parseJavaAnnotation (st);
	}
	else if (isExternCDecl (st, c))
	{
		st->declaration = DECL_NOMANGLE;
		st->scope = SCOPE_GLOBAL;
	} else if (c == STRING_SYMBOL) {
		setToken(st, TOKEN_NONE);
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
			case EOF: longjmp (Exception, (int) ExceptionEOF);  break;
			case '(': analyzeParens (st);                       break;
			case '<': processAngleBracket ();                   break;
			case '*': st->haveQualifyingName = false;           break;
			case ',': setToken (st, TOKEN_COMMA);               break;
			case ':': processColon (st);                        break;
			case ';': setToken (st, TOKEN_SEMICOLON);           break;
			case '=': processInitializer (st);                  break;
			case '[': skipToMatch ("[]");                       break;
			case '{': setToken (st, TOKEN_BRACE_OPEN);          break;
			case '}': setToken (st, TOKEN_BRACE_CLOSE);         break;
			default:  parseGeneralToken (st, c);                break;
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
		deleteToken (st->token [i]);       st->token [i] = NULL;
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

static bool isStatementEnd (const statementInfo *const st)
{
	const tokenInfo *const token = activeToken (st);
	bool isEnd;

	if (isType (token, TOKEN_SEMICOLON))
		isEnd = true;
	else if (isType (token, TOKEN_BRACE_CLOSE))
		/* Java and C# do not require semicolons to end a block. Neither do C++
		 * namespaces. All other blocks require a semicolon to terminate them.
		 */
		isEnd = (bool) (isInputLanguage (Lang_java) || isInputLanguage (Lang_csharp) ||
				 isInputLanguage (Lang_d) || ! isContextualStatement (st));
	else
		isEnd = false;

	return isEnd;
}

static void checkStatementEnd (statementInfo *const st, int corkIndex)
{
	const tokenInfo *const token = activeToken (st);

	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e)
		e->extensionFields.endLine = token->lineNumber;

	if (isType (token, TOKEN_COMMA))
		reinitStatement (st, true);
	else if (isStatementEnd (st))
	{
		DebugStatement ( if (debug (DEBUG_PARSE)) printf ("<ES>"); )
		reinitStatement (st, false);
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
		case DECL_TEMPLATE:
		case DECL_VERSION:
			st->inFunction = false;
		case DECL_CLASS:
		case DECL_ENUM:
		case DECL_INTERFACE:
		case DECL_NAMESPACE:
		case DECL_NOMANGLE:
		case DECL_PRIVATE:
		case DECL_PROTECTED:
		case DECL_PUBLIC:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ANNOTATION:
			createTags (nestLevel, st);
			break;

		case DECL_FUNCTION:
		case DECL_TASK:
			st->inFunction = true;
			/* fall through */
		default:
			if (includeTag (TAG_LOCAL, false) || includeTag (TAG_LABEL, false))
				createTags (nestLevel, st);
			else
				skipToMatch ("{}");
			break;
	}
	advanceToken (st);
	setToken (st, TOKEN_BRACE_CLOSE);
}

static int tagCheck (statementInfo *const st)
{
	const tokenInfo *const token = activeToken (st);
	const tokenInfo *const prev  = prevToken (st, 1);
	const tokenInfo *const prev2 = prevToken (st, 2);
	int corkIndex = CORK_NIL;

	switch (token->type)
	{
		case TOKEN_NAME:
			if (insideEnumBody (st))
				corkIndex = qualifyEnumeratorTag (st, token);
			if (st->declaration == DECL_MIXIN)
				corkIndex = makeTag (token, st, false, TAG_MIXIN);
			if (isInputLanguage (Lang_vera) && insideInterfaceBody (st))
			{
				/* Quoted from
				   http://www.asic-world.com/vera/hdl1.html#Interface_Declaration
				   ------------------------------------------------
				   interface interface_name
				   {
				   signal_direction [signal_width] signal_name signal_type
				   [skew] [depth value][vca q_value][force][hdl_node "hdl_path"];
				   }
				   Where
				   signal_direction : This can be one of the following
				        input : ...
				        output : ...
				        inout : ...
				   signal_width : The signal_width is a range specifying the width of
				                  a vector signal. It must be in the form [msb:lsb].
						  Interface signals can have any integer lsb value,
						  even a negative value. The default width is 1.
				   signal_name : The signal_name identifies the signal being defined.
				                 It is the Vera name for the HDL signal being connected.
				   signal_type : There are many signals types, most commonly used one are
					NHOLD : ...
					PHOLD : ...
					PHOLD NHOLD : ...
					NSAMPLE : ...
					PSAMPLE : ...
					PSAMPLE NSAMPLE : ...
					CLOCK : ...
					PSAMPLE PHOLD : ...
					NSAMPLE NHOLD : ...
					PSAMPLE PHOLD NSAMPLE NHOLD : ...
				   ------------------------------------------------
				   We want to capture "signal_name" here.
				*/
				if (( isType (prev, TOKEN_KEYWORD)
				      && isSignalDirection(prev) ) ||
				    ( isType (prev2, TOKEN_KEYWORD)
				      && isSignalDirection(prev) ))
					corkIndex = makeTag (token, st, false, TAG_SIGNAL);
			}
			break;
#if 0
		case TOKEN_PACKAGE:
			if (st->haveQualifyingName)
				corkIndex = makeTag (token, st, false, TAG_PACKAGE);
			break;
#endif
		case TOKEN_BRACE_OPEN:
			if (isType (prev, TOKEN_ARGS))
			{
				if (st->declaration == DECL_TEMPLATE)
					corkIndex = qualifyBlockTag (st, prev2);
				else if (st->declaration == DECL_FUNCTION_TEMPLATE) {
					corkIndex = qualifyFunctionTag (st, st->blockName);
				}
				else if (st->haveQualifyingName)
				{
					if (isType (prev2, TOKEN_NAME))
						copyToken (st->blockName, prev2);

					/* D declaration templates */
					if (isInputLanguage (Lang_d) &&
						(st->declaration == DECL_CLASS || st->declaration == DECL_STRUCT ||
						st->declaration == DECL_INTERFACE || st->declaration == DECL_UNION))
						corkIndex = qualifyBlockTag (st, prev2);
					else if(isInputLanguage (Lang_cpp) && st->inFunction)
					{
						/* Ignore. C/C++ allows nested function prototypes but
						   this code actually catches far too many of them.
						   Better some missing tags than a lot of false positives. */
					}
					else
					{
						if (! isInputLanguage (Lang_vera))
							st->declaration = DECL_FUNCTION;
						corkIndex = qualifyFunctionTag (st, prev2);
					}
				}
			}
			else if (isContextualStatement (st) ||
					st->declaration == DECL_VERSION ||
					st->declaration == DECL_PROGRAM)
			{
				const tokenInfo *name_token = prev;

				/* C++ 11 allows class <name> final { ... } */
				if (isInputLanguage (Lang_cpp) && isType (prev, TOKEN_NAME) &&
					strcmp("final", vStringValue(prev->name)) == 0 &&
					isType(prev2, TOKEN_NAME))
				{
					name_token = prev2;
				}

				if (isType (name_token, TOKEN_NAME))
					copyToken (st->blockName, name_token);
				else
				{
					/*  For an anonymous struct or union we use a unique ID
					 *  a number, so that the members can be found.
					 */
					char buf [20];  /* length of "_anon" + digits  + null */
					sprintf (buf, "__anon%d", ++AnonymousID);
					vStringCopyS (st->blockName->name, buf);
					st->blockName->type = TOKEN_NAME;
					st->blockName->keyword = KEYWORD_NONE;
				}
				corkIndex = qualifyBlockTag (st, name_token);
			}
			else if (isInputLanguage (Lang_csharp))
				corkIndex = makeTag (prev, st, false, TAG_PROPERTY);
			break;

		case TOKEN_KEYWORD:

			if (token->keyword == KEYWORD_DEFAULT && isType(prev, TOKEN_ARGS) && insideAnnotationBody(st)) {
				corkIndex = qualifyFunctionDeclTag(st, prev2);
			}
			break;

		case TOKEN_SEMICOLON:
		case TOKEN_COMMA:
			if (insideEnumBody (st))
				;
			else if (isType (prev, TOKEN_NAME))
			{
				if (isContextualKeyword (prev2))
					corkIndex = makeTag (prev, st, true, TAG_EXTERN_VAR);
				else
					corkIndex = qualifyVariableTag (st, prev);
			}
			else if (isType (prev, TOKEN_ARGS)  &&  isType (prev2, TOKEN_NAME))
			{
				if (st->isPointer || st->inFunction)
				{
					/* If it looks like a pointer or we are in a function body then
					   it's far more likely to be a variable. */
					corkIndex = qualifyVariableTag (st, prev2);
				}
				else
					corkIndex = qualifyFunctionDeclTag (st, prev2);
			}
			if (isInputLanguage (Lang_java) && token->type == TOKEN_SEMICOLON && insideEnumBody (st))
			{
				/* In Java, after an initial enum-like part,
				 * a semicolon introduces a class-like part.
				 * See Bug #1730485 for the full rationale. */
				st->parent->declaration = DECL_CLASS;
			}
			break;

		default: break;
	}

	return corkIndex;
}

/*  Parses the current file and decides whether to write out and tags that
 *  are discovered.
 */
static void createTags (const unsigned int nestLevel,
						statementInfo *const parent)
{
	statementInfo *const st = newStatement (parent);

	DebugStatement ( if (nestLevel > 0) debugParseNest (true, nestLevel); )
	while (true)
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
			int corkIndex = tagCheck (st);
			if (isType (token, TOKEN_BRACE_OPEN))
				nest (st, nestLevel + 1);
			checkStatementEnd (st, corkIndex);
		}
	}
	deleteStatement ();
	DebugStatement ( if (nestLevel > 0) debugParseNest (false, nestLevel - 1); )
}

static rescanReason findCTags (const unsigned int passCount)
{
	exception_t exception;
	rescanReason rescan;
	int kind_for_define = KIND_GHOST_INDEX;
	int kind_for_header = KIND_GHOST_INDEX;
	int kind_for_param  = KIND_GHOST_INDEX;
	int role_for_macro_undef = ROLE_DEFINITION_INDEX;
	int role_for_macro_condition = ROLE_DEFINITION_INDEX;
	int role_for_header_system   = ROLE_DEFINITION_INDEX;
	int role_for_header_local   = ROLE_DEFINITION_INDEX;

	Assert (passCount < 3);

	AnonymousID = 0;

	if (isInputLanguage (Lang_c) || isInputLanguage (Lang_cpp))
	{
		kind_for_define = CK_DEFINE;
		kind_for_header = CK_HEADER;
		kind_for_param = CK_MACRO_PARAM,
		role_for_macro_undef = CR_MACRO_UNDEF;
		role_for_macro_condition = CR_MACRO_CONDITION;
		role_for_header_system = CR_HEADER_SYSTEM;
		role_for_header_local = CR_HEADER_LOCAL;
	}
	else if (isInputLanguage (Lang_vera))
	{
		kind_for_define = VK_DEFINE;
		kind_for_header = VK_HEADER;
		kind_for_param  = VK_MACRO_PARAM,
		role_for_macro_undef = VR_MACRO_UNDEF;
		role_for_macro_condition = VR_MACRO_CONDITION;
		role_for_header_system = VR_HEADER_SYSTEM;
		role_for_header_local = VR_HEADER_LOCAL;
	}

	cppInit ((bool) (passCount > 1), isInputLanguage (Lang_csharp), isInputLanguage(Lang_cpp),
		 isInputLanguage(Lang_vera),
		 kind_for_define, role_for_macro_undef, role_for_macro_condition, kind_for_param,
		 kind_for_header, role_for_header_system, role_for_header_local,
		 FIELD_UNKNOWN);

	Signature = vStringNew ();

	exception = (exception_t) setjmp (Exception);
	rescan = RESCAN_NONE;
	if (exception == ExceptionNone)
		createTags (0, NULL);
	else
	{
		deleteAllStatements ();
		if (exception == ExceptionBraceFormattingError  &&  passCount == 1)
		{
			rescan = RESCAN_FAILED;
			verbose ("%s: retrying file with fallback brace matching algorithm\n",
					getInputFileName ());
		}
	}
	vStringDelete (Signature);
	cppTerminate ();
	return rescan;
}

static void buildKeywordHash (const langType language, unsigned int idx)
{
	const size_t count = ARRAY_SIZE (KeywordTable);
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

static void initializeCsharpParser (const langType language)
{
	Lang_csharp = language;
	buildKeywordHash (language, 2);
}

static void initializeDParser (const langType language)
{
	Lang_d = language;
	buildKeywordHash (language, 3);
}


static void initializeJavaParser (const langType language)
{
	Lang_java = language;
	buildKeywordHash (language, 4);
}

static void initializeVeraParser (const langType language)
{
	Lang_vera = language;
	buildKeywordHash (language, 5);
}

extern parserDefinition* OldCParser (void)
{
	static const char *const extensions [] = { "c", NULL };
	parserDefinition* def = parserNew ("OldC");
	def->kindTable      = CKinds;
	def->kindCount  = ARRAY_SIZE (CKinds);
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeCParser;
	def->enabled = 0;

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;
	return def;
}

extern parserDefinition* DParser (void)
{
	static const char *const extensions [] = { "d", "di", NULL };
	parserDefinition* def = parserNew ("D");
	def->kindTable      = DKinds;
	def->kindCount  = ARRAY_SIZE (DKinds);
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeDParser;
	// end: field is not tested.

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;
	return def;
}

extern parserDefinition* OldCppParser (void)
{
	static const char *const extensions [] = {
		"c++", "cc", "cp", "cpp", "cxx",
		"h", "h++", "hh", "hp", "hpp", "hxx", "inl",
#ifndef CASE_INSENSITIVE_FILENAMES
		"C", "H",
#endif
		NULL
	};
	static selectLanguage selectors[] = { selectByObjectiveCKeywords,
					      NULL };

	parserDefinition* def = parserNew ("OldC++");
	def->kindTable      = CKinds;
	def->kindCount  = ARRAY_SIZE (CKinds);
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeCppParser;
	def->selectLanguage = selectors;
	def->enabled = 0;

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;
	return def;
}

extern parserDefinition* CsharpParser (void)
{
	static const char *const extensions [] = { "cs", NULL };
	static const char *const aliases [] = { "csharp", NULL };
	parserDefinition* def = parserNew ("C#");
	def->kindTable      = CsharpKinds;
	def->kindCount  = ARRAY_SIZE (CsharpKinds);
	def->extensions = extensions;
	def->aliases    = aliases;
	def->parser2    = findCTags;
	def->initialize = initializeCsharpParser;
	// end: field is not tested.

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;
	return def;
}

extern parserDefinition* JavaParser (void)
{
	static const char *const extensions [] = { "java", NULL };
	parserDefinition* def = parserNew ("Java");
	def->kindTable      = JavaKinds;
	def->kindCount  = ARRAY_SIZE (JavaKinds);
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeJavaParser;
	def->useCork    = CORK_QUEUE;
	return def;
}

extern parserDefinition* VeraParser (void)
{
	static const char *const extensions [] = { "vr", "vri", "vrh", NULL };
	parserDefinition* def = parserNew ("Vera");
	def->kindTable      = VeraKinds;
	def->kindCount  = ARRAY_SIZE (VeraKinds);
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeVeraParser;
	// end: field is not tested.

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;

	return def;
}
