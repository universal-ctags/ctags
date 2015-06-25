/*
*   Copyright (c) 2015, Andri Lim
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Nim files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "main.h"

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "vstring.h"
#include "debug.h"

/*
*   CONSTANT
*/

#define CR  0x0D
#define LF  0x0A
#define ESC 0x1B
#define CR  0x0D
#define FF  0x0C
#define LF  0x0A
#define BEL 0x07
#define BACKSPACE 0x08
#define VT  0x0B

/*
*   DATA DECLARATIONS
*/

typedef enum {
  K_ITERATOR,
  K_PROC,
  K_MACRO,
  K_TEMPLATE,
  K_METHOD,
  K_CONVERTER,
  K_CONSTANT,
  K_OBJECT,
  K_FIELD,
  K_ENUM,
  K_VARIABLE,
  K_LET,
  K_ENUMERATOR,
  K_TYPE,
  K_TUPLE,
  K_ELEMENT,
  K_PARAM_LIST,
  K_IGNORE,
  K_NONE
} nimKind;

static kindOption nimKinds[] = {
  {TRUE, 'i', "iterator", "Iterator"},
  {TRUE, 'p', "proc", "Subroutine"},
  {TRUE, 'm', "macro", "Macro Definition"},
  {TRUE, 'g', "generics", "Generics"},
  {TRUE, 'h', "method", "A method"},
  {TRUE, 'c', "converter", "A converter"},
  {TRUE, 'k', "constant", "A Constant"},
  {TRUE, 'o', "object", "Data Structure"},
  {TRUE, 'f', "field", "An object field"},
  {TRUE, 'e', "enum", "Enum"},
  {TRUE, 'v', "variable", "mutable storage"},
  {TRUE, 'l', "let", "immutable storage"},
  {TRUE, 'r', "enumerator", "An enum variant"},
  {TRUE, 't', "type", "type alias"},
  {TRUE, 'u', "tuple", "A tuple"},
  {TRUE, 'n', "element", "A tuple element"},
};

typedef enum {
  tkInvalid, tkEof,         /* order is important here! */
  tkSymbol, /* keywords: */
  tkAddr, tkAnd, tkAs, tkAsm, tkAtomic,
  tkBind, tkBlock, tkBreak, tkCase, tkCast,
  tkConcept, tkConst, tkContinue, tkConverter,
  tkDefer, tkDiscard, tkDistinct, tkDiv, tkDo,
  tkElif, tkElse, tkEnd, tkEnum, tkExcept, tkExport,
  tkFinally, tkFor, tkFrom, tkFunc,
  tkGeneric, tkIf, tkImport, tkIn, tkInclude, tkInterface,
  tkIs, tkIsnot, tkIterator,
  tkLet,
  tkMacro, tkMethod, tkMixin, tkMod, tkNil, tkNot, tkNotin,
  tkObject, tkOf, tkOr, tkOut,
  tkProc, tkPtr, tkRaise, tkRef, tkReturn, tkShl, tkShr, tkStatic,
  tkTemplate,
  tkTry, tkTuple, tkType, tkUsing,
  tkVar, tkWhen, tkWhile, tkWith, tkWithout, tkXor,
  tkYield, /* end of keywords */
  tkIntLit, tkInt8Lit, tkInt16Lit, tkInt32Lit, tkInt64Lit,
  tkUIntLit, tkUInt8Lit, tkUInt16Lit, tkUInt32Lit, tkUInt64Lit,
  tkFloatLit, tkFloat32Lit, tkFloat64Lit, tkFloat128Lit,
  tkStrLit, tkRStrLit, tkTripleStrLit,
  tkGStrLit, tkGTripleStrLit, tkCharLit, tkParLe, tkParRi, tkBracketLe,
  tkBracketRi, tkCurlyLe, tkCurlyRi,
  tkBracketDotLe, tkBracketDotRi, /* [. and  .] */
  tkCurlyDotLe, tkCurlyDotRi, /* {.  and  .} */
  tkParDotLe, tkParDotRi,   /* (. and .) */
  tkComma, tkSemiColon,
  tkColon, tkColonColon, tkEquals, tkDot, tkDotDot,
  tkOpr, tkComment, tkAccent,
  tkSpaces, tkInfixOpr, tkPrefixOpr, tkPostfixOpr
} tokenType;

typedef enum {
  wInvalid,

  wAddr, wAnd, wAs, wAsm, wAtomic,
  wBind, wBlock, wBreak, wCase, wCast, wConcept, wConst,
  wContinue, wConverter, wDefer, wDiscard, wDistinct, wDiv, wDo,
  wElif, wElse, wEnd, wEnum, wExcept, wExport,
  wFinally, wFor, wFrom, wFunc, wGeneric, wIf, wImport, wIn,
  wInclude, wInterface, wIs, wIsnot, wIterator, wLet,
  wMacro, wMethod, wMixin, wMod, wNil,
  wNot, wNotin, wObject, wOf, wOr, wOut, wProc, wPtr, wRaise, wRef, wReturn,
  wShl, wShr, wStatic, wTemplate, wTry, wTuple, wType, wUsing, wVar,
  wWhen, wWhile, wWith, wWithout, wXor, wYield,

  wColon, wColonColon, wEquals, wDot, wDotDot,
  wStar, wMinus,
  wMagic, wThread, wFinal, wProfiler, wObjChecks,

  wDestroy,

  wImmediate, wConstructor, wDestructor, wDelegator, wOverride,
  wImportCpp, wImportObjC,
  wImportCompilerProc,
  wImportc, wExportc, wIncompleteStruct, wRequiresInit,
  wAlign, wNodecl, wPure, wSideeffect, wHeader,
  wNosideeffect, wGcSafe, wNoreturn, wMerge, wLib, wDynlib,
  wCompilerproc, wProcVar,
  wFatal, wError, wWarning, wHint, wLine, wPush, wPop, wDefine, wUndef,
  wLinedir, wStacktrace, wLinetrace, wLink, wCompile,
  wLinksys, wDeprecated, wVarargs, wCallconv, wBreakpoint, wDebugger,
  wNimcall, wStdcall, wCdecl, wSafecall, wSyscall, wInline, wNoInline,
  wFastcall, wClosure, wNoconv, wOn, wOff, wChecks, wRangechecks,
  wBoundchecks, wOverflowchecks, wNilchecks,
  wFloatchecks, wNanChecks, wInfChecks,
  wAssertions, wPatterns, wWarnings,
  wHints, wOptimization, wRaises, wWrites, wReads, wSize, wEffects, wTags,
  wDeadCodeElim, wSafecode, wNoForward,
  wPragma,
  wCompileTime, wNoInit,
  wPassc, wPassl, wBorrow, wDiscardable,
  wFieldChecks,
  wWatchPoint, wSubsChar,
  wAcyclic, wShallow, wUnroll, wLinearScanEnd, wComputedGoto,
  wInjectStmt, wExperimental,
  wWrite, wGensym, wInject, wDirty, wInheritable, wThreadVar, wEmit,
  wAsmNoStackFrame,
  wImplicitStatic, wGlobal, wCodegenDecl, wUnchecked, wGuard, wLocks,

  wAuto, wBool, wCatch, wChar, wClass,
  wConst_cast, wDefault, wDelete, wDouble, wDynamic_cast,
  wExplicit, wExtern, wFalse, wFloat, wFriend,
  wGoto, wInt, wLong, wMutable, wNamespace, wNew, wOperator,
  wPrivate, wProtected, wPublic, wRegister, wReinterpret_cast,
  wShort, wSigned, wSizeof, wStatic_cast, wStruct, wSwitch,
  wThis, wThrow, wTrue, wTypedef, wTypeid, wTypename,
  wUnion, wPacked, wUnsigned, wVirtual, wVoid, wVolatile, wWchar_t,

  wAlignas, wAlignof, wConstexpr, wDecltype, wNullptr, wNoexcept,
  wThread_local, wStatic_assert, wChar16_t, wChar32_t,

  wStdIn, wStdOut, wStdErr,

  wInOut, wByCopy, wByRef, wOneWay
} TSpecialWords;

static const char* specialWords[] =
{
  "INVALID",
  "addr", "and", "as", "asm", "atomic",
  "bind", "block", "break", "case", "cast",
  "concept", "const", "continue", "converter",
  "defer", "discard", "distinct", "div", "do",
  "elif", "else", "end", "enum", "except", "export",
  "finally", "for", "from", "func", "generic", "if",
  "import", "in", "include", "interface", "is", "isnot", "iterator",
  "let",
  "macro", "method", "mixin", "mod", "nil", "not", "notin",
  "object", "of", "or",
  "out", "proc", "ptr", "raise", "ref", "return",
  "shl", "shr", "static",
  "template", "try", "tuple", "type", "using", "var",
  "when", "while", "with", "without", "xor",
  "yield",

  ":", "::", "=", ".", "..", "*", "-",

  "magic", "thread", "final", "profiler", "objchecks",
  "destroy",

  "immediate", "constructor", "destructor", "delegator", "override",
  "importcpp", "importobjc",
  "importcompilerproc", "importc", "exportc", "incompletestruct",
  "requiresinit", "align", "nodecl", "pure", "sideeffect",
  "header", "nosideeffect", "gcsafe", "noreturn", "merge", "lib", "dynlib",
  "compilerproc", "procvar", "fatal", "error", "warning", "hint", "line",
  "push", "pop", "define", "undef", "linedir", "stacktrace", "linetrace",
  "link", "compile", "linksys", "deprecated", "varargs",
  "callconv", "breakpoint", "debugger", "nimcall", "stdcall",
  "cdecl", "safecall", "syscall", "inline", "noinline", "fastcall", "closure",
  "noconv", "on", "off", "checks", "rangechecks", "boundchecks",
  "overflowchecks", "nilchecks",
  "floatchecks", "nanchecks", "infchecks",

  "assertions", "patterns", "warnings", "hints",
  "optimization", "raises", "writes", "reads", "size", "effects", "tags",
  "deadcodeelim", "safecode", "noforward",
  "pragma",
  "compiletime", "noinit",
  "passc", "passl", "borrow", "discardable", "fieldchecks",
  "watchpoint",
  "subschar", "acyclic", "shallow", "unroll", "linearscanend",
  "computedgoto", "injectstmt", "experimental",
  "write", "gensym", "inject", "dirty", "inheritable", "threadvar", "emit",
  "asmnostackframe", "implicitstatic", "global", "codegendecl", "unchecked",
  "guard", "locks",

  "auto", "bool", "catch", "char", "class",
  "const_cast", "default", "delete", "double",
  "dynamic_cast", "explicit", "extern", "false",
  "float", "friend", "goto", "int", "long", "mutable",
  "namespace", "new", "operator",
  "private", "protected", "public", "register", "reinterpret_cast",
  "short", "signed", "sizeof", "static_cast", "struct", "switch",
  "this", "throw", "true", "typedef", "typeid",
  "typename", "union", "packed", "unsigned", "virtual", "void", "volatile",
  "wchar_t",

  "alignas", "alignof", "constexpr", "decltype", "nullptr", "noexcept",
  "thread_local", "static_assert", "char16_t", "char32_t",

  "stdin", "stdout", "stderr",

  "inout", "bycopy", "byref", "oneway",
  NULL
};

typedef enum {
  /* # order is extremely important, because ranges are used
     # to check whether a node belongs to a certain class */
  nkNone,               /* unknown node kind: indicates an error */
                        /* Expressions: */
                        /* Atoms: */
  nkEmpty,              /* the node is empty */
  nkIdent,              /* node is an identifier */
  nkSym,                /* node is a symbol */
  nkType,               /* node is used for its typ field */

  nkCharLit,            /* a character literal '' */
  nkIntLit,             /* an integer literal */
  nkInt8Lit,
  nkInt16Lit,
  nkInt32Lit,
  nkInt64Lit,
  nkUIntLit,            /* an unsigned integer literal */
  nkUInt8Lit,
  nkUInt16Lit,
  nkUInt32Lit,
  nkUInt64Lit,
  nkFloatLit,           /* a floating point literal */
  nkFloat32Lit,
  nkFloat64Lit,
  nkFloat128Lit,
  nkStrLit,             /* a string literal "" */
  nkRStrLit,            /* a raw string literal r"" */
  nkTripleStrLit,       /* a triple string literal """ */
  nkNilLit,             /* the nil literal */
                        /* end of atoms */
  nkMetaNode_Obsolete,  /* difficult to explain; represents itself */
                        /* (used for macros) *? */
  nkDotCall,            /* used to temporarily flag a nkCall node; */
                        /* this is used */
                        /* for transforming ``s.len`` to ``len(s)`` */

  nkCommand,            /* a call like ``p 2, 4`` without parenthesis */
  nkCall,               /* a call like p(x, y) or an operation like +(a, b) */
  nkCallStrLit,         /* a call with a string literal */
                        /* x"abc" has two sons: nkIdent, nkRStrLit */
                        /* x"""abc""" has two sons: nkIdent, nkTripleStrLit */
  nkInfix,              /* a call like (a + b) */
  nkPrefix,             /* a call like !a */
  nkPostfix,            /* something like a! (also used for visibility) */
  nkHiddenCallConv,     /* an implicit type conversion via a type converter */

  nkExprEqExpr,         /* a named parameter with equals: ''expr = expr'' */
  nkExprColonExpr,      /* a named parameter with colon: ''expr: expr'' */
  nkIdentDefs,          /* a definition like `a, b: typeDesc = expr` */
                        /* either typeDesc or expr may be nil; used in */
                        /* formal parameters, var statements, etc. */
  nkVarTuple,           /* a ``var (a, b) = expr`` construct */
  nkPar,                /* syntactic (); may be a tuple constructor */
  nkObjConstr,          /* object constructor: T(a: 1, b: 2) */
  nkCurly,              /* syntactic {} */
  nkCurlyExpr,          /* an expression like a{i} */
  nkBracket,            /* syntactic [] */
  nkBracketExpr,        /* an expression like a[i..j, k] */
  nkPragmaExpr,         /* an expression like a{.pragmas.} */
  nkRange,              /* an expression like i..j */
  nkDotExpr,            /* a.b */
  nkCheckedFieldExpr,   /* a.b, but b is a field that needs to be checked */
  nkDerefExpr,          /* a^ */
  nkIfExpr,             /* if as an expression */
  nkElifExpr,
  nkElseExpr,
  nkLambda,             /* lambda expression */
  nkDo,                 /* lambda block appering as trailing proc param */
  nkAccQuoted,          /* `a` as a node */

  nkTableConstr,        /* a table constructor {expr: expr} */
  nkBind,               /* ``bind expr`` node */
  nkClosedSymChoice,    /* symbol choice node; a list of nkSyms (closed) */
  nkOpenSymChoice,      /* symbol choice node; a list of nkSyms (open) */
  nkHiddenStdConv,      /* an implicit standard type conversion */
  nkHiddenSubConv,      /* an implicit type conversion from a subtype */
                        /* to a supertype */
  nkConv,               /* a type conversion */
  nkCast,               /* a type cast */
  nkStaticExpr,         /* a static expr */
  nkAddr,               /* a addr expression */
  nkHiddenAddr,         /* implicit address operator */
  nkHiddenDeref,        /* implicit ^ operator */
  nkObjDownConv,        /* down conversion between object types */
  nkChckRangeF,         /* range check for floats */
  nkObjUpConv,          /* up conversion between object types */
  nkChckRange64,        /* range check for 64 bit ints */
  nkChckRange,          /* range check for ints */
  nkStringToCString,    /* string to cstring */
  nkCStringToString,    /* cstring to string */
                        /* end of expressions */

  nkAsgn,               /* a = b */
  nkFastAsgn,           /* internal node for a fast ``a = b`` */
                        /* (no string copy) */
  nkGenericParams,      /* generic parameters */
  nkFormalParams,       /* formal parameters */
  nkOfInherit,          /* inherited from symbol */

  nkImportAs,           /* a 'as' b in an import statement */
  nkProcDef,            /* a proc */
  nkMethodDef,          /* a method */
  nkConverterDef,       /* a converter */
  nkMacroDef,           /* a macro */
  nkTemplateDef,        /* a template */
  nkIteratorDef,        /* an iterator */

  nkOfBranch,           /* used inside case statements */
                        /* for (cond, action)-pairs */
  nkElifBranch,         /* used in if statements */
  nkExceptBranch,       /* an except section */
  nkElse,               /* an else part */
  nkAsmStmt,            /* an assembler block */
  nkPragma,             /* a pragma statement */
  nkPragmaBlock,        /* a pragma with a block */
  nkIfStmt,             /* an if statement */
  nkWhenStmt,           /* a when expression or statement */
  nkForStmt,            /* a for statement */
  nkParForStmt,         /* a parallel for statement */
  nkWhileStmt,          /* a while statement */
  nkCaseStmt,           /* a case statement */
  nkTypeSection,        /* a type section (consists of type definitions) */
  nkVarSection,         /* a var section */
  nkLetSection,         /* a let section */
  nkConstSection,       /* a const section */
  nkConstDef,           /* a const definition */
  nkTypeDef,            /* a type definition */
  nkYieldStmt,          /* the yield statement as a tree */
  nkDefer,              /* the 'defer' statement */
  nkTryStmt,            /* a try statement */
  nkFinally,            /* a finally section */
  nkRaiseStmt,          /* a raise statement */
  nkReturnStmt,         /* a return statement */
  nkBreakStmt,          /* a break statement */
  nkContinueStmt,       /* a continue statement */
  nkBlockStmt,          /* a block statement */
  nkStaticStmt,         /* a static statement */
  nkDiscardStmt,        /* a discard statement */
  nkStmtList,           /* a list of statements */
  nkImportStmt,         /* an import statement */
  nkImportExceptStmt,   /* an import x except a statement */
  nkExportStmt,         /* an export statement */
  nkExportExceptStmt,   /* an 'export except' statement */
  nkFromStmt,           /* a from * import statement */
  nkIncludeStmt,        /* an include statement */
  nkBindStmt,           /* a bind statement */
  nkMixinStmt,          /* a mixin statement */
  nkUsingStmt,          /* an using statement */
  nkCommentStmt,        /* a comment statement */
  nkStmtListExpr,       /* a statement list followed by an expr; this is used */
                        /* to allow powerful multi-line templates */
  nkBlockExpr,          /* a statement block ending in an expr; this is used */
                        /* to allowe powerful multi-line templates that open a */
                        /* temporary scope */
  nkStmtListType,       /* a statement list ending in a type; for macros */
  nkBlockType,          /* a statement block ending in a type; for macros */
                        /* types as syntactic trees: */

  nkWith,               /* distinct with `foo` */
  nkWithout,            /* distinct without `foo` */

  nkTypeOfExpr,         /* type(1+2) */
  nkObjectTy,           /* object body */
  nkTupleTy,            /* tuple body */
  nkTupleClassTy,       /* tuple type class */
  nkTypeClassTy,        /* user-defined type class */
  nkStaticTy,           /* ``static[T]`` */
  nkRecList,            /* list of object parts */
  nkRecCase,            /* case section of object */
  nkRecWhen,            /* when section of object */
  nkRefTy,              /* ``ref T`` */
  nkPtrTy,              /* ``ptr T`` */
  nkVarTy,              /* ``var T`` */
  nkConstTy,            /* ``const T`` */
  nkMutableTy,          /* ``mutable T`` */
  nkDistinctTy,         /* distinct type */
  nkProcTy,             /* proc type */
  nkIteratorTy,         /* iterator type */
  nkSharedTy,           /* 'shared T' */
                        /* we use 'nkPostFix' for the 'not nil' addition */
  nkEnumTy,             /* enum body */
  nkEnumFieldDef,       /* `ident = expr` in an enumeration */
  nkArgList,            /* argument list */
  nkPattern,            /* a special pattern; used for matching */
  nkReturnToken,        /* token used for interpretation */
  nkClosure,            /* (prc, env)-pair (internally used for code gen) */
  nkGotoState,          /* used for the state machine (for iterators) */
  nkState,              /* give a label to a code section (for iterators) */
  nkBreakState,         /* special break statement for easier code generation */
} TNodeKind;

typedef enum {
  pmNormal,
  pmTypeDesc,
  pmTypeDef,
  pmSkipSuffix
} TPrimaryMode;

typedef enum {
  withPragma = 0x01,         /* identifier may have pragma */
  withBothOptional = 0x02,   /* both ':' and '=' parts are optional */
} TDeclaredIdentFlag;

typedef struct {      /* a Nim token */
  tokenType tokType;  /* the type of the token */
  int indent;         /* the indentation; != -1 if the token has been
                         preceded with indentation */
  int ident;          /* the parsed identifier */

  int strongSpaceA;   /* leading spaces of an operator */
  int strongSpaceB;   /* trailing spaces of an operator */
  vString* literal;   /* the parsed (string) literal; and
                         documentation comments are here too */
  int line, col;
  fpos_t filePosition;
  int iNumber;
} nimToken;

typedef struct {
  int indentAhead;    /* if > 0 an indendation has already been read
                         this is needed because scanning comments
                         needs so much look-ahead */
  int currLineIndent;
  int lineNumber;
  int lineStart;      /* index of last line start in buffer */
  const unsigned char* buf;
  int bufpos;
  int buflen;

  vString* lineHolder; /* real buffer */
  fpos_t filePosition;
} nimLexer;

typedef struct {
  int currInd;            /* current indentation level */
  boolean strongSpaces;   /* Is strongSpaces on? */
  nimLexer lex;           /* The lexer that is used for parsing */
  nimToken tok;           /* The current token */
  int inPragma;           /* Pragma level */
  int inSemiStmtList;

  /* these three fields for captured symbol information */
  vString* symbol;
  vString* scope;
  int lineNumber;
  int column;
  fpos_t filePosition;
  nimKind kind;
  nimKind scopeKind;

  int nestLevel;
} nimParser;

typedef TNodeKind (*TDefParser)(nimParser* nim);

static langType nimLang;
static int lastIdent;

#define tokKeywordLow (tkSymbol + 1)
#define tokKeywordHigh (tkIntLit - 1)
#define oprLow ((int)(wColon))
#define oprHigh ((int)(wDotDot))

static void makeNimTag(nimParser* nim, nimKind kind)
{
  if (kind == K_NONE || ! nimKinds[kind].enabled)
    return;

  tagEntryInfo tag;

  initTagEntry(&tag, nim->symbol->buffer);

  tag.lineNumber = nim->lineNumber;
  tag.filePosition = nim->filePosition;
  tag.sourceFileName = getSourceFileName();

  tag.kindName = nimKinds[kind].name;
  tag.kind = nimKinds[kind].letter;

  if (kind == K_OBJECT || kind == K_ENUM || kind == K_TUPLE)
  {
    tag.extensionFields.typeRef[0] = nimKinds[kind].name;
    tag.extensionFields.typeRef[1] = vStringValue(nim->symbol);
    vStringCopy(nim->scope, nim->symbol);
    nim->scopeKind = kind;
  }

  if(kind == K_FIELD || kind == K_ENUMERATOR || kind == K_ELEMENT)
  {
    tag.extensionFields.scope[0] = nimKinds[nim->scopeKind].name;
    tag.extensionFields.scope[1] = nim->scope->buffer;
  }

  makeTagEntry(&tag);
}

static void initToken(nimToken* tok)
{
  tok->literal = vStringNew();
}

static void deInitToken(nimToken* tok)
{
  vStringDelete(tok->literal);
}

static void fillToken(nimToken* tok)
{
  tok->tokType = tkInvalid;
  tok->strongSpaceA = 0;
  tok->indent = 0;
  vStringClear(tok->literal);
  tok->ident = wInvalid;
}

static boolean emptyLine(nimLexer* lex)
{
  int i;
  lex->buflen = strlen((const char*) lex->buf);
  if(lex->buflen == 0) return TRUE;

  for(i = 0; i < lex->buflen; i++)
  {
    if(lex->buf[i] > ' ') return FALSE;
  }

  return TRUE;
}

static void getEmptyLine(nimLexer* lex)
{
  lex->lineStart = 0;
  lex->buf = (const unsigned char*) "\xFF\xFF\xFF\xFF\xFF";
  lex->bufpos = 0;
  lex->buflen = 0;
}

static int fillLexer(nimLexer* lex)
{
  lex->buf = fileReadLine();
  if(lex->buf == NULL) {
    getEmptyLine(lex);
    return 0;
  }

  while(emptyLine(lex))
  {
    lex->buf = fileReadLine();
    if(lex->buf == NULL) {
      getEmptyLine(lex);
      return 0;
    }
  }

  vStringCopyS(lex->lineHolder, (const char*) lex->buf);
  vStringStripTrailing(lex->lineHolder);
  lex->buflen = vStringLength(lex->lineHolder);

  /* add safety net, dummy string for lookahead  */
  vStringCatS(lex->lineHolder, "\xFF\xFF\xFF\xFF\xFF");
  lex->buf = (const unsigned char*) lex->lineHolder->buffer;

  lex->lineNumber = getSourceLineNumber();
  lex->filePosition = getInputFilePosition();

  lex->bufpos = 0;
  lex->lineStart = 0;
  return 1;
}

static void skipUTF8BOM(nimLexer* lex)
{
  if (lex->buf[0] == 0xEF && lex->buf[1] == 0xBB && lex->buf[2] == 0xBF)
  {
    lex->bufpos += 3;
    lex->lineStart += 3;
  };
}

static void initLexer(nimLexer* lex)
{
  lex->indentAhead = -1;
  lex->currLineIndent = 0;
  lex->lineNumber = 0;
  lex->lineStart = 0;
  lex->bufpos = 0;
  lex->buf = NULL;
  lex->lineHolder = vStringNew();
  fillLexer(lex);
  skipUTF8BOM(lex);
}

static void deInitLexer(nimLexer* lex)
{
  vStringDelete(lex->lineHolder);
}

static int getColNumber(nimLexer* lex, int pos)
{
  int result = pos - lex->lineStart;
  return (result < 0 ? -result : result);
}

static void skip(nimLexer* lex, nimToken* tok)
{
  int pos = lex->bufpos;
  const unsigned char* buf = lex->buf;
  int col = getColNumber(lex, pos);
  int indent = 0;

  if (col == 0)
  {
    tok->strongSpaceA = 0;
    while ( (pos < lex->buflen) && (buf[pos] == ' ') )
    {
      pos += 1;
      indent += 1;
    }
    tok->indent = indent;
    lex->currLineIndent = indent;
  }

  while(pos < lex->buflen)
  {
    if(buf[pos] == ' ')
    {
      pos += 1; tok->strongSpaceA++;
    } else if (buf[pos] == '\t')
    {
      pos += 1;
    } else
    {
      break;
    }
  }

  lex->bufpos = pos;
}

static boolean inSymStartChars(unsigned char c)
{
  if (c >= 'a' && c <= 'z') return TRUE;
  if (c >= 'A' && c <= 'Z') return TRUE;
  if (c >= 0x80 && c <= 0xFF) return TRUE;
  return FALSE;
}

static boolean isSymbolPrefix(unsigned char c)
{
  return inSymStartChars(c) && c != 'r' && c != 'R' && c != 'l';
}

static boolean inDigit(unsigned char c)
{
  return (c >= '0' && c <= '9');
}

static boolean inSymChars(unsigned char c)
{
  if ( inSymStartChars(c) ) return TRUE;
  if ( inDigit(c) ) return TRUE;
  return FALSE;
}

static int getSymbol(nimLexer* lex, nimToken* tok)
{
  int pos = lex->bufpos;
  const unsigned char* buf = lex->buf;

  while(pos < lex->buflen)
  {
    int c = buf[pos];
    if(inSymStartChars(c) || inDigit(c))
    {
      vStringPut(tok->literal, c);
      pos += 1;
    }
    else if (c == '_')
    {
      if ( !inSymChars(buf[pos+1]) )
      {
        break;
      }

      vStringPut(tok->literal, c);
      pos += 1;
    } else
    {
      break;
    }
  }

  tok->ident = lookupKeyword(tok->literal->buffer, nimLang);
  lex->bufpos = pos;

  if ( (tok->ident < tokKeywordLow - tkSymbol) ||
        (tok->ident > tokKeywordHigh - tkSymbol) )
    tok->tokType = tkSymbol;
  else
    tok->tokType = (tokenType) (tok->ident + tkSymbol);

   if(pos >= lex->buflen) return 0;
  return 1;
}

static void scanComment(nimLexer* lex, nimToken* tok)
{
  int pos = lex->bufpos;
  const unsigned char* buf = lex->buf;
  int col = getColNumber(lex, pos);
  int indent;

  tok->tokType = tkComment;
  /* iNumber contains the number of '\n' in the token */
  tok->iNumber = 0;

  while(TRUE)
  {
    int lastBackslash = -1;

    while(pos < lex->buflen)
    {
      if (buf[pos] == '\\') lastBackslash = pos + 1;
      vStringPut(tok->literal, buf[pos]);
      pos += 1;
    }

    if (lastBackslash > 0)
    {
      /* a backslash is a continuation character if only followed by spaces
         plus a newline: */
      while (buf[lastBackslash] == ' ') { lastBackslash += 1; }
      if (lastBackslash < lex->buflen)
      {
        /* false positive: */
        lastBackslash = -1;
      }
    }

    if (!fillLexer(lex)) return;
    pos = lex->bufpos;
    buf = lex->buf;

    indent = 0;
    while (buf[pos] == ' ')
    {
      pos += 1;
      indent += 1;
    }

    if (buf[pos] == '#' && (col == indent || lastBackslash > 0))
    {
      vStringPut(tok->literal, '\n');
      col = indent;
      tok->iNumber += 1;
    }
    else
    {
      if (buf[pos] > ' ') lex->indentAhead = indent;
      break;
    }
  }
  lex->bufpos = pos;
}

static boolean inOpChars(unsigned char c)
{
  if (c == '+') return TRUE;
  if (c == '-') return TRUE;
  if (c == '*') return TRUE;
  if (c == '/') return TRUE;
  if (c == '\\') return TRUE;
  if (c == '<') return TRUE;
  if (c == '>') return TRUE;
  if (c == '!') return TRUE;
  if (c == '?') return TRUE;
  if (c == '^') return TRUE;
  if (c == '.') return TRUE;
  if (c == '|') return TRUE;
  if (c == '=') return TRUE;
  if (c == '%') return TRUE;
  if (c == '&') return TRUE;
  if (c == '$') return TRUE;
  if (c == '@') return TRUE;
  if (c == '~') return TRUE;
  if (c == ':') return TRUE;
  if (c >= 0x80 && c <= 0xFF) return TRUE;
  return FALSE;
}

static void endOperator(nimLexer* lex, nimToken* tok, int pos)
{
  tok->ident = lookupKeyword(tok->literal->buffer, nimLang);

  if (tok->ident == -1) {
    tok->ident = (int) (wInvalid + lastIdent);
    addKeyword(tok->literal->buffer, nimLang, tok->ident);
    lastIdent += 1;
  }
  if ((tok->ident < oprLow) || (tok->ident > oprHigh)) tok->tokType = tkOpr;
  else tok->tokType = (tokenType) (tok->ident - oprLow + tkColon);

  lex->bufpos = pos;
}

static void getOperator(nimLexer* lex, nimToken* tok)
{
  int pos = lex->bufpos;
  const unsigned char* buf = lex->buf;
  unsigned char c;

  while(TRUE)
  {
    c = buf[pos];
    if (!inOpChars(c)) break;
    vStringPut(tok->literal, c);
    pos += 1;
  }

  endOperator(lex, tok, pos);
  /* advance pos but don't store it in lex->bufpos so the next token (which might
     be an operator too) gets the preceding spaces: */
  tok->strongSpaceB = 0;
  while(buf[pos] == ' ')
  {
    pos += 1;
    tok->strongSpaceB += 1;
  }

  if(pos >= lex->buflen) tok->strongSpaceB = -1;
}

static boolean matchTwoChars(nimLexer* lex, unsigned char first)
{
  unsigned char c = lex->buf[lex->bufpos + 1];
  return (lex->buf[lex->bufpos] == first) && (c >= '0' && c <= '9');
}

static void handleHexChar(nimLexer* lex, int* result)
{
  int xi = 0;
  int c = lex->buf[lex->bufpos];

  if(c >= '0' && c <= '9')
  {
    xi = (xi << 4) || (c - '0');
    lex->bufpos += 1;
  }
  else if(c >= 'a' && c <= 'f')
  {
    xi = (xi << 4) || (c - 'a' + 10);
    lex->bufpos += 1;
  }
  else if(c >= 'A' && c <= 'F')
  {
    xi = (xi << 4) || (c - 'A' + 10);
    lex->bufpos += 1;
  }

  *result = xi;
}

static void handleDecChars(nimLexer* lex, int* xi)
{
  while (lex->buf[lex->bufpos] >= '0' && lex->buf[lex->bufpos] <= '9')
  {
    *xi = (*xi * 10) + (lex->buf[lex->bufpos] - '0');
    lex->bufpos += 1;
  }
}

static void getEscapedChar(nimLexer* lex, nimToken* tok)
{
  int xi;
  lex->bufpos += 1;               /* skip '\' */

  switch(lex->buf[lex->bufpos])
  {
    case 'n': case 'N':
      if (tok->tokType == tkCharLit)
      {
        /* errNnotAllowedInCharacter */
      }
      vStringPut(tok->literal, '\n');
      lex->bufpos += 1;
      break;

    case 'r': case 'R': case 'c': case 'C':
      vStringPut(tok->literal, CR);
      lex->bufpos += 1;
      break;

    case 'l': case 'L':
      vStringPut(tok->literal, LF);
      lex->bufpos += 1;
      break;

    case 'f': case 'F':
      vStringPut(tok->literal, FF);
      lex->bufpos += 1;
      break;

    case 'e': case 'E':
      vStringPut(tok->literal, ESC);
      lex->bufpos += 1;
      break;

    case 'a': case 'A':
      vStringPut(tok->literal, BEL);
      lex->bufpos += 1;
      break;

    case 'b': case 'B':
      vStringPut(tok->literal, BACKSPACE);
      lex->bufpos += 1;
      break;

    case 'v': case 'V':
      vStringPut(tok->literal, VT);
      lex->bufpos += 1;
      break;

    case 't': case 'T':
      vStringPut(tok->literal, '\t');
      lex->bufpos += 1;
      break;

    case '\'': case '\"':
      vStringPut(tok->literal, lex->buf[lex->bufpos]);
      lex->bufpos += 1;
      break;

    case '\\':
      vStringPut(tok->literal, '\\');
      lex->bufpos += 1;
      break;

    case 'x': case 'X':
      lex->bufpos += 1;
      xi = 0;
      handleHexChar(lex, &xi);
      handleHexChar(lex, &xi);
      vStringPut(tok->literal, xi);
      break;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':
      if ( matchTwoChars(lex, '0') )
      {
        /* warnOctalEscape */
      }
      xi = 0;
      handleDecChars(lex, &xi);
      if (xi <= 255) { vStringPut(tok->literal, xi); }
      else { /* errInvalidCharacterConstant */ }
      break;
    default:
     /* errInvalidCharacterConstant */
      break;
  }
}

static void getString(nimLexer* lex, nimToken* tok, boolean rawMode)
{
  int pos = lex->bufpos + 1;           /* skip " */
  const unsigned char* buf = lex->buf; /* put `buf` in a register */
  int line = lex->lineNumber;          /* save linenumber for better error message */

  vStringPut(tok->literal, '\"');
  if(buf[pos] == '\"' && buf[pos+1] == '\"')
  {
    tok->tokType = tkTripleStrLit; /* long string literal: */
    pos += 2;                      /* skip "" */
    /* skip leading newline: */

    if (buf[pos] == ' ' || buf[pos] == '\t')
    {
      int newpos = pos+1;
      while(buf[newpos] == ' ' || buf[newpos] == '\t')
        newpos += 1;

      if (newpos >= lex->buflen) pos = newpos;
    }

    while(TRUE)
    {
      if (pos >= lex->buflen)
      {
        if (!fillLexer(lex)) {
          int line2 = lex->lineNumber;
          lex->lineNumber = line;
          /* errClosingTripleQuoteExpected */
          lex->lineNumber = line2;
          break;
        }
        pos = lex->bufpos;
        buf = lex->buf;
        vStringPut(tok->literal, '\n');
      }

      if (buf[pos] == '\"')
      {
        if ( strncmp((const char*) &buf[pos], "\"\"\"", 3) == 0 )
        {
          lex->bufpos = pos + 3; /* skip the three """ */
          break;
        }
        vStringPut(tok->literal, '\"');
        pos += 1;
      }
      else
      {
        vStringPut(tok->literal, buf[pos]);
        pos += 1;
      }
    }
  }
  else
  {
    /* ordinary string literal */
    if (rawMode) tok->tokType = tkRStrLit;
    else tok->tokType = tkStrLit;

    boolean closed = FALSE;

    while(pos < lex->buflen)
    {
      int c = buf[pos];
      if (c == '\"')
      {
        if ( rawMode && (buf[pos+1] == '\"') )
        {
          pos += 2;
          vStringPut(tok->literal, '\"');
        }
        else
        {
          pos += 1;  /* skip '"' */
          closed = TRUE;
          break;
        }
      }
      else if ( (c == '\\') && !rawMode )
      {
        lex->bufpos = pos;
        getEscapedChar(lex, tok);
        pos = lex->bufpos;
      }
      else
      {
        vStringPut(tok->literal, c);
        pos += 1;
      }
    }

    if (!closed)
    {
      /* errClosingQuoteExpected */
    }

    lex->bufpos = pos;
  }

  vStringPut(tok->literal, '\"');
}

static void getCharacter(nimLexer* lex, nimToken* tok)
{
  int c = lex->buf[++lex->bufpos]; /* skip ' */

  if ( ((c >= '\0') && (c < ' ')) || (c == '\'') )
  {
    /* errInvalidCharacterConstant */
  }
  else if (c == '\\')
    getEscapedChar(lex, tok);
  else
  {
    vStringPut(tok->literal, c);
    lex->bufpos += 1;
    if (lex->buf[lex->bufpos] != '\'')
    {
      /* errMissingFinalQuote */
    }
    lex->bufpos += 1;               /* skip ' */
  }
}

typedef boolean (*charSet)(unsigned char c);

static void matchUnderscoreChars(nimLexer* lex, nimToken* tok, charSet inChars)
{
  int pos = lex->bufpos;              /* use registers for pos, buf */
  const unsigned char* buf = lex->buf;

  while(TRUE)
  {
    if ( (*inChars)(buf[pos]) )
    {
      vStringPut(tok->literal, buf[pos]);
      pos += 1;
    }
    else
      break;

    if (buf[pos] == '_')
    {
      if ( !(*inChars)(buf[pos]) )
      {
        /* errInvalidToken "_" */
        break;
      }
      vStringPut(tok->literal, '_');
      pos += 1;
    }
  }

  lex->bufpos = pos;
}

static boolean inHexChars(unsigned char c)
{
  if(c >= '0' && c <= '9') return TRUE;
  if(c >= 'a' && c <= 'f') return TRUE;
  if(c >= 'A' && c <= 'F') return TRUE;
  if(c == 'X' || c == 'x') return TRUE;
  return FALSE;
}

static boolean inNumeralBaseChars(unsigned char c)
{
  if(c >= '0' && c <= '9') return TRUE;
  if(c == 'b' || c == 'B') return TRUE;
  if(c >= 'c' || c == 'C') return TRUE;
  if(c == 'o') return TRUE;
  return FALSE;
}

static boolean inNumeralSuffixChars(unsigned char c)
{
  if (c == '\'') return TRUE;
  if (c == 'f' || c == 'F') return TRUE;
  if (c == 'i' || c == 'I') return TRUE;
  if (c == 'u' || c == 'U') return TRUE;
  return FALSE;
}

static void getNumber(nimLexer* lex, nimToken* tok)
{
    int pos, endpos;
    boolean eallowed = FALSE;
    /* get the base: */
    tok->tokType = tkIntLit;   /* int literal until we know better */
    pos = lex->bufpos;         /* make sure the literal is correct for error messages: */

  if (lex->buf[pos] == '0' && (lex->buf[pos+1] == 'X' || lex->buf[pos+1] == 'x') )
    matchUnderscoreChars(lex, tok, inHexChars);
  else
  {
    matchUnderscoreChars(lex, tok, inNumeralBaseChars);
    eallowed = TRUE;
  }

  if ( (lex->buf[lex->bufpos] == '.')
    && (lex->buf[lex->bufpos + 1] >= '0')
    && (lex->buf[lex->bufpos + 1] <= '9') )
  {
    vStringPut(tok->literal, '.');
    lex->bufpos += 1;
    matchUnderscoreChars(lex, tok, inDigit);
    eallowed = TRUE;
  }


  if (eallowed && ((lex->buf[lex->bufpos] == 'e') || (lex->buf[lex->bufpos] == 'E')) )
  {
    vStringPut(tok->literal, 'e');
    lex->bufpos += 1;
    if ( (lex->buf[lex->bufpos] == '+') || (lex->buf[lex->bufpos] == '-') )
    {
      vStringPut(tok->literal, lex->buf[lex->bufpos]);
      lex->bufpos += 1;
    }
    matchUnderscoreChars(lex, tok, inDigit);
  }

  endpos = lex->bufpos;

  if( inNumeralSuffixChars(lex->buf[endpos]) )
  {
    if (lex->buf[endpos] == '\'') {
      vStringPut(tok->literal, lex->buf[endpos]);
      endpos += 1;
    }

    if( inNumeralSuffixChars(lex->buf[endpos]) && lex->buf[endpos] != '\'' )
    {
      vStringPut(tok->literal, lex->buf[endpos]);
      endpos += 1;
      while ( inDigit(lex->buf[endpos]) )
      {
        vStringPut(tok->literal, lex->buf[endpos]);
        endpos += 1;
      }
    }
  }

  lex->bufpos = endpos;
}

static int parseToken(nimLexer* lex, nimToken* tok)
{
  int c = lex->buf[lex->bufpos];
  switch(c)
  {
    case '#':
      scanComment(lex, tok);
      break;
    case '*':
    /* '*:' is unfortunately a special case, because it is two tokens in
       'var v*: int'.*/

      if ( lex->buf[lex->bufpos+1] == ':'
         && !inOpChars(lex->buf[lex->bufpos+2]) )
        {
          endOperator(lex, tok, lex->bufpos+1);
        }
      else
        getOperator(lex, tok);

      break;
    case ',':
      vStringPut(tok->literal, ',');
      tok->tokType = tkComma;
      lex->bufpos += 1;
      break;
    case 'l':
    /* if we parsed exactly one character and its a small L (l), this
       is treated as a warning because it may be confused with the number 1 */
      {
        unsigned char cc = lex->buf[lex->bufpos+1];
        if (!inSymChars(cc) && cc != '_')
        {
          /* warnSmallLshouldNotBeUsed */
        }
        getSymbol(lex, tok);
      }
      break;

    case 'r': case 'R':
      if ( lex->buf[lex->bufpos + 1] == '\"' )
      {
        lex->bufpos += 1;
        getString(lex, tok, TRUE);
      }
      else
      {
        getSymbol(lex, tok);
      }
      break;

    case '(':
      lex->bufpos += 1;
      if ( (lex->buf[lex->bufpos] == '.')
        && (lex->buf[lex->bufpos+1] != '.') )
      {
        tok->tokType = tkParDotLe;
        lex->bufpos += 1;
        vStringPut(tok->literal, '(');
        vStringPut(tok->literal, '.');
      }
      else
      {
        vStringPut(tok->literal, '(');
        tok->tokType = tkParLe;
      }

      break;

    case ')':
      tok->tokType = tkParRi;
      lex->bufpos += 1;
      vStringPut(tok->literal, ')');
      break;

    case '[':
      lex->bufpos += 1;
      if ( (lex->buf[lex->bufpos] == '.')
        && (lex->buf[lex->bufpos+1] != '.') )
      {
        tok->tokType = tkBracketDotLe;
        lex->bufpos += 1;
        vStringPut(tok->literal, '[');
        vStringPut(tok->literal, '.');
      }
      else
      {
        vStringPut(tok->literal, '[');
        tok->tokType = tkBracketLe;
      }

      break;

    case ']':
      tok->tokType = tkBracketRi;
      lex->bufpos += 1;
      vStringPut(tok->literal, ']');
      break;

    case '.':
      {
        int cc = lex->buf[lex->bufpos+1];
        if (cc == ']')
        {
          tok->tokType = tkBracketDotRi;
          lex->bufpos += 2;
          vStringPut(tok->literal, '.');
          vStringPut(tok->literal, ']');
        }
        else if (cc == '}')
        {
          tok->tokType = tkCurlyDotRi;
          lex->bufpos += 2;
          vStringPut(tok->literal, '.');
          vStringPut(tok->literal, '}');
        }
        else if (cc == ')')
        {
          tok->tokType = tkParDotRi;
          lex->bufpos += 2;
          vStringPut(tok->literal, '.');
          vStringPut(tok->literal, ')');
        }
        else
          getOperator(lex, tok);
      }
       break;

    case '{':
      lex->bufpos += 1;
      if ( (lex->buf[lex->bufpos] == '.')
        && (lex->buf[lex->bufpos+1] != '.') )
      {
        tok->tokType = tkCurlyDotLe;
        lex->bufpos += 2;
        vStringPut(tok->literal, '{');
        vStringPut(tok->literal, '.');
      }
      else
      {
        tok->tokType = tkCurlyLe;
        vStringPut(tok->literal, '{');
      }

      break;

    case '}':
      tok->tokType = tkCurlyRi;
      lex->bufpos += 1;
      vStringPut(tok->literal, '}');
      break;

    case ';':
      tok->tokType = tkSemiColon;
      lex->bufpos += 1;
      vStringPut(tok->literal, ';');

      break;

    case '`':
      tok->tokType = tkAccent;
      lex->bufpos += 1;
      vStringPut(tok->literal, '`');
      break;

    case '_':

      lex->bufpos += 1;
      if ( !inSymChars(lex->buf[lex->bufpos]) )
      {
        tok->tokType = tkSymbol;
        tok->ident = lookupKeyword("_", nimLang);
        vStringPut(tok->literal, c);
      }
      else
      {
        vStringPut(tok->literal, c);
        tok->tokType = tkInvalid;
        /* errInvalidToken, c & " (\\" & $(ord(c)) & ')') */
      }

      break;

    case '\"':
      /* check for extended raw string literal: */
      {
        boolean rawMode = lex->bufpos > 0 && inSymChars(lex->buf[lex->bufpos-1]);
        getString(lex, tok, rawMode);
        if (rawMode)
          /* tkRStrLit -> tkGStrLit
             tkTripleStrLit -> tkGTripleStrLit */
          tok->tokType += 2;
      }
      break;

    case '\'':
      tok->tokType = tkCharLit;
      getCharacter(lex, tok);
      tok->tokType = tkCharLit;
      break;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':
      getNumber(lex, tok);
      break;

  default:
    if ( inOpChars(c) )
      getOperator(lex, tok);
    else
    {
      vStringPut(tok->literal, c);
      tok->tokType = tkInvalid;
      /* errInvalidToken, c & " (\\" & $(ord(c)) & ')')*/
      printf("INVALID TOKEN %d\n", (int)c);
      lex->bufpos += 1;
    }
    break;
  }

  if (lex->bufpos >= lex->buflen) return 0;
  return 1;
}

/*
  return values:
  -1 : error
  0  : out of buffer, need more input
  1  : ok
*/
static int rawGetTok(nimLexer* lex, nimToken* tok)
{
  unsigned char c;
  int ret;

  fillToken(tok);

  if(lex->indentAhead >= 0)
  {
    tok->indent = lex->indentAhead;
    lex->currLineIndent = lex->indentAhead;
    lex->indentAhead = -1;
  } else
  {
    tok->indent = -1;
  }

  skip(lex, tok);

  if (lex->bufpos >= lex->buflen) return 0;

  c = lex->buf[lex->bufpos];
  tok->line = lex->lineNumber;
  tok->col = getColNumber(lex, lex->bufpos);
  tok->filePosition = lex->filePosition;

  if (isSymbolPrefix(c))
  {
    ret = getSymbol(lex, tok);
    return ret;
  }

  ret = parseToken(lex, tok);
  return ret;
}

static boolean getToken(nimLexer* lex, nimToken* tok)
{
    int ret = rawGetTok(lex, tok);
    if (ret == 1) return TRUE;
    if (ret == 0) {
      if (!fillLexer(lex))
      {
        tok->indent = 0;
        tok->tokType = tkEof;
        return FALSE;
      }
      else
      {
        return TRUE;
      }
    }
    return FALSE;
}

static void initParser(nimParser* nim)
{
  nim->currInd = 0;
  nim->strongSpaces = FALSE;
  nim->inPragma = 0;
  nim->inSemiStmtList = 0;
  nim->symbol = vStringNew();
  nim->scope  = vStringNew();
  nim->scopeKind = K_NONE;
  nim->kind = K_NONE;
  nim->nestLevel = 0;

  initLexer(&nim->lex);
  initToken(&nim->tok);
}

static void deInitParser(nimParser* nim)
{
  vStringDelete(nim->symbol);
  vStringDelete(nim->scope);
  deInitLexer(&nim->lex);
  deInitToken(&nim->tok);
}

static TNodeKind parseAll(nimParser* nim);

/* helpers for the other parsers */
static boolean isOperator(nimToken* tok);
static tokenType getTok(nimParser* nim);
static void skipComment(nimParser* nim);
static void eat(nimParser* nim, tokenType tokType);
static void skipInd(nimParser* nim);
static void optPar(nimParser* nim);
static void optInd(nimParser* nim);
static void indAndComment(nimParser* nim);
static TNodeKind parseSymbol(nimParser* nim, boolean allowNil);
static TNodeKind parseTry(nimParser* nim, boolean isExpr);
static TNodeKind parseCase(nimParser* nim);
/* implementation */

static tokenType getTok(nimParser* nim)
{
  getToken(&nim->lex, &nim->tok);
  return nim->tok.tokType;
}

/*
  DEBUG TOOLS
*/

#ifdef _MSC_VER
#define __func__ __FUNCTION__
#endif

#ifdef DEBUG
#define BRACE(x) debug_nesting(++nim->nestLevel, __LINE__, __func__, #x); x; nim->nestLevel--
#define GET_TOKEN(x) debug_token(__LINE__, __func__, x)
#define MAKE_NIM_TAG(x,y) debug_tag(__LINE__, __func__, x, y)
#else
#define BRACE(x) x
#define GET_TOKEN(x) getTok(x)
#define MAKE_NIM_TAG(x,y) makeNimTag(x,y)
#endif

#ifdef DEBUG
static void debug_token(int line, const char* func, nimParser* nim)
{
  getTok(nim);
  if(debug(DEBUG_PARSE))
  {
    printf("[TOKEN: %d, %s] %s\n", line, func, vStringValue(nim->tok.literal));
  }
}

static void debug_tag(int line, const char* func, nimParser* nim, nimKind kind)
{
  if(debug(DEBUG_PARSE))
  {
    printf("TRACE TAG: %d func: %s subject: %s\n", line, func, nim->symbol->buffer);
  }
  makeNimTag(nim, kind);
}

static void debug_nesting(int nestLevel, int line, const char* func, const char* parser)
{
  if(debug(DEBUG_PARSE))
  {
    printf("LN: %d F:%s P:%s\n", line, func, parser);
  }
}

#endif

#define startWithInd(nim) int oldInd = nim->currInd; nim->currInd = nim->tok.indent
#define endWithInd(nim) nim->currInd = oldInd

static boolean realInd(nimParser* nim)
{
   return nim->tok.indent > nim->currInd;
}

static boolean sameInd(nimParser* nim)
{
  return nim->tok.indent == nim->currInd;
}

static boolean sameOrNoInd(nimParser* nim)
{
  return (nim->tok.indent == nim->currInd) || (nim->tok.indent < 0);
}

static void rawSkipComment(nimParser* nim)
{
  if (nim->tok.tokType == tkComment)
  {
    GET_TOKEN(nim);
  }
}

static void skipComment(nimParser* nim)
{
  if (nim->tok.indent < 0) {
    BRACE(rawSkipComment(nim));
  }
}

static void skipInd(nimParser* nim)
{
  if (nim->tok.indent >= 0)
  {
    if (!realInd(nim)) {
      /* errInvalidIndentation */
    }
  }
}

static void optPar(nimParser* nim)
{
  if (nim->tok.indent >= 0)
  {
    if (nim->tok.indent < nim->currInd)
    {
        /* errInvalidIndentation */
    }
  }
}

static void optInd(nimParser* nim)
{
  BRACE(skipComment(nim));
  BRACE(skipInd(nim));
}

static void getTokNoInd(nimParser* nim)
{
  GET_TOKEN(nim);
  if (nim->tok.indent >= 0)
  {
    /* errInvalidIndentation */
  }
}

static boolean isKeyword(tokenType kind)
{
  return (kind >= tokKeywordLow) && (kind <= tokKeywordHigh);
}

static void eat(nimParser* nim, tokenType tokType)
{
  /* Move the parser to the next token if the current token is of type
    `tokType`, otherwise error. */
  if (nim->tok.tokType == tokType)
  {
    GET_TOKEN(nim);
  }
  else
  {
     /* errTokenExpected */
  }
}

static void indAndComment(nimParser* nim)
{
  if (nim->tok.indent > nim->currInd)
  {
    if (nim->tok.tokType == tkComment) {
      BRACE(rawSkipComment(nim));
    }
    else /* errInvalidIndentation */ ;
  }
  else
  {
    BRACE(skipComment(nim));
  }
}

static TNodeKind parseExpr(nimParser* nim);
static TNodeKind parseStmt(nimParser* nim);
static TNodeKind parseTypeDesc(nimParser* nim);
static void parseDoBlocks(nimParser* nim);
static TNodeKind parseParamList(nimParser* nim, boolean retColon);

static boolean isSigilLike(nimToken* tok)
{
  if ( vStringLength(tok->literal) < 1) return FALSE;
  return tok->tokType == tkOpr && tok->literal->buffer[0] == '@';
}

static boolean isRightAssociative(nimToken* tok)
{
  /* Determines whether the token is right assocative. */
  return (tok->tokType == tkOpr) && (tok->literal->buffer[0] == '^');
  /* or (let L = tok.ident.s.len; L > 1 and tok.ident.s[L-1] == '>')) */
}

static boolean inArrowLike(unsigned char ch)
{
  if (ch == '-') return TRUE;
  if (ch == '~') return TRUE;
  if (ch == '=') return TRUE;
  return FALSE;
}

static int considerStrongSpaces(nimToken* tok, boolean strongSpaces, int x)
{
  return x + (strongSpaces ? 100 - tok->strongSpaceA * 10 : 0);
}

static int considerAsgn(nimToken* tok, int value)
{
  int L = vStringLength(tok->literal);
  if (L == 0) return value;
  return (tok->literal->buffer[L-1] == '=' ? 1 : value);
}

static int getPrecedence(nimToken* tok, boolean strongSpaces)
{
  /* ## Calculates the precedence of the given token. */
  int result = 0;

  switch(tok->tokType)
  {
    case tkOpr:
    {
      int L = vStringLength(tok->literal);
      int relevantChar = tok->literal->buffer[0];

      /* # arrow like? */
      if ( (L > 1)
        && (tok->literal->buffer[L-1] == '>')
        && (inArrowLike(tok->literal->buffer[L-2]) ) )
        {
          return considerStrongSpaces(tok, strongSpaces, 1);
        }

      switch(relevantChar)
      {
        case '$': case '^':
          result = considerAsgn(tok, 10);
          break;
        case '*': case '%':
        case '/': case '\\':
          result = considerAsgn(tok, 9);
          break;
        case '~': result = 8; break;
        case '+': case '-': case '|':
          result = considerAsgn(tok, 8);
          break;
        case '&':
          result = considerAsgn(tok, 7);
          break;
        case '=': case '<':
        case '>': case '!':
          result = 5; break;
        case '.':
          result = considerAsgn(tok, 6);
          break;
        case '?': result = 2; break;
        default:
          result = considerAsgn(tok, 2);
          break;
        }
      }
      break;
    case tkDiv: case tkMod:
    case tkShl: case tkShr:
      result = 9; break;
    case tkIn: case tkNotin:
    case tkIs: case tkIsnot:
    case tkNot: case tkOf:
    case tkAs:
      result = 5; break;
    case tkDotDot: result = 6; break;
    case tkAnd: result = 4; break;
    case tkOr: case tkXor:
    case tkPtr: case tkRef:
      result = 3; break;
    default: return -10;
  }
  return considerStrongSpaces(tok, strongSpaces, result);
}

static boolean isOperator(nimToken* tok)
{
  /* ## Determines if the given token is an operator type token. */

  if (tok->tokType == tkOpr) return TRUE;
  if (tok->tokType == tkDiv) return TRUE;
  if (tok->tokType == tkMod) return TRUE;
  if (tok->tokType == tkShl) return TRUE;
  if (tok->tokType == tkShr) return TRUE;
  if (tok->tokType == tkIn) return TRUE;
  if (tok->tokType == tkNotin) return TRUE;
  if (tok->tokType == tkIs) return TRUE;
  if (tok->tokType == tkIsnot) return TRUE;
  if (tok->tokType == tkNot) return TRUE;
  if (tok->tokType == tkOf) return TRUE;
  if (tok->tokType == tkAs) return TRUE;
  if (tok->tokType == tkDotDot) return TRUE;
  if (tok->tokType == tkAnd) return TRUE;
  if (tok->tokType == tkOr) return TRUE;
  if (tok->tokType == tkXor) return TRUE;
  return FALSE;
}

static boolean isUnary(nimParser* nim)
{
  /*## Check if the current parser token is a unary operator*/
  if ( ( (nim->tok.tokType == tkOpr)
     || (nim->tok.tokType == tkDotDot) )
     && (nim->tok.strongSpaceB == 0)
     && (nim->tok.strongSpaceA > 0) )
  {
    /* XXX change this after 0.10.4 is out */
    if (nim->strongSpaces) return TRUE;
    else
    {
      /* warnDeprecated,
        "will be parsed as unary operator; inconsistent spacing") */
    }
  }

  return FALSE;
}

static boolean binaryRange(int val)
{
   return (val == 0 || val == 1 || val == 2 || val == 4 || val == 8);
}

static void checkBinary(nimParser* nim)
{
  /* Check if the current parser token is a binary operator.
  # we don't check '..' here as that's too annoying */

  if (nim->strongSpaces && (nim->tok.tokType == tkOpr))
  {
    if ((nim->tok.strongSpaceB > 0) && (nim->tok.strongSpaceA != nim->tok.strongSpaceB))
    {
        /* errGenerated,
          "Number of spaces around '$#' not consistent"
          */
    }
    else if (!binaryRange(nim->tok.strongSpaceA))
    {
      /* errGenerated, "Number of spaces must be 0,1,2,4 or 8"*/
    }
  }
}

static void colcom(nimParser* nim)
{
  BRACE(eat(nim, tkColon));
  BRACE(skipComment(nim));
}

static boolean inBracketLookAhead(nimParser* nim)
{
  if (nim->tok.tokType == tkOpr) return TRUE;
  if (nim->tok.tokType == tkDot) return TRUE;
  if (nim->tok.tokType == tkDotDot) return TRUE;
  if (nim->tok.tokType == tkEquals) return TRUE;
  if (nim->tok.tokType >= tkParLe && nim->tok.tokType <= tkParDotRi) return TRUE;
  return FALSE;
}

static boolean inSymbolLookAhead(nimParser* nim)
{
  if (nim->tok.tokType >= tokKeywordLow
    && nim->tok.tokType <= tokKeywordHigh) return TRUE;

  if (nim->tok.tokType == tkSymbol) return TRUE;

  if (nim->tok.tokType >= tkIntLit
    && nim->tok.tokType <= tkCharLit) return TRUE;

  return FALSE;
}

static TNodeKind parseSymbol(nimParser* nim, boolean allowNil)
{
  /*| symbol = '`' (KEYW|IDENT|literal|(operator|'('|')'|'['|']'|'{'|'}'|'=')+)+ '`'
    |        | IDENT | 'addr' | 'type' */

  TNodeKind result;
  vStringClear(nim->symbol);
  switch(nim->tok.tokType)
  {
    case tkSymbol: case tkAddr: case tkType:
      vStringCopy(nim->symbol, nim->tok.literal);
      nim->lineNumber = nim->tok.line;
      nim->column = nim->tok.col;
      nim->filePosition = nim->tok.filePosition;
      result = nkIdent;
      GET_TOKEN(nim);
      break;
    case tkAccent:
      result = nkAccQuoted;
      GET_TOKEN(nim);
      while(TRUE)
      {
        if ( nim->tok.tokType == tkAccent ) break;
        else if ( inBracketLookAhead(nim) )
        {
            nim->lineNumber = nim->tok.line;
            nim->column = nim->tok.col;
            nim->filePosition = nim->tok.filePosition;
            while ( inBracketLookAhead(nim) )
            {
               vStringCat(nim->symbol, nim->tok.literal);
               GET_TOKEN(nim);
            }
        }
        else if (inSymbolLookAhead(nim))
        {
          vStringCopy(nim->symbol, nim->tok.literal);
          nim->lineNumber = nim->tok.line;
          nim->column = nim->tok.col;
          nim->filePosition = nim->tok.filePosition;
          GET_TOKEN(nim);
        }
        else
        {
          /* errIdentifierExpected, p.tok */
        }

      }
      BRACE(eat(nim, tkAccent));
      break;
    default:
      if (allowNil && nim->tok.tokType == tkNil)
      {
        result = nkNilLit;
        GET_TOKEN(nim);
      }
      else
      {
        /* errIdentifierExpected */
        /* BUGFIX: We must consume a token here to prevent endless loops!
           But: this really sucks for idetools and keywords, so we don't do it
          if it is a keyword: */
        if ( !isKeyword(nim->tok.tokType) ) GET_TOKEN(nim);
        result = nkEmpty;
      }
  }

  return result;
}

static TNodeKind colonOrEquals(nimParser* nim, TNodeKind a)
{
  TNodeKind result;

  if (nim->tok.tokType == tkColon)
  {
    result = nkExprColonExpr;
    GET_TOKEN(nim);
    BRACE(parseExpr(nim));
  }
  else if (nim->tok.tokType == tkEquals)
  {
    result = nkExprEqExpr;
    GET_TOKEN(nim);
    BRACE(parseExpr(nim));
  }
  else result = a;
  return result;
}

static void exprColonEqExpr(nimParser* nim)
{
  /* | exprColonEqExpr = expr (':'|'=' expr)? */
  TNodeKind a;
  BRACE(a = parseExpr(nim));
  BRACE(colonOrEquals(nim, a));
}

static void exprList(nimParser* nim, tokenType endTok)
{
  /* #| exprList = expr ^+ comma */
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  while ((nim->tok.tokType != endTok) && (nim->tok.tokType != tkEof))
  {
    BRACE(parseExpr(nim));
    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
  }
}

static TNodeKind dotExpr(nimParser* nim)
{
  /* #| dotExpr = expr '.' optInd symbol */
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  BRACE(parseSymbol(nim, FALSE));
  return nkDotExpr;
}

static TNodeKind qualifiedIdent(nimParser* nim)
{
  /* #| qualifiedIdent = symbol ('.' optInd symbol)? */
  TNodeKind result;
  BRACE(result = parseSymbol(nim, FALSE));
  if (nim->tok.tokType == tkDot) {
    BRACE(result = dotExpr(nim));
  }
  return result;
}

static boolean inRightBracket(tokenType tokTyp)
{
  if (tokTyp == tkCurlyRi) return TRUE;
  if (tokTyp == tkCurlyDotRi) return TRUE;
  if (tokTyp == tkBracketRi) return TRUE;
  if (tokTyp == tkParRi) return TRUE;
  return FALSE;
}

static void exprColonEqExprListAux(nimParser* nim, tokenType endTok)
{
  Assert(inRightBracket(endTok));
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  while (nim->tok.tokType != endTok && nim->tok.tokType != tkEof)
  {
    BRACE(exprColonEqExpr(nim));
    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
  }
  BRACE(optPar(nim));
  BRACE(eat(nim, endTok));
}

static TNodeKind setOrTableConstr(nimParser* nim)
{
  /* #| setOrTableConstr = '{' ((exprColonEqExpr comma)* | ':' ) '}' */
  GET_TOKEN(nim); /* # skip '{'*/
  BRACE(optInd(nim));
  if (nim->tok.tokType == tkColon)
  {
    GET_TOKEN(nim); /* # skip ':' */
  }
  else
  {
    while ( !((nim->tok.tokType == tkCurlyRi) || (nim->tok.tokType == tkEof)) )
    {
      BRACE(exprColonEqExpr(nim));
      if (nim->tok.tokType != tkComma) break;
      GET_TOKEN(nim);
      BRACE(skipComment(nim));
    }
  }
  BRACE(optPar(nim));
  BRACE(eat(nim, tkCurlyRi)); /* # skip '}' */
  return nkCurly;
}

static TNodeKind parseCast(nimParser* nim)
{
    /* #| castExpr = 'cast' '[' optInd typeDesc optPar ']' '(' optInd expr optPar ')'*/

    GET_TOKEN(nim);
    BRACE(eat(nim, tkBracketLe));
    BRACE(optInd(nim));
    BRACE(parseTypeDesc(nim));
    BRACE(optPar(nim));
    BRACE(eat(nim, tkBracketRi));
    BRACE(eat(nim, tkParLe));
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
    BRACE(optPar(nim));
    BRACE(eat(nim, tkParRi));
    return nkCast;
}

static TNodeKind parseGStrLit(nimParser* nim, TNodeKind kind)
{
  if(nim->tok.tokType == tkGStrLit)
  {
    GET_TOKEN(nim);
    return nkCallStrLit;
  }
  else if(nim->tok.tokType == tkGTripleStrLit)
  {
    GET_TOKEN(nim);
    return nkCallStrLit;
  }
  else
  {
    return kind;
  }
}

static TNodeKind exprColonEqExprList(nimParser* nim, TNodeKind kind, tokenType endTok)
{
  /* #| exprColonEqExprList = exprColonEqExpr (comma exprColonEqExpr)* (comma)? */
  BRACE(exprColonEqExprListAux(nim, endTok));
  return kind;
}

static TNodeKind complexOrSimpleStmt(nimParser* nim);
static TNodeKind simpleExpr(nimParser* nim, TPrimaryMode mode);

static TNodeKind semiStmtList(nimParser* nim)
{
  nim->inSemiStmtList += 1;
  BRACE(complexOrSimpleStmt(nim));
  while (nim->tok.tokType == tkSemiColon)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(complexOrSimpleStmt(nim));
  }
  nim->inSemiStmtList -= 1;
  return nkStmtListExpr;
}

static boolean inParseParLookAhead(tokenType tokType)
{
  if(tokType == tkDiscard) return TRUE;
  if(tokType == tkInclude) return TRUE;
  if(tokType == tkIf) return TRUE;
  if(tokType == tkWhile) return TRUE;
  if(tokType == tkCase) return TRUE;
  if(tokType == tkTry) return TRUE;
  if(tokType == tkDefer) return TRUE;
  if(tokType == tkFinally) return TRUE;
  if(tokType == tkExcept) return TRUE;
  if(tokType == tkFor) return TRUE;
  if(tokType == tkBlock) return TRUE;
  if(tokType == tkConst) return TRUE;
  if(tokType == tkLet) return TRUE;
  if(tokType == tkWhen) return TRUE;
  if(tokType == tkVar) return TRUE;
  if(tokType == tkMixin) return TRUE;
  return FALSE;
}

static TNodeKind parsePar(nimParser* nim)
{
  /*#| parKeyw = 'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
  #|         | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
  #|         | 'when' | 'var' | 'mixin'
  #| par = '(' optInd (&parKeyw complexOrSimpleStmt ^+ ';'
  #|                  | simpleExpr ('=' expr (';' complexOrSimpleStmt ^+ ';' )? )?
  #|                             | (':' expr)? (',' (exprColonEqExpr comma?)*)?  )?
  #|         optPar ')'
  #
  # unfortunately it's ambiguous: (expr: expr) vs (exprStmt); however a
  # leading ';' could be used to enforce a 'stmt' context ...*/

  TNodeKind result = nkPar;

  GET_TOKEN(nim);
  BRACE(optInd(nim));
  if (inParseParLookAhead(nim->tok.tokType))
  {
    /* # XXX 'bind' used to be an expression, so we exclude it here;
    # tests/reject/tbind2 fails otherwise. */
    BRACE(result = semiStmtList(nim));
  }
  else if (nim->tok.tokType == tkSemiColon)
  {
    /* # '(;' enforces 'stmt' context: */
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(result = semiStmtList(nim));
  }
  else if (nim->tok.tokType != tkParRi)
  {
    TNodeKind a;
    BRACE(a = simpleExpr(nim, pmNormal));
    if (nim->tok.tokType == tkEquals)
    {
      /* # special case: allow assignments */
      GET_TOKEN(nim);
      BRACE(optInd(nim));
      BRACE(parseExpr(nim));
      if (nim->tok.tokType == tkSemiColon)
      {
        BRACE(result = semiStmtList(nim));
      }
    }
    else if (nim->tok.tokType == tkSemiColon)
    {
      /* # stmt context: */
      BRACE(result = semiStmtList(nim));
    }
    else
    {
      colonOrEquals(nim, a);
      if (nim->tok.tokType == tkComma)
      {
        GET_TOKEN(nim);
        BRACE(skipComment(nim));
        while ((nim->tok.tokType != tkParRi) && (nim->tok.tokType != tkEof))
        {
          BRACE(exprColonEqExpr(nim));
          if (nim->tok.tokType != tkComma) break;
          GET_TOKEN(nim);
          BRACE(skipComment(nim));
        }
      }
    }
  }
  BRACE(optPar(nim));
  BRACE(eat(nim, tkParRi));
  return result;
}

static TNodeKind identOrLiteral(nimParser* nim, TPrimaryMode mode)
{
  /*#| literal = | INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
  #|           | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
  #|           | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT
  #|           | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
  #|           | CHAR_LIT
  #|           | NIL
  #| generalizedLit = GENERALIZED_STR_LIT | GENERALIZED_TRIPLESTR_LIT
  #| identOrLiteral = generalizedLit | symbol | literal
  #|                | par | arrayConstr | setOrTableConstr
  #|                | castExpr
  #| tupleConstr = '(' optInd (exprColonEqExpr comma?)* optPar ')'
  #| arrayConstr = '[' optInd (exprColonEqExpr comma?)* optPar ']'*/

  TNodeKind result;
  switch(nim->tok.tokType)
  {
  case tkSymbol: case tkType: case tkAddr:
    GET_TOKEN(nim);
    BRACE(result = parseGStrLit(nim, nkIdent));
    break;
  case tkAccent:
    BRACE(result = parseSymbol(nim, FALSE)); /* # literals */
    break;
  case tkIntLit: case tkInt8Lit: case tkInt16Lit:
  case tkInt32Lit: case tkInt64Lit: case tkUIntLit:
  case tkUInt8Lit: case tkUInt16Lit: case tkUInt32Lit:
  case tkUInt64Lit: case tkFloatLit: case tkFloat32Lit:
  case tkFloat64Lit: case tkFloat128Lit: case tkStrLit:
  case tkRStrLit: case tkTripleStrLit: case tkCharLit:
  case tkNil:
    result = nkIntLit; /* quick hack */
    GET_TOKEN(nim);
    break;
  case tkParLe:
    /* # () constructor */
    if ((mode == pmTypeDesc) || (mode == pmTypeDef))
    {
      BRACE(result = exprColonEqExprList(nim, nkPar, tkParRi));
    }
    else
    {
      BRACE(result = parsePar(nim));
    }
    break;
  case tkCurlyLe:
    /* # {} constructor */
    BRACE(result = setOrTableConstr(nim));
    break;
  case tkBracketLe:
    /* # [] constructor */
    BRACE(result = exprColonEqExprList(nim, nkBracket, tkBracketRi));
    break;
  case tkCast:
    BRACE(result = parseCast(nim));
    break;
  default:
    /* errExprExpected */
    GET_TOKEN(nim);  /*# we must consume a token here to prevend endless loops!*/
    result = nkEmpty;
    break;
  }
  return result;
}

static TNodeKind namedParams(nimParser* nim, TNodeKind kind, tokenType endTok)
{
  BRACE(exprColonEqExprListAux(nim, endTok));
  return kind;
}

static boolean inPrimarySuffixLookAhead(tokenType tokTyp)
{
  if (tokTyp == tkSymbol) return TRUE;
  if (tokTyp == tkAccent) return TRUE;
  if (tokTyp >= tkIntLit && tokTyp <= tkCharLit) return TRUE;
  if (tokTyp == tkNil) return TRUE;
  if (tokTyp == tkCast) return TRUE;
  if (tokTyp == tkAddr) return TRUE;
  if (tokTyp == tkType) return TRUE;
  return FALSE;
}

static TNodeKind primarySuffix(nimParser* nim, TNodeKind kind, int baseIndent)
{
  /* #| primarySuffix = '(' (exprColonEqExpr comma?)* ')' doBlocks?
     #|       | doBlocks
     #|       | '.' optInd symbol generalizedLit?
     #|       | '[' optInd indexExprList optPar ']'
     #|       | '{' optInd indexExprList optPar '}'
     #|       | &( '`'|IDENT|literal|'cast'|'addr'|'type') expr # command syntax
  */

  TNodeKind result = kind;

  while (nim->tok.indent < 0 ||
       (nim->tok.tokType == tkDot
        && nim->tok.indent >= baseIndent) )
  {
    if (nim->tok.tokType == tkParLe)
    {
      if (nim->strongSpaces && nim->tok.strongSpaceA > 0) break;
      BRACE(result = namedParams(nim, nkCall, tkParRi));
      BRACE(parseDoBlocks(nim));
    }
    else if (nim->tok.tokType == tkDo)
    {
      result = nkCall;
      BRACE(parseDoBlocks(nim));
    }
    else if (nim->tok.tokType == tkDot)
    {
      BRACE(result = dotExpr(nim));
      BRACE(result = parseGStrLit(nim, result));
    }
    else if (nim->tok.tokType == tkBracketLe)
    {
      if (nim->strongSpaces && nim->tok.strongSpaceA > 0) break;
      BRACE(result = namedParams(nim, nkBracketExpr, tkBracketRi));
    }
    else if (nim->tok.tokType == tkCurlyLe)
    {
      if (nim->strongSpaces && nim->tok.strongSpaceA > 0) break;
      BRACE(result = namedParams(nim, nkCurlyExpr, tkCurlyRi));
    }
    else if (inPrimarySuffixLookAhead(nim->tok.tokType))
    {
      if (nim->inPragma == 0)
      {
        /* actually parsing {.push hints:off.} as {.push(hints:off).} is a sweet
           solution, but pragmas.nim can't handle that */
          result = nkCommand;
          BRACE(parseExpr(nim));
      }
      break;
    }
    else
    {
      break;
    }
  }

  return result;
}

static TNodeKind simpleExprAux(nimParser* nim, int limit, TPrimaryMode mode);
static TNodeKind primary(nimParser* nim, TPrimaryMode mode);

static TNodeKind parseOperators(nimParser* nim, TNodeKind head, int limit, TPrimaryMode mode)
{
  TNodeKind result = head;
  /* expand while operators have priorities higher than 'limit' */
  int opPrec = getPrecedence(&nim->tok, nim->strongSpaces);
  TPrimaryMode modeB = mode == pmTypeDef ? pmTypeDesc : mode;
  int leftAssoc;

  /* the operator itself must not start on a new line: */
  while ((opPrec >= limit) && (nim->tok.indent < 0) && !isUnary(nim))
  {
    checkBinary(nim);
    leftAssoc = 1-isRightAssociative(&nim->tok);
    result = nkInfix;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    /* read sub-expression with higher priority: */
    BRACE(simpleExprAux(nim, opPrec + leftAssoc, modeB));
    BRACE(opPrec = getPrecedence(&nim->tok, nim->strongSpaces));
  }
  return result;
}

static TNodeKind simpleExprAux(nimParser* nim, int limit, TPrimaryMode mode)
{
  TNodeKind result;
  BRACE(result = primary(nim, mode));
  BRACE(result = parseOperators(nim, result, limit, mode));
  return result;
}

static TNodeKind simpleExpr(nimParser* nim, TPrimaryMode mode)
{
  TNodeKind result;
  BRACE(result = simpleExprAux(nim, -1, mode));
  return result;
}

static TNodeKind parseIfExpr(nimParser* nim, TNodeKind kind)
{
  /*#| condExpr = expr colcom expr optInd
  #|         ('elif' expr colcom expr optInd)*
  #|          'else' colcom expr
  #| ifExpr = 'if' condExpr
  #| whenExpr = 'when' condExpr*/

  TNodeKind result = kind;

  while(TRUE)
  {
    GET_TOKEN(nim);      /* skip `if`, `elif` */
    BRACE(parseExpr(nim));
    BRACE(colcom(nim));
    BRACE(parseExpr(nim));
    BRACE(optInd(nim));
    if (nim->tok.tokType != tkElif) break;
  }
  BRACE(eat(nim, tkElse));
  BRACE(colcom(nim));
  BRACE(parseExpr(nim));
  return result;
}

static boolean inPragmaLookAhead(nimParser* nim)
{
  if (nim->tok.tokType == tkCurlyDotRi) return TRUE;
  if (nim->tok.tokType == tkCurlyRi) return TRUE;
  if (nim->tok.tokType == tkEof) return TRUE;
  return FALSE;
}

static TNodeKind parsePragma(nimParser* nim)
{
  /* | pragma = '{.' optInd (exprColonExpr comma?)* optPar ('.}' | '}') */
  nim->inPragma += 1;
  GET_TOKEN(nim);
  BRACE(optInd(nim));

  while ( !inPragmaLookAhead(nim) )
  {
    BRACE(exprColonEqExpr(nim));
    if (nim->tok.tokType == tkComma)
    {
      GET_TOKEN(nim);
      BRACE(skipComment(nim));
    }
  }

  BRACE(optPar(nim));
  if (nim->tok.tokType == tkCurlyDotRi
    || nim->tok.tokType == tkCurlyRi )
  {
    GET_TOKEN(nim);
  }
  else
  {
    /* errTokenExpected, ".}" */
  }
  nim->inPragma -= 1;
  return nkPragma;
}

static TNodeKind identVis(nimParser* nim)
{
  /* | identVis = symbol opr?  # postfix position */
  TNodeKind result;
  BRACE(result = parseSymbol(nim, FALSE));

  if (nim->tok.tokType == tkOpr)
  {
    result = nkPostfix;
    GET_TOKEN(nim);
  }
  return result;
}

static TNodeKind identWithPragma(nimParser* nim)
{
  /* | identWithPragma = identVis pragma? */
  nimKind kind = nim->kind; /* save prev state */
  BRACE(TNodeKind result = identVis(nim));
  if (nim->tok.tokType == tkCurlyDotLe)
  {
    result = nkPragmaExpr;
    BRACE(parsePragma(nim));
  }
  if (kind == K_PARAM_LIST) nim->kind = kind;
  return result;
}

static TNodeKind parseIdentColonEquals(nimParser* nim, TDeclaredIdentFlag flags)
{
  /* #| declColonEquals = identWithPragma (comma identWithPragma)* comma?
     #|                   (':' optInd typeDesc)? ('=' optInd expr)?
     #| identColonEquals = ident (comma ident)* comma?
     #|      (':' optInd typeDesc)? ('=' optInd expr)?) */

  TNodeKind a;
  TNodeKind result = nkIdentDefs;

  while(TRUE)
  {
    if ( (nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent) )
    {
      if ( (flags & withPragma) == withPragma)
      {
        BRACE(a = identWithPragma(nim));
        if((a != nkEmpty) && (nim->kind != K_NONE) && (nim->kind != K_PARAM_LIST))
        {
          MAKE_NIM_TAG(nim, nim->kind);
        }
      }
      else
      {
        BRACE(a = parseSymbol(nim, FALSE));
        if((a != nkEmpty) && (nim->kind != K_NONE) && (nim->kind != K_PARAM_LIST))
        {
          MAKE_NIM_TAG(nim, nim->kind);
        }
      }
      if (a == nkEmpty) return result;
    }
    else break;

    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
  }

  if ( nim->tok.tokType == tkColon)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseTypeDesc(nim));
  }
  else
  {
    if ( (nim->tok.tokType != tkEquals)
      && ((flags & withBothOptional) != withBothOptional) )
    {
       /* errColonOrEqualsExpected */
    }
  }

  if (nim->tok.tokType == tkEquals)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
  }
  else
  {
    /* addSon(result, ast.emptyNode) */
  }

  return result;
}

static TNodeKind parseTuple(nimParser* nim, boolean indentAllowed)
{
  /* #| inlTupleDecl = 'tuple'
     #|     [' optInd  (identColonEquals (comma/semicolon)?)*  optPar ']'
     #| extTupleDecl = 'tuple'
     #|     COMMENT? (IND{>} identColonEquals (IND{=} identColonEquals)*)?
     #| tupleClass = 'tuple' */

  TNodeKind result = nkTupleTy;

  GET_TOKEN(nim);
  if (nim->tok.tokType == tkBracketLe)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    while ( (nim->tok.tokType == tkSymbol)
      || (nim->tok.tokType == tkAccent) )
    {
      if(nim->kind != K_PARAM_LIST)
      {
        nim->kind = K_ELEMENT;
      }
      BRACE(parseIdentColonEquals(nim, 0));
      if ( !((nim->tok.tokType == tkComma)
        || (nim->tok.tokType == tkSemiColon)) ) break;
      GET_TOKEN(nim);
      BRACE(skipComment(nim));
    }
    BRACE(optPar(nim));
    BRACE(eat(nim, tkBracketRi));
  }
  else if (indentAllowed)
  {
    BRACE(skipComment(nim));
    if (realInd(nim))
    {
      startWithInd(nim);
        BRACE(skipComment(nim));
        while (TRUE)
        {
          if ((nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent))
          {
            BRACE(parseIdentColonEquals(nim, 0));
            BRACE(skipComment(nim));
          }
          else if (nim->tok.tokType == tkEof) break;
          else
          {
            /* errIdentifierExpected */
            break;
          }
          if (!sameInd(nim)) break;
        }
      endWithInd(nim);
    }
  }
  else
  {
    result = nkTupleClassTy;
  }
  return result;
}

static TNodeKind parseParamList(nimParser* nim, boolean retColon)
{
  /* #| paramList = '(' declColonEquals ^* (comma/semicolon) ')'
     #| paramListArrow = paramList? ('->' optInd typeDesc)?
     #| paramListColon = paramList? (':' optInd typeDesc)? */

  TNodeKind result = nkFormalParams;
  boolean hasParLe = (nim->tok.tokType == tkParLe) && (nim->tok.indent < 0);
  boolean hasRet;

  if (hasParLe)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    while(TRUE)
    {
      if ( (nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent) )
      {
        nim->kind = K_PARAM_LIST;
        BRACE(parseIdentColonEquals(nim, withBothOptional|withPragma));
      }
      else if (nim->tok.tokType == tkParRi)
      {
        break;
      }
      else
      {
        /* errTokenExpected, ")" */
        break;
      }

      if ( !((nim->tok.tokType == tkComma)
        && (nim->tok.tokType == tkSemiColon)) ) break;

      GET_TOKEN(nim);
      BRACE(skipComment(nim));
    }
    BRACE(optPar(nim));
    BRACE(eat(nim, tkParRi));
  }

  hasRet = retColon ? (nim->tok.tokType == tkColon)
               : ( (nim->tok.tokType == tkOpr)
               && (strcmp(nim->tok.literal->buffer, "->") == 0) );

  if (hasRet && (nim->tok.indent < 0) )
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseTypeDesc(nim));
  }
  else if (!retColon && !hasParLe)
  {
    result = nkEmpty;
    /* Mark as "not there" in order to mark for deprecation in the semantic pass: */
  }
  return result;
}

static TNodeKind optPragmas(nimParser* nim)
{
  TNodeKind result = nkEmpty;
  if ( nim->tok.tokType == tkCurlyDotLe
    && (nim->tok.indent < 0 || realInd(nim)) )
  {
    BRACE(result = parsePragma(nim));
  }
  return result;
}

static TNodeKind parseDoBlock(nimParser* nim)
{
  /* #| doBlock = 'do' paramListArrow pragmas? colcom stmt */
  GET_TOKEN(nim);
  BRACE(parseParamList(nim, FALSE));
  BRACE(optPragmas(nim));
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return nkDo;
}

static void parseDoBlocks(nimParser* nim)
{
  /* #| doBlocks = doBlock ^* IND{=} */
  if (nim->tok.tokType == tkDo)
  {
    BRACE(parseDoBlock(nim));
    while (sameInd(nim) && nim->tok.tokType == tkDo)
    {
      BRACE(parseDoBlock(nim));
    }
  }
}

static TNodeKind parseProcExpr(nimParser* nim, boolean isExpr)
{
  /* #| procExpr = 'proc' paramListColon pragmas? ('=' COMMENT? stmt)?
     # either a proc type or a anonymous proc */
  GET_TOKEN(nim);
  /*boolean hasSignature = (
    ((nim->tok.tokType == tkParLe)
    || (nim->tok.tokType == tkColon))
    && (nim->tok.indent < 0) );*/

  BRACE(parseParamList(nim, FALSE));
  BRACE(optPragmas(nim));

  if ((nim->tok.tokType == tkEquals) && isExpr)
  {
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
    BRACE(parseStmt(nim));
    return nkLambda;
  }
  return nkProcTy;
}

static boolean isExprStart(nimParser* nim)
{
  switch(nim->tok.tokType)
  {
    case tkSymbol: case tkAccent: case tkOpr:
    case tkNot: case tkNil: case tkCast: case tkIf:
    case tkProc: case tkIterator: case tkBind: case tkAddr:
    case tkParLe: case tkBracketLe: case tkCurlyLe:
    case tkIntLit: case tkInt8Lit: case tkInt16Lit:
    case tkInt32Lit: case tkInt64Lit: case tkUIntLit:
    case tkUInt8Lit: case tkUInt16Lit: case tkUInt32Lit:
    case tkUInt64Lit:  case tkFloatLit: case tkFloat32Lit:
    case tkFloat64Lit: case tkFloat128Lit: case tkStrLit:
    case tkRStrLit: case tkTripleStrLit: case tkGStrLit:
    case tkGTripleStrLit: case tkCharLit:
    case tkVar: case tkRef: case tkPtr:
    case tkTuple: case tkObject: case tkType:
    case tkWhen: case tkCase:
      return TRUE;
    default:
      return FALSE;
  }
}

static void parseSymbolList(nimParser* nim, boolean allowNil)
{
  while(TRUE)
  {
    BRACE(TNodeKind a = parseSymbol(nim, allowNil));
    if (a == nkEmpty) break;
    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
  }
}

static TNodeKind parseTypeDescKAux(nimParser* nim, TNodeKind kind, TPrimaryMode mode)
{
  /* #| distinct = 'distinct' optInd typeDesc */
  TNodeKind result = kind;
  GET_TOKEN(nim);
  BRACE(optInd(nim));

  if (!isOperator(&nim->tok) && isExprStart(nim))
  {
    BRACE(primary(nim, mode));
  }

  if ( (kind == nkDistinctTy) &&
    ((nim->tok.tokType == tkWith)
    || (nim->tok.tokType ==tkWithout)) )
  {
    /*TNodeKind kind = (nim->tok.tokType == tkWith ? nkWith : nkWithout);*/
    GET_TOKEN(nim);
    BRACE(parseSymbolList(nim, TRUE));
  }

  return result;
}

static TNodeKind parseExpr(nimParser* nim)
{
  /*#| expr = (ifExpr
    #|       | whenExpr
    #|       | caseExpr
    #|       | tryExpr)
    #|       / simpleExpr */
  TNodeKind result;

  switch(nim->tok.tokType)
  {
    case tkIf:
    case tkWhen:
      BRACE(result = parseIfExpr(nim, nkIfExpr));
      break;
    case tkCase:
      BRACE(result = parseCase(nim));
      break;
    case tkTry:
      BRACE(result = parseTry(nim, TRUE));
      break;
    default:
      {
        BRACE(result = simpleExpr(nim, pmNormal));
      }
      break;
  }

  return result;
}

static TNodeKind parseEnum(nimParser* nim);
static TNodeKind parseObject(nimParser* nim);
static TNodeKind parseTypeClass(nimParser* nim);

static TNodeKind primary(nimParser* nim, TPrimaryMode mode)
{
  /* #| typeKeyw = 'var' | 'ref' | 'ptr' | 'shared' | 'tuple'
     #|          | 'proc' | 'iterator' | 'distinct' | 'object' | 'enum'
     #| primary = typeKeyw typeDescK
     #|         /  prefixOperator* identOrLiteral primarySuffix*
     #|         / 'static' primary
     #|         / 'bind' primary */

  TNodeKind result = nkEmpty;

  if ( isOperator(&nim->tok) )
  {
    boolean isSigil = isSigilLike(&nim->tok);
    result = nkPrefix;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    if(isSigil)
    {
      /* XXX prefix operators */
      int baseInd = nim->lex.currLineIndent;
      BRACE(primary(nim, pmSkipSuffix));
      BRACE(result = primarySuffix(nim, result, baseInd));
    }
    else
    {
      BRACE(primary(nim, pmNormal));
    }
    return result;
  }

  switch(nim->tok.tokType)
  {
    case tkTuple:
      if(nim->kind != K_PARAM_LIST)
      {
        MAKE_NIM_TAG(nim, K_TUPLE);
        nim->kind = K_TUPLE;
      }
      BRACE(result = parseTuple(nim, mode == pmTypeDef));
      break;

    case tkProc:
      {
        boolean isExpr = !(
          (mode & pmTypeDesc) == pmTypeDesc
          || (mode & pmTypeDef) == pmTypeDef);

        BRACE(result = parseProcExpr(nim, isExpr));
      }
      break;

    case tkIterator:
      {
        boolean isExpr = !(
          (mode & pmTypeDesc) == pmTypeDesc
          || (mode & pmTypeDef) == pmTypeDef);

        BRACE(result = parseProcExpr(nim, isExpr));
      }
      break;

    case tkEnum:
      if (mode == pmTypeDef) {
        MAKE_NIM_TAG(nim, K_ENUM);
        nim->kind = K_ENUM;
        BRACE(result = parseEnum(nim));
      }
      else {
        GET_TOKEN(nim);
        result = nkEnumTy;
      }
      break;

    case tkObject:
      if (mode == pmTypeDef) {
        MAKE_NIM_TAG(nim, K_OBJECT);
        nim->kind = K_OBJECT;
        BRACE(result = parseObject(nim));
      }
      else {
        GET_TOKEN(nim);
        result = nkObjectTy;
      }
      break;

    case tkGeneric: case tkConcept:
      if (mode == pmTypeDef) {
        BRACE(result = parseTypeClass(nim));
      }
      break;

    case tkStatic:
      BRACE(getTokNoInd(nim));
      BRACE(primary(nim, pmNormal));
      result = nkStaticExpr;
      break;

    case tkBind:
      GET_TOKEN(nim);
      BRACE(optInd(nim));
      BRACE(primary(nim, pmNormal));
      result = nkBind;
      break;

    case tkVar:
      if(nim->kind != K_PARAM_LIST)
      {
        MAKE_NIM_TAG(nim, K_TYPE);
      }
      BRACE(result = parseTypeDescKAux(nim, nkVarTy, mode));
      break;

    case tkRef:
      BRACE(result = parseTypeDescKAux(nim, nkRefTy, mode));
      break;

    case tkPtr:
      if(nim->kind != K_PARAM_LIST)
      {
        MAKE_NIM_TAG(nim, K_TYPE);
        nim->kind = K_TYPE;
      }
      BRACE(result = parseTypeDescKAux(nim, nkPtrTy, mode));
      break;

    case tkDistinct:
      MAKE_NIM_TAG(nim, K_TYPE);
      nim->kind = K_TYPE;
      BRACE(result = parseTypeDescKAux(nim, nkDistinctTy, mode));
      break;

    default:
      {
        int baseInd = nim->lex.currLineIndent;
        BRACE(result = identOrLiteral(nim, mode));
        if (nim->kind == K_NONE && nim->kind != K_PARAM_LIST && nim->kind != K_IGNORE) {
          MAKE_NIM_TAG(nim, K_TYPE);
        }
        if (mode != pmSkipSuffix) {
          BRACE(result = primarySuffix(nim, result, baseInd));
        }
      }
      break;
  }
  return result;
}

static TNodeKind parseTypeDesc(nimParser* nim)
{
  /*| typeDesc = simpleExpr */
  TNodeKind result;
  BRACE(result = simpleExpr(nim, pmNormal));
  return result;
}

static TNodeKind parseTypeDefAux(nimParser* nim)
{
  /* #| typeDefAux = simpleExpr
     #|            | 'concept' typeClass */
  TNodeKind result;
  BRACE(result = simpleExpr(nim, pmTypeDef));
  return result;
}

static boolean inMacroColonLookAhead(tokenType tokType)
{
  if (tokType == tkOf) return TRUE;
  if (tokType == tkElif) return TRUE;
  if (tokType == tkElse) return TRUE;
  if (tokType == tkExcept) return TRUE;
  return FALSE;
}

static TNodeKind parseMacroColon(nimParser* nim, TNodeKind x)
{
  /*#| macroColon = ':' stmt? ( IND{=} 'of' exprList ':' stmt
    #|                        | IND{=} 'elif' expr ':' stmt
    #|                        | IND{=} 'except' exprList ':' stmt
    #|                        | IND{=} 'else' ':' stmt )**/

  TNodeKind result = x;

  if ((nim->tok.tokType == tkColon) && (nim->tok.indent < 0))
  {
    result = nkCall;
    GET_TOKEN(nim);
    BRACE(skipComment(nim));

    if ( !inMacroColonLookAhead(nim->tok.tokType) )
    {
      BRACE(parseStmt(nim));
    }

    while (sameInd(nim))
    {
      TNodeKind b;
      if(nim->tok.tokType == tkOf)
      {
        b = nkOfBranch;
        BRACE(exprList(nim, tkColon));
      }
      else if(nim->tok.tokType == tkElif)
      {
        b = nkElifBranch;
        GET_TOKEN(nim);
        BRACE(optInd(nim));
        BRACE(parseExpr(nim));
      }
      else if(nim->tok.tokType == tkExcept)
      {
        b = nkExceptBranch;
        BRACE(exprList(nim, tkColon));
      }
      else if(nim->tok.tokType == tkElse)
      {
        b = nkElse;
        GET_TOKEN(nim);
      }
      else break;

      BRACE(eat(nim, tkColon));
      BRACE(parseStmt(nim));
      if (b == nkElse) break;
    }
  }
  return result;
}

static TNodeKind parseExprStmt(nimParser* nim)
{
  /*#| exprStmt = simpleExpr
    #|          (( '=' optInd expr )
    #|          / ( expr ^+ comma
    #|              doBlocks
    #|               / macroColon
    #|            ))?*/

  TNodeKind a;
  TNodeKind result;

  BRACE(a = simpleExpr(nim, pmNormal));
  if (nim->tok.tokType == tkEquals)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
    result = nkAsgn;
  }
  else
  {
    /* # simpleExpr parsed 'p a' from 'p a, b'? */
    if ((nim->tok.indent < 0) && (nim->tok.tokType == tkComma) && (a == nkCommand))
    {
      result = a;
      while(TRUE)
      {
        GET_TOKEN(nim);
        BRACE(optInd(nim));
        BRACE(parseExpr(nim));
        if(nim->tok.tokType != tkComma) break;
      }
    }
    else if ( (nim->tok.indent < 0) && (isExprStart(nim)) )
    {
      result = nkCommand;
      while(TRUE)
      {
        BRACE(parseExpr(nim));
        if (nim->tok.tokType != tkComma) break;
        GET_TOKEN(nim);
        BRACE(optInd(nim));
      }
    }
    else
    {
      result = a;
    }

    if ((nim->tok.tokType == tkDo) && (nim->tok.indent < 0))
    {
      result = nkCall;
      BRACE(parseDoBlocks(nim));
      return result;
    }

    BRACE(result = parseMacroColon(nim, result));
  }

  return result;
}

static TNodeKind parseModuleName(nimParser* nim)
{
  TNodeKind result;
  BRACE(result = parseExpr(nim));
  /* # parseExpr already handles 'as' syntax ... */
  return result;
}

static TNodeKind parseImport(nimParser* nim, TNodeKind kind)
{
  /* #| importStmt = 'import' optInd expr
     #|               ((comma expr)*
     #|               / 'except' optInd (expr ^+ comma))*/

  nim->kind = K_IGNORE;
  GET_TOKEN(nim);  /* # skip `import` or `export` */
  BRACE(optInd(nim));
  BRACE(parseModuleName(nim));

  if ((nim->tok.tokType == tkComma) || (nim->tok.tokType == tkExcept))
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    while(TRUE)
    {
      /* # was: while nim->tok.tokType notin {tkEof, tkSad, tkDed}: */
      BRACE(TNodeKind a = parseModuleName(nim));
      if (a == nkEmpty) break;
      if (nim->tok.tokType != tkComma) break;
      GET_TOKEN(nim);
      BRACE(optInd(nim));
    }
  }

  return kind;
}

static TNodeKind parseIncludeStmt(nimParser* nim)
{
  /* #| includeStmt = 'include' optInd expr ^+ comma */

  GET_TOKEN(nim);    /* # skip `import` or `include`*/
  nim->kind = K_IGNORE;
  BRACE(optInd(nim));
  while(TRUE)
  {
    /* # was: while nim->tok.tokType notin {tkEof, tkSad, tkDed}: */
      BRACE(TNodeKind a = parseExpr(nim));
      if (a == nkEmpty) break;
      if (nim->tok.tokType != tkComma) break;
      GET_TOKEN(nim);
      BRACE(optInd(nim));
  }

  return nkIncludeStmt;
}

static TNodeKind parseFromStmt(nimParser* nim)
{
  /* #| fromStmt = 'from' moduleName 'import' optInd expr (comma expr)* */
  GET_TOKEN(nim); /* # skip `from` */
  BRACE(optInd(nim));
  BRACE(parseModuleName(nim));
  BRACE(eat(nim, tkImport));
  BRACE(optInd(nim));
  while(TRUE)
  {
    /*  # nim->tok.tokType notin {tkEof, tkSad, tkDed}:*/
    BRACE(TNodeKind a = parseExpr(nim));
    if (a == nkEmpty) break;
    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
  }

  return nkFromStmt;
}

static TNodeKind parseReturnOrRaise(nimParser* nim, TNodeKind kind)
{
  /* #| returnStmt = 'return' optInd expr?
     #| raiseStmt = 'raise' optInd expr?
     #| yieldStmt = 'yield' optInd expr?
     #| discardStmt = 'discard' optInd expr?
     #| breakStmt = 'break' optInd expr?
     #| continueStmt = 'break' optInd expr? */

  GET_TOKEN(nim);
  if (nim->tok.tokType == tkComment)
  {
    BRACE(skipComment(nim));
  }
  else if ( ((nim->tok.indent >= 0)
    && (nim->tok.indent <= nim->currInd))
    || !isExprStart(nim))
  {
    /*# NL terminates:
    addSon(result, ast.emptyNode) */
  }
  else {
    BRACE(parseExpr(nim));
  }
  return kind;
}

static TNodeKind parseIfOrWhen(nimParser* nim, TNodeKind kind)
{
  /*#| condStmt = expr colcom stmt COMMENT?
    #|            (IND{=} 'elif' expr colcom stmt)*
    #|            (IND{=} 'else' colcom stmt)?
    #| ifStmt = 'if' condStmt
    #| whenStmt = 'when' condStmt*/

  while(TRUE)
  {
    GET_TOKEN(nim);        /* # skip `if`, `when`, `elif` */
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
    BRACE(colcom(nim));
    BRACE(parseStmt(nim));
    BRACE(skipComment(nim));
    if ((nim->tok.tokType != tkElif) || !sameOrNoInd(nim)) break;
  }

  if ((nim->tok.tokType == tkElse) && sameOrNoInd(nim))
  {
    BRACE(eat(nim, tkElse));
    BRACE(colcom(nim));
    BRACE(parseStmt(nim));
  }

  return kind;
}

static TNodeKind parseWhile(nimParser* nim)
{
  /* #| whileStmt = 'while' expr colcom stmt */
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  BRACE(parseExpr(nim));
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return nkWhileStmt;
}

static TNodeKind parseCase(nimParser* nim)
{
  /*  #| ofBranch = 'of' exprList colcom stmt
      #| ofBranches = ofBranch (IND{=} ofBranch)*
      #|                       (IND{=} 'elif' expr colcom stmt)*
      #|                       (IND{=} 'else' colcom stmt)?
      #| caseStmt = 'case' expr ':'? COMMENT?
      #|             (IND{>} ofBranches DED
      #|             | IND{=} ofBranches)*/

    boolean inElif = FALSE;
    boolean wasIndented = FALSE;
    int oldInd;
    boolean needToBreak = FALSE;

    GET_TOKEN(nim);
    BRACE(parseExpr(nim));
    if (nim->tok.tokType == tkColon) GET_TOKEN(nim);
    BRACE(skipComment(nim));

    oldInd = nim->currInd;
    if (realInd(nim))
    {
      nim->currInd = nim->tok.indent;
      wasIndented = TRUE;
    }

    while (sameInd(nim))
    {
      if (nim->tok.tokType == tkOf)
      {
        if (inElif) break;
        BRACE(exprList(nim, tkColon));
      }
      else if (nim->tok.tokType == tkElif)
      {
        inElif = TRUE;
        GET_TOKEN(nim);
        BRACE(optInd(nim));
        BRACE(parseExpr(nim));
      }
      if (nim->tok.tokType == tkElse)
      {
        needToBreak = TRUE;
        GET_TOKEN(nim);
      }
      else
      {
        break;
      }

      BRACE(colcom(nim));
      BRACE(parseStmt(nim));
      if (needToBreak) break;
    }

    if (wasIndented) nim->currInd = oldInd;
    return nkCaseStmt;
}

static TNodeKind parseTry(nimParser* nim, boolean isExpr)
{
    /* #| tryStmt = 'try' colcom stmt &(IND{=}? 'except'|'finally')
       #|            (IND{=}? 'except' exprList colcom stmt)*
       #|            (IND{=}? 'finally' colcom stmt)?
       #| tryExpr = 'try' colcom stmt &(optInd 'except'|'finally')
       #|            (optInd 'except' exprList colcom stmt)*
       #|            (optInd 'finally' colcom stmt)? */

  GET_TOKEN(nim);
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  boolean needToBreak = FALSE;

  while (sameOrNoInd(nim) || isExpr)
  {
      if (nim->tok.tokType == tkExcept)
      {
        BRACE(exprList(nim, tkColon));
      }
      else if (nim->tok.tokType == tkFinally)
      {
        GET_TOKEN(nim);
        needToBreak = TRUE;
      }
      else
      {
         break;
      }

      BRACE(colcom(nim));
      BRACE(parseStmt(nim));
      if (needToBreak) break;
  }
  return nkTryStmt;
}

static TNodeKind parseExceptBlock(nimParser* nim, TNodeKind kind)
{
  /* #| exceptBlock = 'except' colcom stmt */
  GET_TOKEN(nim);
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return kind;
}

static TNodeKind parseFor(nimParser* nim)
{
  /* #| forStmt = 'for' (identWithPragma ^+ comma) 'in' expr colcom stmt */
  BRACE(getTokNoInd(nim));
  BRACE(identWithPragma(nim));
  while(nim->tok.tokType == tkComma)
  {
      GET_TOKEN(nim);
      BRACE(optInd(nim));
      BRACE(identWithPragma(nim));
  }
  BRACE(eat(nim, tkIn));
  BRACE(parseExpr(nim));
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return nkForStmt;
}

static TNodeKind parseBlock(nimParser* nim)
{
  /* #| blockStmt = 'block' symbol? colcom stmt */
  BRACE(getTokNoInd(nim));
  if (nim->tok.tokType == tkColon) {
      /* addSon(result, ast.emptyNode) */
  }
  else {
    BRACE(parseSymbol(nim, FALSE));
  }
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return nkBlockStmt;
}

static TNodeKind parseStaticOrDefer(nimParser* nim, TNodeKind kind)
{
  /* #| staticStmt = 'static' colcom stmt
     #| deferStmt = 'defer' colcom stmt */
  GET_TOKEN(nim);
  BRACE(colcom(nim));
  BRACE(parseStmt(nim));
  return kind;
}

static TNodeKind parseAsm(nimParser* nim)
{
  /* #| asmStmt = 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLE_STR_LIT) */
  BRACE(getTokNoInd(nim));
  if (nim->tok.tokType == tkCurlyDotLe) {
    BRACE(parsePragma(nim));
  }

  /* ignore stringlit, rstringlit, triplestrlit here */
  GET_TOKEN(nim);
  return nkAsmStmt;
}

static TNodeKind parseGenericParam(nimParser* nim)
{
  /* #| genericParam = symbol (comma symbol)* (colon expr)? ('=' optInd expr)? */
  TNodeKind result = nkIdentDefs;

  while(TRUE)
  {
      if ((nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent))
      {
          BRACE(TNodeKind a = parseSymbol(nim, FALSE));
          if (a == nkEmpty) return result;
      }
      else break;
      if (nim->tok.tokType != tkComma) break;
      GET_TOKEN(nim);
      BRACE(optInd(nim));
  }

  if (nim->tok.tokType == tkColon)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
  }

  if(nim->tok.tokType == tkEquals)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
  }

  return result;
}

static TNodeKind parseGenericParamList(nimParser* nim)
{
  /* #| genericParamList = '[' optInd
     #|   genericParam ^* (comma/semicolon) optPar ']' */
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  while ((nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent))
  {
    BRACE(parseGenericParam(nim));
    if ( !((nim->tok.tokType == tkComma) || (nim->tok.tokType == tkSemiColon)) ) break;
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
  }
  BRACE(optPar(nim));
  BRACE(eat(nim, tkBracketRi));
  return nkGenericParams;
}

static TNodeKind parsePattern(nimParser* nim)
{
  /* #| pattern = '{' stmt '}' */
  TNodeKind result;
  BRACE(eat(nim, tkCurlyLe));
  BRACE(result = parseStmt(nim));
  BRACE(eat(nim, tkCurlyRi));
  return result;
}

static boolean validInd(nimParser* nim)
{
  return (nim->tok.indent < 0) || (nim->tok.indent > nim->currInd);
}

static nimKind routineKind(TNodeKind kind)
{
  switch(kind)
  {
    case nkProcDef: return K_PROC;
    case nkMethodDef: return K_METHOD;
    case nkIteratorDef: return K_ITERATOR;
    case nkMacroDef: return K_MACRO;
    case nkTemplateDef: return K_TEMPLATE;
    case nkConverterDef: return K_CONVERTER;
    case nkVarSection: return K_VARIABLE;
    case nkLetSection: return K_LET;
    case nkConstSection: return K_CONSTANT;
    default: return K_NONE;
  }
}

static TNodeKind parseRoutine(nimParser* nim, TNodeKind kind)
{
  /*#| indAndComment = (IND{>} COMMENT)? | COMMENT?
    #| routine = optInd identVis pattern? genericParamList?
    #|   paramListColon pragma? ('=' COMMENT? stmt)? indAndComment*/
  GET_TOKEN(nim);
  BRACE(optInd(nim));

  if (identVis(nim) != nkEmpty)
  {
    MAKE_NIM_TAG(nim, routineKind(kind));
  }

  if ((nim->tok.tokType == tkCurlyLe) && validInd(nim) )
  {
    BRACE(parsePattern(nim));
  }

  if ((nim->tok.tokType == tkBracketLe) && validInd(nim))
  {
    BRACE(parseGenericParamList(nim));
  }

  BRACE(parseParamList(nim, TRUE));

  if ((nim->tok.tokType == tkCurlyDotLe) && validInd(nim))
  {
    BRACE(parsePragma(nim));
  }

  /* empty exception tracking: */

  if ((nim->tok.tokType == tkEquals) && validInd(nim))
  {
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
    BRACE(parseStmt(nim));
  }

  BRACE(indAndComment(nim));

  return kind;
}

static TNodeKind newCommentStmt(nimParser* nim)
{
  /*| commentStmt = COMMENT */
  GET_TOKEN(nim);
  return nkCommentStmt;
}

static boolean inSectionLookAhead(nimParser* nim)
{
  /* tkParLe is allowed for ``var (x, y) = ...`` tuple parsing */
  if (nim->tok.tokType == tkSymbol) return TRUE;
  if (nim->tok.tokType == tkAccent) return TRUE;
  if (nim->tok.tokType == tkParLe) return TRUE;
  return FALSE;
}

static TNodeKind parseSection(nimParser* nim, TNodeKind kind, TDefParser defparser)
{
  /* section(p) = COMMENT? p / (IND{>} (p / COMMENT)^+IND{=} DED) */

  if(nim->kind != K_PARAM_LIST) {
    nim->kind = routineKind(kind);
  }

  if (kind != nkTypeSection) GET_TOKEN(nim);
  BRACE(skipComment(nim));

  if (realInd(nim) )
  {
    startWithInd(nim);
    BRACE(skipComment(nim));
    while ( sameInd(nim) )
    {
        if (nim->tok.tokType == tkSymbol
            || nim->tok.tokType == tkAccent
            || nim->tok.tokType == tkParLe)
        {
          (*defparser)(nim);
          BRACE(skipComment(nim));
        }
        else if (nim->tok.tokType == tkComment)
        {
          BRACE(newCommentStmt(nim));
        }
        else
        {
          /*  errIdentifierExpected */
          break;
        }
    }
    endWithInd(nim);
  }
  else if ( inSectionLookAhead(nim) && nim->tok.indent < 0)
  {
    (*defparser)(nim);
  }
  else
  {
    /* errIdentifierExpected */
  }

  return kind;
}

static TNodeKind parseConstant(nimParser* nim)
{
  /* | constant = identWithPragma (colon typedesc)? '=' optInd expr indAndComment */

  if (identWithPragma(nim) != nkEmpty)
  {
    MAKE_NIM_TAG(nim, K_CONSTANT);
  }

  if (nim->tok.tokType == tkColon)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseTypeDesc(nim));
  }

  BRACE(eat(nim, tkEquals));
  BRACE(optInd(nim));
  BRACE(parseExpr(nim));
  BRACE(indAndComment(nim));
  return nkConstDef;
}

static TNodeKind parseEnum(nimParser* nim)
{
  /* #| enum = 'enum' optInd (symbol optInd ('=' optInd expr COMMENT?)? comma?)+ */
  TNodeKind result = nkEnumTy;

  GET_TOKEN(nim);
  BRACE(optInd(nim));
  while(TRUE)
  {
    TNodeKind a = parseSymbol(nim, FALSE);
    if (a != nkEmpty && nim->kind == K_ENUM)
    {
      MAKE_NIM_TAG(nim, K_ENUMERATOR);
    }
    if (a == nkEmpty) return result;
    if ( (nim->tok.indent >= 0) && (nim->tok.indent <= nim->currInd) )
    {
      break;
    }

    if ( (nim->tok.tokType == tkEquals) && (nim->tok.indent < 0) )
    {
      GET_TOKEN(nim);
      BRACE(optInd(nim));
      BRACE(parseExpr(nim));
      BRACE(skipComment(nim));
    }

    if (( nim->tok.tokType == tkComma) && (nim->tok.indent < 0) )
    {
      GET_TOKEN(nim);
      BRACE(rawSkipComment(nim));
    }
    else
    {
      BRACE(skipComment(nim));
    }

    if ( ((nim->tok.indent >= 0)
      &&  (nim->tok.indent <= nim->currInd))
      || (nim->tok.tokType == tkEof)  )
    {
      break;
    }
  }
  return result;
}

static TNodeKind parseObjectPart(nimParser* nim);
static TNodeKind parseObjectWhen(nimParser* nim)
{
   /* #| objectWhen = 'when' expr colcom objectPart COMMENT?
      #|             ('elif' expr colcom objectPart COMMENT?)*
      #|             ('else' colcom objectPart COMMENT?)?*/

  while (sameInd(nim))
  {
    GET_TOKEN(nim);     /* # skip `when`, `elif` */
    BRACE(optInd(nim));
    BRACE(parseExpr(nim));
    BRACE(colcom(nim));
    BRACE(parseObjectPart(nim));
    BRACE(skipComment(nim));
    if (nim->tok.tokType != tkElif) break;
  }

  if ( (nim->tok.tokType == tkElse) && sameInd(nim) )
  {
    BRACE(eat(nim, tkElse));
    BRACE(colcom(nim));
    BRACE(parseObjectPart(nim));
    BRACE(skipComment(nim));
  }

  return nkRecWhen;
}

static TNodeKind parseObjectCase(nimParser* nim)
{
  /*  #| objectBranch = 'of' exprList colcom objectPart
      #| objectBranches = objectBranch (IND{=} objectBranch)*
      #|                       (IND{=} 'elif' expr colcom objectPart)*
      #|                       (IND{=} 'else' colcom objectPart)?
      #| objectCase = 'case' identWithPragma ':' typeDesc ':'? COMMENT?
      #|             (IND{>} objectBranches DED
      #|             | IND{=} objectBranches)*/

  boolean wasIndented;
  int oldInd;
  boolean needToBreak = FALSE;

  BRACE(getTokNoInd(nim));
  BRACE(identWithPragma(nim));
  BRACE(eat(nim, tkColon));
  BRACE(parseTypeDesc(nim));

  if (nim->tok.tokType == tkColon) GET_TOKEN(nim);
  BRACE(skipComment(nim));

  wasIndented = FALSE;
  oldInd = nim->currInd;

  if (realInd(nim))
  {
    nim->currInd = nim->tok.indent;
    wasIndented = TRUE;
  }

  while (sameInd(nim))
  {
    if (nim->tok.tokType == tkOf)
    {
      BRACE(exprList(nim, tkColon));
    }
    if (nim->tok.tokType == tkElse)
    {
      GET_TOKEN(nim);
      needToBreak = TRUE;
    }
    else
    {
       break;
    }

    BRACE(colcom(nim));
    BRACE(parseObjectPart(nim));
    if (needToBreak) break;
  }

  if (wasIndented) nim->currInd = oldInd;
  return nkRecCase;
}

static boolean inObjectPartLookAhead(tokenType tokTyp)
{
  if (tokTyp == tkCase) return TRUE;
  if (tokTyp == tkWhen) return TRUE;
  if (tokTyp == tkSymbol) return TRUE;
  if (tokTyp == tkAccent) return TRUE;
  if (tokTyp == tkNil) return TRUE;
  if (tokTyp == tkDiscard) return TRUE;
  return FALSE;
}

static TNodeKind parseObjectPart(nimParser* nim)
{
  /*  #| objectPart = IND{>} objectPart^+IND{=} DED
      #|            / objectWhen / objectCase / 'nil' / 'discard' / declColonEquals */

  TNodeKind result = nkRecList;

  if (realInd(nim))
  {
    startWithInd(nim);
    BRACE(rawSkipComment(nim));
    while (sameInd(nim))
    {
       if (inObjectPartLookAhead(nim->tok.tokType))
       {
          BRACE(parseObjectPart(nim));
       }
       else
       {
         /* errIdentifierExpected */
         break;
       }
    }
    endWithInd(nim);
  }
  else
  {
    switch(nim->tok.tokType)
    {
      case tkWhen:
        BRACE(parseObjectWhen(nim));
        break;
      case tkCase:
        BRACE(parseObjectCase(nim));
        break;
      case tkSymbol: case tkAccent:
        nim->kind = K_FIELD;
        BRACE(parseIdentColonEquals(nim, withPragma));
        BRACE(skipComment(nim));
        break;
      case tkNil: case tkDiscard:
        GET_TOKEN(nim);
        break;
      default:
        result = nkEmpty;
        break;
    }
  }

  return result;
}

static TNodeKind parseObject(nimParser* nim)
{
  /* #| object = 'object' pragma? ('of' typeDesc)? COMMENT? objectPart */

  TNodeKind result = nkObjectTy;

  GET_TOKEN(nim);
  if ((nim->tok.tokType == tkCurlyDotLe) && validInd(nim))
  {
    BRACE(parsePragma(nim));
  }

  if ((nim->tok.tokType == tkOf) && (nim->tok.indent < 0))
  {
    GET_TOKEN(nim);
    BRACE(parseTypeDesc(nim));
  }

  if (nim->tok.tokType == tkComment)
  {
    BRACE(skipComment(nim));
  }

  /* an initial IND{>} HAS to follow: */
  if (!realInd(nim))
  {
    return result;
  }

  BRACE(parseObjectPart(nim));
  return result;
}

static TNodeKind parseTypeClassParam(nimParser* nim)
{
  TNodeKind result;
  if (nim->tok.tokType == tkVar)
  {
    result = nkVarTy;
    GET_TOKEN(nim);
    BRACE(parseSymbol(nim, FALSE));
  }
  else {
    BRACE(result = parseSymbol(nim, FALSE));
  }

  return result;
}

static TNodeKind parseTypeClass(nimParser* nim)
{
  /*#| typeClassParam = ('var')? symbol
    #| typeClass = typeClassParam ^* ',' (pragma)? ('of' typeDesc ^* ',')?
    #|               &IND{>} stmt*/

  GET_TOKEN(nim);
  BRACE(parseTypeClassParam(nim));

  while (nim->tok.tokType == tkComma)
  {
    GET_TOKEN(nim);
    BRACE(parseTypeClassParam(nim));
  }

  if ((nim->tok.tokType == tkCurlyDotLe) && validInd(nim))
  {
    BRACE(parsePragma(nim));
  }

  if ((nim->tok.tokType == tkOf) && (nim->tok.indent < 0))
  {
    GET_TOKEN(nim);
    while(TRUE)
    {
      BRACE(parseTypeDesc(nim));
      if (nim->tok.tokType != tkComma) break;
      GET_TOKEN(nim);
    }
  }

  if (nim->tok.tokType == tkComment) {
    BRACE(skipComment(nim));
  }

  /* an initial IND{>} HAS to follow: */
  if (realInd(nim)) {
    BRACE(parseStmt(nim));
  }

  return nkTypeClassTy;
}

static TNodeKind parseTypeDef(nimParser* nim)
{
  /*#| typeDef = identWithPragma genericParamList? '=' optInd typeDefAux
    #|             indAndComment?*/
  TNodeKind a;
  BRACE(a = identWithPragma(nim));
  if(a != nkEmpty) {
    nim->kind = K_NONE;
  }

  if ((nim->tok.tokType == tkBracketLe) && validInd(nim))
  {
    BRACE(parseGenericParamList(nim));
  }

  if (nim->tok.tokType == tkEquals)
  {
    GET_TOKEN(nim);
    BRACE(optInd(nim));
    BRACE(parseTypeDefAux(nim));
  }

  BRACE(indAndComment(nim));   /* # special extension! */
  return nkTypeDef;
}

static TNodeKind parseVarTuple(nimParser* nim)
{
  /*  #| varTuple = '(' optInd identWithPragma ^+ comma optPar ')' '=' optInd expr*/
  GET_TOKEN(nim);       /* # skip '('*/
  BRACE(optInd(nim));
  while ((nim->tok.tokType == tkSymbol) || (nim->tok.tokType == tkAccent))
  {
    TNodeKind a;
    BRACE(a = identWithPragma(nim));
    if(a != nkEmpty) {
      MAKE_NIM_TAG(nim, nim->kind);
    }
    if (nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
  }
  BRACE(optPar(nim));
  BRACE(eat(nim, tkParRi));
  BRACE(eat(nim, tkEquals));
  BRACE(optInd(nim));
  BRACE(parseExpr(nim));
  return nkVarTuple;
}

static TNodeKind parseVariable(nimParser* nim)
{
  /*  #| variable = (varTuple / identColonEquals) indAndComment */
  TNodeKind result;
  if (nim->tok.tokType == tkParLe) {
    BRACE(result = parseVarTuple(nim));
  }
  else {
    BRACE(result = parseIdentColonEquals(nim, withPragma));
  }
  BRACE(indAndComment(nim));
  return result;
}

static TNodeKind parseBind(nimParser* nim, TNodeKind kind)
{
 /* #| bindStmt = 'bind' optInd qualifiedIdent ^+ comma
    #| mixinStmt = 'mixin' optInd qualifiedIdent ^+ comma */
  GET_TOKEN(nim);
  BRACE(optInd(nim));
  while(TRUE)
  {
    BRACE(qualifiedIdent(nim));
    if(nim->tok.tokType != tkComma) break;
    GET_TOKEN(nim);
    BRACE(optInd(nim));
  }
  return kind;
}

static TNodeKind parseStmtPragma(nimParser* nim)
{
  /* #| pragmaStmt = pragma (':' COMMENT? stmt)? */
  TNodeKind result;
  BRACE(result = parsePragma(nim));
  if ((nim->tok.tokType == tkColon) && (nim->tok.indent < 0))
  {
    result = nkPragmaBlock;
    GET_TOKEN(nim);
    BRACE(skipComment(nim));
    BRACE(parseStmt(nim));
  }
  return result;
}

static TNodeKind simpleStmt(nimParser* nim)
{
  /* #| simpleStmt = ((returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
     #|            | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
     #|            | includeStmt | commentStmt) / exprStmt) COMMENT?
     #|*/

  TNodeKind result;

  switch(nim->tok.tokType)
  {
  case tkReturn: BRACE(result = parseReturnOrRaise(nim, nkReturnStmt)); break;
  case tkRaise: BRACE(result = parseReturnOrRaise(nim, nkRaiseStmt)); break;
  case tkYield: BRACE(result = parseReturnOrRaise(nim, nkYieldStmt)); break;
  case tkDiscard: BRACE(result = parseReturnOrRaise(nim, nkDiscardStmt)); break;
  case tkBreak: BRACE(result = parseReturnOrRaise(nim, nkBreakStmt)); break;
  case tkContinue: BRACE(result = parseReturnOrRaise(nim, nkContinueStmt)); break;
  case tkCurlyDotLe: BRACE(result = parseStmtPragma(nim)); break;
  case tkImport: BRACE(result = parseImport(nim, nkImportStmt)); break;
  case tkExport: BRACE(result = parseImport(nim, nkExportStmt)); break;
  case tkFrom: BRACE(result = parseFromStmt(nim)); break;
  case tkInclude: BRACE(result = parseIncludeStmt(nim)); break;
  case tkComment: BRACE(result = newCommentStmt(nim)); break;
  default:
    if (isExprStart(nim))
    {
      BRACE(result = parseExprStmt(nim));
      break;
    }
    else
    {
      result = nkEmpty;
    }
  }

  if ((result == nkEmpty) || (result == nkCommentStmt))
  {
    BRACE(skipComment(nim));
  }
  return result;
}

static boolean inParseStmtLookAhead(tokenType tokType)
{
  if (tokType == tkIf) return TRUE;
  if (tokType == tkWhile) return TRUE;
  if (tokType == tkCase) return TRUE;
  if (tokType == tkTry) return TRUE;
  if (tokType == tkFor) return TRUE;
  if (tokType == tkBlock) return TRUE;
  if (tokType == tkAsm) return TRUE;
  if (tokType == tkProc) return TRUE;
  if (tokType == tkIterator) return TRUE;
  if (tokType == tkMacro) return TRUE;
  if (tokType == tkType) return TRUE;
  if (tokType == tkConst) return TRUE;
  if (tokType == tkWhen) return TRUE;
  if (tokType == tkVar) return TRUE;
  return FALSE;
}

static TNodeKind complexOrSimpleStmt(nimParser* nim)
{
  /* #| complexOrSimpleStmt = (ifStmt | whenStmt | whileStmt
  #|                     | tryStmt | forStmt
  #|                     | blockStmt | staticStmt | deferStmt | asmStmt
  #|                     | 'proc' routine
  #|                     | 'method' routine
  #|                     | 'iterator' routine
  #|                     | 'macro' routine
  #|                     | 'template' routine
  #|                     | 'converter' routine
  #|                     | 'type' section(typeDef)
  #|                     | 'const' section(constant)
  #|                     | ('let' | 'var') section(variable)
  #|                     | bindStmt | mixinStmt)
  #|                     / simpleStmt*/

  TNodeKind result;

  switch(nim->tok.tokType)
  {
  case tkIf: BRACE(result = parseIfOrWhen(nim, nkIfStmt)); break;
  case tkWhile: BRACE(result = parseWhile(nim)); break;
  case tkCase: BRACE(result = parseCase(nim)); break;
  case tkTry: BRACE(result = parseTry(nim, FALSE)); break;
  case tkFinally: BRACE(result = parseExceptBlock(nim, nkFinally)); break;
  case tkExcept: BRACE(result = parseExceptBlock(nim, nkExceptBranch)); break;
  case tkFor: BRACE(result = parseFor(nim)); break;
  case tkBlock: BRACE(result = parseBlock(nim)); break;
  case tkStatic: BRACE(result = parseStaticOrDefer(nim, nkStaticStmt)); break;
  case tkDefer: BRACE(result = parseStaticOrDefer(nim, nkDefer)); break;
  case tkAsm: BRACE(result = parseAsm(nim)); break;
  case tkProc: BRACE(result = parseRoutine(nim, nkProcDef)); break;
  case tkMethod: BRACE(result = parseRoutine(nim, nkMethodDef)); break;
  case tkIterator: BRACE(result = parseRoutine(nim, nkIteratorDef)); break;
  case tkMacro: BRACE(result = parseRoutine(nim, nkMacroDef)); break;
  case tkTemplate: BRACE(result = parseRoutine(nim, nkTemplateDef)); break;
  case tkConverter: BRACE(result = parseRoutine(nim, nkConverterDef)); break;
  case tkType:
    GET_TOKEN(nim);
    if (nim->tok.tokType == tkParLe)
    {
      GET_TOKEN(nim);
      result = nkTypeOfExpr;
      BRACE(primary(nim, pmTypeDesc));
      BRACE(eat(nim, tkParRi));
      BRACE(result = parseOperators(nim, result, -1, pmNormal));
    }
    else
    {
      BRACE(result = parseSection(nim, nkTypeSection, parseTypeDef));
    }
    break;
  case tkConst: BRACE(result = parseSection(nim, nkConstSection, parseConstant)); break;
  case tkLet: BRACE(result = parseSection(nim, nkLetSection, parseVariable)); break;
  case tkWhen: BRACE(result = parseIfOrWhen(nim, nkWhenStmt)); break;
  case tkVar: BRACE(result = parseSection(nim, nkVarSection, parseVariable)); break;
  case tkBind: BRACE(result = parseBind(nim, nkBindStmt)); break;
  case tkMixin: BRACE(result = parseBind(nim, nkMixinStmt)); break;
  case tkUsing: BRACE(result = parseBind(nim, nkUsingStmt)); break;
  default: BRACE(result = simpleStmt(nim)); break;
  }

  return result;
}

static TNodeKind parseStmt(nimParser* nim)
{
/*  #| stmt = (IND{>} complexOrSimpleStmt^+(IND{=} / ';') DED)
    #|      / simpleStmt ^+ ';'*/

  TNodeKind result, b;

  if (nim->tok.indent > nim->currInd)
  {
    result = nkStmtList;
    startWithInd(nim);
    while(TRUE)
    {
        if (nim->tok.indent == nim->currInd)
        {
           /* discard */
        }
        else if (nim->tok.tokType == tkSemiColon)
        {
          GET_TOKEN(nim);
          if ((nim->tok.indent < 0) || (nim->tok.indent == nim->currInd)) { /*discard*/}
          else break;
        }
        else
        {
          if ((nim->tok.indent > nim->currInd) && (nim->tok.tokType != tkDot))
          {
            /* errInvalidIndentation */
          }
          break;
        }

        if (inRightBracket(nim->tok.tokType))
        {
          /*# XXX this ensures tnamedparamanonproc still compiles;
          # deprecate this syntax later*/
          break;
        }

        BRACE(b = complexOrSimpleStmt(nim));
        if (b == nkEmpty)
        {
          /* errExprExpected */
          GET_TOKEN(nim);
        }
    }
    endWithInd(nim);
  }
  else
  {
    /* # the case statement is only needed for better error messages: */
    if (inParseStmtLookAhead(nim->tok.tokType))
    {
      /* errComplexStmtRequiresInd */
      result = nkEmpty;
    }
    else
    {
      if (nim->inSemiStmtList > 0)
      {
        BRACE(result = simpleStmt(nim));
      }
      else
      {
        result = nkStmtList;
        while(TRUE)
        {
          BRACE(simpleStmt(nim));
          if (nim->tok.tokType != tkSemiColon) break;
          GET_TOKEN(nim);
        }
      }
    }
  }

  return result;
}

static TNodeKind parseAll(nimParser* nim)
{
  /* ## Parses the rest of the input stream held by the parser into a PNode. */

  while (nim->tok.tokType != tkEof)
  {
    BRACE(TNodeKind a = complexOrSimpleStmt(nim));
    if (a == nkEmpty) GET_TOKEN(nim);
  }

  return nkStmtList;
}

static void findNimTags(void)
{
  nimParser nim;
  initParser(&nim);
  getTok(&nim);
  parseAll(&nim);
  deInitParser(&nim);
}

static void buildNimKeywordHash(const langType language)
{
  int i = 0;
  do
  {
    addKeyword(specialWords[i], language, (int) (wInvalid + i));
    i += 1;
  } while(specialWords[i] != NULL);

  addKeyword("_", language, (int) (wInvalid + i));
  i += 1;
  lastIdent = i;
}

static void initializeNimParser(const langType language)
{
  buildNimKeywordHash(language);
  nimLang = language;
}

extern parserDefinition *NimParser (void)
{
  static const char *const extensions[] = { "nim", NULL };
  parserDefinition *def = parserNew ("Nim");
  def->kinds = nimKinds;
  def->kindCount = KIND_COUNT (nimKinds);
  def->fileKind = KIND_FILE_ALT;
  def->extensions = extensions;
  def->parser = findNimTags;
  def->initialize = initializeNimParser;
  return def;
}
