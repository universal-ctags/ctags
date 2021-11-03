/*
 *	Copyright (c) 2002-2003, Darren Hiebert
 *
 *	This source code is released for free distribution under the terms of the
 *	GNU General Public License version 2 or (at your option) any later version.
 *
 *	This module contains functions for generating tags for PL/SQL language
 *	files.
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include <ctype.h>	/* to define isalpha () */
#ifdef DEBUG
#include <stdio.h>
#endif
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"
#include "promise.h"

/*
 *	On-line "Oracle Database PL/SQL Language Reference":
 *	http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28370/toc.htm
 *
 *	Sample PL/SQL code is available from:
 *	http://www.orafaq.com/faqscrpt.htm#GENPLSQL
 *
 *	On-line SQL Anywhere Documentation
 *	http://www.ianywhere.com/developer/product_manuals/sqlanywhere/index.html
 */

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isReservedWord(token) (SqlReservedWord[(token)->keyword].fn \
							   ?(bool)SqlReservedWord[(token)->keyword].fn(token) \
							   :SqlReservedWord[(token)->keyword].bit)
#define isIdentChar1(c) \
	/*
	 * Other databases are less restrictive on the first character of
	 * an identifier.
	 * isIdentChar1 is used to identify the first character of an
	 * identifier, so we are removing some restrictions.
	 */ \
	(isalpha (c) || (c) == '@' || (c) == '_' )
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '$' || \
		(c) == '@' || (c) == '_' || (c) == '#')

/*
 *	 DATA DECLARATIONS
 */

/*
 * Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_at,
	KEYWORD_begin,
	KEYWORD_body,
	KEYWORD_call,
	KEYWORD_case,
	KEYWORD_check,
	KEYWORD_commit,
	KEYWORD_comment,
	KEYWORD_constraint,
	KEYWORD_create,
	KEYWORD_cursor,
	KEYWORD_database,
	KEYWORD_datatype,
	KEYWORD_declare,
	KEYWORD_do,
	KEYWORD_domain,
	KEYWORD_drop,
	KEYWORD_else,
	KEYWORD_elseif,
	KEYWORD_end,
	KEYWORD_endif,
	KEYWORD_event,
	KEYWORD_exception,
	KEYWORD_extension,
	KEYWORD_external,
	KEYWORD_for,
	KEYWORD_foreign,
	KEYWORD_from,
	KEYWORD_function,
	KEYWORD_go,
	KEYWORD_handler,
	KEYWORD_if,
	KEYWORD_index,
	KEYWORD_internal,
	KEYWORD_is,
	KEYWORD_language,
	KEYWORD_local,
	KEYWORD_loop,
	KEYWORD_ml_conn,
	KEYWORD_ml_conn_chk,
	KEYWORD_ml_conn_dnet,
	KEYWORD_ml_conn_java,
	KEYWORD_ml_conn_lang,
	KEYWORD_ml_prop,
	KEYWORD_ml_table,
	KEYWORD_ml_table_chk,
	KEYWORD_ml_table_dnet,
	KEYWORD_ml_table_java,
	KEYWORD_ml_table_lang,
	KEYWORD_object,
	KEYWORD_on,
	KEYWORD_package,
	KEYWORD_pragma,
	KEYWORD_inquiry_directive,
	KEYWORD_primary,
	KEYWORD_procedure,
	KEYWORD_publication,
	KEYWORD_record,
	KEYWORD_ref,
	KEYWORD_references,
	KEYWORD_rem,
	KEYWORD_result,
	KEYWORD_return,
	KEYWORD_returns,
	KEYWORD_schema,
	KEYWORD_select,
	KEYWORD_service,
	KEYWORD_subtype,
	KEYWORD_synonym,
	KEYWORD_table,
	KEYWORD_temporary,
	KEYWORD_then,
	KEYWORD_trigger,
	KEYWORD_type,
	KEYWORD_unique,
	KEYWORD_url,
	KEYWORD_variable,
	KEYWORD_view,
	KEYWORD_when,
	KEYWORD_while,
	KEYWORD_with,
	KEYWORD_without,
	SQLKEYWORD_COUNT,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_BLOCK_LABEL_BEGIN,
	TOKEN_BLOCK_LABEL_END,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_COLON,
	TOKEN_SEMICOLON,
	TOKEN_COMMA,
	TOKEN_IDENTIFIER,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_OTHER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_TILDE,
	TOKEN_FORWARD_SLASH,
	TOKEN_EQUAL
} tokenType;

typedef struct sTokenInfoSQL {
	tokenType	type;
	keywordId	keyword;
	vString *	string;
	vString *	scope;
	int         scopeKind;
	int         begin_end_nest_lvl;
	unsigned long lineNumber;
	MIOPos filePosition;

	/* When the "guest" extra is enabled, a promise is
	 * made always when reading a string (literal or dollar quote).
	 * The lexer stores the id of promise to this member.
	 * When making the promise, the language of guest parser
	 * may not be determined yet.
	 *
	 *   CREATE FUNCTION ... AS ' sub code_written_in_perl {... ' LANGUAGE plperl;
	 *
	 * After reading a string, the parser may find LANGUAGE keyword. In the case,
	 * the parser updates the language of the promies.
	 *
	 * This field is filled only when `guest` extra is enabled.
	 *
	 */
	int promise;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_sql;

typedef enum {
	SQLTAG_PLSQL_CCFLAGS,
	SQLTAG_DOMAIN,
	SQLTAG_FIELD,
	SQLTAG_BLOCK_LABEL,
	SQLTAG_PACKAGE,
	SQLTAG_SERVICE,
	SQLTAG_SCHEMA,
	SQLTAG_TRIGGER,
	SQLTAG_PUBLICATION,
	SQLTAG_VIEW,
	SQLTAG_DATABASE,
	SQLTAG_CURSOR,
	SQLTAG_PROTOTYPE,
	SQLTAG_EVENT,
	SQLTAG_FUNCTION,
	SQLTAG_INDEX,
	SQLTAG_LOCAL_VARIABLE,
	SQLTAG_SYNONYM,
	SQLTAG_PROCEDURE,
	SQLTAG_RECORD,
	SQLTAG_SUBTYPE,
	SQLTAG_TABLE,
	SQLTAG_VARIABLE,
	SQLTAG_MLTABLE,
	SQLTAG_MLCONN,
	SQLTAG_MLPROP,
	SQLTAG_COUNT
} sqlKind;

static kindDefinition SqlKinds [] = {
	{ true,  'C', "ccflag",		  "PLSQL_CCFLAGS"          },
	{ true,  'D', "domain",		  "domains"				   },
	{ true,  'E', "field",		  "record fields"		   },
	{ true,  'L', "label",		  "block label"			   },
	{ true,  'P', "package",	  "packages"			   },
	{ true,  'R', "service",	  "services"			   },
	{ true,  'S', "schema",		  "schemas"			  	   },
	{ true,  'T', "trigger",	  "triggers"			   },
	{ true,  'U', "publication",  "publications"		   },
	{ true,  'V', "view",		  "views"				   },
	{ true,  'b', "database",	  "database"			   },
	{ true,  'c', "cursor",		  "cursors"				   },
	{ false, 'd', "prototype",	  "prototypes"			   },
	{ true,  'e', "event",		  "events"				   },
	{ true,  'f', "function",	  "functions"			   },
	{ true,  'i', "index",		  "indexes"				   },
	{ false, 'l', "local",		  "local variables"		   },
	{ true,  'n', "synonym",	  "synonyms"			   },
	{ true,  'p', "procedure",	  "procedures"			   },
	{ false, 'r', "record",		  "records"				   },
	{ true,  's', "subtype",	  "subtypes"			   },
	{ true,  't', "table",		  "tables"				   },
	{ true,  'v', "variable",	  "variables"			   },
	{ true,  'x', "mltable",	  "MobiLink Table Scripts" },
	{ true,  'y', "mlconn",		  "MobiLink Conn Scripts"  },
	{ true,  'z', "mlprop",		  "MobiLink Properties"    },
};

static const keywordTable SqlKeywordTable [] = {
	/* keyword		keyword ID */
	{ "as",								KEYWORD_is				      },
	{ "at",								KEYWORD_at				      },
	{ "begin",							KEYWORD_begin			      },
	{ "body",							KEYWORD_body			      },
	{ "call",							KEYWORD_call			      },
	{ "case",							KEYWORD_case			      },
	{ "check",							KEYWORD_check			      },
	{ "commit",							KEYWORD_commit				  },
	{ "comment",						KEYWORD_comment			      },
	{ "constraint",						KEYWORD_constraint		      },
	{ "create",							KEYWORD_create				  },
	{ "cursor",							KEYWORD_cursor			      },
	{ "database",						KEYWORD_database		      },
	{ "datatype",						KEYWORD_datatype		      },
	{ "declare",						KEYWORD_declare			      },
	{ "do",								KEYWORD_do				      },
	{ "domain",							KEYWORD_domain				  },
	{ "drop",							KEYWORD_drop			      },
	{ "else",							KEYWORD_else			      },
	{ "elseif",							KEYWORD_elseif			      },
	{ "end",							KEYWORD_end				      },
	{ "endif",							KEYWORD_endif			      },
	{ "event",							KEYWORD_event			      },
	{ "exception",						KEYWORD_exception		      },
	{ "extension",						KEYWORD_extension		      },
	{ "external",						KEYWORD_external		      },
	{ "for",							KEYWORD_for				      },
	{ "foreign",						KEYWORD_foreign			      },
	{ "from",							KEYWORD_from			      },
	{ "function",						KEYWORD_function		      },
	{ "go",								KEYWORD_go				      },
	{ "handler",						KEYWORD_handler			      },
	{ "if",								KEYWORD_if				      },
	{ "index",							KEYWORD_index			      },
	{ "internal",						KEYWORD_internal		      },
	{ "is",								KEYWORD_is				      },
	{ "language",						KEYWORD_language              },
	{ "local",							KEYWORD_local			      },
	{ "loop",							KEYWORD_loop			      },
	{ "ml_add_connection_script",		KEYWORD_ml_conn			      },
	{ "ml_add_dnet_connection_script",	KEYWORD_ml_conn_dnet	      },
	{ "ml_add_dnet_table_script",		KEYWORD_ml_table_dnet	      },
	{ "ml_add_java_connection_script",	KEYWORD_ml_conn_java	      },
	{ "ml_add_java_table_script",		KEYWORD_ml_table_java	      },
	{ "ml_add_lang_conn_script_chk",	KEYWORD_ml_conn_chk 	      },
	{ "ml_add_lang_connection_script",	KEYWORD_ml_conn_lang	      },
	{ "ml_add_lang_table_script",		KEYWORD_ml_table_lang	      },
	{ "ml_add_lang_table_script_chk",	KEYWORD_ml_table_chk	      },
	{ "ml_add_property",				KEYWORD_ml_prop		 	      },
	{ "ml_add_table_script",			KEYWORD_ml_table		      },
	{ "object",							KEYWORD_object			      },
	{ "on",								KEYWORD_on				      },
	{ "package",						KEYWORD_package			      },
	{ "pragma",							KEYWORD_pragma			      },
	{ "primary",						KEYWORD_primary			      },
	{ "procedure",						KEYWORD_procedure		      },
	{ "publication",					KEYWORD_publication		      },
	{ "record",							KEYWORD_record			      },
	{ "ref",							KEYWORD_ref				      },
	{ "references",						KEYWORD_references		      },
	{ "rem",							KEYWORD_rem				      },
	{ "result",							KEYWORD_result			      },
	{ "return",							KEYWORD_return			      },
	{ "returns",						KEYWORD_returns			      },
	{ "schema",							KEYWORD_schema			      },
	{ "select",							KEYWORD_select			      },
	{ "service",						KEYWORD_service			      },
	{ "subtype",						KEYWORD_subtype			      },
	{ "synonym",						KEYWORD_synonym			      },
	{ "table",							KEYWORD_table			      },
	{ "temporary",						KEYWORD_temporary		      },
	{ "then",							KEYWORD_then			      },
	{ "trigger",						KEYWORD_trigger			      },
	{ "type",							KEYWORD_type			      },
	{ "unique",							KEYWORD_unique			      },
	{ "url",							KEYWORD_url				      },
	{ "variable",						KEYWORD_variable		      },
	{ "view",							KEYWORD_view			      },
	{ "when",							KEYWORD_when			      },
	{ "while",							KEYWORD_while			      },
	{ "with",							KEYWORD_with			      },
	{ "without",						KEYWORD_without			      },
};

const static struct keywordGroup predefinedInquiryDirective = {
	.value = KEYWORD_inquiry_directive,
	.addingUnlessExisting = false,
	.keywords = {
		/* https://docs.oracle.com/en/database/oracle/oracle-database/18/lnpls/plsql-language-fundamentals.html#GUID-3DABF5E1-AC84-448B-810F-31196991EA10 */
		"PLSQL_LINE",
		"PLSQL_UNIT",
		"PLSQL_UNIT_OWNER",
		"PLSQL_UNIT_TYPE",
		/* https://docs.oracle.com/en/database/oracle/oracle-database/18/lnpls/overview.html#GUID-DF63BC59-22C2-4BA8-9240-F74D505D5102 */
		"PLSCOPE_SETTINGS",
		"PLSQL_CCFLAGS",
		"PLSQL_CODE_TYPE",
		"PLSQL_OPTIMIZE_LEVEL",
		"PLSQL_WARNINGS",
		"NLS_LENGTH_SEMANTICS",
		"PERMIT_92_WRAP_FORMAT",
		NULL
	},
};

/* A table representing whether a keyword is "reserved word" or not.
 * "reserved word" cannot be used as an name.
 * See https://dev.mysql.com/doc/refman/8.0/en/keywords.html about the
 * difference between keywords and the reserved words.
 *
 * We will mark a keyword as a reserved word only if all the SQL dialects
 * specify it as a reserved word.
 */
struct SqlReservedWord {
	/* If fn is non-NULL, value returned from fn(token) is used
	 * to repreesnt whether a keyword is reserved (true) or not.
	 * If fn is NULL, bit is used. */
	unsigned int bit;
	bool (* fn) (tokenInfo *const token);
};

/*
 * MYSQL
 * => https://dev.mysql.com/doc/refman/8.0/en/keywords.html
 * POSTGRESQL,SQL2016,SQL2011,SQL92
 * => https://www.postgresql.org/docs/12/sql-keywords-appendix.html
 * ORACLE11g, PLSQL
 * => https://docs.oracle.com/cd/B28359_01/appdev.111/b31231/appb.htm#CJHIIICD
 * SQLANYWERE
 * => http://dcx.sap.com/1200/en/dbreference/alhakeywords.html <the page is gone>
 */
static bool SqlReservedWordPredicatorForIsOrAs (tokenInfo *const token);
static struct SqlReservedWord SqlReservedWord [SQLKEYWORD_COUNT] = {
	/*
	 * RESERVED_BIT: MYSQL & POSTGRESQL&SQL2016&SQL2011&SQL92 & ORACLE11g&PLSQL & SQLANYWERE
	 *
	 * {  0  } means we have not inspect whether the keyword is reserved or not.
	 */
	[KEYWORD_at]            = {0 & 0&1&1&1 & 0&1 & 0},
	[KEYWORD_begin]         = {0 & 0&1&1&1 & 0&1 & 1},
	[KEYWORD_body]          = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_call]          = {1 & 0&1&1&0 & 0&0 & 1},
	[KEYWORD_case]          = {1 & 1&1&1&1 & 0&1 & 1},
	[KEYWORD_check]         = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_commit]        = {0 & 0&1&1&1 & 0&0 & 0}, /* SQLANYWERE:??? */
	[KEYWORD_comment]       = {0 & 0&0&0&0 & 1&1 & 1},
	[KEYWORD_constraint]    = {1 & 1&1&1&1 & 0&1 & 1},
	[KEYWORD_create]        = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_cursor]        = {1 & 0&1&1&1 & 0&1 & 1},
	[KEYWORD_database]      = {         0           },
	[KEYWORD_datatype]      = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_declare]       = {1 & 0&1&1&1 & 0&1 & 1},
	[KEYWORD_do]            = {0 & 1&0&0&0 & 0&1 & 1},
	[KEYWORD_domain]        = {0 & 0&0&0&1 & 0&0 & 0},
	[KEYWORD_drop]          = {1 & 0&1&1&1 & 1&1 & 1},
	[KEYWORD_else]          = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_elseif]        = {1 & 0&0&0&0 & 0&0 & 1},
	[KEYWORD_end]           = {0 & 1&1&1&1 & 0&1 & 1},
	[KEYWORD_endif]         = {0 & 0&0&0&0 & 0&0 & 1},
	[KEYWORD_event]         = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_exception]     = {0 & 0&0&0&1 & 0&1 & 1},
	[KEYWORD_extension]     = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_external]      = {0 & 0&1&1&1 & 0&0 & 0},
	[KEYWORD_for]           = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_foreign]       = {1 & 1&1&1&1 & 0&0 & 1},
	[KEYWORD_from]          = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_function]      = {1 & 0&1&1&0 & 0&1 & 0},
	[KEYWORD_go]            = {0 & 0&0&0&1 & 0&0 & 0},
	[KEYWORD_handler]       = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_if]            = {1 & 0&0&0&0 & 0&1 & 1},
	[KEYWORD_index]         = {1 & 0&0&0&0 & 1&1 & 1},
	[KEYWORD_inquiry_directive] = {        0        },
	[KEYWORD_internal]      = {1 & 0&1&1&0 & 0&0 & 0},
	[KEYWORD_is]            = {0, SqlReservedWordPredicatorForIsOrAs},
	[KEYWORD_language]      = {            0        },
	[KEYWORD_local]         = {0 & 0&1&1&1 & 0&0 & 0},
	[KEYWORD_loop]          = {1 & 1&1&1&1 & 0&1 & 0},
	[KEYWORD_ml_conn]       = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_conn_dnet]  = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_table_dnet] = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_conn_java]  = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_table_java] = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_conn_chk]   = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_conn_lang]  = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_table_lang] = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_table_chk]  = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_prop]       = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_ml_table]      = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_object]        = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_on]            = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_package]       = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_pragma]        = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_primary]       = {1 & 1&1&1&1 & 0&0 & 1},
	[KEYWORD_procedure]     = {1 & 0&0&0&0 & 0&1 & 1},
	[KEYWORD_publication]   = {0 & 0&0&0&0 & 0&0 & 1},
	[KEYWORD_record]        = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_ref]           = {0 & 0&1&1&0 & 0&0 & 0},
	[KEYWORD_references]    = {1 & 1&1&1&1 & 0&0 & 1},
	[KEYWORD_rem]           = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_result]        = {0 & 0&1&1&0 & 0&0 & 0},
	[KEYWORD_return]        = {1 & 0&1&1&0 & 0&1 & 1},
	[KEYWORD_returns]       = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_schema]        = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_select]        = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_service]       = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_subtype]       = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_synonym]       = {0 & 0&0&0&0 & 1&0 & 0},
	[KEYWORD_table]         = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_temporary]     = {0 & 0&0&0&1 & 0&0 & 1},
	[KEYWORD_then]          = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_trigger]       = {1 & 0&1&1&0 & 1&0 & 1},
	[KEYWORD_type]          = {0 & 0&0&0&0 & 0&1 & 0},
	[KEYWORD_unique]        = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_url]           = {0 & 0&0&0&0 & 0&0 & 0},
	[KEYWORD_variable]      = {0 & 0&0&0&0 & 0&0 & 1},
	[KEYWORD_view]          = {0 & 0&0&0&1 & 1&1 & 1},
	[KEYWORD_when]          = {1 & 1&1&1&1 & 0&1 & 1},
	[KEYWORD_while]         = {1 & 0&0&0&0 & 0&1 & 1},
	[KEYWORD_with]          = {1 & 1&1&1&1 & 1&1 & 1},
	[KEYWORD_without]       = {0 & 0&1&1&0 & 0&0 & 0},
};

/*
 *	 FUNCTION DECLARATIONS
 */

/* Recursive calls */
static void parseBlock (tokenInfo *const token, const bool local);
static void parseBlockFull (tokenInfo *const token, const bool local, langType lang);
static void parseDeclare (tokenInfo *const token, const bool local);
static void parseKeywords (tokenInfo *const token);
static tokenType parseSqlFile (tokenInfo *const token);

/*
 *	 FUNCTION DEFINITIONS
 */

static bool SqlReservedWordPredicatorForIsOrAs (tokenInfo *const token)
{
	if (strcasecmp ("as", vStringValue (token->string)) == 0)
		return (bool) (1 & 1&1&1&1 & 1&1 & 1);
	else						/* for "is" */
		return (bool) (1 & 0&1&1&1 & 1&1 & 1);
	/* PostgresSQL can use "is" as a name of function. */
}

static bool isCmdTerm (tokenInfo *const token)
{
	DebugStatement (
			debugPrintf (DEBUG_PARSE
				, "\n isCmdTerm: token same  tt:%d  tk:%d\n"
				, token->type
				, token->keyword
				);
			);

	/*
	 * Based on the various customer sites I have been at
	 * the most common command delimiters are
	 *	   ;
	 *	   ~
	 *	   /
	 *	   go
	 * This routine will check for any of these, more
	 * can easily be added by modifying readToken and
	 * either adding the character to:
	 *	   enum eTokenType
	 *	   enum eTokenType
	 */
	return (isType (token, TOKEN_SEMICOLON) ||
			isType (token, TOKEN_TILDE) ||
			isType (token, TOKEN_FORWARD_SLASH) ||
			isKeyword (token, KEYWORD_go));
}

static bool isMatchedEnd(tokenInfo *const token, int nest_lvl)
{
	bool terminated = false;
	/*
	 * Since different forms of SQL allow the use of
	 * BEGIN
	 * ...
	 * END
	 * blocks, some statements may not be terminated using
	 * the standard delimiters:
	 *	   ;
	 *	   ~
	 *	   /
	 *	   go
	 * This routine will check to see if we encounter and END
	 * for the matching nest level of BEGIN ... END statements.
	 * If we find one, then we can assume, the statement was terminated
	 * since we have fallen through to the END statement of the BEGIN
	 * block.
	 */
	if ( nest_lvl > 0 && isKeyword (token, KEYWORD_end) )
	{
		if ( token->begin_end_nest_lvl == nest_lvl )
			terminated = true;
	}

	return terminated;
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type               = TOKEN_UNDEFINED;
	token->keyword            = KEYWORD_NONE;
	token->string             = vStringNew ();
	token->scope              = vStringNew ();
	token->scopeKind          = SQLTAG_COUNT;
	token->begin_end_nest_lvl = 0;
	token->lineNumber         = getInputLineNumber ();
	token->filePosition       = getInputFilePosition ();
	token->promise            = -1;

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

/*
 *	 Tag generation functions
 */

static void makeSqlTag (tokenInfo *const token, const sqlKind kind)
{
	if (SqlKinds [kind].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name, kind);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;

		if (vStringLength (token->scope) > 0)
		{
			Assert (token->scopeKind < SQLTAG_COUNT);
			e.extensionFields.scopeKindIndex = token->scopeKind;
			e.extensionFields.scopeName = vStringValue (token->scope);

			if (isXtagEnabled (XTAG_QUALIFIED_TAGS))
			{
				vString *fulltag;
				tagEntryInfo xe = e;

				fulltag =  vStringNewCopy (token->scope);
				vStringPut (fulltag, '.');
				vStringCat (fulltag, token->string);
				xe.name = vStringValue (fulltag);
				markTagExtraBit (&xe, XTAG_QUALIFIED_TAGS);
				makeTagEntry (&xe);
				vStringDelete (fulltag);
			}
		}

		makeTagEntry (&e);
	}
}

/*
 *	 Parsing functions
 */

static void parseString (vString *const string, const int delimiter, int *promise)
{
	int offset[2];
	unsigned long linenum[3];
	enum { START, END, SOURCE };

	int c0;

	if (promise && !isXtagEnabled(XTAG_GUEST))
		promise = NULL;

	if (promise)
	{
		c0 = getcFromInputFile ();
		linenum[START] = getInputLineNumber ();
		offset[START]  = getInputLineOffset ();
		linenum[SOURCE] = getSourceLineNumber ();
		ungetcToInputFile(c0);
	}

	bool end = false;
	while (! end)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			end = true;
		/*
		else if (c == '\\')
		{
			c = getcFromInputFile(); // This maybe a ' or ". //
			vStringPut(string, c);
		}
		*/
		else if (c == delimiter)
		{
			if (promise)
			{
				ungetcToInputFile(c);
				linenum[END] = getInputLineNumber ();
				offset[END]  = getInputLineOffset ();
				(void)getcFromInputFile ();
				*promise = makePromise (NULL,
										linenum [START], offset [START],
										linenum [END], offset [END],
										linenum [SOURCE]);
			}
			end = true;
		}
		else
			vStringPut (string, c);
	}
}

/*	Read a C identifier beginning with "firstChar" and places it into "name".
*/
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar1 (c));
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isIdentChar (c));
	if (!isspace (c))
		ungetcToInputFile (c);		/* unget non-identifier character */
}

static bool isCCFlag(const char *str)
{
	return (anyKindEntryInScope(CORK_NIL, str, SQLTAG_PLSQL_CCFLAGS) != 0);
}

/* Parse a PostgreSQL: dollar-quoted string
 * https://www.postgresql.org/docs/current/static/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING
 *
 * The syntax for dollar-quoted string ca collide with PL/SQL inquiry directive ($$name).
 * https://docs.oracle.com/en/database/oracle/oracle-database/18/lnpls/plsql-language-fundamentals.html#GUID-E918087C-D5A8-4CEE-841B-5333DE6D4C15
 * https://github.com/universal-ctags/ctags/issues/3006
 */
static tokenType parseDollarQuote (vString *const string, const int delimiter, int *promise)
{
	int offset[2];
	unsigned long linenum[3];
	enum { START, END, SOURCE };

	unsigned int len = 0;
	char tag[32 /* arbitrary limit */] = {0};
	int c = 0;

	/* read the tag */
	tag[len++] = (char) delimiter;
	while ((len + 1) < sizeof tag && c != delimiter)
	{
		c = getcFromInputFile ();
		if (isIdentChar(c))
			tag[len++] = (char) c;
		else
			break;
	}
	tag[len] = 0;

	bool empty_tag = (len == 2);

	if (c != delimiter)
	{
		/* damn that's not valid, what can we do? */
		ungetcToInputFile (c);
		return TOKEN_UNDEFINED;
	}

	if (promise && !isXtagEnabled(XTAG_GUEST))
		promise = NULL;

	if (promise)
	{
		linenum[START] = getInputLineNumber ();
		offset[START]  = getInputLineOffset ();
		linenum[SOURCE] = getSourceLineNumber ();
	}

	/* and read the content (until a matching end tag) */
	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c != delimiter)
		{
			vStringPut (string, c);
			if (empty_tag
				&& (KEYWORD_inquiry_directive == lookupCaseKeyword (vStringValue (string),
																	Lang_sql)
					|| isCCFlag(vStringValue (string))))
			{
				/* PL/SQL inquiry directives */
				int c0 = getcFromInputFile ();

				if (c0 != delimiter && (isalnum(c0) || c0 == '_'))
				{
					vStringPut (string, c0);
					continue;
				}

				ungetcToInputFile (c0);
				/* Oracle PL/SQL's inquiry directive ($$name) */
				return TOKEN_UNDEFINED;
			}
		}
		else
		{
			char *end_p = tag;

			while (c != EOF && *end_p && ((int) c) == *end_p)
			{
				c = getcFromInputFile ();
				end_p++;
			}

			if (c != EOF)
				ungetcToInputFile (c);

			if (! *end_p) /* full tag match */
			{
				if (promise)
				{
					linenum[END] = getInputLineNumber ();
					offset[END]  = getInputLineOffset ();
					if (offset[END] > len)
						offset[END] -= len;
					*promise = makePromise (NULL,
											linenum [START], offset [START],
											linenum [END], offset [END],
											linenum [SOURCE]);
				}
				break;
			}
			else
				vStringNCatS (string, tag, (size_t) (end_p - tag));
		}
	}

	return TOKEN_STRING;
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);
	token->promise      = -1;

getNextChar:
	do
	{
		c = getcFromInputFile ();
		token->lineNumber   = getInputLineNumber ();
		token->filePosition = getInputFilePosition ();
		/*
		 * Added " to the list of ignores, not sure what this
		 * might break but it gets by this issue:
		 *	  create table "t1" (...)
		 *
		 * Darren, the code passes all my tests for both
		 * Oracle and SQL Anywhere, but maybe you can tell me
		 * what this may effect.
		 */
	}
	while (c == '\t'  ||  c == ' ' ||  c == '\n');

	switch (c)
	{
		case EOF: token->type = TOKEN_EOF;				break;
		case '(': token->type = TOKEN_OPEN_PAREN;		break;
		case ')': token->type = TOKEN_CLOSE_PAREN;		break;
		case ':': token->type = TOKEN_COLON;			break;
		case ';': token->type = TOKEN_SEMICOLON;		break;
		case '.': token->type = TOKEN_PERIOD;			break;
		case ',': token->type = TOKEN_COMMA;			break;
		case '{': token->type = TOKEN_OPEN_CURLY;		break;
		case '}': token->type = TOKEN_CLOSE_CURLY;		break;
		case '~': token->type = TOKEN_TILDE;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;		break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;		break;
		case '=': token->type = TOKEN_EQUAL;			break;

		case '\'':
		case '"':
				  token->type = TOKEN_STRING;
				  parseString (token->string, c, &token->promise);
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '#':
				skipToCharacterInInputFile ('\n');
				goto getNextChar;
		case '-':
				  c = getcFromInputFile ();
				  if (c == '-')		/* -- is this the start of a comment? */
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

		case '<':
		case '>':
				  {
					  const int initial = c;
					  int d = getcFromInputFile ();
					  if (d == initial)
					  {
						  if (initial == '<')
							  token->type = TOKEN_BLOCK_LABEL_BEGIN;
						  else
							  token->type = TOKEN_BLOCK_LABEL_END;
					  }
					  else
					  {
						  ungetcToInputFile (d);
						  token->type = TOKEN_UNDEFINED;
					  }
					  break;
				  }

		case '\\':
				  c = getcFromInputFile ();
				  if (c != '\\'  && c != '"'  && c != '\''  &&  !isspace (c))
					  ungetcToInputFile (c);
				  token->type = TOKEN_CHARACTER;
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		case '/':
				  {
					  int d = getcFromInputFile ();
					  if ((d != '*') &&		/* is this the start of a comment? */
						  (d != '/'))		/* is a one line comment? */
					  {
						  token->type = TOKEN_FORWARD_SLASH;
						  ungetcToInputFile (d);
					  }
					  else
					  {
						  if (d == '*')
						  {
							  skipToCharacterInInputFile2('*', '/');
							  goto getNextChar;
						  }
						  else if (d == '/')	/* is this the start of a comment?  */
						  {
							  skipToCharacterInInputFile ('\n');
							  goto getNextChar;
						  }
					  }
					  break;
				  }

		case '$':
				  token->type = parseDollarQuote (token->string, c, &token->promise);
				  token->lineNumber = getInputLineNumber ();
				  token->filePosition = getInputFilePosition ();
				  break;

		default:
				  if (! isIdentChar1 (c))
					  token->type = TOKEN_UNDEFINED;
				  else
				  {
					  parseIdentifier (token->string, c);
					  token->lineNumber = getInputLineNumber ();
					  token->filePosition = getInputFilePosition ();
					  token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_sql);
					  if (isKeyword (token, KEYWORD_rem))
					  {
						  vStringClear (token->string);
						  skipToCharacterInInputFile ('\n');
						  goto getNextChar;
					  }
					  else if (isKeyword (token, KEYWORD_NONE))
						  token->type = TOKEN_IDENTIFIER;
					  else
						  token->type = TOKEN_KEYWORD;
				  }
				  break;
	}
}

/*
 * reads an identifier, possibly quoted:
 * 		identifier
 * 		"identifier"
 * 		[identifier]
 */
static void readIdentifier (tokenInfo *const token)
{
	readToken (token);
	if (isType (token, TOKEN_OPEN_SQUARE))
	{
		tokenInfo *const close_square = newToken ();

		readToken (token);
		/* eat close square */
		readToken (close_square);
		deleteToken (close_square);
	}
}

/*
 *	 Token parsing functions
 */

/*
 * static void addContext (tokenInfo* const parent, const tokenInfo* const child)
 * {
 *	   if (vStringLength (parent->string) > 0)
 *	   {
 *		   vStringPut (parent->string, '.');
 *	   }
 *	   vStringCat (parent->string, child->string);
 * }
 */

static void addToScope (tokenInfo* const token, vString* const extra, sqlKind kind)
{
	if (vStringLength (token->scope) > 0)
	{
		vStringPut (token->scope, '.');
	}
	vStringCat (token->scope, extra);
	token->scopeKind = kind;
}

/*
 *	 Scanning functions
 */

static bool isOneOfKeyword (tokenInfo *const token, const keywordId *const keywords, unsigned int count)
{
	unsigned int i;
	for (i = 0; i < count; i++)
	{
		if (isKeyword (token, keywords[i]))
			return true;
	}
	return false;
}

static void findTokenOrKeywords (tokenInfo *const token, const tokenType type,
				 const keywordId *const keywords,
				 unsigned int kcount)
{
	while (! isType (token, type) &&
	       ! (isType (token, TOKEN_KEYWORD) && isOneOfKeyword (token, keywords, kcount)) &&
	       ! isType (token, TOKEN_EOF))
	{
		readToken (token);
	}
}

static void findToken (tokenInfo *const token, const tokenType type)
{
	while (! isType (token, type) &&
		   ! isType (token, TOKEN_EOF))
	{
		readToken (token);
	}
}

static void findCmdTerm (tokenInfo *const token, const bool check_first)
{
	int begin_end_nest_lvl = token->begin_end_nest_lvl;

	if (check_first)
	{
		if (isCmdTerm(token))
			return;
	}
	do
	{
		readToken (token);
	} while (! isCmdTerm(token) &&
			 ! isMatchedEnd(token, begin_end_nest_lvl) &&
			 ! isType (token, TOKEN_EOF));
}

static void skipToMatched(tokenInfo *const token)
{
	int nest_level = 0;
	tokenType open_token;
	tokenType close_token;

	switch (token->type)
	{
		case TOKEN_OPEN_PAREN:
			open_token  = TOKEN_OPEN_PAREN;
			close_token = TOKEN_CLOSE_PAREN;
			break;
		case TOKEN_OPEN_CURLY:
			open_token  = TOKEN_OPEN_CURLY;
			close_token = TOKEN_CLOSE_CURLY;
			break;
		case TOKEN_OPEN_SQUARE:
			open_token  = TOKEN_OPEN_SQUARE;
			close_token = TOKEN_CLOSE_SQUARE;
			break;
		default:
			return;
	}

	/*
	 * This routine will skip to a matching closing token.
	 * It will also handle nested tokens like the (, ) below.
	 *	 (	name varchar(30), text binary(10)  )
	 */

	if (isType (token, open_token))
	{
		nest_level++;
		while (nest_level > 0 && !isType (token, TOKEN_EOF))
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

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy(dest->string, src->string);
	vStringCopy(dest->scope, src->scope);
	dest->scopeKind = src->scopeKind;
}

static void skipArgumentList (tokenInfo *const token)
{
	/*
	 * Other databases can have arguments with fully declared
	 * datatypes:
	 *	 (	name varchar(30), text binary(10)  )
	 * So we must check for nested open and closing parentheses
	 */

	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	{
		skipToMatched (token);
	}
}

static langType getNamedLanguageFromToken(tokenInfo *const token)
{
	langType lang = LANG_IGNORE;

	if (isType (token, TOKEN_IDENTIFIER))
	{
		if (vStringLength (token->string) > 2
			&& vStringValue (token->string) [0] == 'p'
			&& vStringValue (token->string) [1] == 'l')
		{
			/* Remove first 'pl' and last 'u' for extracting the
			 * name of the language. */
			bool unsafe = (vStringLast(token->string) == 'u');
			lang = getNamedLanguageOrAlias (vStringValue (token->string) + 2,
											vStringLength (token->string)
											- 2
											- (unsafe? 1: 0));
		}
	}
	return lang;
}

static void parseSubProgram (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();
	vString * saveScope = vStringNew ();
	sqlKind saveScopeKind;

	/*
	 * This must handle both prototypes and the body of
	 * the procedures.
	 *
	 * Prototype:
	 *	   FUNCTION func_name RETURN integer;
	 *	   PROCEDURE proc_name( parameters );
	 * Procedure
	 *	   FUNCTION GET_ML_USERNAME RETURN VARCHAR2
	 *	   IS
	 *	   BEGIN
	 *		   RETURN v_sync_user_id;
	 *	   END GET_ML_USERNAME;
	 *
	 *	   PROCEDURE proc_name( parameters )
	 *		   IS
	 *		   BEGIN
	 *		   END;
	 *	   CREATE PROCEDURE proc_name( parameters )
	 *		   EXTERNAL NAME ... ;
	 *	   CREATE PROCEDURE proc_name( parameters )
	 *		   BEGIN
	 *		   END;
	 *
	 *	   CREATE FUNCTION f_GetClassName(
	 *		   IN @object VARCHAR(128)
	 *		  ,IN @code   VARCHAR(128)
	 *	   )
	 *	   RETURNS VARCHAR(200)
	 *	   DETERMINISTIC
	 *	   BEGIN
	 *
	 *		   IF( @object = 'user_state' ) THEN
	 *			   SET something = something;
	 *		   END IF;
	 *
	 *		   RETURN @name;
	 *	   END;
	 *
	 * Note, a Package adds scope to the items within.
     *     create or replace package demo_pkg is
     *         test_var number;
     *         function test_func return varchar2;
     *         function more.test_func2 return varchar2;
     *     end demo_pkg;
	 * So the tags generated here, contain the package name:
     *         demo_pkg.test_var
     *         demo_pkg.test_func
     *         demo_pkg.more.test_func2
	 */
	const sqlKind kind = isKeyword (token, KEYWORD_function) ?
		SQLTAG_FUNCTION : SQLTAG_PROCEDURE;
	Assert (isKeyword (token, KEYWORD_function) ||
			isKeyword (token, KEYWORD_procedure));

	vStringCopy(saveScope, token->scope);
	saveScopeKind = token->scopeKind;
	readToken (token);
	copyToken (name, token);
	readToken (token);

	if (isType (token, TOKEN_PERIOD))
	{
		/*
		 * If this is an Oracle package, then the token->scope should
		 * already be set.  If this is the case, also add this value to the
		 * scope.
		 * If this is not an Oracle package, chances are the scope should be
		 * blank and the value just read is the OWNER or CREATOR of the
		 * function and should not be considered part of the scope.
		 */
		if (vStringLength(saveScope) > 0)
		{
			addToScope(token, name->string, kind);
		}
		readToken (token);
		copyToken (name, token);
		readToken (token);
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		/* Reads to the next token after the TOKEN_CLOSE_PAREN */
		skipArgumentList(token);
	}

	if (kind == SQLTAG_FUNCTION)
	{
		if (isKeyword (token, KEYWORD_return) ||
			isKeyword (token, KEYWORD_returns))
		{
			/* Read datatype */
			readToken (token);
			/*
			 * Read token after which could be the
			 * command terminator if a prototype
			 * or an open parenthesis
			 */
			readToken (token);
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				/* Reads to the next token after the TOKEN_CLOSE_PAREN */
				skipArgumentList(token);
			}
		}
	}
	if (isCmdTerm (token))
	{
		makeSqlTag (name, SQLTAG_PROTOTYPE);
	}
	else
	{
		langType lang = LANG_IGNORE;

		while (! isKeyword (token, KEYWORD_is) &&
			   ! isKeyword (token, KEYWORD_begin) &&
			   ! isKeyword (token, KEYWORD_at) &&
			   ! isKeyword (token, KEYWORD_internal) &&
			   ! isKeyword (token, KEYWORD_external) &&
			   ! isKeyword (token, KEYWORD_url) &&
			   ! isType (token, TOKEN_EQUAL) &&
			   ! isType (token, TOKEN_EOF) &&
			   ! isCmdTerm (token))
		{
			if (isKeyword (token, KEYWORD_result))
			{
				readToken (token);
				if (isType (token, TOKEN_OPEN_PAREN))
				{
					/* Reads to the next token after the TOKEN_CLOSE_PAREN */
					skipArgumentList(token);
				}
			} else if (lang == LANG_IGNORE
					   && isKeyword (token, KEYWORD_language)) {
				readToken (token);
				lang = getNamedLanguageFromToken (token);
				if (lang != LANG_IGNORE)
					readToken (token);
			} else {
				readToken (token);
			}
		}
		if (isKeyword (token, KEYWORD_at) ||
			isKeyword (token, KEYWORD_url) ||
			isKeyword (token, KEYWORD_internal) ||
			isKeyword (token, KEYWORD_external))
		{
			addToScope(token, name->string, kind);
			if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING) ||
				isType (name, TOKEN_KEYWORD))
			{
				makeSqlTag (name, kind);
			}

			vStringClear (token->scope);
			token->scopeKind = SQLTAG_COUNT;
		}
		if (isType (token, TOKEN_EQUAL))
			readToken (token);

		if (isKeyword (token, KEYWORD_declare))
			parseDeclare (token, false);

		if (isKeyword (token, KEYWORD_is) ||
			isKeyword (token, KEYWORD_begin))
		{
			addToScope(token, name->string, kind);
			if (isType (name, TOKEN_IDENTIFIER) ||
				isType (name, TOKEN_STRING) ||
				isType (name, TOKEN_KEYWORD))
			{
				makeSqlTag (name, kind);
			}

			parseBlockFull (token, true, lang);
			vStringClear (token->scope);
			token->scopeKind = SQLTAG_COUNT;
		}
	}
	vStringCopy(token->scope, saveScope);
	token->scopeKind = saveScopeKind;
	deleteToken (name);
	vStringDelete(saveScope);
}

static void parseRecord (tokenInfo *const token)
{
	/*
	 * Make it a bit forgiving, this is called from
	 * multiple functions, parseTable, parseType
	 */
	if (!isType (token, TOKEN_OPEN_PAREN))
		readToken (token);
	if (!isType (token, TOKEN_OPEN_PAREN))
		return;

	do
	{
		if (isType (token, TOKEN_COMMA) ||
			isType (token, TOKEN_OPEN_PAREN))
		{
			readToken (token);
		}

		/*
		 * Create table statements can end with various constraints
		 * which must be excluded from the SQLTAG_FIELD.
		 *	  create table t1 (
		 *		  c1 integer,
		 *		  c2 char(30),
		 *		  c3 numeric(10,5),
		 *		  c4 integer,
		 *		  constraint whatever,
		 *		  primary key(c1),
		 *		  foreign key (),
		 *		  check ()
		 *	  )
		 */
		if (! isKeyword(token, KEYWORD_primary) &&
			! isKeyword(token, KEYWORD_references) &&
			! isKeyword(token, KEYWORD_unique) &&
			! isKeyword(token, KEYWORD_check) &&
			! isKeyword(token, KEYWORD_constraint) &&
			! isKeyword(token, KEYWORD_foreign))
		{
			/* keyword test above is redundant as only a TOKEN_KEYWORD could
			 * match any isKeyword() anyway */
			if (isType (token, TOKEN_IDENTIFIER) ||
				isType (token, TOKEN_STRING)     ||
				(isType (token, TOKEN_KEYWORD)
				 && (!isReservedWord (token))))
			{
				makeSqlTag (token, SQLTAG_FIELD);
			}
		}

		while (! isType (token, TOKEN_COMMA) &&
			   ! isType (token, TOKEN_CLOSE_PAREN) &&
			   ! isType (token, TOKEN_OPEN_PAREN) &&
			   ! isType (token, TOKEN_EOF))
		{
			readToken (token);
			/*
			 * A table structure can look like this:
			 *	  create table t1 (
			 *		  c1 integer,
			 *		  c2 char(30),
			 *		  c3 numeric(10,5),
			 *		  c4 integer
			 *	  )
			 * We can't just look for a COMMA or CLOSE_PAREN
			 * since that will not deal with the numeric(10,5)
			 * case.  So we need to skip the argument list
			 * when we find an open paren.
			 */
			if (isType (token, TOKEN_OPEN_PAREN))
			{
				/* Reads to the next token after the TOKEN_CLOSE_PAREN */
				skipArgumentList(token);
			}
		}
	} while (! isType (token, TOKEN_CLOSE_PAREN) &&
			 ! isType (token, TOKEN_EOF));
}

static void parseType (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	vString * saveScope = vStringNew ();
	sqlKind saveScopeKind;

	vStringCopy(saveScope, token->scope);
	/* If a scope has been set, add it to the name */
	addToScope (name, token->scope, token->scopeKind);
	saveScopeKind = token->scopeKind;
	readToken (name);
	if (isType (name, TOKEN_IDENTIFIER))
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_is))
		{
			readToken (token);
			switch (token->keyword)
			{
				case KEYWORD_record:
				case KEYWORD_object:
					makeSqlTag (name, SQLTAG_RECORD);
					addToScope (token, name->string, SQLTAG_RECORD);
					parseRecord (token);
					break;

				case KEYWORD_table:
					makeSqlTag (name, SQLTAG_TABLE);
					break;

				case KEYWORD_ref:
					readToken (token);
					if (isKeyword (token, KEYWORD_cursor))
						makeSqlTag (name, SQLTAG_CURSOR);
					break;

				default: break;
			}
			vStringClear (token->scope);
			token->scopeKind = SQLTAG_COUNT;
		}
	}
	vStringCopy(token->scope, saveScope);
	token->scopeKind = saveScopeKind;
	deleteToken (name);
	vStringDelete(saveScope);
}

static void parseSimple (tokenInfo *const token, const sqlKind kind)
{
	/* This will simply make the tagname from the first word found */
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) ||
		isType (token, TOKEN_STRING))
	{
		makeSqlTag (token, kind);
	}
}

static void parseDeclare (tokenInfo *const token, const bool local)
{
	/*
	 * PL/SQL declares are of this format:
	 *	  IS|AS
	 *	  [declare]
	 *		 CURSOR curname ...
	 *		 varname1 datatype;
	 *		 varname2 datatype;
	 *		 varname3 datatype;
	 *	  begin
	 */

	if (isKeyword (token, KEYWORD_declare))
		readToken (token);
	while (! isKeyword (token, KEYWORD_begin) &&
		   ! isKeyword (token, KEYWORD_end) &&
		   ! isType (token, TOKEN_EOF))
	{
		keywordId stoppers [] = {
			KEYWORD_begin,
			KEYWORD_end,
		};

		switch (token->keyword)
		{
			case KEYWORD_cursor:	parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_function:	parseSubProgram (token); break;
			case KEYWORD_procedure: parseSubProgram (token); break;
			case KEYWORD_subtype:	parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_trigger:	parseSimple (token, SQLTAG_TRIGGER); break;
			case KEYWORD_type:		parseType (token); break;

			default:
				if (isType (token, TOKEN_IDENTIFIER))
				{
					makeSqlTag (token, local? SQLTAG_LOCAL_VARIABLE: SQLTAG_VARIABLE);
				}
				break;
		}
		findTokenOrKeywords (token, TOKEN_SEMICOLON, stoppers, ARRAY_SIZE (stoppers));
		if (isType (token, TOKEN_SEMICOLON))
			readToken (token);
	}
}

static void parseDeclareANSI (tokenInfo *const token, const bool local)
{
	tokenInfo *const type = newToken ();
	/*
	 * ANSI declares are of this format:
	 *	 BEGIN
	 *		 DECLARE varname1 datatype;
	 *		 DECLARE varname2 datatype;
	 *		 ...
	 *
	 * This differ from PL/SQL where DECLARE precedes the BEGIN block
	 * and the DECLARE keyword is not repeated.
	 */
	while (isKeyword (token, KEYWORD_declare))
	{
		readToken (token);
		readToken (type);

		if (isKeyword (type, KEYWORD_cursor))
			makeSqlTag (token, SQLTAG_CURSOR);
		else if (isKeyword (token, KEYWORD_local) &&
				 isKeyword (type, KEYWORD_temporary))
		{
			/*
			 * DECLARE LOCAL TEMPORARY TABLE table_name (
			 *	  c1 int,
			 *	  c2 int
			 * );
			 */
			readToken (token);
			if (isKeyword (token, KEYWORD_table))
			{
				readToken (token);
				if (isType(token, TOKEN_IDENTIFIER) ||
					isType(token, TOKEN_STRING))
				{
					makeSqlTag (token, SQLTAG_TABLE);
				}
			}
		}
		else if (isType (token, TOKEN_IDENTIFIER) ||
				 isType (token, TOKEN_STRING))
		{
			makeSqlTag (token, local? SQLTAG_LOCAL_VARIABLE: SQLTAG_VARIABLE);
		}
		findToken (token, TOKEN_SEMICOLON);
		readToken (token);
	}
	deleteToken (type);
}

static void parseLabel (tokenInfo *const token)
{
	/*
	 * A label has this format:
	 *	   <<tobacco_dependency>>
	 *	   DECLARE
	 *		  v_senator VARCHAR2(100) := 'THURMOND, JESSE';
	 *	   BEGIN
	 *		  IF total_contributions (v_senator, 'TOBACCO') > 25000
	 *		  THEN
	 *			 <<alochol_dependency>>
	 *			 DECLARE
	 *				v_senator VARCHAR2(100) := 'WHATEVERIT, TAKES';
	 *			 BEGIN
	 *				...
	 */

	Assert (isType (token, TOKEN_BLOCK_LABEL_BEGIN));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		makeSqlTag (token, SQLTAG_BLOCK_LABEL);
		readToken (token);		  /* read end of label */
	}
}

static void parseStatements (tokenInfo *const token, const bool exit_on_endif )
{
	/* bool isAnsi   = true; */
	bool stmtTerm = false;
	do
	{

		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else
		{
			switch (token->keyword)
			{
				case KEYWORD_exception:
					/*
					 * EXCEPTION
					 *	 <exception handler>;
					 *
					 * Where an exception handler could be:
					 *	 BEGIN
					 *		WHEN OTHERS THEN
					 *			x := x + 3;
					 *	 END;
					 * In this case we need to skip this keyword and
					 * move on to the next token without reading until
					 * TOKEN_SEMICOLON;
					 */
					readToken (token);
					continue;

				case KEYWORD_when:
					/*
					 * WHEN statements can be used in exception clauses
					 * and CASE statements.  The CASE statement should skip
					 * these given below we skip over to an END statement.
					 * But for an exception clause, we can have:
					 *	   EXCEPTION
					 *		   WHEN OTHERS THEN
					 *		   BEGIN
					 *				  x := x + 3;
					 *		   END;
					 * If we skip to the TOKEN_SEMICOLON, we miss the begin
					 * of a nested BEGIN END block.  So read the next token
					 * after the THEN and restart the LOOP.
					 */
					while (! isKeyword (token, KEYWORD_then) &&
						   ! isType (token, TOKEN_EOF))
						readToken (token);

					readToken (token);
					continue;

				case KEYWORD_if:
					/*
					 * We do not want to look for a ; since for an empty
					 * IF block, it would skip over the END.
					 *	IF...THEN
					 *	END IF;
					 *
					 *	IF...THEN
					 *	ELSE
					 *	END IF;
					 *
					 *	IF...THEN
					 *	ELSEIF...THEN
					 *	ELSE
					 *	END IF;
					 *
					 *	or non-ANSI
					 *	IF ...
					 *	BEGIN
					 *	END
					 */
					while (! isKeyword (token, KEYWORD_then)  &&
						   ! isKeyword (token, KEYWORD_begin) &&
						   ! isType (token, TOKEN_EOF))
					{
						readToken (token);
					}

					if (isKeyword (token, KEYWORD_begin))
					{
						/* isAnsi = false; */
						parseBlock(token, false);

						/*
						 * Handle the non-Ansi IF blocks.
						 * parseBlock consumes the END, so if the next
						 * token in a command terminator (like GO)
						 * we know we are done with this statement.
						 */
						if (isCmdTerm (token))
							stmtTerm = true;
					}
					else
					{
						readToken (token);

						while (! isKeyword (token, KEYWORD_end) &&
							   ! isKeyword (token, KEYWORD_endif) &&
							   ! isType (token, TOKEN_EOF))
						{
							if (isKeyword (token, KEYWORD_else) ||
								isKeyword (token, KEYWORD_elseif))
							{
								readToken (token);
							}

							parseStatements (token, true);

							if (isCmdTerm(token))
								readToken (token);

						}

						/*
						 * parseStatements returns when it finds an END, an IF
						 * should follow the END for ANSI anyway.
						 *	IF...THEN
						 *	END IF;
						 */
						if (isKeyword (token, KEYWORD_end))
							readToken (token);

						if (isKeyword (token, KEYWORD_if) ||
							isKeyword (token, KEYWORD_endif))
						{
							readToken (token);
							if (isCmdTerm(token))
								stmtTerm = true;
						}
						else
						{
							/*
							 * Well we need to do something here.
							 * There are lots of different END statements
							 * END;
							 * END CASE;
							 * ENDIF;
							 * ENDCASE;
							 */
						}
					}
					break;

				case KEYWORD_loop:
				case KEYWORD_case:
				case KEYWORD_for:
					/*
					 *	LOOP...
					 *	END LOOP;
					 *
					 *	CASE
					 *	WHEN '1' THEN
					 *	END CASE;
					 *
					 *	FOR loop_name AS cursor_name CURSOR FOR ...
					 *	DO
					 *	END FOR;
					 */
					if (isKeyword (token, KEYWORD_for))
					{
						/* loop name */
						readToken (token);
						/* AS */
						readToken (token);

						while (! isKeyword (token, KEYWORD_is) &&
							   ! isType (token, TOKEN_EOF))
						{
							/*
							 * If this is not an AS keyword this is
							 * not a proper FOR statement and should
							 * simply be ignored
							 */
							return;
						}

						while (! isKeyword (token, KEYWORD_do) &&
							   ! isType (token, TOKEN_EOF))
							readToken (token);
					}


					readToken (token);
					while (! isKeyword (token, KEYWORD_end) &&
						   ! isType (token, TOKEN_EOF))
					{
						/*
						if ( isKeyword (token, KEYWORD_else) ||
								isKeyword (token, KEYWORD_elseif)    )
							readToken (token);
							*/

						parseStatements (token, false);

						if (isCmdTerm(token))
							readToken (token);
					}


					if (isKeyword (token, KEYWORD_end ))
						readToken (token);

					/*
					 * Typically ended with
					 *    END LOOP [loop name];
					 *    END CASE
					 *    END FOR [loop name];
					 */
					if (isKeyword (token, KEYWORD_loop) ||
						isKeyword (token, KEYWORD_case) ||
						isKeyword (token, KEYWORD_for))
					{
						readToken (token);
					}

					if (isCmdTerm(token))
						stmtTerm = true;

					break;

				case KEYWORD_create:
					readToken (token);
					parseKeywords(token);
					break;

				case KEYWORD_declare:
				case KEYWORD_begin:
					parseBlock (token, true);
					break;

				case KEYWORD_end:
					break;

				default:
					readToken (token);
					break;
			}
			/*
			 * Not all statements must end in a semi-colon
			 *	   begin
			 *		   if current publisher <> 'publish' then
			 *			 signal UE_FailStatement
			 *		   end if
			 *	   end;
			 * The last statement prior to an end ("signal" above) does
			 * not need a semi-colon, nor does the end if, since it is
			 * also the last statement prior to the end of the block.
			 *
			 * So we must read to the first semi-colon or an END block
			 */
			while (! stmtTerm &&
				   ! isKeyword (token, KEYWORD_end) &&
				   ! isCmdTerm(token) &&
				   ! isType(token, TOKEN_EOF))
			{
				if (exit_on_endif && isKeyword (token, KEYWORD_endif))
					return;

				if (isType (token, TOKEN_COLON) )
				{
					/*
					 * A : can signal a loop name
					 *    myloop:
					 *    LOOP
					 *        LEAVE myloop;
					 *    END LOOP;
					 * Unfortunately, labels do not have a
					 * cmd terminator, therefore we have to check
					 * if the next token is a keyword and process
					 * it accordingly.
					 */
					readToken (token);
					if (isKeyword (token, KEYWORD_loop) ||
						isKeyword (token, KEYWORD_while) ||
						isKeyword (token, KEYWORD_for))
					{
						/* parseStatements (token); */
						return;
					}
				}

				readToken (token);

				if (isType (token, TOKEN_OPEN_PAREN) ||
				    isType (token, TOKEN_OPEN_CURLY) ||
				    isType (token, TOKEN_OPEN_SQUARE))
				{
					skipToMatched (token);
				}

				/*
				 * Since we know how to parse various statements
				 * if we detect them, parse them to completion
				 */
				if (isType (token, TOKEN_BLOCK_LABEL_BEGIN) ||
					isKeyword (token, KEYWORD_exception) ||
					isKeyword (token, KEYWORD_loop) ||
					isKeyword (token, KEYWORD_case) ||
					isKeyword (token, KEYWORD_for) ||
					isKeyword (token, KEYWORD_begin))
				{
					parseStatements (token, false);
				}
				else if (isKeyword (token, KEYWORD_if))
					parseStatements (token, true);

			}
		}
		/*
		 * We assumed earlier all statements ended with a command terminator.
		 * See comment above, now, only read if the current token
		 * is not a command terminator.
		 */
		if (isCmdTerm(token) && ! stmtTerm)
			stmtTerm = true;

	} while (! isKeyword (token, KEYWORD_end) &&
			 ! (exit_on_endif && isKeyword (token, KEYWORD_endif) ) &&
			 ! isType (token, TOKEN_EOF) &&
			 ! stmtTerm );
}

static void parseBlock (tokenInfo *const token, const bool local)
{
	parseBlockFull (token, local, LANG_IGNORE);
}

static void parseBlockFull (tokenInfo *const token, const bool local, langType lang)
{
	int promise = -1;

	if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
	{
		parseLabel (token);
		readToken (token);
	}
	if (! isKeyword (token, KEYWORD_begin))
	{
		readToken (token);
		if (isType (token, TOKEN_STRING))
		{
			/* Likely a PostgreSQL FUNCTION name AS '...'
			 * https://www.postgresql.org/docs/current/static/sql-createfunction.html */
			promise = token->promise;
			token->promise = -1;

			readToken (token);
			while (! isCmdTerm (token)
				   && !isType (token, TOKEN_EOF))
			{
				if (lang == LANG_IGNORE &&
					isKeyword (token, KEYWORD_language))
				{
					readToken (token);
					lang = getNamedLanguageFromToken (token);
					if (lang != LANG_IGNORE)
						readToken (token);
				}
				else
					readToken (token);
			}

			if (promise != -1 && lang != LANG_IGNORE)
				promiseUpdateLanguage(promise, lang);
		}
		else
		{
			/*
			 * These are Oracle style declares which generally come
			 * between an IS/AS and BEGIN block.
			 */
			parseDeclare (token, local);
		}
	}
	if (isKeyword (token, KEYWORD_begin))
	{
		bool is_transaction = false;

		readToken (token);

		/* BEGIN of Postgresql initiates a transaction.
		 *
		 *   BEGIN [ WORK | TRANSACTION ] [ transaction_mode [, ...] ]
		 *
		 * BEGIN of MySQL does the same.
		 *
		 *   BEGIN [WORK]
		 *
		 * BEGIN of SQLite does the same.
		 *
		 *   BEGIN [[DEFERRED | IMMEDIATE | EXCLUSIVE] TRANSACTION]
		 *
		 */
		if (isCmdTerm(token))
		{
			is_transaction = true;
			readToken (token);
		}
		else if (isType (token, TOKEN_IDENTIFIER)
				 && (strcasecmp (vStringValue(token->string), "work") == 0
					 || strcasecmp (vStringValue(token->string), "transaction") == 0
					 || (
						 strcasecmp (vStringValue(token->string), "deferred") == 0
						 || strcasecmp (vStringValue(token->string), "immediate") == 0
						 || strcasecmp (vStringValue(token->string), "exclusive") == 0
						 )
					 ))
			is_transaction = true;
		else
		{
			/*
			 * Check for ANSI declarations which always follow
			 * a BEGIN statement.  This routine will not advance
			 * the token if none are found.
			 */
			parseDeclareANSI (token, local);
		}

		token->begin_end_nest_lvl++;
		while (! isKeyword (token, KEYWORD_end) &&
			   ! (is_transaction && isKeyword(token, KEYWORD_commit)) &&
			   ! isType (token, TOKEN_EOF))
		{
			parseStatements (token, false);

			if (isCmdTerm(token))
				readToken (token);
		}
		token->begin_end_nest_lvl--;

		/*
		 * Read the next token (we will assume
		 * it is the command delimiter)
		 */
		readToken (token);

		/*
		 * Check if the END block is terminated
		 */
		if (! isCmdTerm (token))
		{
			/*
			 * Not sure what to do here at the moment.
			 * I think the routine that calls parseBlock
			 * must expect the next token has already
			 * been read since it is possible this
			 * token is not a command delimiter.
			 */
			/* findCmdTerm (token, false); */
		}
	}
}

static void parsePackage (tokenInfo *const token)
{
	/*
	 * Packages can be specified in a number of ways:
	 *		CREATE OR REPLACE PACKAGE pkg_name AS
	 * or
	 *		CREATE OR REPLACE PACKAGE owner.pkg_name AS
	 * or by specifying a package body
	 *	   CREATE OR REPLACE PACKAGE BODY pkg_name AS
	 *	   CREATE OR REPLACE PACKAGE BODY owner.pkg_name AS
	 */
	tokenInfo *const name = newToken ();
	readIdentifier (name);
	if (isKeyword (name, KEYWORD_body))
	{
		/*
		 * Ignore the BODY tag since we will process
		 * the body or prototypes in the same manner
		 */
		readIdentifier (name);
	}
	/* Check for owner.pkg_name */
	while (! isKeyword (token, KEYWORD_is) &&
		   ! isType (token, TOKEN_EOF))
	{
		readToken (token);
		if ( isType(token, TOKEN_PERIOD) )
		{
			readIdentifier (name);
		}
	}
	if (isKeyword (token, KEYWORD_is))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_PACKAGE);
		}
		addToScope (token, name->string, SQLTAG_PACKAGE);
		parseBlock (token, false);
		vStringClear (token->scope);
		token->scopeKind = SQLTAG_COUNT;
	}
	findCmdTerm (token, false);
	deleteToken (name);
}

static void parseColumnsAndAliases (tokenInfo *const token)
{
	bool columnAcceptable = true;
	tokenInfo *const lastId = newToken ();

	/*
	 * -- A
	 * create table foo as select A;
	 *
	 * -- B
	 * create table foo as select B from ...;
	 *
	 * -- D
	 * create table foo as select C as D from ...;
	 *
	 * -- E, F
	 * create table foo as select E, a.F;
	 *
	 * -- G, H
	 * create table foo as select G, a.H from ...;
	 *
	 * -- J, K
	 * create table foo as select I as J, a.K from ...;
	 *
	 * lastID is used for capturing A, B, E, F, G, H, and K.
	 */
	readToken (token);
	do
	{
		if (isType (token, TOKEN_KEYWORD)
			&& isKeyword (token, KEYWORD_is))
		{
			readToken (token);
			if (isType (token, TOKEN_IDENTIFIER))
			{
				/* Emit the alias */
				makeSqlTag (token, SQLTAG_FIELD);
				columnAcceptable = true;
			}
			lastId->type = TOKEN_UNDEFINED;
		}
		else if ((isType (token, TOKEN_KEYWORD)
				  && isKeyword (token, KEYWORD_from))
				 || isType (token, TOKEN_SEMICOLON)
				 || isType(token, TOKEN_COMMA))
		{
			if (lastId->type == TOKEN_IDENTIFIER)
			{
				/* Emit the column */
				makeSqlTag(lastId, SQLTAG_FIELD);
				columnAcceptable = true;
			}

			if (isType(token, TOKEN_COMMA))
				lastId->type = TOKEN_UNDEFINED;
			else
				break;
		}
		else if (isType (token, TOKEN_OPEN_PAREN))
		{
			columnAcceptable = false;
			skipToMatched (token);
			lastId->type = TOKEN_UNDEFINED;
			continue;
		}
		else if (isType (token, TOKEN_PERIOD))
		{
			lastId->type = TOKEN_UNDEFINED;
		}
		else if (isType (token, TOKEN_IDENTIFIER))
		{
			if (columnAcceptable)
				copyToken (lastId, token);
		}
		else
		{
			columnAcceptable = false;
			lastId->type = TOKEN_UNDEFINED;
		}

		readToken (token);
	} while (! isType (token, TOKEN_EOF));

	deleteToken (lastId);
}

/* Skip "IF NOT EXISTS"
 * https://dev.mysql.com/doc/refman/8.0/en/create-table.html
 * https://www.postgresql.org/docs/current/sql-createtable.html
 * https://sqlite.org/lang_createtable.html
 */
static bool parseIdAfterIfNotExists(tokenInfo *const name,
									tokenInfo *const token,
									bool authorization_following)
{
	if (isKeyword (name, KEYWORD_if)
		&& (isType (token, TOKEN_IDENTIFIER)
			&& vStringLength (token->string) == 3
			&& strcasecmp ("not", vStringValue (token->string)) == 0))
	{
		readToken (token);
		if (isType (token, TOKEN_IDENTIFIER)
			&& vStringLength (token->string) == 6
			&& strcasecmp ("exists", vStringValue (token->string)) == 0)
		{
			readIdentifier (name);
			if (authorization_following
				&& isType (name, TOKEN_IDENTIFIER)
				&& vStringLength (name->string) == 13
				&& strcasecmp("authorization", vStringValue(name->string)) == 0)
			{
				/*
				 * PostgreSQL:
				 * - CREATE SCHEMA IF NOT EXISTS AUTHORIZATION role_specification
				 */
				readIdentifier (name);
			}
			readToken (token);
			return true;
		}
	}
	return false;
}

static void parseTable (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();
	bool emitted = false;

	/*
	 * This deals with these formats:
	 *	   create table t1 (c1 int);
	 *	   create global temporary table t2 (c1 int);
	 *	   create table "t3" (c1 int);
	 *	   create table bob.t4 (c1 int);
	 *	   create table bob."t5" (c1 int);
	 *	   create table "bob"."t6" (c1 int);
	 *	   create table bob."t7" (c1 int);
	 * Proxy tables use this format:
	 *	   create existing table bob."t7" AT '...';
	 * SQL Server and Sybase formats
     *     create table OnlyTable (
     *     create table dbo.HasOwner (
     *     create table [dbo].[HasOwnerSquare] (
     *     create table master.dbo.HasDb (
     *     create table master..HasDbNoOwner (
     *     create table [master].dbo.[HasDbAndOwnerSquare] (
     *     create table [master]..[HasDbNoOwnerSquare] (
	 * Oracle and PostgreSQL use this format:
	 *     create table FOO as select...
	 * MySQL allows omitting "as" like:
	 *     create table FOO select...
	 *     create table FOO (...) select...
	 * (At least) MYSQL, PostgreSQL, and SQLite takes "IF NOT EXISTS"
	 * between "table" and a table name:
	 *     create table if not exists foo ...
	 */

	/* This could be a database, owner or table name */
	readIdentifier (name);
	readToken (token);

	parseIdAfterIfNotExists(name, token, false);

	if (isType (token, TOKEN_PERIOD))
	{
		/*
		 * This could be a owner or table name.
		 * But this is also a special case since the table can be
		 * referenced with a blank owner:
		 *     dbname..tablename
		 */
		readIdentifier (name);
		/* Check if a blank name was provided */
		if (isType (name, TOKEN_PERIOD))
		{
			readIdentifier (name);
		}
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			/* This can only be the table name */
			readIdentifier (name);
			readToken (token);
		}
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING) ||
			(isType (name, TOKEN_KEYWORD)
			 && (!isReservedWord (name))))
		{
			makeSqlTag (name, SQLTAG_TABLE);
			emitted = true;

			vStringCopy(token->scope, name->string);
			token->scopeKind = SQLTAG_TABLE;
			parseRecord (token);
			vStringClear (token->scope);
			token->scopeKind = SQLTAG_COUNT;
			readToken (token);
		}
		else
			skipToMatched(token);
	}
	else if (isKeyword (token, KEYWORD_at))
	{
		if (isType (name, TOKEN_IDENTIFIER))
		{
			makeSqlTag (name, SQLTAG_TABLE);
		}
	}

	if (isKeyword (token, KEYWORD_select)
			 /* KEYWORD_is is for recognizing "as" */
			 || isKeyword (token, KEYWORD_is))
	{
		if (isType (name, TOKEN_IDENTIFIER))
		{
			if (!emitted)
				makeSqlTag (name, SQLTAG_TABLE);

			if (isKeyword (token, KEYWORD_is))
				readToken (token);

			if (isKeyword (token, KEYWORD_select))
			{
				addToScope (token, name->string, SQLTAG_TABLE);
				parseColumnsAndAliases (token);
				vStringClear (token->scope);
			}
		}
	}
	findCmdTerm (token, true);
	deleteToken (name);
}

static void parseIndex (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();
	tokenInfo *const owner = newToken ();

	/*
	 * This deals with these formats
	 *	   create index i1 on t1(c1) create index "i2" on t1(c1)
	 *	   create virtual unique clustered index "i3" on t1(c1)
	 *	   create unique clustered index "i4" on t1(c1)
	 *	   create clustered index "i5" on t1(c1)
	 *	   create bitmap index "i6" on t1(c1)
	 */

	readIdentifier (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readIdentifier (name);
		readToken (token);
	}
	if (isKeyword (token, KEYWORD_on) &&
		(isType (name, TOKEN_IDENTIFIER) ||
		 isType (name, TOKEN_STRING)))
	{
		readIdentifier (owner);
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			readIdentifier (owner);
			readToken (token);
		}
		addToScope(name, owner->string, SQLTAG_TABLE /* FIXME? */);
		makeSqlTag (name, SQLTAG_INDEX);
	}
	findCmdTerm (token, false);
	deleteToken (name);
	deleteToken (owner);
}

static void parseEvent (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create event e1 handler begin end;
	 *	   create event "e2" handler begin end;
	 *	   create event dba."e3" handler begin end;
	 *	   create event "dba"."e4" handler begin end;
	 */

	readIdentifier (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readIdentifier (name);
	}
	while (! isKeyword (token, KEYWORD_handler) &&
		   ! isType (token, TOKEN_SEMICOLON) &&
		   ! isType (token, TOKEN_EOF))
	{
		readToken (token);
	}

	if ((isKeyword (token, KEYWORD_handler) ||
		 isType (token, TOKEN_SEMICOLON))
		&& (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING)     ||
			(isType (name, TOKEN_KEYWORD)
			 && (!isReservedWord (name)))))
	{
		makeSqlTag (name, SQLTAG_EVENT);
	}

	if (isKeyword (token, KEYWORD_handler))
	{
		readToken (token);
		if (isKeyword (token, KEYWORD_begin))
		{
			parseBlock (token, true);
		}
		findCmdTerm (token, true);
	}
	deleteToken (name);
}

static void parseTrigger (tokenInfo *const token)
{
	tokenInfo *const name  = newToken ();
	tokenInfo *const table = newToken ();

	/*
	 * This deals with these formats
	 *	   create or replace trigger tr1 begin end;
	 *	   create trigger "tr2" begin end;
	 *	   drop trigger "droptr1";
	 *	   create trigger "tr3" CALL sp_something();
	 *	   create trigger "owner"."tr4" begin end;
	 *	   create trigger "tr5" not valid;
	 *	   create trigger "tr6" begin end;
	 */

	readIdentifier (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readIdentifier (name);
		readToken (token);
	}

	while (! isKeyword (token, KEYWORD_on) &&
		   ! isType (token, TOKEN_EOF) &&
		   ! isCmdTerm (token))
	{
		readToken (token);
	}

	/*if (! isType (token, TOKEN_SEMICOLON) ) */
	if (! isCmdTerm (token))
	{
		readToken (table);
		readToken (token);
		if (isType (token, TOKEN_PERIOD))
		{
			readToken (table);
			readToken (token);
		}

		while (! isKeyword (token, KEYWORD_begin) &&
			   ! isKeyword (token, KEYWORD_call) &&
			   ! isCmdTerm (token) &&
			   ! isType (token, TOKEN_EOF))
		{
			if (isKeyword (token, KEYWORD_declare))
			{
				addToScope(token, name->string, SQLTAG_TRIGGER);
				parseDeclare(token, true);
				vStringClear(token->scope);
				token->scopeKind = SQLTAG_COUNT;
			}
			else
				readToken (token);
		}

		if (isKeyword (token, KEYWORD_begin) ||
			isKeyword (token, KEYWORD_call))
		{
			addToScope(name, table->string, SQLTAG_TABLE);
			makeSqlTag (name, SQLTAG_TRIGGER);
			addToScope(token, table->string, SQLTAG_TABLE);
			if (isKeyword (token, KEYWORD_begin))
			{
				parseBlock (token, true);
			}
			vStringClear(token->scope);
			token->scopeKind = SQLTAG_COUNT;
		}
	}

	findCmdTerm (token, true);
	deleteToken (name);
	deleteToken (table);
}

static void parsePublication (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create or replace publication pu1 ()
	 *	   create publication "pu2" ()
	 *	   create publication dba."pu3" ()
	 *	   create publication "dba"."pu4" ()
	 */

	readIdentifier (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readIdentifier (name);
		readToken (token);
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_PUBLICATION);
		}
	}
	findCmdTerm (token, false);
	deleteToken (name);
}
static void parseService (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   CREATE SERVICE s1 TYPE 'HTML'
	 *		   AUTHORIZATION OFF USER DBA AS
	 *		   SELECT *
	 *			 FROM SYS.SYSTABLE;
	 *	   CREATE SERVICE "s2" TYPE 'HTML'
	 *		   AUTHORIZATION OFF USER DBA AS
	 *		   CALL sp_Something();
	 */

	readIdentifier (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_type))
	{
		if (isType (name, TOKEN_IDENTIFIER) ||
			isType (name, TOKEN_STRING))
		{
			makeSqlTag (name, SQLTAG_SERVICE);
		}
	}
	findCmdTerm (token, false);
	deleteToken (name);
}

static void parseDomain (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   CREATE DOMAIN|DATATYPE [AS] your_name ...;
	 */

	readIdentifier (name);
	if (isKeyword (name, KEYWORD_is))
	{
		readIdentifier (name);
	}
	readToken (token);
	if (isType (name, TOKEN_IDENTIFIER) ||
		isType (name, TOKEN_STRING))
	{
		makeSqlTag (name, SQLTAG_DOMAIN);
	}
	findCmdTerm (token, false);
	deleteToken (name);
}

static void parseDrop (tokenInfo *const token)
{
	/*
	 * This deals with these formats
	 *	   DROP TABLE|PROCEDURE|DOMAIN|DATATYPE name;
	 *
	 * Just simply skip over these statements.
	 * They are often confused with PROCEDURE prototypes
	 * since the syntax is similar, this effectively deals with
	 * the issue for all types.
	 */

	findCmdTerm (token, false);
}

static void parseVariable (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create variable varname1 integer;
	 *	   create variable @varname2 integer;
	 *	   create variable "varname3" integer;
	 *	   drop   variable @varname3;
	 */

	readIdentifier (name);
	readToken (token);
	if (! isType (token, TOKEN_SEMICOLON) &&
		(isType (name, TOKEN_IDENTIFIER) ||
		 isType (name, TOKEN_STRING)))
	{
		makeSqlTag (name, SQLTAG_VARIABLE);
	}
	findCmdTerm (token, true);

	deleteToken (name);
}

static void parseSynonym (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *	   create variable varname1 integer;
	 *	   create variable @varname2 integer;
	 *	   create variable "varname3" integer;
	 *	   drop   variable @varname3;
	 */

	readIdentifier (name);
	readToken (token);
	if (isKeyword (token, KEYWORD_for) &&
		(isType (name, TOKEN_IDENTIFIER) ||
		 isType (name, TOKEN_STRING)))
	{
		makeSqlTag (name, SQLTAG_SYNONYM);
	}
	findCmdTerm (token, true);

	deleteToken (name);
}

static void parseView (tokenInfo *const token)
{
	tokenInfo *const name = newToken ();

	/*
	 * This deals with these formats
	 *     create view VIEW;
	 *     create view VIEW as ...;
	 *     create view VIEW (...) as ...;
	 */

	readIdentifier (name);
	readToken (token);
	if (isType (token, TOKEN_PERIOD))
	{
		readIdentifier (name);
		readToken (token);
	}
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		skipArgumentList(token);
	}

	while (! isKeyword (token, KEYWORD_is) &&
		   ! isType (token, TOKEN_SEMICOLON) &&
		   ! isType (token, TOKEN_EOF))
	{
		readToken (token);
	}

	if (isKeyword (token, KEYWORD_is) &&
		(isType (name, TOKEN_IDENTIFIER) ||
		 isType (name, TOKEN_STRING)))
	{
		makeSqlTag (name, SQLTAG_VIEW);
	}

	findCmdTerm (token, true);

	deleteToken (name);
}

static void parseMLTable (tokenInfo *const token)
{
	tokenInfo *const version = newToken ();
	tokenInfo *const table	 = newToken ();
	tokenInfo *const event	 = newToken ();

	/*
	 * This deals with these formats
	 *	  call dbo.ml_add_table_script( 'version', 'table_name', 'event',
	 *		   'some SQL statement'
	 *		   );
	 */

	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		readToken (version);
		readToken (token);
		while (! isType (token, TOKEN_COMMA) &&
			   ! isType (token, TOKEN_CLOSE_PAREN) &&
			   ! isType (token, TOKEN_EOF))
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
		{
			readToken (table);
			readToken (token);
			while (! isType (token, TOKEN_COMMA) &&
				   ! isType (token, TOKEN_CLOSE_PAREN) &&
				   ! isType (token, TOKEN_EOF))
			{
				readToken (token);
			}

			if (isType (token, TOKEN_COMMA))
			{
				readToken (event);

				if (isType (version, TOKEN_STRING) &&
					isType (table, TOKEN_STRING) &&
					isType (event, TOKEN_STRING))
				{
					addToScope(version, table->string, SQLTAG_TABLE);
					addToScope(version, event->string, SQLTAG_EVENT);
					makeSqlTag (version, SQLTAG_MLTABLE);
				}
			}
			if (! isType (token, TOKEN_CLOSE_PAREN))
				findToken (token, TOKEN_CLOSE_PAREN);
		}
	}

	findCmdTerm (token, true);

	deleteToken (version);
	deleteToken (table);
	deleteToken (event);
}

static void parseMLConn (tokenInfo *const token)
{
	tokenInfo *const version = newToken ();
	tokenInfo *const event	 = newToken ();

	/*
	 * This deals with these formats
	 *	  call ml_add_connection_script( 'version', 'event',
	 *		   'some SQL statement'
	 *		   );
	 */

	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		readToken (version);
		readToken (token);
		while (! isType (token, TOKEN_COMMA) &&
			   ! isType (token, TOKEN_CLOSE_PAREN) &&
			   ! isType (token, TOKEN_EOF))
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
		{
			readToken (event);

			if (isType (version, TOKEN_STRING) &&
				isType (event, TOKEN_STRING))
			{
				addToScope(version, event->string, SQLTAG_EVENT);
				makeSqlTag (version, SQLTAG_MLCONN);
			}
		}
		if (! isType (token, TOKEN_CLOSE_PAREN))
			findToken (token, TOKEN_CLOSE_PAREN);

	}

	findCmdTerm (token, true);

	deleteToken (version);
	deleteToken (event);
}

static void parseMLProp (tokenInfo *const token)
{
	tokenInfo *const component     = newToken ();
	tokenInfo *const prop_set_name = newToken ();
	tokenInfo *const prop_name     = newToken ();

	/*
	 * This deals with these formats
     *   ml_add_property (
     *       'comp_name',
     *       'prop_set_name',
     *       'prop_name',
     *       'prop_value'
     *   )
	 */

	readToken (token);
	if (isType (token, TOKEN_OPEN_PAREN))
	{
		readToken (component);
		readToken (token);
		while (! isType (token, TOKEN_COMMA) &&
			   ! isType (token, TOKEN_CLOSE_PAREN) &&
			   ! isType (token, TOKEN_EOF))
		{
			readToken (token);
		}

		if (isType (token, TOKEN_COMMA))
		{
			readToken (prop_set_name);
			readToken (token);
			while (! isType (token, TOKEN_COMMA) &&
				   ! isType (token, TOKEN_CLOSE_PAREN) &&
				   ! isType (token, TOKEN_EOF))
			{
				readToken (token);
			}

			if (isType (token, TOKEN_COMMA))
			{
				readToken (prop_name);

				if (isType (component, TOKEN_STRING) &&
					isType (prop_set_name, TOKEN_STRING) &&
					isType (prop_name, TOKEN_STRING))
				{
					addToScope(component, prop_set_name->string, SQLTAG_MLPROP /* FIXME */);
					addToScope(component, prop_name->string, SQLTAG_MLPROP /* FIXME */);
					makeSqlTag (component, SQLTAG_MLPROP);
				}
			}
			if (! isType (token, TOKEN_CLOSE_PAREN))
				findToken (token, TOKEN_CLOSE_PAREN);
		}
	}

	findCmdTerm (token, true);

	deleteToken (component);
	deleteToken (prop_set_name);
	deleteToken (prop_name);
}

static void parseComment (tokenInfo *const token)
{
	/*
	 * This deals with this statement:
	 *	   COMMENT TO PRESERVE FORMAT ON PROCEDURE "DBA"."test" IS
	 *	   {create PROCEDURE DBA."test"()
	 *	   BEGIN
	 *		signal dave;
	 *	   END
	 *	   }
	 *	   ;
	 * The comment can contain anything between the CURLY
	 * braces
	 *	   COMMENT ON USER "admin" IS
	 *			'Administration Group'
	 *			;
	 * Or it could be a simple string with no curly braces
	 */
	while (! isKeyword (token, KEYWORD_is) &&
		   ! isType (token, TOKEN_EOF))
	{
		readToken (token);
	}
	readToken (token);
	if (isType(token, TOKEN_OPEN_CURLY))
	{
		findToken (token, TOKEN_CLOSE_CURLY);
	}

	findCmdTerm (token, true);
}

static void parseCCFLAGS (tokenInfo *const token)
{
	readToken(token);
	if (!isType (token, TOKEN_EQUAL))
	{
		findCmdTerm (token, true);
		return;
	}

	readToken(token);
	if (!isType (token, TOKEN_STRING))
	{
		findCmdTerm (token, true);
		return;
	}

	bool in_var = true;
	const char *s = vStringValue(token->string);
	vString *ccflag = vStringNew();
	/* http://web.deu.edu.tr/doc/oracle/B19306_01/server.102/b14237/initparams158.htm#REFRN10261 */
	while (*s)
	{
		if (in_var && isIdentChar1((int)*s))
			vStringPut(ccflag, *s);
		else if (*s == ':' && !vStringIsEmpty(ccflag))
		{
			if (lookupCaseKeyword(vStringValue(ccflag), Lang_sql)
				!= KEYWORD_inquiry_directive)
			{
				int index = makeSimpleTag(ccflag, SQLTAG_PLSQL_CCFLAGS);
				registerEntry(index);
				vStringClear(ccflag);
				in_var = false;
			}
		}
		else if (*s == ',')
			in_var = true;
		s++;
	}
	vStringDelete(ccflag);

}

static void parseDatabase (tokenInfo *const token, enum eKeywordId keyword)
{
	tokenInfo * name;

	/*
	 * In MySQL and HPL/SQL, "CREATE DATABASE" and "CREATE SCHEMA"
	 * are the same. However, In PostgreSQL, they are different.
	 * Too support PostgreSQL, we prepare different kinds for them.
	 *
	 * MySQL
	 * A. CREATE {DATABASE | SCHEMA} [IF NOT EXISTS] db_name ...;
	 *
	 * PostgreSQL
	 *
	 * B. CREATE DATABASE name ...;
	 *
	 * C. CREATE SCHEMA schema_name [ AUTHORIZATION role_specification ] [ schema_element [ ... ] ]
	 * D. CREATE SCHEMA AUTHORIZATION role_specification [ schema_element [ ... ] ]
	 * E. CREATE SCHEMA IF NOT EXISTS schema_name [ AUTHORIZATION role_specification ]
	 * F. CREATE SCHEMA IF NOT EXISTS AUTHORIZATION role_specification
	 *
	 * HPL/SQL
	 * G. CREATE DATABASE | SCHEMA [IF NOT EXISTS] dbname_expr...;
	 */
	readIdentifier (token);
	if (keyword == KEYWORD_schema
		&& isType (token, TOKEN_IDENTIFIER)
		&& vStringLength (token->string) == 13
		&& strcasecmp("authorization", vStringValue(token->string)) == 0)
	{
		/* D. */
		readIdentifier (token);
		makeSqlTag (token, SQLTAG_SCHEMA);
		findCmdTerm (token, false);
		return;
	}

	name = newToken ();
	copyToken (name, token);
	readIdentifier (token);
	parseIdAfterIfNotExists (name, token, true);

	makeSqlTag (name,
				keyword == KEYWORD_database
				? SQLTAG_DATABASE: SQLTAG_SCHEMA);
	deleteToken (name);

	/* TODO:
	 *
	 * In PostgreSQL, CREATE FOO can follow to CREATE SCHEMA like:
	 *
	 * -- https://www.postgresql.org/docs/current/sql-createschema.html
	 *
	 *     CREATE SCHEMA hollywood
	 *         CREATE TABLE films (title text, release date, awards text[])
	 *         CREATE VIEW winners AS
	 *             SELECT title, release FROM films WHERE awards IS NOT NULL;
	 *
	 * In above example, "hollywood.films" and "hollywood.winners" should be
	 * tagged.
	 */
	findCmdTerm (token, true);
}

static void parseKeywords (tokenInfo *const token)
{
		switch (token->keyword)
		{
			case KEYWORD_begin:			parseBlock (token, false); break;
			case KEYWORD_inquiry_directive:
				if (strcasecmp(vStringValue(token->string), "PLSQL_CCFLAGS") == 0)
					parseCCFLAGS (token);
				break;
			case KEYWORD_comment:		parseComment (token); break;
			case KEYWORD_cursor:		parseSimple (token, SQLTAG_CURSOR); break;
			case KEYWORD_database:		parseDatabase (token, KEYWORD_database); break;
			case KEYWORD_datatype:		parseDomain (token); break;
			case KEYWORD_declare:		parseBlock (token, false); break;
			case KEYWORD_domain:		parseDomain (token); break;
			case KEYWORD_drop:			parseDrop (token); break;
			case KEYWORD_event:			parseEvent (token); break;
			case KEYWORD_extension:		findCmdTerm (token, false); break;
			case KEYWORD_function:		parseSubProgram (token); break;
			case KEYWORD_if:			parseStatements (token, false); break;
			case KEYWORD_index:			parseIndex (token); break;
			case KEYWORD_ml_table:		parseMLTable (token); break;
			case KEYWORD_ml_table_lang: parseMLTable (token); break;
			case KEYWORD_ml_table_dnet: parseMLTable (token); break;
			case KEYWORD_ml_table_java: parseMLTable (token); break;
			case KEYWORD_ml_table_chk:  parseMLTable (token); break;
			case KEYWORD_ml_conn:		parseMLConn (token); break;
			case KEYWORD_ml_conn_lang:	parseMLConn (token); break;
			case KEYWORD_ml_conn_dnet:	parseMLConn (token); break;
			case KEYWORD_ml_conn_java:	parseMLConn (token); break;
			case KEYWORD_ml_conn_chk:	parseMLConn (token); break;
			case KEYWORD_ml_prop:		parseMLProp (token); break;
			case KEYWORD_package:		parsePackage (token); break;
			case KEYWORD_procedure:		parseSubProgram (token); break;
			case KEYWORD_publication:	parsePublication (token); break;
			case KEYWORD_schema:		parseDatabase (token, KEYWORD_schema); break;
			case KEYWORD_service:		parseService (token); break;
			case KEYWORD_subtype:		parseSimple (token, SQLTAG_SUBTYPE); break;
			case KEYWORD_synonym:		parseSynonym (token); break;
			case KEYWORD_table:			parseTable (token); break;
			case KEYWORD_trigger:		parseTrigger (token); break;
			case KEYWORD_type:			parseType (token); break;
			case KEYWORD_variable:		parseVariable (token); break;
			case KEYWORD_view:			parseView (token); break;
			case KEYWORD_with:			readToken (token); break; /* skip next token */
			case KEYWORD_without:		readToken (token); break; /* skip next token */
			default:				    break;
		}
}

static tokenType parseSqlFile (tokenInfo *const token)
{
	do
	{
		readToken (token);

		if (isType (token, TOKEN_BLOCK_LABEL_BEGIN))
			parseLabel (token);
		else
			parseKeywords (token);
	} while (! isKeyword (token, KEYWORD_end) &&
			 ! isType (token, TOKEN_EOF));

	return token->type;
}

static void initialize (const langType language)
{
	Assert (ARRAY_SIZE (SqlKinds) == SQLTAG_COUNT);
	Lang_sql = language;
	addKeywordGroup (&predefinedInquiryDirective, language);
}

static void findSqlTags (void)
{
	tokenInfo *const token = newToken ();

	while (parseSqlFile (token) != TOKEN_EOF);

	deleteToken (token);
}

extern parserDefinition* SqlParser (void)
{
	static const char *const extensions [] = { "sql", NULL };
	static const char *const aliases [] = {"pgsql", NULL };
	parserDefinition* def = parserNew ("SQL");
	def->kindTable	= SqlKinds;
	def->kindCount	= ARRAY_SIZE (SqlKinds);
	def->extensions = extensions;
	def->aliases    = aliases;
	def->parser		= findSqlTags;
	def->initialize = initialize;
	def->keywordTable = SqlKeywordTable;
	def->keywordCount = ARRAY_SIZE (SqlKeywordTable);
	def->useCork = CORK_QUEUE | CORK_SYMTAB;
	return def;
}
