/* Based on php-pear/ext/json/json_parser.y */
%require "3.0"
%code top {
#include "php.h"
int json_yydebug = 1;
}

%define api.prefix {php_json_yy}
%define api.pure full
%param  { php_json_parser *parser  }

%union {
	zval value;
}


%token <value> PHP_JSON_T_NUL
%token PHP_JSON_T_EOI

%destructor { zval_ptr_dtor_nogc(&$$); } <value>

%code {
static int php_json_yylex(union YYSTYPE *value, php_json_parser *parser);
static void php_json_yyerror(php_json_parser *parser, char const *msg);
}

%% /* Rules */

start:
		value PHP_JSON_T_EOI
			{
				ZVAL_COPY_VALUE(&$$, &$1);
				ZVAL_COPY_VALUE(parser->return_value, &$1);
				(void) php_json_yynerrs;
				YYACCEPT;
			}
;


%% /* Functions */

static int php_json_parser_array_create(php_json_parser *parser, zval *array)
{
	array_init(array);
	return SUCCESS;
}

