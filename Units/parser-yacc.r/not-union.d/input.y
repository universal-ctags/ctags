%{

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include "ssfilter.h"

typedef struct ssfilter * ssfilter_t;

#define YYSTYPE ssfilter_t

static struct ssfilter * alloc_node(int type, void *pred)
{
	struct ssfilter *n = malloc(sizeof(*n));
	if (n == NULL)
		abort();
	n->type = type;
	n->pred = pred;
	n->post = NULL;
	return n;
}

static char		**yy_argv;
static int		yy_argc;
static FILE		*yy_fp;
static ssfilter_t	*yy_ret;
static int tok_type = -1;

static int yylex(void);

static void yyerror(char *s)
{
	fprintf(stderr, "ss: bison bellows (while parsing filter): \"%s!\"", s);
}

%}

%token HOSTCOND DCOND SCOND DPORT SPORT LEQ GEQ NEQ AUTOBOUND
%left '|'
%left '&'
%nonassoc '!'

%%
applet: null expr
        {
                *yy_ret = $2;
                $$ = $2;
        }
        | null
        ;
null:   /* NOTHING */ { $$ = NULL; }
        ;
expr:	DCOND HOSTCOND
        {
		$$ = alloc_node(SSF_DCOND, $2);
        }
        | SCOND HOSTCOND
        {
		$$ = alloc_node(SSF_SCOND, $2);
        }
        | DPORT GEQ HOSTCOND
        {
                $$ = alloc_node(SSF_D_GE, $3);
        }
        | DPORT LEQ HOSTCOND
        {
                $$ = alloc_node(SSF_D_LE, $3);
        }
        | DPORT '>' HOSTCOND
        {
                $$ = alloc_node(SSF_NOT, alloc_node(SSF_D_LE, $3));
        }
        | DPORT '<' HOSTCOND
        {
                $$ = alloc_node(SSF_NOT, alloc_node(SSF_D_GE, $3));
        }
        | DPORT '=' HOSTCOND
        {
		$$ = alloc_node(SSF_DCOND, $3);
        }
        | DPORT NEQ HOSTCOND
        {
		$$ = alloc_node(SSF_NOT, alloc_node(SSF_DCOND, $3));
        }

        | SPORT GEQ HOSTCOND
        {
                $$ = alloc_node(SSF_S_GE, $3);
        }
        | SPORT LEQ HOSTCOND
        {
                $$ = alloc_node(SSF_S_LE, $3);
        }
        | SPORT '>' HOSTCOND
        {
                $$ = alloc_node(SSF_NOT, alloc_node(SSF_S_LE, $3));
        }
        | SPORT '<' HOSTCOND
        {
                $$ = alloc_node(SSF_NOT, alloc_node(SSF_S_GE, $3));
        }
        | SPORT '=' HOSTCOND
        {
		$$ = alloc_node(SSF_SCOND, $3);
        }
        | SPORT NEQ HOSTCOND
        {
		$$ = alloc_node(SSF_NOT, alloc_node(SSF_SCOND, $3));
        }

        | AUTOBOUND
        {
                $$ = alloc_node(SSF_S_AUTO, NULL);
        }
        | expr '|' expr
        {
                $$ = alloc_node(SSF_OR, $1);
	        $$->post = $3;
        }
        | expr expr
        {
                $$ = alloc_node(SSF_AND, $1);
	        $$->post = $2;
        }
        | expr '&' expr

        {
                $$ = alloc_node(SSF_AND, $1);
	        $$->post = $3;
        }
        | '!' expr
        {
                $$ = alloc_node(SSF_NOT, $2);
        }
        | '(' expr ')'
        {
                $$ = $2;
        }
;
%%

static char *get_token_from_line(char **ptr)
{
	char *tok, *cp = *ptr;

	while (*cp == ' ' || *cp == '\t') cp++;

	if (*cp == 0) {
		*ptr = cp;
		return NULL;
	}

	tok = cp;

	while (*cp != 0 && *cp != ' ' && *cp != '\t') {
		/* Backslash escapes everything. */
		if (*cp == '\\') {
			char *tp;
			for (tp = cp; tp != tok; tp--)
				*tp = *(tp-1);
			cp++;
			tok++;
			if (*cp == 0)
				break;
		}
		cp++;
	}
	if (*cp)
		*cp++ = 0;
	*ptr = cp;
	return tok;
}

int yylex(void)
{
	static char argbuf[1024];
	static char *tokptr = argbuf;
	static int argc;
	char *curtok;

	do {
		while (*tokptr == 0) {
			tokptr = NULL;
			if (argc < yy_argc) {
				tokptr = yy_argv[argc];
				argc++;
			} else if (yy_fp) {
				while (tokptr == NULL) {
					if (fgets(argbuf, sizeof(argbuf)-1, yy_fp) == NULL)
						return 0;
					argbuf[sizeof(argbuf)-1] = 0;
					if (strlen(argbuf) == sizeof(argbuf) - 1) {
						fprintf(stderr, "Too long line in filter");
						exit(-1);
					}
					if (argbuf[strlen(argbuf)-1] == '\n')
						argbuf[strlen(argbuf)-1] = 0;
					if (argbuf[0] == '#' || argbuf[0] == '0')
						continue;
					tokptr = argbuf;
				}
			} else {
				return 0;
			}
		}
	} while ((curtok = get_token_from_line(&tokptr)) == NULL);

	if (strcmp(curtok, "!") == 0 ||
	    strcmp(curtok, "not") == 0)
		return '!';
	if (strcmp(curtok, "&") == 0 ||
	    strcmp(curtok, "&&") == 0 ||
	    strcmp(curtok, "and") == 0)
		return '&';
	if (strcmp(curtok, "|") == 0 ||
	    strcmp(curtok, "||") == 0 ||
	    strcmp(curtok, "or") == 0)
		return '|';
	if (strcmp(curtok, "(") == 0)
		return '(';
	if (strcmp(curtok, ")") == 0)
		return ')';
	if (strcmp(curtok, "dst") == 0) {
		tok_type = DCOND;
		return DCOND;
	}
	if (strcmp(curtok, "src") == 0) {
                tok_type = SCOND;
		return SCOND;
        }
	if (strcmp(curtok, "dport") == 0) {
		tok_type = DPORT;
		return DPORT;
	}
	if (strcmp(curtok, "sport") == 0) {
		tok_type = SPORT;
		return SPORT;
	}
	if (strcmp(curtok, ">=") == 0 ||
	    strcmp(curtok, "ge") == 0 ||
	    strcmp(curtok, "geq") == 0)
		return GEQ;
	if (strcmp(curtok, "<=") == 0 ||
	    strcmp(curtok, "le") == 0 ||
	    strcmp(curtok, "leq") == 0)
		return LEQ;
	if (strcmp(curtok, "!=") == 0 ||
	    strcmp(curtok, "ne") == 0 ||
	    strcmp(curtok, "neq") == 0)
		return NEQ;
	if (strcmp(curtok, "=") == 0 ||
	    strcmp(curtok, "==") == 0 ||
	    strcmp(curtok, "eq") == 0)
		return '=';
	if (strcmp(curtok, ">") == 0 ||
	    strcmp(curtok, "gt") == 0)
		return '>';
	if (strcmp(curtok, "<") == 0 ||
	    strcmp(curtok, "lt") == 0)
		return '<';
	if (strcmp(curtok, "autobound") == 0) {
		tok_type = AUTOBOUND;
		return AUTOBOUND;
	}
	yylval = (void*)parse_hostcond(curtok, tok_type == SPORT || tok_type == DPORT);
	if (yylval == NULL) {
		fprintf(stderr, "Cannot parse dst/src address.\n");
		exit(1);
	}
	return HOSTCOND;
}

int ssfilter_parse(struct ssfilter **f, int argc, char **argv, FILE *fp)
{
	yy_argc = argc;
	yy_argv = argv;
	yy_fp   = fp;
	yy_ret  = f;

	if (yyparse()) {
		fprintf(stderr, " Sorry.\n");
		return -1;
	}
	return 0;
}
