/* Taken from xorg-x11-xkb-utils-7.7/xkbevd-1.1.3/cfgparse.y */
/************************************************************
 Copyright (c) 1994 by Silicon Graphics Computer Systems, Inc.

 Permission to use, copy, modify, and distribute this
 software and its documentation for any purpose and without
 fee is hereby granted, provided that the above copyright
 notice appear in all copies and that both that copyright
 notice and this permission notice appear in supporting
 documentation, and that the name of Silicon Graphics not be
 used in advertising or publicity pertaining to distribution
 of the software without specific prior written permission.
 Silicon Graphics makes no representation about the suitability
 of this software for any purpose. It is provided "as is"
 without any express or implied warranty.

 SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
 GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 THE USE OR PERFORMANCE OF THIS SOFTWARE.

 ********************************************************/

%token
	END_OF_FILE	0
	ERROR		255
	BELL		1
	ACCESSX		2
	MESSAGE		3

	NONE		20
	IGNORE		21
	ECHO		22
	PRINT_EV	23
	SHELL		24
	SOUND		25

	EQUALS		40
	PLUS		41
	MINUS		42
	DIVIDE		43
	TIMES		44
	OBRACE		45
	CBRACE		46
	OPAREN		47
	CPAREN		48
	OBRACKET	49
	CBRACKET	50
	DOT		51
	COMMA		52
	SEMI		53
	EXCLAM		54
	INVERT		55
	STRING		60
	INTEGER		61
	FLOAT		62
	IDENT		63
	KEYNAME		64
%{
#ifdef DEBUG
#define	YYDEBUG 1
#endif
#define	DEBUG_VAR parseDebug
#include "xkbevd.h"
#include <stdlib.h>
%}
%right	EQUALS
%left	PLUS MINUS
%left	TIMES DIVIDE
%left	EXCLAM INVERT
%left	OPAREN
%start	CfgFile
%union	{
	char *		str;
	int		ival;
	CfgEntryPtr	entry;
	ActDefPtr	act;
}
%type <str>	Ident String OptString NameSpec OptNameSpec
%type <ival>	ActionType EventType
%type <act>	ActionDef
%type <entry>	CfgFile CfgEntryList CfgEntry EventDef VarDef
%%
CfgFile		
		:	CfgEntryList
			{ InterpretConfigs($1); }
		;

CfgEntryList	:	CfgEntryList CfgEntry
			{
			    CfgEntryPtr tmp;
			    if ($1!=NULL) {
				for (tmp=$1;tmp->next!=NULL;tmp=tmp->next) {
				    /* conditional does the work */
				}
				tmp->next= $2;
				$$= $1;
			    }
			    else $$= $2;
			}
		|	CfgEntry { $$= $1; }
		;

CfgEntry	:	EventDef ActionDef
			{
			    if (($1)&&($2))
				$1->action= *($2);
			    if ($2)
				free($2);
			    $$= $1;
			}
		|	VarDef 		{ $$= $1; }
		;

VarDef		:	Ident EQUALS NameSpec
			{
			    CfgEntryPtr cfg;
			    cfg= calloc(1,sizeof(CfgEntryRec));
			    if (cfg) {
				cfg->entry_type= VariableDef;
				cfg->event_type= 0;
				cfg->name.str= $1;
				cfg->action.type= UnknownAction;
				cfg->action.text= $3;
				cfg->action.priv= 0;
				cfg->next= NULL;
			    }
			    $$= cfg;
			}
		;

EventDef	:	EventType OPAREN OptNameSpec CPAREN
			{
			    CfgEntryPtr cfg;
			    cfg= calloc(1,sizeof(CfgEntryRec));
			    if (cfg) {
				cfg->entry_type= EventDef;
				cfg->event_type= $1;
				cfg->name.str= $3;
				cfg->action.type= UnknownAction;
				cfg->action.text= NULL;
				cfg->action.priv= 0;
				cfg->next= NULL;
			    }
			    $$= cfg;
			}
		;

EventType	:	BELL		{ $$= XkbBellNotify; }
		|	ACCESSX		{ $$= XkbAccessXNotify; }
		|	MESSAGE		{ $$= XkbActionMessage; }
		;

ActionDef	:	ActionType OptString
			{
			    ActDefPtr act;
			    act= calloc(1,sizeof(ActDefRec));
			    if (act) {
				act->type= $1;
				act->text= $2;
			    }
			    $$= act;
			}
		;

ActionType	:	NONE	 { $$ = NoAction; }
		|	IGNORE	 { $$ = NoAction; }
		|	ECHO	 { $$ = EchoAction; }
		|	PRINT_EV { $$ = PrintEvAction; }
		|	SHELL	 { $$ = ShellAction; }
		|	SOUND	 { $$ = SoundAction; }
		|		 { $$ = UnknownAction; }
		;

OptNameSpec	:	NameSpec { $$= $1; }
		|		 { $$= NULL; }
		;

NameSpec	:	Ident	{ $$= $1; }
		|	String	{ $$= $1; }
		;

Ident		:	IDENT	{ $$= scanStr; scanStr= NULL; }
		;

OptString	:	String	{ $$= $1; }
		|		{ $$= NULL; }
		;

String		:	STRING	{ $$= scanStr; scanStr= NULL; }
		;
%%
int
yyerror(char *s)
{
    (void)fprintf(stderr,"%s: line %d of %s\n",s,lineNum,
					(scanFile?scanFile:"(unknown)"));
    if (scanStr)
	(void)fprintf(stderr,"last scanned symbol is: %s\n",scanStr);
    return 1;
}


int
yywrap(void)
{
   return 1;
}

int
CFGParseFile(FILE *file)
{
    if (file) {
	yyin= file;
	if (yyparse()==0) {
	    return 1;
	}
	return 0;
    }
    return 1;
}
