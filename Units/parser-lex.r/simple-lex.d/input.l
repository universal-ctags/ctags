/* 
 * Taken from texlive-2007/texk/lacheck/lacheck.lex
 * with a minor modification.
 */

/*                    -*- Mode: C -*-
 * 
 * lacheck.lex - A consistency checker checker for LaTeX documents
 *	
 * Copyright (C) 1991, 1992 Kresten Krab Thorup.
 * Copyright (C) 1993 --- 1998 Per Abrahamsen.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Please send bugs, suggestions, and queries to auc-tex_mgr@sunsite.auc.dk.
 * 
 * $Revision: 1.26 $
 * Author          : Kresten Krab Thorup
 * Created On      : Sun May 26 18:11:58 1991
 * 
 * HISTORY
 * 07-Mar-1998		Per Abrahamsen
 *    Added return to yywrap.  Patch by Fabrice POPINEAU 
 *    <popineau@esemetz.ese-metz.fr>.
 * 14-Jan-1998		Per Abrahamsen
 *    Added GPL blurp.
 * 27-Oct-1997		Per Abrahamsen
 *    Count newline after newenvironment and newcommand.
 * 12-Jan-1996          Per Abrahamsen
 *    \\} used not to end a group in definitions.  Reported by Piet
 *    van Oostrum <piet@cs.ruu.nl>.
 * 03-Jan-1995		Per Abrahamsen
 *    Fix bug which prevented detection of multiple illegal characters
 *    in labels.  Reported by eeide@jaguar.cs.utah.edu (Eric Eide).
 * 30-Jul-1994		Per Abrahamsen
 *    Define dummy yywrap so we no longer depend on `libl.a'.
 * 26-Apr-1994		Per Abrahamsen
 *    Removed a few warnings, by Richard Lloyd <R.K.Lloyd@csc.liv.ac.uk>.
 * 23-Apr-1994		Per Abrahamsen
 *    Changed all `%i' to `%d' for VMS portability.  Reported by
 *    Stephen Harker <PHS172M@vaxc.cc.monash.edu.au>.
 * 16-Feb-1994		Per Abrahamsen
 *    Try file name with `.tex' appended before trying it bare.  This
 *    will make the case where a directory and a TeX file share the
 *    same name work.
 * 19-Jan-1994		Per Abrahamsen
 *    Comments don't imply whitespace.  Pointed out by Jacco van
 *    Ossenbruggen <jrvosse@cs.vu.nl>.
 * 14-Jan-1994		Per Abrahamsen
 *    Don't complain about \ref at the beginning of a paragraph.
 *    Suggested by Jean-Marc Lasgouttes <Jean-Marc.Lasgouttes@inria.fr>.
 * 11-Jan-1994		Per Abrahamsen
 *    Added version string to usage message.  Suggested by Uwe Bonnes
 *    <bon@LTE.E-TECHNIK.uni-erlangen.de> .
 * 04-Jan-1994		Per Abrahamsen
 *    Warn about newlines in \verb.  Suggested by Mark Burton
 *    <markb@ordern.demon.co.uk>.  The LaTeX Book agrees (p. 168).
 * 10-Sep-1993          Per Abrahamsen
 *    Removed complain about missing ~ before \cite.  Requested by 
 *    Nelson H. F. Beebe <beebe@math.utah.edu>.  The LaTeX Book seems
 *    to agree.  
 * 03-Sep-1993	        Per Abrahamsen
 *    Check for illegal characters in labels.
 * 16-Aug-1993	        Per Abrahamsen
 *    Recognize \endinput.  Suggested by Stefan Farestam
 *    <Stefan.Farestam@cerfacs.fr>.
 * 13-Aug-1993          Per Abrahamsen
 *    } was eaten after display math.  Reported by Eckhard Rüggeberg
 *    <eckhard@ts.go.dlr.de>.
 * 13-Aug-1993          Per Abrahamsen
 *    Recognize \verb*.  Reported by Eckhard Rüggeberg
 *    <eckhard@ts.go.dlr.de>.  
 * 08-Aug-1993          Per Abrahamsen
 *    Better catch begin and end without arguments.
 * 08-Aug-1993          Per Abrahamsen
 *    Removed free(NULL) as reported by Darrel R. Hankerson 
 *    <hankedr@mail.auburn.edu>.
 * 08-Aug-1993		Per Abrahamsen
 *    Removed declaration of realloc for some C compilers.  Reported by 
 *    Darrel R. Hankerson <hankedr@mail.auburn.edu>
 * 30-Jul-1993          Per Abrahamsen
 *    Added check for italic correction after normal text.
 * 29-Jul-1993          Per Abrahamsen
 *    Added cast for (char*) malloc as suggested by John Interrante
 *    <interran@uluru.Stanford.EDU>.
 * 29-Jul-1993          Per Abrahamsen
 *    Added check for missing and extra italic correction.
 * 29-Jul-1993	        Per Abrahamsen
 *    Made line number counting more reliable (but it still needs a rewrite)!
 * 28-Jul-1993	        Per Abrahamsen
 *    Added check for italic correction before point or comma.
 * 6-Jun-1992		Kresten Krab Thorup	
 *    Last Modified: Sat Jun  6 16:37:44 1992 #48 (Kresten Krab Thorup)
 *    Added test for whitespace before punctuation mark
 * 17-Dec-1991  (Last Mod: Tue Dec 17 21:01:24 1991 #41)  Kresten Krab Thorup
 *    Added 'word word` and missing ~ before cite and ref
 * 18-Jun-1991  (Last Mod: Tue Jun 18 19:20:43 1991 #17)  Kresten Krab Thorup
 *    Added check (or rather management) for \newenvironment and
 *    \newcommand - as suggested by Per Abrahamsen abrham@hugin.dk
 * 30-May-1991  (Last Mod: Thu May 30 02:22:33 1991 #15)  Kresten Krab Thorup
 *    Added check for `$${punct}' and `{punct}$' constructions
 * 30-May-1991  (Last Mod: Wed May 29 10:31:35 1991 #6)  Kresten Krab Thorup
 *    Improved (dynamic) stack management from Andreas Stolcke ...
 *                                       <stolcke@ICSI.Berkeley.EDU> 
 * 26-May-1991  Kresten Krab Thorup
 *    Initial distribution version.
 */

%{ 

#include <stdio.h>
#include <string.h>

#ifdef WIN32
#include <win32lib.h>
#endif
/* #include <sys/param.h> */

/* extern char *realloc(); */

#define YY_SKIP_YYWRAP
int yywrap() { return 1; }

#ifdef NEED_STRSTR
char *strstr();
#endif

#define GROUP_STACK_SIZE 10
#define INPUT_STACK_SIZE 10

#define PROGNAME "LaCheck"

  /* macros */

#define CG_NAME (char *)gstack[gstackp-1].s_name
#define CG_TYPE gstack[gstackp-1].s_type
#define CG_LINE gstack[gstackp-1].s_line
#define CG_ITALIC gstack[gstackp-1].italic
#define CG_FILE gstack[gstackp-1].s_file

char *bg_command();
void pop();
void push();
void linecount();
void g_checkend();
void e_checkend();
void f_checkend();
void input_file();
void print_bad_match();
int check_top_level_end();

  /* global variables */

char returnval[100];
int line_count = 1;
int warn_count = 0;
char *file_name;
char verb_char;

  /* the group stack */

typedef struct tex_group 
 {
    unsigned char *s_name;
    int s_type;
    int s_line;
    int italic;
    char *s_file; 
 } tex_group;

tex_group *gstack;
int gstack_size = GROUP_STACK_SIZE;
int gstackp = 0;

typedef struct input_ 
 {
    YY_BUFFER_STATE stream;
    char *name;
    int linenum;
 } input_;

input_ *istack;
int istack_size = INPUT_STACK_SIZE;
int istackp = 0;

int def_count = 0;

%}

%x B_ENVIRONMENT E_ENVIRONMENT VERBATIM INCLUDE MATH COMMENT VERB DEF
%x AFTER_DISPLAY ENV_DEF ICOR GETICOR

%s START

b_group ("{"|\\bgroup)
e_group ("}"|\\egroup)

b_math \\\(
e_math \\\)
math \$

b_display \\\[
e_display \\\]
display \$\$

par ([ \t]*\n[ \t]*\n[ \t\n]*)
non_par_ws ([ \t]+\n?[ \t]*|[ \t]*\n[ \t]*|[ \t]*\n?[ \t]+)

ws [ \n\t](%[^\n]\n)*
space ({ws}|\~|\\space)
hard_space (\~|\\space)

u_letter [A-ZÆØÅ] 
l_letter [a-zæøå] 
punct [\!\.\?]
atoz [a-zA-Z]
letter [A-ZÆØÅa-zæøå]

c_bin ("-"|"+"|"\\cdot"|"\\oplus"|"\\otimes"|"\\times")
l_bin (",")

general_abbrev {letter}+{punct}

non_abbrev {u_letter}{u_letter}+{punct}

italic_spec (sl|it)
normal_spec normalshape
swap_spec em
font_spec (rm|bf|{italic_spec}|tt|{swap_spec}|mediumseries|{normal_spec})

primitive \\(above|advance|catcode|chardef|closein|closeout|copy|count|countdef|cr|crcr|csname|delcode|dimendef|dimen|divide|expandafter|font|hskp|vskip|openout)

symbol ("$"("\\"{atoz}+|.)"$"|"\\#"|"\\$"|"\\%"|"\\ref")

%%

<*>"\\\\" { ; }

<ENV_DEF,DEF,INITIAL>"\\\%" { ; }

<ICOR,GETICOR,ENV_DEF,DEF,INITIAL>"%"[^\n]*\n { line_count++; }

<ICOR,GETICOR,ENV_DEF,DEF,INITIAL>\n 	{ line_count++; }

<ENV_DEF,DEF,INITIAL>"\\\{" { ; }

<ENV_DEF,DEF,INITIAL>"\\\}" { ; }

"\\\$" { ; }

<ICOR,INITIAL,GETICOR>"{"{ws} {  
  if (CG_TYPE != 4 && CG_TYPE != 5) {
    if (!(CG_TYPE == 2 && strstr(CG_NAME, "array"))) {
      printf( "\"%s\", line %d: possible unwanted space at \"{\"\n", 
	     file_name, line_count); 
      ++warn_count ;
    }
  }
  push( "{", 0, line_count);
  linecount();
 }

<ICOR,INITIAL,GETICOR>{b_group} {  push( "{", 0, line_count);}

<INITIAL,GETICOR>{e_group} {  
  {
    int italic = CG_ITALIC;
    g_checkend(0); 
    if (italic && !CG_ITALIC)
      BEGIN(GETICOR) ;
    else
      BEGIN(INITIAL);
  }}

<ICOR>{e_group} {  g_checkend(0); }

<GETICOR>[A-Za-zæøåÆØÅ0-9;:!()]+ {
 {
   if (!CG_ITALIC)
     {
       printf("\"%s\", line %d: you may need a \\/ before \"%s\"\n",
	      file_name, line_count, yytext); 
       ++warn_count;
     }
    BEGIN(INITIAL); 
 }}

<ICOR>[A-Za-zæøåÆØÅ0-9;:!?()`']+ {
 {
   if (CG_ITALIC)
     {
       printf("\"%s\", line %d: \\/ not needed before italic text \"%s\"\n",
	      file_name, line_count, yytext); 
       ++warn_count;
     }
    BEGIN(INITIAL); 
 }}

^[A-Za-zæøåÆØÅ0-9;:!?()`',.]+{ws}*/\\\/ {
  {
   linecount();
   if (!CG_ITALIC)
     {
       printf("\"%s\", line %d: \\/ not needed after non-italic text \"%s\"\n",
              file_name, line_count, yytext);
       ++warn_count;
     }
 }}

{ws}[A-Za-zæøåÆØÅ0-9;:!?()`',.]+{ws}*/\\\/ {
  {
   linecount();
   if (!CG_ITALIC)
     {
       printf("\"%s\", line %d: \\/ is not needed after non-italic \"%s\"\n",
              file_name, line_count, yytext);
       ++warn_count;
     }
 }}

<GETICOR>\\\/ { BEGIN(INITIAL); }

<INITIAL>\\\/ { BEGIN(ICOR); }

<ICOR>\\\/ {
  {
    printf("\"%s\", line %d: double \\/ found \"%s\"\n",
           file_name, line_count, yytext);
    ++warn_count;
    BEGIN(ICOR);
  }}

<INITIAL,GETICOR,ICOR>\\{italic_spec}/[^a-zA-Z] { CG_ITALIC = 1; }

<INITIAL,GETICOR>\\{normal_spec}/[^a-zA-Z] {
  {
    if(CG_ITALIC)
      BEGIN(GETICOR);
    else
      BEGIN(INITIAL);
    CG_ITALIC = 0;
  }}

<ICOR>\\{normal_spec}/[^a-zA-Z] { CG_ITALIC = 0; }

<INITIAL,GETICOR>\\{swap_spec}/[^a-zA-Z] {
  {
    if(CG_ITALIC)
      BEGIN(GETICOR);
    else
      BEGIN(INITIAL);
    CG_ITALIC = !CG_ITALIC;
  }}

<ICOR>\\{swap_spec}/[^a-zA-Z] { CG_ITALIC = !CG_ITALIC; }

<ICOR>[,.] {
 {
    printf("\"%s\", line %d: do not use \\/ before \"%s\"\n",
	   file_name, line_count, yytext); 
    ++warn_count; 
    BEGIN(INITIAL);
 }}

<GETICOR,ICOR>{ws} { ; }

<GETICOR,ICOR>~ { ; }

<GETICOR,ICOR>[^\n] { 
  {
    unput(yytext[0]);
    BEGIN(INITIAL); 
  }}

"\\"[exg]?(def|newcommand)[^\n\{]+ 	BEGIN(DEF);

<DEF>{b_group} { ++def_count; }

<DEF>{e_group} { --def_count;
		 if(def_count == 0)
		     BEGIN(INITIAL); }

<DEF>. { ; }

"\\"newenvironment"{"[a-zA-Z]+"}"[^\n\{]+ 	BEGIN(ENV_DEF);

<ENV_DEF>{b_group} { ++def_count; }

<ENV_DEF>{e_group} { --def_count;
		 if(def_count == 0)
		     BEGIN(DEF); }

<ENV_DEF>. { ; }

{b_math} {
    if(CG_TYPE == 4 || CG_TYPE == 5)
	print_bad_match(yytext,4);
    else
    {
	push( yytext, 4, line_count);
    }}

{e_math} {  g_checkend(4); }

{b_display} {
    if(CG_TYPE == 4 || CG_TYPE == 5)
	print_bad_match(yytext,5);
    else 
    {
	push( yytext, 5, line_count);
    }}


{e_display} {  g_checkend(5);     BEGIN(AFTER_DISPLAY);}

<AFTER_DISPLAY>{punct} { 

    printf( "\"%s\", line %d: punctuation mark \"%s\" should be placed before end of displaymath\n", 
	   file_name, line_count, yytext); 
    ++warn_count ; 

  BEGIN(INITIAL); }

<AFTER_DISPLAY>(\n|.) { unput(yytext[0]); BEGIN(INITIAL); }

<ICOR,INITIAL,GETICOR>{punct}/("\$"|"\\)") { if (CG_TYPE == 4)
       {
	 printf( "\"%s\", line %d: punctuation mark \"%s\" should be placed after end of math mode\n", 
		file_name, line_count, yytext); 
	 ++warn_count ;
	 BEGIN(INITIAL);
       }}

{math} {

    if(CG_TYPE == 5)
	print_bad_match(yytext, 4);
    else 

    if(CG_TYPE == 4)
    {
	e_checkend(4, yytext);
    }
    else
    {
	push( yytext, 4, line_count); 
    }}


{display}  {

    if(CG_TYPE == 4)
	print_bad_match(yytext,5);
    else 

    if(CG_TYPE == 5)
    {
	e_checkend(5, yytext);
        BEGIN(AFTER_DISPLAY);
    }
    else
    {
	push( yytext, 5, line_count);
    }}

\\begingroup/[^a-zA-Z]  {
 {
    push((unsigned char *)"\\begingroup", 1, line_count); 
 }}


\\endgroup/[^a-zA-Z]  {
 {
    g_checkend(1);
 }}


\\begin[ \t]*"{" { BEGIN(B_ENVIRONMENT); }

\\begin[ \t]*(%[^\n]*)?/\n { 
 {
    
    printf("\"%s\", line %d: {argument} missing for \\begin\n",
	   file_name, line_count) ;
    ++warn_count;
 }}

<B_ENVIRONMENT>[^\}\n]+ { 
 {
    if (strcmp( yytext, "verbatim" ) == 0 )
	{
	 input();
	 BEGIN(VERBATIM);
	}
    else
	{
    	 push(yytext, 2, line_count);

	 if (   strcmp (yytext, "sl" ) == 0
	     || strcmp (yytext, "it" ) == 0)
	   CG_ITALIC = 1;
	 else if (strcmp (yytext, "normalshape") == 0)
	   CG_ITALIC = 0;
	 else if (strcmp (yytext, "em") == 0)
	   CG_ITALIC = !CG_ITALIC;
	   
 	 input();
	 BEGIN(INITIAL);
	}
 }}

<VERBATIM>\\end[ \t]*\{verbatim\} { BEGIN(INITIAL); }

<VERBATIM>\t {
     printf("\"%s\", line %d: TAB character in verbatim environment\n",
	   file_name, line_count) ;
    ++warn_count;
 }

<VERBATIM>. { ; }

<VERBATIM>\n { ++line_count; }


<ICOR,INITIAL,GETICOR>\\verb\*?. { 
          verb_char = yytext[yyleng-1];
	  BEGIN(VERB); 
	}

<VERB>\n {
  printf("\"%s\", line %d: \\verb should not contain end of line characters\n",
	 file_name, line_count) ;
  ++line_count;
} 

<VERB>. {
  if ( *yytext == verb_char )
    BEGIN(INITIAL); 
} 


\\end[ \t]*"{" { BEGIN(E_ENVIRONMENT); }

\\end[ \t]*(%[^\n]*)?/\n { 
 {
    printf("\"%s\", line %d: {argument} missing for \\end\n",
	   file_name, line_count) ;
    ++warn_count;
 }}


<E_ENVIRONMENT>[^\}\n]+ { 
 {
    e_checkend(2, yytext);
    input();
    
    BEGIN(INITIAL);
 }}


<ICOR,INITIAL,GETICOR>{ws}({letter}".")*{letter}*{l_letter}"."/{non_par_ws}+{l_letter}    { 
 {
    linecount();
    printf( "\"%s\", line %d: missing `\\ ' after \"%s\"\n", 
	   file_name, line_count, ++yytext); 
    ++warn_count ;
    BEGIN(INITIAL);
 }}

<ICOR,INITIAL,GETICOR>({l_letter}".")*{letter}*{l_letter}"."/{non_par_ws}+{l_letter}    { 
 {
    printf( "\"%s\", line %d: missing `\\ ' after \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count ; 
    BEGIN(INITIAL);
 }}

<ICOR,INITIAL,GETICOR>{non_abbrev}/{non_par_ws}{u_letter}   { 
 {
   linecount();
   printf("\"%s\", line %d: missing `\\@' before `.' in \"%s\"\n", 
	  file_name, line_count, yytext); 
   ++warn_count ; 
   BEGIN(INITIAL);
 }}

<ICOR,INITIAL,GETICOR>({hard_space}{space}|{space}{hard_space})  { 

    printf("\"%s\", line %d: double space at \"%s\"\n",
	   file_name, line_count, yytext); 
    ++warn_count;
	linecount();
    BEGIN(INITIAL);
  }

{c_bin}{ws}?(\\(\.|\,|\;|\:))*{ws}?\\ldots{ws}?(\\(\.|\,|\;|\:))*{ws}?{c_bin} {
	printf("\"%s\", line %d: \\ldots should be \\cdots in \"%s\"\n",
	   file_name, line_count, yytext); 
	++warn_count;
	linecount();
  }

<ICOR,INITIAL,GETICOR>[^\\]{l_bin}{ws}?(\\(\.|\,|\;|\:))*{ws}?\\cdots{ws}?(\\(\.|\,|\;|\:))*{ws}?[^\\]{l_bin} {
	printf("\"%s\", line %d: \\cdots should be \\ldots in \"%s\"\n",
	   file_name, line_count, yytext); 
	++warn_count;
	linecount();
    BEGIN(INITIAL);
  }

{c_bin}{ws}?(\\(\.|\,|\;|\:))*{ws}?"."+{ws}?(\\(\.|\,|\;|\:))*{ws}?{c_bin} {
	printf("\"%s\", line %d: Dots should be \\cdots in \"%s\"\n",
	   file_name, line_count, yytext); 
	++warn_count;
	linecount();
  }

<ICOR,INITIAL,GETICOR>[^\\]{l_bin}{ws}?(\\(\.|\,|\;|\:))*{ws}?"."+{ws}?(\\(\.|\,|\;|\:))*{ws}?[^\\]{l_bin} {
	printf("\"%s\", line %d: Dots should be \\ldots in \"%s\"\n",
	   file_name, line_count, yytext); 
	++warn_count;
	linecount();
    BEGIN(INITIAL);
  }


<ICOR,INITIAL,GETICOR>\.\.\. { 
    printf("\"%s\", line %d: Dots should be ellipsis \"%s\"\n",
	   file_name, line_count, yytext); 
    ++warn_count;
    BEGIN(INITIAL);
  }

<ICOR,INITIAL,GETICOR>\\label\{[^#$%^&*+={\~"<>\n\t }]*[#$%^&*+={\~"<>\n\t ][^%}]*\} {
    linecount();
    printf("\"%s\", line %d: bad character in label \"%s\", see C.10.2\n",
           file_name, line_count, yytext);
  }
    
<ICOR,INITIAL,GETICOR>{par}"\\"ref/[^A-Za-z]  {
    linecount();
    BEGIN(INITIAL);
  }

<ICOR,INITIAL,GETICOR>{ws}"\\"ref/[^A-Za-z]  {
    linecount();
    printf("\"%s\", line %d: perhaps you should insert a `~' before \"%s\"\n",
	   file_name, line_count, ++yytext); 
    BEGIN(INITIAL);
  }

<ICOR,INITIAL,GETICOR>{ws}"\\"footnote/[^A-Za-z]  {
    linecount();
    printf("\"%s\", line %d: whitespace before footnote in \"%s\"\n",
	   file_name, line_count, ++yytext); 
    BEGIN(INITIAL);
  }

 
{primitive}/[^a-zA-Z] {
 {
    printf("\"%s\", line %d: Don't use \"%s\" in LaTeX documents\n", 
	   file_name, line_count, yytext); 
    ++warn_count ; 
 }}    

\\left{ws}*\\?. { linecount() ;}
\\right{ws}*\\?. {	linecount(); }

<ICOR,INITIAL,GETICOR>[^\{]\\{font_spec}/[ \t]*"{" { 
 {
   linecount();
    printf("\"%s\", line %d: Fontspecifiers don't take arguments. \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count; 
  /*    (void) input(); */
    BEGIN(INITIAL);
 }}

\\([a-zA-Z\@]+\@[a-zA-Z\@]*|[a-zA-Z\@]*\@[a-zA-Z\@]+) { 
 {
    printf("\"%s\", line %d: Do not use @ in LaTeX macro names. \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count; 
 }}

<ICOR,INITIAL,GETICOR>{ws}"'"+{letter}+ { 
 {
   linecount();
    printf("\"%s\", line %d: Use ` to begin quotation, not ' \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count; 
    BEGIN(INITIAL);
 }}

<ICOR,INITIAL,GETICOR>{letter}+"`" { 
 {
    printf("\"%s\", line %d: Use ' to end quotation, not ` \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count; 
    BEGIN(INITIAL);
 }}


<ICOR,INITIAL,GETICOR>{ws}+{punct} { 
 {
    printf("\"%s\", line %d: Whitespace before punctation mark in \"%s\"\n", 
	   file_name, line_count, yytext); 
    ++warn_count; 
	linecount();
    BEGIN(INITIAL);
 }}

"%"  { BEGIN(COMMENT); }

<COMMENT>\n	{ BEGIN(INITIAL); ++line_count; }

<COMMENT>.	{ ; }


\\(input|include)([ \t]|"{")	{ BEGIN(INCLUDE); }

<INCLUDE>[^\}\n]+	{
 {
	if ( strstr(yytext,".sty") == NULL )
	{
	  printf("** %s:\n", yytext);
	  input_file(yytext);
	}
	else
	{
		printf("\"%s\", line %d: Style file `%s\' omitted.\n",
			file_name,
			line_count,
			yytext);
		input();
	}
	BEGIN(INITIAL);
 }}

\\endinput/[^A-Za-z] |
<<EOF>> { 
	  if (--istackp < 0)
		  yyterminate(); 

	  else
		{ 
		  fclose(yyin);
	  	  f_checkend(file_name);
		  yy_switch_to_buffer(istack[istackp].stream);
		  free(file_name);
		  line_count = istack[istackp].linenum;
		  file_name = istack[istackp].name;
		  input();
		  BEGIN(INITIAL);
		}    	
	 
	}


. { ; }
%%
int main( argc, argv )
int argc;
char *argv[];
{
    /* allocate initial stacks */
    gstack = (tex_group *)malloc(gstack_size * sizeof(tex_group));
    istack = (input_ *)malloc(istack_size * sizeof(input_));
    if ( gstack == NULL || istack == NULL ) {
	fprintf(stderr, "%s: not enough memory for stacks\n", PROGNAME);
	exit(3);
    }
	
    if(argc > 1)
    {
        if ( (file_name = (char*) malloc(strlen(argv[1]) + 5)) == NULL ) {
		fprintf(stderr, "%s: out of memory\n", PROGNAME);
		exit(3);
	}
	
	strcpy(file_name, argv[1]);
	strcat(file_name, ".tex" );
	
	if ((yyin = fopen( file_name, "r")) != NULL )
	{
	    push(file_name, 3, 1);
	    yylex();
	    f_checkend(file_name);
	}
	else {   
                 file_name[strlen(file_name) - 4] = '\0';
		 if ((yyin = fopen( file_name, "r")) != NULL )
		 {
		     push(file_name, 3, 1);
		     yylex();
		     f_checkend(file_name);
		 }
		 else
		     fprintf(stderr,
			     "%s: Could not open : %s\n",PROGNAME, argv[1]);
	     }
    }
    else
    {
	printf("\n* %s *\n\n",PROGNAME);
	printf("\t...a consistency checker for LaTeX documents.\n");
	printf("$Id: lacheck.lex,v 1.26 1998/03/07 07:46:45 abraham Exp $\n\n");

	printf("Usage:\n\tlacheck filename[.tex] <return>\n\n\n");

	printf("\tFrom within Emacs:\n\n");
	printf("\tM-x compile RET lacheck filename[.tex] RET\n\n");
	printf("\tUse C-x ` to step through the messages.\n\n");
	printf("\n\tThe found context is displayed in \"double quotes\"\n\n");
	printf("Remark:\n\tAll messages are only warnings!\n\n");
	printf("\tYour document may be right even though LaCheck says ");
	printf("something else.\n\n");
    }
    return(0);
}

#ifdef NEED_STRSTR
char *
strstr(string, substring)
    register char *string;	/* String to search. */
    char *substring;		/* Substring to try to find in string. */
{
    register char *a, *b;

    /* First scan quickly through the two strings looking for a
     * single-character match.  When it's found, then compare the
     * rest of the substring.
     */

    b = substring;
    if (*b == 0) {
	return string;
    }
    for ( ; *string != 0; string += 1) {
	if (*string != *b) {
	    continue;
	}
	a = string;
	while (1) {
	    if (*b == 0) {
		return string;
	    }
	    if (*a++ != *b++) {
		break;
	    }
	}
	b = substring;
    }
    return (char *) 0;
}
#endif /* NEED_STRSTR */

void push(p_name, p_type, p_line)
unsigned char *p_name;
int p_type;
int p_line;
{
    if ( gstackp == gstack_size ) {	/* extend stack */
	gstack_size *= 2;
	gstack = (tex_group *)realloc(gstack, gstack_size * sizeof(tex_group));
	if ( gstack == NULL ) {
		fprintf(stderr, "%s: stack out of memory", PROGNAME);
	exit(3);
    }
    }
    
    if ( (gstack[gstackp].s_name =
		(unsigned char *)malloc(strlen((char *)p_name) + 1)) == NULL ||
         (gstack[gstackp].s_file =
		(char *)malloc(strlen(file_name) + 1)) == NULL ) {
	fprintf(stderr, "%s: out of memory\n", PROGNAME);
	exit(3);
    }

    strcpy((char *)gstack[gstackp].s_name,(char *)p_name);
    gstack[gstackp].s_type = p_type;
    gstack[gstackp].s_line = p_line;	
    gstack[gstackp].italic = (  (p_type == 4 || p_type == 5)
			      ? 1
			      : (  gstackp
				 ? gstack[gstackp - 1].italic
				 : 0));
    strcpy(gstack[gstackp].s_file,file_name);
    ++gstackp;	

}

void input_file(file_nam)
char *file_nam;
{
    char *tmp_file_name;
    FILE *tmp_yyin;

    if ( (tmp_file_name = (char*) malloc(strlen(file_nam) + 5)) == NULL ) {
	fprintf(stderr, "%s: out of memory\n", PROGNAME);
	exit(3);
    }
    strcpy(tmp_file_name,file_nam);

    if (istackp == istack_size) {	/* extend stack */
	istack_size *= 2;
	istack = (input_ *)realloc(istack, istack_size * sizeof(input_));
	if ( istack == NULL ) {
		fprintf(stderr, "%s: \\input stack out of memory\n", PROGNAME);
	exit(3);
        } 
    } 
    	
    istack[istackp].stream = YY_CURRENT_BUFFER;
    istack[istackp].linenum = line_count;
    istack[istackp].name = file_name;
    ++istackp;    

    (void) strcat(tmp_file_name, ".tex");
    if ((tmp_yyin = fopen( tmp_file_name, "r")) != NULL )
	{
	  yyin = tmp_yyin;
	  yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
	  file_name = tmp_file_name;
	  push(file_name, 3, 1);
          line_count = 1;
	}
    else {
          tmp_file_name[strlen(tmp_file_name) - 4] = '\0';
	  if ((tmp_yyin = fopen( tmp_file_name , "r")) != NULL )
	    {
		yyin = tmp_yyin;
	   	yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
		file_name = tmp_file_name;
		push(file_name, 3, 1);
   	        line_count = 1;
	    }
          else
	  {
	       --istackp;
	       free(tmp_file_name);
	       printf("\"%s\", line %d: Could not open \"%s\"\n", 
			file_name,
			line_count,
			file_nam);
	       input();
	  }
	 }
}

void pop()
{
    if ( gstackp == 0 )
    {
       	fprintf(stderr, "%s: Stack underflow\n", PROGNAME);
	exit(4);
    }
    --gstackp;

    free(gstack[gstackp].s_name);
    free(gstack[gstackp].s_file);
}

char *bg_command(name)
char *name;
{
    
    switch (CG_TYPE) {
	
    case 2:
	(void) strcpy( returnval, "\\begin{" );
	(void) strcat( returnval, (char *) name);
	(void) strcat( returnval, "}" );
	break;
	
    case 3:
	(void) strcpy( returnval, "beginning of file " );
	(void) strcat( returnval, (char *) name);
	break;
    
    case 4:
	(void) strcpy( returnval, "math begin " );
	(void) strcat( returnval, (char *) name);
	break;
    
    case 5:
	(void) strcpy( returnval, "display math begin " );
	(void) strcat( returnval, (char *) name);
	break;
    
    default:
    	(void) strcpy( returnval, name );
	
    }
    
    return ((char *)returnval);
}

char *eg_command(name,type)
int type;
char *name;
{
    
    switch (type) {
	
    case 2:
	(void) strcpy( returnval, "\\end{" );
	(void) strcat( returnval, (char *) name);
	(void) strcat( returnval, "}" );
	break;
	
    case 3:
	(void) strcpy( returnval, "end of file " );
	(void) strcat( returnval, (char *) name);
	break;
    
    case 4:
	(void) strcpy( returnval, "math end " );
	(void) strcat( returnval, (char *) name);
	break;
    
    case 5:
	(void) strcpy( returnval, "display math end " );
	(void) strcat( returnval, (char *) name);
	break;
    
    default:
    	(void) strcpy( returnval, name );
	break;
    }
    
    return ((char *)returnval);
}


void g_checkend(n)
int n;
{
    if ( check_top_level_end(yytext,n) == 1 ) {
       if (  CG_TYPE != n  )
	 print_bad_match(yytext,n);
       else
	pop();
    }
}

void e_checkend(n, name)
int n;
char *name;
{
   if ( check_top_level_end(name,n) == 1 )
    {
     if (  CG_TYPE != n  ||  strcmp( CG_NAME, name ) != 0 )
    	print_bad_match(name,n);

     pop();

    }
}

void f_checkend(name)
char *name;
{
    if ( check_top_level_end(name,3) == 1 )
     {
       if (  CG_TYPE != 3  ||  strcmp( CG_NAME, name ) != 0 )

    	while( CG_TYPE != 3  )
	{
	  print_bad_match(name,3);
          pop();
          if (gstackp <= 0) return;
        }

         pop();  
     }
}

void print_bad_match(end_command,type)
char *end_command;	      
int type;
{
	  printf("\"%s\", line %d: <- unmatched \"%s\"\n",
	         file_name, 
		 line_count, 
		 eg_command( end_command , type) ) ;

	  if (gstackp > 0) {
		 printf("\"%s\", line %d: -> unmatched \"%s\"\n",
		 	 CG_FILE, 
		 	 CG_LINE, 
		 	 bg_command( CG_NAME ) ) ;
		 warn_count += 2;
	  }
}

int check_top_level_end(end_command,type)
char *end_command;	      
int type;
{
    if ( gstackp == 0 )
	{
	 printf("\"%s\", line %d: \"%s\" found at top level\n",
	       file_name, 
	       line_count, 
	       eg_command( end_command, type )) ;
	 ++warn_count;
         return(0);
	}
    else
    	return(1);
}

void linecount()
{
  int i;
  for (i = 0; i < yyleng; i++)
    if(yytext[i] == '\n')
      line_count++;
}

