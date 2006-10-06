/*
 *   $Id$
 *
 *   Copyright (c) 2003, Darren Hiebert
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
 *
 *   This module contains functions for generating tags for JavaScript language
 *   files.
 *
 *   This is a good reference for different forms of the function statement:
 *       http://www.permadi.com/tutorial/jsFunc/
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#include <setjmp.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *   MACROS
 */
#define isType(token,t)		(boolean) ((token)->type == (t))
#define isKeyword(token,k)	(boolean) ((token)->keyword == (k))

/*
 *   DATA DECLARATIONS
 */

typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*
 * Tracks class names already created
 */
static stringList *ClassNames;

/*  Used to specify type of keyword.
*/
typedef enum eKeywordId {
    KEYWORD_NONE = -1,
    KEYWORD_function,
    KEYWORD_prototype,
    KEYWORD_var,
    KEYWORD_this,
    KEYWORD_for,
    KEYWORD_while,
    KEYWORD_do,
    KEYWORD_if
} keywordId;

/*  Used to determine whether keyword is valid for the token language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
    const char *name;
    keywordId id;
} keywordDesc;

typedef enum eTokenType {
    TOKEN_UNDEFINED,
    TOKEN_CHARACTER,
    TOKEN_CLOSE_PAREN,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COMMA,
    TOKEN_KEYWORD,
    TOKEN_OPEN_PAREN,
    TOKEN_OPERATOR,
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_PERIOD,
    TOKEN_OPEN_CURLY,
    TOKEN_CLOSE_CURLY,
    TOKEN_EQUAL_SIGN,
    TOKEN_FORWARD_SLASH
} tokenType;

typedef struct sTokenInfo {
    tokenType	type;
    keywordId	keyword;
    vString *	string;
    vString *	scope;
    unsigned long lineNumber;
    fpos_t filePosition;
} tokenInfo;

/*
 *  DATA DEFINITIONS
 */

static langType Lang_js;

static jmp_buf Exception;

typedef enum {
    JSTAG_FUNCTION,
    JSTAG_CLASS,
    JSTAG_METHOD,
    JSTAG_VARIABLE,
    JSTAG_COUNT
} jsKind;

static kindOption JsKinds [] = {
    { TRUE,  'f', "function",     "functions"	           },
    { TRUE,  'c', "class",        "classes"	           },
    { TRUE,  'm', "method",       "methods"	           },
    { TRUE,  'v', "variable",     "global variables"	   }
};

static const keywordDesc JsKeywordTable [] = {
    /* keyword		keyword ID */
    { "function",	KEYWORD_function		},
    { "prototype",	KEYWORD_prototype		},
    { "var",		KEYWORD_var		        },
    { "this",		KEYWORD_this		    },
    { "for",		KEYWORD_for		        },
    { "while",		KEYWORD_while		    },
    { "do",			KEYWORD_do		        },
    { "if",			KEYWORD_if		        }
};

/*
 *   DEBUG function
 */

static void dispToken (tokenInfo *const token, char * location)
{
#ifdef DEBUG
    if ( isKeyword(token, KEYWORD_NONE) )
    {
        if ( isType(token, TOKEN_IDENTIFIER) || isType(token, TOKEN_STRING)  )
        {
            printf( "\n%s: token string  value:%s  scope:%s  line:%lu  pos:%ld \n"
                    , location
                    , vStringValue(token->string)
                    , vStringValue(token->scope)
                    , token->lineNumber
                    , token->filePosition
                  );
        } 
		else 
		{
            printf( "\n%s: token  type:%d  scope:%s  line:%lu  pos:%lu\n"
                    , location
                    , token->type
                    , vStringValue(token->scope)
                    , token->lineNumber
                    , token->filePosition
                  );
        }
    } 
	else 
	{
        printf( "\n%s: keyword:%s k:%d  s:%s  l:%lu  p:%ld\n"
                , location
                , vStringValue(token->string)
                , token->keyword
                , vStringValue(token->scope)
                , token->lineNumber
                , token->filePosition
              );
    }
#endif
}

/*
 *   FUNCTION DEFINITIONS
 */

static void makeConstTag (tokenInfo *const token, const jsKind kind);
static void readToken (tokenInfo *const token);
static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent);
static void parseFunction (tokenInfo *const token);
static void parseLine (tokenInfo *const token, boolean is_inside_class);
static void parseStatement (tokenInfo *const token, boolean is_inside_class);

static boolean isIdentChar1 (const int c)
{
    // Other databases are less restrictive on the first character of
    // an identifier.
    // isIdentChar1 is used to identify the first character of an 
    // identifier, so we are removing some restrictions.
    return (boolean)
        (isalpha (c) || c == '@' || c == '_' );
}

static boolean isIdentChar (const int c)
{
    return (boolean)
        (isalpha (c) || isdigit (c) || c == '$' || 
         c == '@' || c == '_' || c == '#');
}

static boolean isCmdTerm (tokenInfo *const token)
{
#ifdef DEBUGed
    printf( "\n isCmdTerm: token same  tt:%d  tk:%d\n"
            , token->type
            , token->keyword
          );
#endif

    /*
     * Based on the various customer sites I have been at
     * the most common command delimiters are
     *     ;
     *     close curly brace
     * This routine will check for any of these, more
     * can easily be added by modifying readToken and
     * either adding the character to:
     *     enum eTokenType
     *     enum eTokenType
     */
    return ( isType (token, TOKEN_SEMICOLON) || 
            isType (token, TOKEN_CLOSE_CURLY) );
}

static void skipArgumentList (tokenInfo *const token)
{
    int nest_level = 0;

    /*
     * Other databases can have arguments with fully declared
     * datatypes:
     *   (  name varchar(30), text binary(10)  )
     * So we must check for nested open and closing parantheses
     */

    if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
    {
        nest_level++;
        //findToken (token, TOKEN_CLOSE_PAREN);
        while (! (isType (token, TOKEN_CLOSE_PAREN) && (nest_level == 0)))
        {
            readToken (token);
            if (isType (token, TOKEN_OPEN_PAREN))
            {
                nest_level++;
            }
            if (isType (token, TOKEN_CLOSE_PAREN))
            {
                if (nest_level > 0)
                {
                    nest_level--;
                }
            }
        } //while
        readToken (token);
    }
}

static void buildJsKeywordHash (void)
{
    const size_t count = sizeof (JsKeywordTable) /
        sizeof (JsKeywordTable [0]);
    size_t i;
    for (i = 0  ;  i < count  ;  ++i)
    {
        const keywordDesc* const p = &JsKeywordTable [i];
        addKeyword (p->name, Lang_js, (int) p->id);
    }
}

static tokenInfo *newToken (void)
{
    tokenInfo *const token = xMalloc (1, tokenInfo);

    token->type         = TOKEN_UNDEFINED;
    token->keyword      = KEYWORD_NONE;
    token->string       = vStringNew ();
    token->scope        = vStringNew ();

    return token;
}

static void deleteToken (tokenInfo *const token)
{
    vStringDelete (token->string);
    vStringDelete (token->scope);
    eFree (token);
}

/*
 *   Tag generation functions
 */

static void makeJsTag (tokenInfo *const token, const jsKind kind)
{
    vString *	fulltag;

    if (JsKinds [kind].enabled)
    {
        /*
         * If a scope has been added to the token, change the token
         * string to include the scope when making the tag.
         */
        if ( vStringLength(token->scope) > 0 )
        {
            fulltag = vStringNew ();
            vStringCopy(fulltag, token->scope);
            vStringCatS (fulltag, ".");
            vStringCatS (fulltag, vStringValue(token->string));
            vStringTerminate(fulltag);
            vStringCopy(token->string, fulltag);
            vStringDelete (fulltag);
        }
        makeConstTag (token, kind);
    }
}

static void makeConstTag (tokenInfo *const token, const jsKind kind)
{
    if (JsKinds [kind].enabled)
    {
        const char *const name = vStringValue (token->string);
        tagEntryInfo e;
        initTagEntry (&e, name);

        e.lineNumber   = token->lineNumber;
        e.filePosition = token->filePosition;
        e.kindName     = JsKinds [kind].name;
        e.kind         = JsKinds [kind].letter;

        makeTagEntry (&e);
    }
}

static void makeClassTag (tokenInfo *const token)
{
    if ( ! stringListHas(ClassNames, vStringValue (token->string)) )
    {
        stringListAdd (ClassNames, vStringNewCopy (token->string));
        makeJsTag (token, JSTAG_CLASS);
    }
}

/*
 *   Parsing functions
 */

static int skipToCharacter (const int c)
{
    int d;
    do
    {
        d = fileGetc ();
    } while (d != EOF  &&  d != c);
    return d;
}

static void parseString (vString *const string, const int delimiter)
{
    boolean end = FALSE;
    int c;
    while (! end)
    {
        c = fileGetc ();
        //        printf( "\nps: %c\n", c  );
        if (c == EOF)
            end = TRUE;
        else if (c == delimiter)
            end = TRUE;
        else
            vStringPut (string, c);
    }
    vStringTerminate (string);
}

/*  Read a C identifier beginning with "firstChar" and places it into
 *  "name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
    int c = firstChar;
    Assert (isIdentChar1 (c));
    do
    {
        vStringPut (string, c);
        c = fileGetc ();
    } while (isIdentChar (c));
    vStringTerminate (string);
    if (!isspace (c))
        fileUngetc (c);		/* unget non-identifier character */
}

static keywordId analyzeToken (vString *const name)
{
    static vString *keyword = NULL;
    if (keyword == NULL)
        keyword = vStringNew ();
    vStringCopyToLower (keyword, name);
    return (keywordId) lookupKeyword (vStringValue (keyword), Lang_js);
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
    dest->lineNumber = src->lineNumber;
    dest->filePosition = src->filePosition;
    dest->type = src->type;
    dest->keyword = src->keyword;
    vStringCopy(dest->string, src->string);
    vStringCopy(dest->scope, src->scope);
}

static void readToken (tokenInfo *const token)
{
    int c;

    token->type         = TOKEN_UNDEFINED;
    token->keyword      = KEYWORD_NONE;
    vStringClear (token->string);

getNextChar:
    do
    {
        c = fileGetc ();
        //        printf( "\nrtc: %c\n", c );
        /* 
         * Added " to the list of ignores, not sure what this 
         * might break but it gets by this issue:
         *    create table "t1" (...)
         */
    }
    while (c == '\t'  ||  c == ' ' ||  c == '\n');

    switch (c)
    {
        case EOF: longjmp (Exception, (int)ExceptionEOF);	break;
        case '(': token->type = TOKEN_OPEN_PAREN;		break;
        case ')': token->type = TOKEN_CLOSE_PAREN;		break;
        case ';': token->type = TOKEN_SEMICOLON;		break;
        case ',': token->type = TOKEN_COMMA;    		break;
        case '.': token->type = TOKEN_PERIOD;	        	break;
        case ':': token->type = TOKEN_COLON;			break;
        case '{': token->type = TOKEN_OPEN_CURLY;		break;
        case '}': token->type = TOKEN_CLOSE_CURLY;		break;
        case '=': token->type = TOKEN_EQUAL_SIGN;    		break;

        case '\'':
        case '"':
                  token->type = TOKEN_STRING;
                  parseString (token->string, c);
                  token->lineNumber = getSourceLineNumber ();
                  token->filePosition = getInputFilePosition ();
                  break;

        case '/':
                  {
                      int d = fileGetc ();
                      if ( (d != '*') && 		/* is this the start of a comment? */
                              (d != '/') )  		// is a one line comment?
                      {
                          token->type = TOKEN_FORWARD_SLASH;
                          fileUngetc (d);
                      }
                      else
                      {
                          if (d == '*')
                          {
                              do
                              {
                                  skipToCharacter ('*');
                                  c = fileGetc ();
                                  if (c == '/')
                                      break;
                                  else
                                      fileUngetc (c);
                              } while (c != EOF && c != '\0');
                              goto getNextChar;
                          }
                          else if (d == '/')	// is this the start of a comment? 
                          {
                              skipToCharacter ('\n');
                              goto getNextChar;
                          }
                      }
                      break;
                  }

        default:
                  if (! isIdentChar1 (c))
                      token->type = TOKEN_UNDEFINED;
                  else
                  {
                      parseIdentifier (token->string, c);
                      token->lineNumber = getSourceLineNumber ();
                      token->filePosition = getInputFilePosition ();
                      token->keyword = analyzeToken (token->string);
                      if (isKeyword (token, KEYWORD_NONE))
                          token->type = TOKEN_IDENTIFIER;
                      else
                          token->type = TOKEN_KEYWORD;
                  }
                  break;
    }
    //dispToken(token, "rte");
}

/*
 *   Token parsing functions
 */

static void addContext (tokenInfo* const parent, const tokenInfo* const child)
{
    if (vStringLength (parent->string) > 0)
    {
        vStringCatS (parent->string, ".");
    }
    vStringCatS (parent->string, vStringValue(child->string));
    vStringTerminate(parent->string);
}

static void addToScope (tokenInfo* const token, vString* const extra)
{
    if (vStringLength (token->scope) > 0)
    {
        vStringCatS (token->scope, ".");
    }
    vStringCatS (token->scope, vStringValue(extra));
    vStringTerminate(token->scope);
}

/*
 *   Scanning functions
 */

static void findToken (tokenInfo *const token, const tokenType type)
{
    while (! isType (token, type))
    {
        readToken (token);
    }
}

static void findCmdTerm (tokenInfo *const token, const boolean check_first)
{
    /*
     * Read until we find either a semicolon or closing brace. 
     * Any nested braces will be handled within.
     */
    while (! ( isType (token, TOKEN_SEMICOLON) ||
                isType (token, TOKEN_CLOSE_CURLY) ) )
    {
        // Handle nested blocks
        if ( isType (token, TOKEN_OPEN_CURLY))
        {
            parseBlock (token, token);
        } 
		else 
		{
            readToken (token);
        }
    } 
}

static void parseSwitch (tokenInfo *const token)
{
    /*
     * switch (expression){
     * case value1:
     *     statement;
     *     break;
     * case value2:
     *     statement;
     *     break;
     * default : statement;
     * }
     */

    readToken (token);

    if (isType (token, TOKEN_OPEN_PAREN)) 
	{
        // Handle nameless functions, these will only
        // be considered methods.
        skipArgumentList(token);
    }

    if (isType (token, TOKEN_OPEN_CURLY)) 
    {
        // This will be either a function or a class.
        // We can only determine this by checking the body
        // of the function.  If we find a "this." we know
        // it is a class, otherwise it is a function.
        parseBlock (token, token);
    }

}

static void parseLoop (tokenInfo *const token)
{
    /*
     * Handles these statements
     *     for (x=0; x<3; x++)
     *         document.write("This text is repeated three times<br>");
     *     
     *     for (x=0; x<3; x++)
     *     {
     *         document.write("This text is repeated three times<br>");
     *     }
     *     
     *     while (number<5){
     *         document.write(number+"<br>");
     *         number++;
     *     }
     *     
     *     do{
     *         document.write(number+"<br>");
     *         number++;
     *     }
     *     while (number<5);
     */

    if (isKeyword (token, KEYWORD_for) || isKeyword (token, KEYWORD_while))
    {
        readToken(token);

        if (isType (token, TOKEN_OPEN_PAREN)) 
		{
            // Handle nameless functions, these will only
            // be considered methods.
            skipArgumentList(token);
        }

        if (isType (token, TOKEN_OPEN_CURLY)) 
        {
            // This will be either a function or a class.
            // We can only determine this by checking the body
            // of the function.  If we find a "this." we know
            // it is a class, otherwise it is a function.
            parseBlock (token, token);
        } 
		else 
		{
            parseLine(token, FALSE);
        }
    } 
	else if (isKeyword (token, KEYWORD_do))
    {
        readToken(token);

        if (isType (token, TOKEN_OPEN_CURLY)) 
        {
            // This will be either a function or a class.
            // We can only determine this by checking the body
            // of the function.  If we find a "this." we know
            // it is a class, otherwise it is a function.
            parseBlock (token, token);
        } 
		else 
		{
            parseLine(token, FALSE);
        }

        readToken(token);

        if (isKeyword (token, KEYWORD_while))
        {
            readToken(token);

            if (isType (token, TOKEN_OPEN_PAREN)) 
			{
                // Handle nameless functions, these will only
                // be considered methods.
                skipArgumentList(token);
            }
        }
    }
}

static void parseIf (tokenInfo *const token)
{
    /*
     * If statements have two forms
     *     if ( ... )
     *         one line;
     *
     *     if ( ... ) {
     *        multiple;
     *        statements;
     *     }
     *
     *     if ( ... ) {
     *        return elem
     *     }
	 *
	 * TODO:  Deal with statements that can optional end
	 *        without a semi-colon.  Currently this messes up
	 *        the parsing of blocks.
	 *        Need to somehow detect this has happened, and either
	 *        backup a token, or skip reading the next token if 
	 *        that is possible from all code locations.
	 *
     */

    readToken (token);

    if (isType (token, TOKEN_OPEN_PAREN)) 
	{
        // Handle nameless functions, these will only
        // be considered methods.
        skipArgumentList(token);
    }

    if (isType (token, TOKEN_OPEN_CURLY)) 
    {
        // This will be either a function or a class.
        // We can only determine this by checking the body
        // of the function.  If we find a "this." we know
        // it is a class, otherwise it is a function.
        parseBlock (token, token);
    } 
	else 
	{
        findCmdTerm (token, TRUE);
    }
}

static boolean parseBlock (tokenInfo *const token, tokenInfo *const parent)
{
    boolean is_class = FALSE;
    vString * saveScope = vStringNew ();

    /*
     * Make this routine a bit more forgiving.
     * If called on an open_curly advance it
     */
    if ( isType (token, TOKEN_OPEN_CURLY) && 
            isKeyword(token, KEYWORD_NONE) )
        readToken(token);

    if (! isType (token, TOKEN_CLOSE_CURLY))
    {
        /*
         * Read until we find the closing brace, 
         * any nested braces will be handled within
         */
        do
        {
            if (isKeyword (token, KEYWORD_this))
            {
                /*
                 * Then we are inside a class and we have found
                 * a class, not a function
                 */
                is_class = TRUE;
                vStringCopy(saveScope, token->scope);
                addToScope (token, parent->string);
                // Move past this
                readToken(token);

                // Move past a potential .
                if ( isType (token, TOKEN_PERIOD) )
                    readToken(token);

                parseLine (token, is_class);
                vStringCopy(token->scope, saveScope);
            } 
			else if (isKeyword (token, KEYWORD_var))
            {
                /*
                 * Potentially we have found an inner function.
                 * Set something to indicate the scope
                 */
                vStringCopy(saveScope, token->scope);
                addToScope (token, parent->string);
                parseLine (token, is_class);
                vStringCopy(token->scope, saveScope);
            } 
			else if (isKeyword (token, KEYWORD_function))
            {
                vStringCopy(saveScope, token->scope);
                addToScope (token, parent->string);
                parseFunction (token);
                vStringCopy(token->scope, saveScope);
            } 
			else if (isType (token, TOKEN_OPEN_CURLY))
            {
                // Handle nested blocks
                parseBlock (token, parent);
            } 
			else 
            {
                parseLine (token, is_class);
            }

            readToken(token);

        } while (! isType (token, TOKEN_CLOSE_CURLY));
    }

    vStringDelete(saveScope);

    return is_class;
}

static void parseMethods (tokenInfo *const token, tokenInfo *const class)
{
    tokenInfo *const name = newToken ();

    /*
     * This deals with these formats
     *     'validMethod' : function(a,b) {}
     */

    do
    {
        readToken (token);
        if (isType (token, TOKEN_STRING))
        {
            copyToken(name, token);

            readToken (token);
            if ( isType (token, TOKEN_COLON) )
            {
                readToken (token);
                if ( isKeyword (token, KEYWORD_function) )
                {
                    readToken (token);
                    if ( isType (token, TOKEN_OPEN_PAREN) )
                    {
                        skipArgumentList(token);
                    }

                    if (isType (token, TOKEN_OPEN_CURLY)) 
					{
                        addToScope (name, class->string);
                        makeJsTag (name, JSTAG_METHOD);
                        parseBlock (token, name);

                        /*
                         * Read to the closing curly, check next
                         * token, if comma, we must loop again
                         */
                        readToken (token);
                    }
                }
            }
        }
    } while ( isType(token, TOKEN_COMMA) );

    findCmdTerm (token, TRUE);

    deleteToken (name);
}

static void parseFunction (tokenInfo *const token)
{
    tokenInfo *const name = newToken ();
    boolean is_class = FALSE;

    /*
     * This deals with these formats
     *     function validFunctionTwo(a,b) {}
     */

    readToken (name);
    // Add scope in case this is an INNER function
    addToScope(name, token->scope);
    readToken (token);
    if (isType (token, TOKEN_PERIOD))
    {
        do
        {
            readToken (token);
            if ( isKeyword(token, KEYWORD_NONE) )
            {
                addContext (name, token);
                readToken (token);
            }
        } while (isType (token, TOKEN_PERIOD));
    }

    if ( isType (token, TOKEN_OPEN_PAREN) )
    {
        skipArgumentList(token);
    }

    if ( isType (token, TOKEN_OPEN_CURLY) )
    {
        is_class = parseBlock (token, name);
        if ( is_class ) 
        {
            makeClassTag (name);
        } 
		else 
		{
            makeJsTag (name, JSTAG_FUNCTION);
        }
    }

    findCmdTerm (token, TRUE);

    deleteToken (name);
}

static void parseLine (tokenInfo *const token, boolean is_inside_class)
{
    /*
     * Functions can be named or unnamed.
     * This deals with these formats:
     * Function
     *     validFunctionOne = function(a,b) {}
     *     testlib.validFunctionFive = function(a,b) {}
     *     var innerThree = function(a,b) {}
     *     var innerFour = (a,b) {}
     * Class
     *     testlib.extras.ValidClassOne = function(a,b) {
     *         this.a = a; 
     *     }
     * Class Methods
     *     testlib.extras.ValidClassOne.prototype = {
     *         'validMethodOne' : function(a,b) {},
     *         'validMethodTwo' : function(a,b) {}
     *     }
     *     Database.prototype.getTodaysDate = Database_getTodaysDate;
     */

    if (isType(token, TOKEN_KEYWORD))
    {
        switch (token->keyword)
        {
            case KEYWORD_for:      
            case KEYWORD_while:
            case KEYWORD_do:
                parseLoop (token); 
                break;
            case KEYWORD_if:
                parseIf (token); 
                break;
            default:               
                parseStatement (token, is_inside_class); 
                break;
        }
    } 
	else 
	{
        parseStatement (token, is_inside_class); 
    }
}

static void parseStatement (tokenInfo *const token, boolean is_inside_class)
{
    tokenInfo *const name = newToken ();
    vString * saveScope = vStringNew ();
    boolean is_class = FALSE;

    vStringClear(saveScope);
    /*
     * Functions can be named or unnamed.
     * This deals with these formats:
     * Function
     *     validFunctionOne = function(a,b) {}
     *     testlib.validFunctionFive = function(a,b) {}
     *     var innerThree = function(a,b) {}
     *     var innerFour = (a,b) {}
     * Class
     *     testlib.extras.ValidClassOne = function(a,b) {
     *         this.a = a; 
     *     }
     * Class Methods
     *     testlib.extras.ValidClassOne.prototype = {
     *         'validMethodOne' : function(a,b) {},
     *         'validMethodTwo' : function(a,b) {}
     *     }
     *     Database.prototype.getTodaysDate = Database_getTodaysDate;
     */

    if ( is_inside_class ) 
        is_class = TRUE;
    /*
     * var can preceed an inner function
     */
    if ( isKeyword(token, KEYWORD_var) )
        readToken(token);

    copyToken(name, token);

    // Potentially the name of the function
    readToken (token);
    if (isType (token, TOKEN_PERIOD))
    {
        do
        {
            readToken (token);
            if ( isKeyword(token, KEYWORD_NONE) )
            {
                if ( is_class )
                {
                    vStringCopy(saveScope, token->scope);
                    addToScope(token, name->string);
                    makeJsTag (token, JSTAG_METHOD);

                    // Find to the end of the statement
                    findCmdTerm (token, TRUE);
                    goto cleanUp;
                } 
				else 
				{
                    addContext (name, token);
                }
            } 
			else if ( isKeyword(token, KEYWORD_prototype) ) 
			{
                makeClassTag (name);
                is_class = TRUE;
            }
            readToken (token);
        } while (isType (token, TOKEN_PERIOD));
    }

    if ( isType (token, TOKEN_EQUAL_SIGN) )
    {
        readToken (token);

        if (isKeyword (token, KEYWORD_function))
        {
            readToken (token);
            if ( isType (token, TOKEN_OPEN_PAREN) )
            {
                skipArgumentList(token);
            }

            if (isType (token, TOKEN_OPEN_CURLY)) 
			{
                // This will be either a function or a class.
                // We can only determine this by checking the body
                // of the function.  If we find a "this." we know
                // it is a class, otherwise it is a function.
                if ( is_inside_class ) 
				{
                    makeJsTag (name, JSTAG_METHOD);
                    parseBlock (token, name);
                } 
				else 
				{
                    is_class = parseBlock (token, name);
                    if ( is_class ) 
                    {
                        makeClassTag (name);
                    } 
					else 
					{
                        makeJsTag (name, JSTAG_FUNCTION);
                    }
                }
            }
        } 
		else if (isType (token, TOKEN_OPEN_PAREN)) 
		{
            // Handle nameless functions, these will only
            // be considered methods.
            skipArgumentList(token);

            if (isType (token, TOKEN_OPEN_CURLY)) 
			{
                // This will be either a function or a class.
                // We can only determine this by checking the body
                // of the function.  If we find a "this." we know
                // it is a class, otherwise it is a function.
                makeJsTag (name, JSTAG_METHOD);
                parseBlock (token, name);
            }
        } 
		else if (isType (token, TOKEN_OPEN_CURLY)) 
		{
            parseMethods(token, name);
        }
    }
    findCmdTerm (token, TRUE);

cleanUp:
    vStringCopy(token->scope, saveScope);
    deleteToken (name);
    vStringDelete(saveScope);
}

static void parseGobalVar (tokenInfo *const token)
{
    tokenInfo *const name = newToken ();

    /*
     * A variable defined outside of a block is considered
     * a global variable:
     *     var g_var1 = 1;
     *     var g_var2;
     * This is not a global variable:
     *     var g_var = function;
     */

    // Potentially the name of the function
    readToken (name);
    readToken (token);

    if ( isType (token, TOKEN_EQUAL_SIGN) )
    {
        readToken (token);

        if (!isKeyword (token, KEYWORD_function))
        {
            makeJsTag (name, JSTAG_VARIABLE);
        }
    } 
	else if ( isType (token, TOKEN_SEMICOLON) ) 
    {
        makeJsTag (name, JSTAG_VARIABLE);
    }
    findCmdTerm (token, TRUE);

    deleteToken (name);
}

static void parseJsFile (tokenInfo *const token)
{
    do
    {
        readToken (token);

        if (isType(token, TOKEN_KEYWORD))
        {
            switch (token->keyword)
            {
                case KEYWORD_function:  parseFunction (token); break;
                case KEYWORD_var:       parseGobalVar (token); break;
                default:                break;
            }
        } 
		else 
		{
            parseLine (token, FALSE); 
        }
    } while (TRUE);
}

static void initialize (const langType language)
{
    Assert (sizeof (JsKinds) / sizeof (JsKinds [0]) == JSTAG_COUNT);
    Lang_js = language;
    buildJsKeywordHash ();
}

static void findJsTags (void)
{
    tokenInfo *const token = newToken ();
    exception_t exception = (exception_t) (setjmp (Exception));
    ClassNames = stringListNew ();

    while (exception == ExceptionNone)
        parseJsFile (token);

    stringListDelete (ClassNames);
    ClassNames = NULL;
    deleteToken (token);
}

//static void installJavaScriptRegex (const langType language)
//{
//    /*
//     * Deals with functions defined as
//     * function validFunctionOne(...)
//     */
//    addTagRegex (language, 
//            "^[ \t]*function[ \t]*([A-Za-z0-9_.]+)[ \t]*\\(",
//	    "\\1", "f,function,functions", NULL);
//    /*
//     * Deals with functions defined as
//     * validFunctionTwo = function(...)
//     */
//    addTagRegex (language, 
//            "([A-Za-z0-9_.]+)[ \t]*=[ \t]*function[ \t]*\\(",
//	    "\\1", "f,function,functions", NULL);
//}

/* Create parser definition stucture */
extern parserDefinition* JavaScriptParser (void)
{
    static const char *const extensions [] = { "js", NULL };
    parserDefinition *const def = parserNew ("JavaScript");
    def->extensions = extensions;
    /*
     * New definitions for parsing instead of regex
     */
    def->kinds      = JsKinds;
    def->kindCount  = KIND_COUNT (JsKinds);
    def->parser     = findJsTags;
    def->initialize = initialize;

    //def->initialize = installJavaScriptRegex;
    //def->regex      = TRUE;
    return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
