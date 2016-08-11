/*
*   $Id: ruby.c 571 2007-06-24 23:32:14Z elliotth $
*
*   Copyright (c) 2000-2001, Thaddeus Covert <sahuagin@mediaone.net>
*   Copyright (c) 2002 Matthias Veit <matthias_veit@yahoo.de>
*   Copyright (c) 2004 Elliott Hughes <enh@acm.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Ruby language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "nestlevel.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_UNDEFINED = -1, K_CLASS, K_METHOD, K_MODULE, K_SINGLETON,
} rubyKind;

/*
*   DATA DEFINITIONS
*/
static kindOption RubyKinds [] = {
	{ TRUE, 'c', "class",  "classes" },
	{ TRUE, 'f', "method", "methods" },
	{ TRUE, 'm', "module", "modules" },
	{ TRUE, 'F', "singletonMethod", "singleton methods" },
#if 0
	/* Following two kinds are reserved. */
	{ TRUE, 'd', "describe", "describes and contexts for Rspec" },
	{ TRUE, 'C', "constant", "constants" },
#endif
};

static NestingLevels* nesting = NULL;

#define SCOPE_SEPARATOR '.'

/*
*   FUNCTION DEFINITIONS
*/

static void enterUnnamedScope (void);

/*
* Returns a string describing the scope in 'nls'.
* We record the current scope as a list of entered scopes.
* Scopes corresponding to 'if' statements and the like are
* represented by empty strings. Scopes corresponding to
* modules and classes are represented by the name of the
* module or class.
*/
static vString* nestingLevelsToScope (const NestingLevels* nls)
{
	int i;
	unsigned int chunks_output = 0;
	vString* result = vStringNew ();
	for (i = 0; i < nls->n; ++i)
	{
	    NestingLevel *nl = nestingLevelsGetNth (nls, i);
	    tagEntryInfo *e = getEntryOfNestingLevel (nl);
	    if (e && strlen (e->name) > 0 && (!e->placeholder))
	    {
	        if (chunks_output++ > 0)
	            vStringPut (result, SCOPE_SEPARATOR);
	        vStringCatS (result, e->name);
	    }
	}
	return result;
}

/*
* Attempts to advance 's' past 'literal'.
* Returns TRUE if it did, FALSE (and leaves 's' where
* it was) otherwise.
*/
static boolean canMatch (const unsigned char** s, const char* literal,
                         boolean (*end_check) (int))
{
	const int literal_length = strlen (literal);
	const int s_length = strlen ((const char *)*s);

	if (s_length < literal_length)
		return FALSE;

	const unsigned char next_char = *(*s + literal_length);
	if (strncmp ((const char*) *s, literal, literal_length) != 0)
	{
	    return FALSE;
	}
	/* Additionally check that we're at the end of a token. */
	if (! end_check (next_char))
	{
	    return FALSE;
	}
	*s += literal_length;
	return TRUE;
}

static boolean isIdentChar (int c)
{
	return (isalnum (c) || c == '_');
}

static boolean notIdentChar (int c)
{
	return ! isIdentChar (c);
}

static boolean notOperatorChar (int c)
{
	return ! (c == '[' || c == ']' ||
	          c == '=' || c == '!' || c == '~' ||
	          c == '+' || c == '-' ||
	          c == '@' || c == '*' || c == '/' || c == '%' ||
	          c == '<' || c == '>' ||
	          c == '&' || c == '^' || c == '|');
}

static boolean isWhitespace (int c)
{
	return c == 0 || isspace (c);
}

static boolean canMatchKeyword (const unsigned char** s, const char* literal)
{
	return canMatch (s, literal, notIdentChar);
}

/*
* Attempts to advance 'cp' past a Ruby operator method name. Returns
* TRUE if successful (and copies the name into 'name'), FALSE otherwise.
*/
static boolean parseRubyOperator (vString* name, const unsigned char** cp)
{
	static const char* RUBY_OPERATORS[] = {
	    "[]", "[]=",
	    "**",
	    "!", "~", "+@", "-@",
	    "*", "/", "%",
	    "+", "-",
	    ">>", "<<",
	    "&",
	    "^", "|",
	    "<=", "<", ">", ">=",
	    "<=>", "==", "===", "!=", "=~", "!~",
	    "`",
	    NULL
	};
	int i;
	for (i = 0; RUBY_OPERATORS[i] != NULL; ++i)
	{
	    if (canMatch (cp, RUBY_OPERATORS[i], notOperatorChar))
	    {
	        vStringCatS (name, RUBY_OPERATORS[i]);
	        return TRUE;
	    }
	}
	return FALSE;
}

/*
* Emits a tag for the given 'name' of kind 'kind' at the current nesting.
*/
static void emitRubyTag (vString* name, rubyKind kind)
{
	tagEntryInfo tag;
	vString* scope;
	tagEntryInfo *parent;
	rubyKind parent_kind = K_UNDEFINED;
	NestingLevel *lvl;
	const char *unqualified_name;
	const char *qualified_name;
	int r;

        if (!RubyKinds[kind].enabled) {
            return;
        }

	vStringTerminate (name);
	scope = nestingLevelsToScope (nesting);
	lvl = nestingLevelsGetCurrent (nesting);
	parent = getEntryOfNestingLevel (lvl);
	if (parent)
		parent_kind =  parent->kind - RubyKinds;

	qualified_name = vStringValue (name);
	unqualified_name = strrchr (qualified_name, SCOPE_SEPARATOR);
	if (unqualified_name && unqualified_name[1])
	{
		if (unqualified_name > qualified_name)
		{
			if (vStringLength (scope) > 0)
				vStringPut (scope, SCOPE_SEPARATOR);
			vStringNCatS (scope, qualified_name,
			              unqualified_name - qualified_name);
			/* assume module parent type for a lack of a better option */
			parent_kind = K_MODULE;
		}
		unqualified_name++;
	}
	else
		unqualified_name = qualified_name;

	initTagEntry (&tag, unqualified_name, &(RubyKinds [kind]));
	if (vStringLength (scope) > 0) {
		Assert (0 <= parent_kind &&
		        (size_t) parent_kind < (ARRAY_SIZE (RubyKinds)));

		tag.extensionFields.scopeKind = &(RubyKinds [parent_kind]);
		tag.extensionFields.scopeName = vStringValue (scope);
	}
	r = makeTagEntry (&tag);

	nestingLevelsPush (nesting, r);

	vStringClear (name);
	vStringDelete (scope);
}

/* Tests whether 'ch' is a character in 'list'. */
static boolean charIsIn (char ch, const char* list)
{
	return (strchr (list, ch) != NULL);
}

/* Advances 'cp' over leading whitespace. */
static void skipWhitespace (const unsigned char** cp)
{
	while (isspace (**cp))
	{
	    ++*cp;
	}
}

/*
* Copies the characters forming an identifier from *cp into
* name, leaving *cp pointing to the character after the identifier.
*/
static rubyKind parseIdentifier (
		const unsigned char** cp, vString* name, rubyKind kind)
{
	/* Method names are slightly different to class and variable names.
	 * A method name may optionally end with a question mark, exclamation
	 * point or equals sign. These are all part of the name.
	 * A method name may also contain a period if it's a singleton method.
	 */
	boolean had_sep = FALSE;
	const char* also_ok;
	if (kind == K_METHOD)
	{
		also_ok = ".?!=";
	}
	else if (kind == K_SINGLETON)
	{
		also_ok = "?!=";
	}
	else
	{
		also_ok = "";
	}

	skipWhitespace (cp);

	/* Check for an anonymous (singleton) class such as "class << HTTP". */
	if (kind == K_CLASS && **cp == '<' && *(*cp + 1) == '<')
	{
		return K_UNDEFINED;
	}

	/* Check for operators such as "def []=(key, val)". */
	if (kind == K_METHOD || kind == K_SINGLETON)
	{
		if (parseRubyOperator (name, cp))
		{
			return kind;
		}
	}

	/* Copy the identifier into 'name'. */
	while (**cp != 0 && (**cp == ':' || isIdentChar (**cp) || charIsIn (**cp, also_ok)))
	{
		char last_char = **cp;

		if (last_char == ':')
			had_sep = TRUE;
		else
		{
			if (had_sep)
			{
				vStringPut (name, SCOPE_SEPARATOR);
				had_sep = FALSE;
			}
			vStringPut (name, last_char);
		}
		++*cp;

		if (kind == K_METHOD)
		{
			/* Recognize singleton methods. */
			if (last_char == '.')
			{
				vStringTerminate (name);
				vStringClear (name);
				return parseIdentifier (cp, name, K_SINGLETON);
			}
		}

		if (kind == K_METHOD || kind == K_SINGLETON)
		{
			/* Recognize characters which mark the end of a method name. */
			if (charIsIn (last_char, "?!="))
			{
				break;
			}
		}
	}
	return kind;
}

static void readAndEmitTag (const unsigned char** cp, rubyKind expected_kind)
{
	if (isspace (**cp))
	{
		vString *name = vStringNew ();
		rubyKind actual_kind = parseIdentifier (cp, name, expected_kind);

		if (actual_kind == K_UNDEFINED || vStringLength (name) == 0)
		{
			/*
			* What kind of tags should we create for code like this?
			*
			*    %w(self.clfloor clfloor).each do |name|
			*        module_eval <<-"end;"
			*            def #{name}(x, y=1)
			*                q, r = x.divmod(y)
			*                q = q.to_i
			*                return q, r
			*            end
			*        end;
			*    end
			*
			* Or this?
			*
			*    class << HTTP
			*
			* For now, we don't create any.
			*/
			enterUnnamedScope ();
		}
		else
		{
			emitRubyTag (name, actual_kind);
		}
		vStringDelete (name);
	}
}

static void enterUnnamedScope (void)
{
	int r = CORK_NIL;
	NestingLevel *parent = nestingLevelsGetCurrent (nesting);
	tagEntryInfo *e_parent = getEntryOfNestingLevel (parent);

	if (e_parent)
	{
		tagEntryInfo e;
		initTagEntry (&e, "", e_parent->kind);
		e.placeholder = 1;
		r = makeTagEntry (&e);
	}
	nestingLevelsPush (nesting, r);
}

static void findRubyTags (void)
{
	const unsigned char *line;
	boolean inMultiLineComment = FALSE;

	nesting = nestingLevelsNew (0);

	/* FIXME: this whole scheme is wrong, because Ruby isn't line-based.
	* You could perfectly well write:
	*
	*  def
	*  method
	*   puts("hello")
	*  end
	*
	* if you wished, and this function would fail to recognize anything.
	*/
	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char *cp = line;
		/* if we expect a separator after a while, for, or until statement
		 * separators are "do", ";" or newline */
		boolean expect_separator = FALSE;

		if (canMatch (&cp, "=begin", isWhitespace))
		{
			inMultiLineComment = TRUE;
			continue;
		}
		if (canMatch (&cp, "=end", isWhitespace))
		{
			inMultiLineComment = FALSE;
			continue;
		}
		if (inMultiLineComment)
			continue;

		skipWhitespace (&cp);

		/* Avoid mistakenly starting a scope for modifiers such as
		*
		*   return if <exp>
		*
		* FIXME: this is fooled by code such as
		*
		*   result = if <exp>
		*               <a>
		*            else
		*               <b>
		*            end
		*
		* FIXME: we're also fooled if someone does something heinous such as
		*
		*   puts("hello") \
		*       unless <exp>
		*/
		if (canMatchKeyword (&cp, "for") ||
		    canMatchKeyword (&cp, "until") ||
		    canMatchKeyword (&cp, "while"))
		{
			expect_separator = TRUE;
			enterUnnamedScope ();
		}
		else if (canMatchKeyword (&cp, "case") ||
		         canMatchKeyword (&cp, "if") ||
		         canMatchKeyword (&cp, "unless"))
		{
			enterUnnamedScope ();
		}

		/*
		* "module M", "class C" and "def m" should only be at the beginning
		* of a line.
		*/
		if (canMatchKeyword (&cp, "module"))
		{
			readAndEmitTag (&cp, K_MODULE);
		}
		else if (canMatchKeyword (&cp, "class"))
		{
			readAndEmitTag (&cp, K_CLASS);
		}
		else if (canMatchKeyword (&cp, "def"))
		{
			rubyKind kind = K_METHOD;
			NestingLevel *nl = nestingLevelsGetCurrent (nesting);
			tagEntryInfo *e  = getEntryOfNestingLevel (nl);

			/* if the def is inside an unnamed scope at the class level, assume
			 * it's from a singleton from a construct like this:
			 *
			 * class C
			 *   class << self
			 *     def singleton
			 *       ...
			 *     end
			 *   end
			 * end
			 */
			if (e && (e->kind - RubyKinds) == K_CLASS && strlen (e->name) == 0)
				kind = K_SINGLETON;
			readAndEmitTag (&cp, kind);
		}
		while (*cp != '\0')
		{
			/* FIXME: we don't cope with here documents,
			* or regular expression literals, or ... you get the idea.
			* Hopefully, the restriction above that insists on seeing
			* definitions at the starts of lines should keep us out of
			* mischief.
			*/
			if (inMultiLineComment || isspace (*cp))
			{
				++cp;
			}
			else if (*cp == '#')
			{
				/* FIXME: this is wrong, but there *probably* won't be a
				* definition after an interpolated string (where # doesn't
				* mean 'comment').
				*/
				break;
			}
			else if (canMatchKeyword (&cp, "begin"))
			{
				enterUnnamedScope ();
			}
			else if (canMatchKeyword (&cp, "do"))
			{
				if (! expect_separator)
					enterUnnamedScope ();
				else
					expect_separator = FALSE;
			}
			else if (canMatchKeyword (&cp, "end") && nesting->n > 0)
			{
				/* Leave the most recent scope. */
				nestingLevelsPop (nesting);
			}
			else if (*cp == '"')
			{
				/* Skip string literals.
				 * FIXME: should cope with escapes and interpolation.
				 */
				do {
					++cp;
				} while (*cp != 0 && *cp != '"');
				if (*cp == '"')
					cp++; /* skip the last found '"' */
			}
			else if (*cp == ';')
			{
				++cp;
				expect_separator = FALSE;
			}
			else if (*cp != '\0')
			{
				do
					++cp;
				while (isIdentChar (*cp));
			}
		}
	}
	nestingLevelsFree (nesting);
}

extern parserDefinition* RubyParser (void)
{
	static const char *const extensions [] = { "rb", "ruby", NULL };
	parserDefinition* def = parserNewFull ("Ruby", KIND_FILE_ALT);
	def->kinds      = RubyKinds;
	def->kindCount  = ARRAY_SIZE (RubyKinds);
	def->extensions = extensions;
	def->parser     = findRubyTags;
	def->useCork    = TRUE;
	return def;
}
