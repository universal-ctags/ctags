/*
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

#include <ctype.h>
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "promise.h"
#include "nestlevel.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"
#include "subparser.h"
#include "vstring.h"

#include "ruby.h"

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_UNDEFINED = -1,
	K_CLASS,
	K_METHOD,
	K_MODULE,
	K_SINGLETON,
	K_CONST,
	K_ACCESSOR,
	K_ALIAS,
	K_LIBRARY,
} rubyKind;

typedef enum {
	RUBY_LIBRARY_REQUIRED,
	RUBY_LIBRARY_REQUIRED_REL,
	RUBY_LIBRARY_LOADED,
} rubyLibraryRole;

/*
*   DATA DEFINITIONS
*/

static roleDefinition RubyLibraryRoles [] = {
	{ true, "required",  "loaded by \"require\" method" },
	{ true, "requiredRel", "loaded by \"require_relative\" method" },
	{ true, "loaded", "loaded by \"load\" method" },
};

static kindDefinition RubyKinds [] = {
	{ true, 'c', "class",  "classes" },
	{ true, 'f', "method", "methods" },
	{ true, 'm', "module", "modules" },
	{ true, 'S', "singletonMethod", "singleton methods" },
	{ true, 'C', "constant", "constants" },
	{ true, 'A', "accessor", "accessors" },
	{ true, 'a', "alias",    "aliases" },
	{ true, 'L', "library",  "libraries",
	  .referenceOnly = true, ATTACH_ROLES(RubyLibraryRoles) },
};

typedef enum {
	F_MIXIN,
} rubyField;

static fieldDefinition RubyFields[] = {
	{ .name = "mixin",
	  .description = "how the class or module is mixed in (mixin:HOW:MODULE)",
	  .enabled = true },
};

struct blockData {
	stringList *mixin;
	rubySubparser *subparser;
	int subparserCorkIndex;
};

static NestingLevels* nesting = NULL;

#define SCOPE_SEPARATOR '.'

/*
*   FUNCTION DEFINITIONS
*/

static void enterUnnamedScope (void);

/*
* Attempts to advance 's' past 'literal'.
* Returns true if it did, false (and leaves 's' where
* it was) otherwise.
*/
static bool canMatchWithEnd (const unsigned char** s, const unsigned char *end,
							 const char* literal, bool (*end_check) (int))
{
	const int literal_length = strlen (literal);
	const int s_length = end - *s;

	if (s_length < literal_length)
		return false;

	const unsigned char next_char = *(*s + literal_length);
	if (strncmp ((const char*) *s, literal, literal_length) != 0)
	{
		return false;
	}
	/* Additionally check that we're at the end of a token. */
	if (! end_check (next_char))
	{
		return false;
	}
	*s += literal_length;
	return true;
}

static bool isIdentChar (int c)
{
	return (isalnum (c) || c == '_');
}

static bool notIdentCharButColon (int c)
{
	return ! (isIdentChar (c) || c == ':');
}

static bool isOperatorChar (int c)
{
	return (c == '[' || c == ']' ||
			c == '=' || c == '!' || c == '~' ||
			c == '+' || c == '-' ||
			c == '@' || c == '*' || c == '/' || c == '%' ||
			c == '<' || c == '>' ||
			c == '&' || c == '^' || c == '|');
}

static bool notOperatorChar (int c)
{
	return ! isOperatorChar (c);
}

static bool isSigilChar (int c)
{
	return (c == '@' || c == '$');
}

static bool isWhitespace (int c)
{
	return c == 0 || isspace (c);
}

/*
 * Advance 's' while the passed predicate is true. Returns true if
 * advanced by at least one position.
 */
static bool advanceWhileFull (const unsigned char** s, bool (*predicate) (int), vString *repr)
{
	const unsigned char* original_pos = *s;

	while (**s != '\0')
	{
		if (! predicate (**s))
		{
			return *s != original_pos;
		}

		if (repr)
			vStringPut (repr, **s);
		(*s)++;
	}

	return *s != original_pos;
}

static bool advanceWhile (const unsigned char** s, bool (*predicate) (int))
{
	return advanceWhileFull (s, predicate, NULL);
}

extern bool rubyCanMatchKeyword (const unsigned char** s, const char* literal)
{
	/* Using notIdentCharButColon() here.
	 *
	 * A hash can be defined like {for: nil, foo: 0}.
	 *"for" in the above example is not a keyword.
	 */
	size_t s_length = strlen ((const char *)*s);
	return canMatchWithEnd (s, *s + s_length, literal, notIdentCharButColon);
}

extern bool canMatchKeyword (const unsigned char** s, const unsigned char *end, const char* literal)
{
	/* Using notIdentCharButColon() here.
	 *
	 * A hash can be defined like {for: nil, foo: 0}.
	 *"for" in the above example is not a keyword.
	 */
	return canMatchWithEnd (s, end, literal, notIdentCharButColon);
}

/*
 * Extends canMatch. Works similarly, but allows assignment to precede
 * the keyword, as block assignment is a common Ruby idiom.
 */
#define vStringTruncateMaybe(VS,LEN) if (VS) vStringTruncate ((VS), (LEN))

static bool canMatchKeywordWithAssignFull (const unsigned char** s, const unsigned char *end,
										   const char* literal, vString *assignee)
{
	const unsigned char* original_pos = *s;
	size_t original_len;
	if (assignee)
		original_len = vStringLength  (assignee);

	if (canMatchKeyword (s, end, literal))
	{
		return true;
	}

	advanceWhile (s, isSigilChar);

	if (! advanceWhileFull (s, isIdentChar, assignee))
	{
		*s = original_pos;
		vStringTruncateMaybe (assignee, original_len);
		return false;
	}

	advanceWhile (s, isWhitespace);

	if (! (advanceWhile (s, isOperatorChar) && *(*s - 1) == '='))
	{
		*s = original_pos;
		vStringTruncateMaybe (assignee, original_len);
		return false;
	}

	advanceWhile (s, isWhitespace);

	if (canMatchKeyword (s, end, literal))
	{
		return true;
	}

	*s = original_pos;
	vStringTruncateMaybe (assignee, original_len);
	return false;
}

static bool canMatchKeywordWithAssign (const unsigned char** s, const unsigned char *end,
									   const char* literal)
{
	return canMatchKeywordWithAssignFull (s, end, literal, NULL);
}

extern bool rubyCanMatchKeywordWithAssign (const unsigned char** s, const char* literal)
{
	size_t s_length = strlen ((const char *)*s);
	return canMatchKeywordWithAssign (s, *s + s_length, literal);
}

/*
* Attempts to advance 'cp' past a Ruby operator method name. Returns
* true if successful (and copies the name into 'name'), false otherwise.
*/
static bool parseRubyOperator (vString* name, const unsigned char** cp,
							   const unsigned char *end)
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
		if (canMatchWithEnd (cp, end, RUBY_OPERATORS[i], notOperatorChar))
		{
			vStringCatS (name, RUBY_OPERATORS[i]);
			return true;
		}
	}
	return false;
}

/*
* Emits a tag for the given 'name' of kind 'kind' at the current nesting.
*/
static int emitRubyTagFull (vString* name, rubyKind kind, bool pushLevel, bool clearName)
{
	tagEntryInfo tag;
	vString* scope;
	tagEntryInfo *parent;
	rubyKind parent_kind = K_UNDEFINED;
	NestingLevel *lvl;
	const char *unqualified_name;
	const char *qualified_name;
	int r;
	bool anonymous = false;

	if (!name)
	{
		name = anonGenerateNew ("__anon", kind == K_UNDEFINED? K_CLASS: kind);
		anonymous = true;
	}

	if (!RubyKinds[kind].enabled) {
		return CORK_NIL;
	}

	scope = nestingLevelsToScopeNew (nesting, SCOPE_SEPARATOR);
	lvl = nestingLevelsGetCurrent (nesting);
	parent = getEntryOfNestingLevel (lvl);
	if (parent)
		parent_kind =  parent->kindIndex;

	qualified_name = vStringValue (name);
	unqualified_name = strrchr (qualified_name, SCOPE_SEPARATOR);
	if (unqualified_name && unqualified_name[1])
	{
		if (unqualified_name > qualified_name)
		{
			vStringPutUnlessEmpty (scope, SCOPE_SEPARATOR);
			vStringNCatS (scope, qualified_name,
						  unqualified_name - qualified_name);
			/* assume module parent type for a lack of a better option */
			parent_kind = K_MODULE;
		}
		unqualified_name++;
	}
	else
		unqualified_name = qualified_name;

	initTagEntry (&tag, unqualified_name, kind);

	/* Don't fill the scope field for a tag entry representing
	 * a global variable. */
	if (unqualified_name[0] != '$'
		&& vStringLength (scope) > 0) {
		Assert (0 <= parent_kind &&
				(size_t) parent_kind < (ARRAY_SIZE (RubyKinds)));

		tag.extensionFields.scopeKindIndex = parent_kind;
		tag.extensionFields.scopeName = vStringValue (scope);
	}

	if (anonymous)
		markTagExtraBit (&tag, XTAG_ANONYMOUS);

	r = makeTagEntry (&tag);

	if (pushLevel)
		nestingLevelsPush (nesting, r);

	if (clearName)
		vStringClear (name);

	if (anonymous)
		vStringDelete (name);

	vStringDelete (scope);

	return r;
}

static int emitRubyTag (vString* name, rubyKind kind)
{
	return emitRubyTagFull (name, kind, kind != K_CONST, true);
}

/* Tests whether 'ch' is a character in 'list'. */
static bool charIsIn (char ch, const char* list)
{
	return (strchr (list, ch) != NULL);
}

/* Advances 'cp' over leading whitespace. */
#define skipWhitespace rubySkipWhitespace
extern bool rubySkipWhitespace (const unsigned char** cp)
{
	bool r = false;
	while (isspace (**cp))
	{
		++*cp;
		r = true;
	}
	return r;
}

static void parseString (const unsigned char** cp, unsigned char boundary, vString* vstr)
{
	while (**cp != 0 && **cp != boundary)
	{
		if (vstr)
			vStringPut (vstr, **cp);
		++*cp;
	}

	/* skip the last found '"' */
	if (**cp == boundary)
		++*cp;
}

extern bool rubyParseString (const unsigned char** cp, unsigned char boundary, vString* vstr)
{
	const unsigned char *p = *cp;
	parseString (cp, boundary, vstr);
	return (p != *cp);
}

/* If the current scope is A.B, and the name is B.C, B is overlapped.
 * In that case, the function returns true. Else false.
 */
static bool doesNameOverlapCurrentScope (vString *name)
{
	bool r = false;
	vString *scope = nestingLevelsToScopeNew (nesting, SCOPE_SEPARATOR);

	const char *scope_cstr = vStringValue(scope);
	const char *scope_last = strrchr (scope_cstr, SCOPE_SEPARATOR);
	size_t scope_last_slen;

	if (scope_last)
	{
		scope_last++;
		scope_last_slen = strlen (scope_last);
	}
	else
	{
		scope_last = scope_cstr;
		scope_last_slen = vStringLength (scope);
	}

	if (strncmp (vStringValue (name), scope_last, scope_last_slen) == 0
		&& *(vStringValue (name) + scope_last_slen) == '.')
		r = true;

	vStringDelete (scope);

	return r;
}

/*
* Copies the characters forming an identifier from *cp into
* name, leaving *cp pointing to the character after the identifier.
*/
static rubyKind parseIdentifier (
	const unsigned char** cp, const unsigned char *end,
	vString* name, rubyKind kind)
{
	/* Method names are slightly different to class and variable names.
	 * A method name may optionally end with a question mark, exclamation
	 * point or equals sign. These are all part of the name.
	 * A method name may also contain a period if it's a singleton method.
	 */
	bool had_sep = false;
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
		if (parseRubyOperator (name, cp, end))
		{
			return kind;
		}
	}

	/* Copy the identifier into 'name'. */
	if (**cp == ':')
	{
		if (*((*cp) + 1) == '"' || *((*cp) + 1) == '\'')
		{
			/* The symbol is defined with string literal like:
			   ----
			   :"[]"
			   :"[]="
			   ----
			*/
			unsigned char b = *(++*cp);
			++*cp;
			parseString (cp, b, name);
			return kind;
		}
		/* May be symbol like :name. Skip the first character. */
		++*cp;
	}

	while (**cp != 0 && (**cp == ':' || isIdentChar (**cp) || charIsIn (**cp, also_ok)))
	{
		char last_char = **cp;

		if (last_char == ':')
			had_sep = true;
		else
		{
			if (had_sep)
			{
				vStringPut (name, SCOPE_SEPARATOR);
				had_sep = false;
			}
			vStringPut (name, last_char);
		}
		++*cp;

		if (kind == K_METHOD)
		{
			/* Recognize singleton methods. */
			if (last_char == '.')
			{
				if (strcmp (vStringValue (name), "self.") == 0
					|| doesNameOverlapCurrentScope (name))
					vStringClear (name);
				return parseIdentifier (cp, end, name, K_SINGLETON);
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

extern bool rubyParseMethodName (const unsigned char **cp, vString* vstr)
{
	size_t cp_length = strlen ((const char *)*cp);
	return (parseIdentifier (cp, *cp + cp_length, vstr, K_METHOD) == K_METHOD);
}

extern bool rubyParseModuleName (const unsigned char **cp, vString* vstr)
{
	size_t cp_length = strlen ((const char *)*cp);
	return (parseIdentifier (cp, *cp + cp_length, vstr, K_MODULE) == K_MODULE);
}

static void parseSignature (const unsigned char** cp, vString* vstr)
{
	int depth = 1;

	while (1)
	{
		/* FIXME:
		 * - handle string literals including ( or ), and
		 * - skip comments.
		 */
		while (! (depth == 0 || **cp == '\0'))
		{
			if (**cp == '(' || **cp == ')')
			{
				depth += (**cp == '(')? 1: -1;
				vStringPut (vstr, **cp);
			}
			else if (**cp == '#')
			{
				++*cp;
				while (**cp != '\0')
					++*cp;
				break;
			}
			else if (**cp == '\'' || **cp == '"')
			{
				unsigned char b = **cp;
				vStringPut (vstr, b);
				++*cp;
				parseString (cp, b, vstr);
				vStringPut (vstr, b);
				continue;
			}
			else if (isspace ((unsigned char) vStringLast (vstr)))
			{
				if (! (isspace (**cp)))
				{
					if (**cp == ',')
						vStringChop (vstr);
					vStringPut (vstr, **cp);
				}
			}
			else
				vStringPut (vstr, **cp);
			++*cp;
		}
		if (depth == 0)
			return;

		const unsigned char *line = readLineFromInputFile ();
		if (line == NULL)
			return;
		else
			*cp = line;
	}
}

static int readAndEmitTagFull (const unsigned char** cp, const unsigned char *end,
							   rubyKind expected_kind,
							   bool pushLevel, bool clearName)
{
	int r = CORK_NIL;
	if (isspace (**cp))
	{
		vString *name = vStringNew ();
		rubyKind actual_kind = parseIdentifier (cp, end, name, expected_kind);

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
			r = emitRubyTagFull (name, actual_kind, pushLevel, clearName);
		}
		vStringDelete (name);
	}
	return r;
}

static int readAndEmitTag (const unsigned char** cp, const unsigned char *end, rubyKind expected_kind)
{
	return readAndEmitTagFull (cp, end, expected_kind, expected_kind != K_CONST, true);
}

static void readAndStoreMixinSpec (const unsigned char** cp, const unsigned char *end, const char *how_mixin)
{

	NestingLevel *nl = NULL;
	tagEntryInfo *e = NULL;
	int ownerLevel = 0;

	for (ownerLevel = 0; ownerLevel < nesting->n; ownerLevel++)
	{
		nl = nestingLevelsGetNthParent (nesting, ownerLevel);
		e = nl? getEntryOfNestingLevel (nl): NULL;

		/* Ignore "if", "unless", "while" ... */
		if ((nl && (nl->corkIndex == CORK_NIL)) || (e && e->placeholder))
			continue;
		break;
	}

	if (!e)
		return;

	if (e->kindIndex == K_SINGLETON)
	{
		nl = nestingLevelsGetNthParent (nesting,
										ownerLevel + 1);
		if (nl == NULL)
			return;
		e = getEntryOfNestingLevel (nl);
	}

	if (!e)
		return;

	if (! (e->kindIndex == K_CLASS || e->kindIndex == K_MODULE))
		return;

	if (isspace (**cp) || (**cp == '('))
	{
		if (isspace (**cp))
			skipWhitespace (cp);
		if (**cp == '(')
			++*cp;

		vString *spec = vStringNewInit (how_mixin);
		vStringPut(spec, ':');

		size_t len = vStringLength (spec);
		parseIdentifier (cp, end, spec, K_MODULE);
		if (len == vStringLength (spec))
		{
			vStringDelete (spec);
			return;
		}

		struct blockData *bdata =  nestingLevelGetUserData (nl);
		if (bdata->mixin == NULL)
			bdata->mixin = stringListNew ();
		stringListAdd (bdata->mixin, spec);
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
		initTagEntry (&e, "", e_parent->kindIndex);
		markTagAsPlaceholder(&e, true);
		r = makeTagEntry (&e);
	}
	nestingLevelsPush (nesting, r);
}

static void parasiteToScope (rubySubparser *subparser, int subparserCorkIndex)
{
	NestingLevel *nl = nestingLevelsGetCurrent (nesting);
	struct blockData *bdata =  nestingLevelGetUserData (nl);
	bdata->subparser = subparser;
	bdata->subparserCorkIndex = subparserCorkIndex;

	if (subparser->enterBlockNotify)
		subparser->enterBlockNotify (subparser, subparserCorkIndex);
}

static void attachMixinField (int corkIndex, stringList *mixinSpec)
{
	vString *mixinField = stringListItem (mixinSpec, 0);
	for (unsigned int i = 1; i < stringListCount (mixinSpec); i++)
	{
		vStringPut (mixinField, ',');
		vStringCat (mixinField, stringListItem (mixinSpec, i));
	}

	attachParserFieldToCorkEntry (corkIndex, RubyFields [F_MIXIN].ftype,
								  vStringValue (mixinField));
}

static void deleteBlockData (NestingLevel *nl, void *data CTAGS_ATTR_UNUSED)
{
	struct blockData *bdata = nestingLevelGetUserData (nl);

	if (nl->corkIndex != CORK_NIL
		&& bdata->mixin != NULL
		&& stringListCount (bdata->mixin) > 0)
		attachMixinField (nl->corkIndex, bdata->mixin);

	tagEntryInfo *e = getEntryInCorkQueue (nl->corkIndex);
	if (e && !e->placeholder)
		setTagEndLine (e, getInputLineNumber ());

	tagEntryInfo *sub_e;
	if (bdata->subparserCorkIndex != CORK_NIL
		&& (sub_e = getEntryInCorkQueue (bdata->subparserCorkIndex)))
	{
		setTagEndLine (sub_e, getInputLineNumber ());
		if (bdata->subparser)
			bdata->subparser->leaveBlockNotify (bdata->subparser,
												bdata->subparserCorkIndex);
	}

	if (bdata->mixin)
		stringListDelete (bdata->mixin);
}

static bool doesLineIncludeConstant (const unsigned char **cp, vString *constant)
{
	const unsigned char *p = *cp;

	if (isspace (*p))
		skipWhitespace (&p);

	if (isupper (*p))
	{
		while (*p != 0 && isIdentChar (*p))
		{
			vStringPut (constant, *p);
			++p;
		}
		if (isspace (*p))
			skipWhitespace (&p);
		if (*p == '=')
		{
			*cp = p;
			return true;
		}
		vStringClear (constant);
	}

	return false;
}

static void emitRubyAccessorTags (vString *a, bool reader, bool writer)
{
	if (vStringLength (a) == 0)
		return;

	if (reader)
		emitRubyTagFull (a, K_ACCESSOR, false, !writer);
	if (writer)
	{
		vStringPut (a, '=');
		emitRubyTagFull (a, K_ACCESSOR, false, true);
	}
}

static void readAttrsAndEmitTags (const unsigned char **cp, const unsigned char *end,
								  bool reader, bool writer)
{
	vString *a = vStringNew ();

	skipWhitespace (cp);
	if (**cp == '(')
		++*cp;

	do {
		skipWhitespace (cp);
		if (**cp == ':')
		{
			if (K_METHOD == parseIdentifier (cp, end, a, K_METHOD))
			{
				emitRubyAccessorTags (a, reader, writer);
				skipWhitespace (cp);
				if (**cp == ',')
				{
					++*cp;
					continue;
				}
			}
		}
		else if (**cp == '"' || **cp == '\'')
		{
			unsigned char b = **cp;
			++*cp;
			parseString (cp, b, a);

			emitRubyAccessorTags (a, reader, writer);
			skipWhitespace (cp);
			if (**cp == ',')
			{
				++*cp;
				continue;
			}
		}
		break;
	} while (1);

	vStringDelete (a);
}

/*
 * The following patterns doen't make a scope:
 * define_method (:name,...
 * define_method ("name",...
 * define_method ('name',...
 * define_method :name, ...
 * define_method "name", ...
 * define_method 'name', ...
 *
 * The following patterns make a scope:
 * define_method (:name) do ...
 * define_method ("name") do ...
 * define_method ('name') do ...
 * define_method :name  do ...
 * define_method "name" do ...
 * define_method 'name' do ...
 *
 * The following patterns may make a scope:
 * define_method (:name) ...
 * define_method ("name") ...
 * define_method ('name') ...
 * define_method :name ...
 * define_method "name" ...
 * define_method 'name' ...
 */
static int readMethodAndEmitTags (const unsigned char **cp, const unsigned char *end, rubyKind kind)
{
	int r = CORK_NIL;
	vString *a = vStringNew ();

	skipWhitespace (cp);
	if (**cp == '(')
		++*cp;

	skipWhitespace (cp);
	if (**cp == ':')
	{
		if (K_METHOD != parseIdentifier (cp, end, a, K_METHOD))
			vStringClear (a);
	}
	else if (**cp == '"' || **cp == '\'')
	{
		unsigned char b = **cp;
		++*cp;
		parseString (cp, b, a);
	}

	if (vStringLength (a) > 0)
	{
		bool pushLevel = false;
		skipWhitespace (cp);
		if (kind == K_METHOD
			&& (**cp == ')'
				 || strncmp((const char *)*cp, "do", 2) == 0)) {
			if (**cp == ')')
				++*cp;
			pushLevel = true;
		}

		r = emitRubyTagFull (a, kind, pushLevel, false);

		/* If the name doesn't make a scope, fill the end: field of the tag. */
		if (kind == K_METHOD && !pushLevel)
		{
			tagEntryInfo *e = getEntryInCorkQueue (r);
			if (e)
				setTagEndLine (e, e->lineNumber);
		}
	}

	vStringDelete (a);
	return r;
}

static int readStringAndEmitTag (const unsigned char **cp, rubyKind kind, int role)
{
	int r = CORK_NIL;
	vString *s = NULL;

	skipWhitespace (cp);
	if (**cp == '(')
		++*cp;

	skipWhitespace (cp);
	if (**cp == '"' || **cp == '\'')
	{
		unsigned char b = **cp;
		++*cp;
		s = vStringNew ();
		parseString (cp, b, s);
	}

	if (s && vStringLength (s) > 0)
		r = makeSimpleRefTag (s, kind, role);

	vStringDelete (s);
	return r;
}

static int readAndEmitDef (const unsigned char **cp, const unsigned char *end)
{
	rubyKind kind = K_METHOD;
	NestingLevel *nl = nestingLevelsGetCurrent (nesting);
	tagEntryInfo *e_scope  = getEntryOfNestingLevel (nl);

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
	if (e_scope && (e_scope->kindIndex == K_CLASS || e_scope->kindIndex == K_MODULE)
		&& (
			/* Class.new do
			   ... in this case, an anonymous class is created and
			   ... pushed. */
			isTagExtraBitMarked (e_scope, XTAG_ANONYMOUS)
			||
			/* class <<
			   ... in this case, a placeholder tag having an empty
			   ... name is created and pushed. */
			*(e_scope->name) == '\0'
			))
		kind = K_SINGLETON;
	int corkIndex = readAndEmitTag (cp, end, kind);
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);

	/* Fill signature: field. */
	if (e)
	{
		vString *signature = vStringNewInit ("(");
		skipWhitespace (cp);
		if (**cp == '(')
		{
			++(*cp);
			parseSignature (cp, signature);
			if (vStringLast(signature) != ')')
			{
				vStringDelete (signature);
				signature = NULL;
			}
		}
		else
			vStringPut (signature, ')');
		e->extensionFields.signature = vStringDeleteUnwrap (signature);
		signature = NULL;
		vStringDelete (signature);
	}
	return corkIndex;
}

static rubySubparser *notifyLine (const unsigned char **cp)
{
	subparser *sub;
	rubySubparser *rubysub = NULL;

	foreachSubparser (sub, false)
	{
		rubysub = (rubySubparser *)sub;
		rubysub->corkIndex = CORK_NIL;

		if (rubysub->lineNotify)
		{
			enterSubparser(sub);
			const unsigned char *base = *cp;
			rubysub->corkIndex = rubysub->lineNotify(rubysub, cp);
			leaveSubparser();
			if (rubysub->corkIndex != CORK_NIL)
				break;
			*cp = base;
		}
	}

	if (rubysub && rubysub->corkIndex != CORK_NIL)
		return rubysub;
	return NULL;
}

static void findRubyTags (void)
{
	const unsigned char *line;
	bool inMultiLineComment = false;
	vString *constant = vStringNew ();
	vString *leftSide = vStringNew ();
	bool found_rdoc = false;

	nesting = nestingLevelsNewFull (sizeof (struct blockData), deleteBlockData);

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
	size_t llen;
	while ((line = readLineFromInputFileWithLength (&llen)) != NULL)
	{
		rubySubparser *subparser = CORK_NIL;
		const unsigned char *cp = line;
		const unsigned char *lend = line + llen;
		/* if we expect a separator after a while, for, or until statement
		 * separators are "do", ";" or newline */
		bool expect_separator = false;

		if (found_rdoc == false)
		{
			if (strncmp ((const char*)cp, "__END__", 7) == 0)
				break;

			if (strncmp ((const char*)cp, "# =", 3) == 0)
			{
				found_rdoc = true;
				makePromise ("RDoc", 0, 0, 0, 0, 0);
			}
		}

		if (canMatchWithEnd (&cp, lend, "=begin", isWhitespace))
		{
			inMultiLineComment = true;
			continue;
		}
		if (canMatchWithEnd (&cp, lend, "=end", isWhitespace))
		{
			inMultiLineComment = false;
			continue;
		}
		if (inMultiLineComment)
			continue;

		skipWhitespace (&cp);

		/* Avoid mistakenly starting a scope for modifiers such as
		*
		*   return if <exp>
		*
		* FIXME: we're fooled if someone does something heinous such as
		*
		*   puts("hello") \
		*       unless <exp>
		*/

		if (canMatchKeywordWithAssign (&cp, lend, "for") ||
			canMatchKeywordWithAssign (&cp, lend, "until") ||
			canMatchKeywordWithAssign (&cp, lend, "while"))
		{
			expect_separator = true;
			enterUnnamedScope ();
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "case") ||
				 canMatchKeywordWithAssign (&cp, lend, "if") ||
				 canMatchKeywordWithAssign (&cp, lend, "unless"))
		{
			enterUnnamedScope ();
		}

		/*
		* "module M", "class C" and "def m" should only be at the beginning
		* of a line.
		*/
		if (canMatchKeywordWithAssign (&cp, lend, "class")
			|| canMatchKeywordWithAssign (&cp, lend, "module")
			|| (canMatchKeywordWithAssignFull (&cp, lend, "Class.new", leftSide))
			|| (canMatchKeywordWithAssignFull (&cp, lend, "Module.new", leftSide)))
		{
			int r;

			int kind = K_UNDEFINED;
			if (*(cp - 1) == 's')
				/* clas*s* */
				kind = K_CLASS;
			else if (*(cp - 1) == 'e')
				/* *modul*e* */
				kind = K_MODULE;
			else if (*(cp - 5) == 's')
			{
				/* Clas*s*.new */
				kind = K_CLASS;
				expect_separator = true;
			}
			else if (*(cp - 5) == 'e')
			{
				/* Modul*e*.new */
				kind = K_MODULE;
				expect_separator = true;
			}
			else
				AssertNotReached();

			if (expect_separator)
			{
				r = emitRubyTagFull(vStringLength (leftSide) > 0? leftSide: NULL, kind, true, false);
				vStringClear (leftSide);
			}
			else
				r = readAndEmitTag (&cp, lend, kind);

			tagEntryInfo *e = getEntryInCorkQueue (r);

			if (e)
			{
				skipWhitespace (&cp);
				if ((*cp == '<' && *(cp + 1) != '<')
					|| (expect_separator && (*cp == '(')))
				{
					cp++;
					vString *parent = vStringNew ();
					parseIdentifier (&cp, lend, parent, K_CLASS);
					if (vStringLength (parent) > 0)
						e->extensionFields.inheritance = vStringDeleteUnwrap (parent);
					else
						vStringDelete (parent);
					if (expect_separator)
					{
						skipWhitespace (&cp);
						if (*cp == ')')
							cp++;
					}
				}
			}
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "include"))
		{
			readAndStoreMixinSpec (&cp, lend, "include");
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "prepend"))
		{
			readAndStoreMixinSpec (&cp, lend, "prepend");
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "extend"))
		{
			readAndStoreMixinSpec (&cp, lend, "extend");
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "def"))
		{
			readAndEmitDef (&cp, lend);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "attr_reader"))
		{
			readAttrsAndEmitTags (&cp, lend, true, false);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "attr_writer"))
		{
			readAttrsAndEmitTags (&cp, lend, false, true);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "attr_accessor"))
		{
			readAttrsAndEmitTags (&cp, lend, true, true);
		}
		else if (doesLineIncludeConstant (&cp, constant))
		{
			emitRubyTag (constant, K_CONST);
			vStringClear (constant);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "require"))
		{
			readStringAndEmitTag (&cp, K_LIBRARY, RUBY_LIBRARY_REQUIRED);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "require_relative"))
		{
			readStringAndEmitTag (&cp, K_LIBRARY, RUBY_LIBRARY_REQUIRED_REL);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "load"))
		{
			readStringAndEmitTag (&cp, K_LIBRARY, RUBY_LIBRARY_LOADED);
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "alias"))
		{
			if (!readAndEmitTagFull (&cp, lend, K_ALIAS, false, true)
				&& (*cp == '$'))
			{
				/* Alias for a global variable. */
				++cp;
				vString *alias = vStringNew ();
				vStringPut (alias, '$');
				if (K_METHOD == parseIdentifier (&cp, lend, alias, K_METHOD)
					&& vStringLength (alias) > 0)
					emitRubyTagFull (alias, K_ALIAS, false, false);
				vStringDelete (alias);
			}
		}
		else if (canMatchKeywordWithAssign (&cp, lend, "alias_method"))
			readMethodAndEmitTags (&cp, lend, K_ALIAS);
		else if (canMatchKeywordWithAssign (&cp, lend, "define_method"))
		{
			int r = readMethodAndEmitTags (&cp, lend, K_METHOD);
			/* "define_method(m)" makes a scope.
			 * In that case, we know we will see '{' or 'do' soon.
			 */
			NestingLevel *nl = nestingLevelsGetCurrent (nesting);
			if (nl && r == nl->corkIndex)
				expect_separator = true;
		}
		else if ((canMatchKeywordWithAssign (&cp, lend, "private")
				  || canMatchKeywordWithAssign (&cp, lend, "protected")
				  || canMatchKeywordWithAssign (&cp, lend, "public")
				  || canMatchKeywordWithAssign (&cp, lend, "private_class_method")
				  || canMatchKeywordWithAssign (&cp, lend, "public_class_method")))
		{
			skipWhitespace (&cp);
			if (canMatchKeywordWithAssign (&cp, lend, "def"))
				readAndEmitDef (&cp, lend);
			/* TODO: store the method for controlling visibility
			 * to the "access:" field of the tag.*/
		}
		else
			subparser = notifyLine(&cp);


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
			else if (canMatchKeyword (&cp, lend, "begin"))
			{
				enterUnnamedScope ();
			}
			else if (canMatchKeyword (&cp, lend, "do") || (*cp == '{'))
			{
				if (*cp == '{')
					++cp;

				if (! expect_separator)
				{
					/* We saw '{' or 'do' unexpectedly.
					 * Let's make an placeholder scope for it.
					 *
					 * If '{' or 'do' is expected, a scope will be pushed already.
					 * If they are not expected, a scope is pushed here.
					 * Either case a scope is pushed. See the code handling "end".
					 */
					enterUnnamedScope ();
					if (subparser && subparser->corkIndex)
						parasiteToScope (subparser, subparser->corkIndex);
				}
				else
					expect_separator = false;
			}
			else if ((canMatchKeyword (&cp, lend, "end") || (*cp == '}')) && nesting->n > 0)
			{
				if (*cp == '}')
					++cp;

				/* Leave the most recent scope. */
				nestingLevelsPop (nesting);
			}
			else if (*cp == '"' || *cp == '\'')
			{
				unsigned char b = *cp;
				/* Skip string literals.
				 * FIXME: should cope with escapes and interpolation.
				 */
				++cp;
				parseString (&cp, b, NULL);
			}
			else if (*cp == ';')
			{
				++cp;
				expect_separator = false;
			}
			else if (*cp != '\0')
			{
				do
					++cp;
				while (isIdentChar (*cp));
			}
		}

		if (expect_separator)
		{
			NestingLevel *nl = nestingLevelsGetCurrent (nesting);
			tagEntryInfo *e_scope  = getEntryOfNestingLevel (nl);

			if (e_scope && (e_scope->kindIndex == K_CLASS
							|| e_scope->kindIndex == K_MODULE
							/* Though "define_method(m)" is seen, no
							 * "do" or "{" is found.
							 *
							 * If "define_method(m, x)" is seen, neither
							 * "do" nor "{" is expected; a scope was not
							 * pushed.
							 */
							|| ((e_scope->kindIndex == K_METHOD) &&
								!e_scope->placeholder)))
				/* Class.new() ... was found but "do" or `{' is not
				 * found at the end; no block is made. Let's
				 * pop the nesting level push when Class.new()
				 * was found. This is applicable to Module.new. */
				nestingLevelsPop (nesting);
		}
	}
	nestingLevelsFree (nesting);
	vStringDelete (leftSide);
	vStringDelete (constant);
}

extern parserDefinition* RubyParser (void)
{
	static const char *const extensions [] = { "rb", "ruby", NULL };
	parserDefinition* def = parserNew ("Ruby");
	def->kindTable      = RubyKinds;
	def->kindCount  = ARRAY_SIZE (RubyKinds);
	def->extensions = extensions;
	def->parser     = findRubyTags;
	def->fieldTable = RubyFields;
	def->fieldCount = ARRAY_SIZE (RubyFields);
	def->useCork    = CORK_QUEUE;
	return def;
}
