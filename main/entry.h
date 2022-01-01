/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/
#ifndef CTAGS_MAIN_ENTRY_H
#define CTAGS_MAIN_ENTRY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "types.h"

#include <stdint.h>
#include <time.h>

#include "field.h"
#include "xtag.h"
#include "mio.h"
#include "ptrarray.h"
#include "nestlevel.h"

/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/
typedef struct sTagField {
	fieldType  ftype;
	const char* value;
	bool valueOwner;			/* used only in parserFieldsDynamic */
} tagField;

typedef uint64_t roleBitsType;

/*  Information about the current tag candidate.
 */
struct sTagEntryInfo {
	unsigned int lineNumberEntry:1;  /* pattern or line number entry */
	unsigned int isFileScope    :1;  /* is tag visible only within input file? */
	unsigned int isFileEntry    :1;  /* is this just an entry for a file name? */
	unsigned int truncateLineAfterTag :1;  /* truncate tag line at end of tag name? */
	unsigned int placeholder    :1;	 /* is used only for keeping corkIndex based scope chain.
					    Put this entry to cork queue but
					    don't print it to tags file. */
	unsigned int skipAutoFQEmission:1; /* If a parser makes a fq tag for the
										  current tag by itself, set this. */
	unsigned int isPseudoTag:1;	/* Used only in xref output.
								   If a tag is a pseudo, set this. */
	unsigned int inCorkQueue:1;

	unsigned long lineNumber;     /* line number of tag */
	const char* pattern;	      /* pattern for locating input line
				       * (may be NULL if not present) *//*  */
	unsigned int boundaryInfo;    /* info about nested input stream */
	MIOPos      filePosition;     /* file position of line containing tag */
	langType langType;         /* language of input file */
	const char *inputFileName;   /* name of input file */
	const char *name;             /* name of the tag */
	int kindIndex;	      /* kind descriptor */
	uint8_t extra[ ((XTAG_COUNT) / 8) + 1 ];
	uint8_t *extraDynamic;		/* Dynamically allocated but freed by per parser TrashBox */

	struct {
		const char* access;
		const char* implementation;
		const char* inheritance;

		/* Which scopeKindIndex belong to. If the value is LANG_AUTO,
		   the value for langType field of this structure is used as default value.
		   LANG_AUTO is set automatically in initTagEntryInfo. */
		langType    scopeLangType;
		int         scopeKindIndex;
		const char* scopeName;
		int         scopeIndex;   /* cork queue entry for upper scope tag.
					     This field is meaningful if the value
					     is not CORK_NIL and scope[0]  and scope[1] are
					     NULL. */

		const char* signature;

		/* type (union/struct/etc.) and name for a variable or typedef. */
		const char* typeRef [2];  /* e.g., "struct" and struct name */

#define ROLE_DEFINITION_INDEX -1
#define ROLE_DEFINITION_NAME "def"
#define ROLE_MAX_COUNT (sizeof(roleBitsType) * 8)
		roleBitsType roleBits; /* for role of reference tag */

#ifdef HAVE_LIBXML
		const char* xpath;
#endif
		unsigned long endLine;
		time_t epoch;
#define NO_NTH_FIELD -1
		short nth;
	} extensionFields;  /* list of extension fields*/

	/* `usedParserFields' tracks how many parser own fields are
	   used. If it is a few (less than PRE_ALLOCATED_PARSER_FIELDS),
	   statically allocated parserFields is used. If more fields than
	   PRE_ALLOCATED_PARSER_FIELDS is defined and attached, parserFieldsDynamic
	   is used. */
	unsigned int usedParserFields;
#define PRE_ALLOCATED_PARSER_FIELDS 5
#define NO_PARSER_FIELD -1
	tagField     parserFields [PRE_ALLOCATED_PARSER_FIELDS];
	ptrArray *   parserFieldsDynamic;

	/* Following source* fields are used only when #line is found
	   in input and --line-directive is given in ctags command line. */
	langType sourceLangType;
	const char *sourceFileName;
	unsigned long sourceLineNumberDifference;
};

typedef bool (* entryForeachFunc) (int corkIndex,
								   tagEntryInfo * entry,
								   void * data);

/*
*   GLOBAL VARIABLES
*/


/*
*   FUNCTION PROTOTYPES
*/
extern int makeTagEntry (const tagEntryInfo *const tag);
extern void initTagEntry (tagEntryInfo *const e, const char *const name,
			  int kindIndex);
extern void initRefTagEntry (tagEntryInfo *const e, const char *const name,
			     int kindIndex, int roleIndex);

/* initForeignRefTagEntry() is for making a tag for the language X when parsing
 * source code of Y language.
 * From the view point of the language Y, we call the language X a foreign
 * language.
 *
 * When making a tag for a foreign with this function, you must declare the
 * language X in the parser of Y with DEPTYPE_FOREIGNER dependency.
 */
extern void initForeignRefTagEntry (tagEntryInfo *const e, const char *const name,
									langType type,
									int kindIndex, int roleIndex);
extern void assignRole(tagEntryInfo *const e, int roleIndex);
extern bool isRoleAssigned(const tagEntryInfo *const e, int roleIndex);

extern int makeQualifiedTagEntry (const tagEntryInfo *const e);

extern void setTagPositionFromTag (tagEntryInfo *const dst, const tagEntryInfo *const src);

#define CORK_NIL 0
tagEntryInfo *getEntryInCorkQueue   (int n);
tagEntryInfo *getEntryOfNestingLevel (const NestingLevel *nl);
size_t        countEntryInCorkQueue (void);

/* If a parser sets (CORK_QUEUE and )CORK_SYMTAB to useCork,
 * the parsesr can use symbol lookup tables for the current input.
 * Each scope has a symbol lookup table.
 * To register an tag to the table, use registerEntry().
 * registerEntry registers CORKINDEX to a symbol table of a parent tag
 * specified in the scopeIndex field of the tag specified with CORKINDEX.
 */
void          registerEntry (int corkIndex);

/* foreachEntriesInScope is for traversing the symbol table for a table
 * specified with CORKINDEX. If CORK_NIL is given, this function traverses
 * top-level entries. If name is NULL, this function traverses all entries
 * under the scope.
 *
 * If FUNC returns false, this function returns false.
 * If FUNC never returns false, this function returns true.
 * If FUNC is not called because no node for NAME in the symbol table,
 * this function returns true.
 */
bool          foreachEntriesInScope (int corkIndex,
									 const char *name, /* or NULL */
									 entryForeachFunc func,
									 void *data);

/* Return the cork index for NAME in the scope specified with CORKINDEX.
 * Even if more than one entries for NAME are in the scope, this function
 * just returns one of them. Returning CORK_NIL means there is no entry
 * for NAME.
 */
int           anyEntryInScope       (int corkIndex,
									 const char *name);

int           anyKindEntryInScope (int corkIndex,
								   const char *name, int kind);

int           anyKindsEntryInScope (int corkIndex,
									const char *name,
									const int * kinds, int count);

int           anyKindsEntryInScopeRecursive (int corkIndex,
											 const char *name,
											 const int * kinds, int count);

extern void    markTagExtraBit     (tagEntryInfo *const tag, xtagType extra);
extern void    unmarkTagExtraBit   (tagEntryInfo *const tag, xtagType extra);
extern bool isTagExtraBitMarked (const tagEntryInfo *const tag, xtagType extra);

/* If any extra bit is on, return true. */
extern bool isTagExtra (const tagEntryInfo *const tag);

/* Functions for attaching parser specific fields
 *
 * Which function you should use?
 * ------------------------------
 * Case A:
 *
 * If your parser uses the Cork API, and your parser called
 * makeTagEntry () already, you can use both
 * attachParserFieldToCorkEntry () and attachParserField ().  Your
 * parser has the cork index returned from makeTagEntry ().  With the
 * cork index, your parser can call attachParserFieldToCorkEntry ().
 * If your parser already call getEntryInCorkQueue () to get the tag
 * entry for the cork index, your parser can call attachParserField ()
 * with passing true for `inCorkQueue' parameter. attachParserField ()
 * is a bit faster than attachParserFieldToCorkEntry ().
 *
 * attachParserField () and attachParserFieldToCorkEntry () duplicates
 * the memory object specified with `value' and stores the duplicated
 * object to the entry on the cork queue. So the parser must/can free
 * the original one passed to the functions after calling. The cork
 * queue manages the life of the duplicated object. It is not the
 * parser's concern.
 *
 *
 * Case B:
 *
 * If your parser called one of initTagEntry () family but didn't call
 * makeTagEntry () for a tagEntry yet, use attachParserField () with
 * false for `inCorkQueue' whether your parser uses the Cork API or
 * not.
 *
 * The parser (== caller) keeps the memory object specified with `value'
 * till calling makeTagEntry (). The parser must free the memory object
 * after calling makeTagEntry () if it is allocated dynamically in the
 * parser side.
 *
 * Interpretation of VALUE
 * -----------------------
 * For FIELDTYPE_STRING:
 * Both json writer and xref writer prints it as-is.
 *
 * For FIELDTYPE_STRING|FIELDTYPE_BOOL:
 * If VALUE points "" (empty C string), the json writer prints it as
 * false, and the xref writer prints it as -.
 * If VALUE points a non-empty C string, Both json writer and xref
 * writer print it as-is.
 *
 * For FIELDTYPE_BOOL
 * The json writer always prints true.
 * The xref writer always prints the name of field.
 * Set "" explicitly though the value pointed by VALUE is not referred,
 *
 *
 * The other data type and the combination of types are not implemented yet.
 *
 */
extern void attachParserField (tagEntryInfo *const tag, bool inCorkQueue, fieldType ftype, const char* value);
extern void attachParserFieldToCorkEntry (int index, fieldType ftype, const char* value);
extern const char* getParserFieldValueForType (tagEntryInfo *const tag, fieldType ftype);

extern int makePlaceholder (const char *const name);
extern void markTagPlaceholder (tagEntryInfo *e, bool placeholder);

/* Marking all tag entries entries under the scope specified
 * with index recursively.
 *
 * The parser calling this function enables CORK_SYMTAB.
 * Entries to be marked must be registered to the scope
 * specified with index or its descendant scopes with
 * registerEntry ().
 *
 * Call makePlaceholder () at the start of your parser for
 * making the root scope where the entries are registered.
 */
extern void markAllEntriesInScopeAsPlaceholder (int index);

#endif  /* CTAGS_MAIN_ENTRY_H */
