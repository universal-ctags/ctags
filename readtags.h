/*
*   $Id$
*
*   Copyright (c) 1996-2001, Darren Hiebert
*
*   This source code is released for the public domain.
*
*   This module defines the public interface for reading tag files.
*/

/*
*  DATA DECLARATIONS
*/

typedef enum { TagFailure = 0, TagSuccess = 1 } tagResult;

/* This structure contains information about the tag file. */
typedef struct {

	    /* information about the structure of the tag file */
	struct {
		    /* format of tag file (1 = original, 2 = extended) */
		short format;

		    /* is the tag file sorted? (0 = unsorted, 1 = sorted) */
		short sorted;
	} file;

	    /* information about the program which created this tag file */
	struct {
		    /* name of author */
		const char *author;

		    /* name of program */
		const char *name;

		    /* URL of distribution */
		const char *url;

		    /* program version */
		const char *version;
	} program;

} tagFileInfo;

/* This structure contains information about an extension field for a tag. */
/* These exist at the end of the tag in the form "key:value"). */
typedef struct {

	    /* the key of the extension field */
	const char *key;

	    /* the value of the extension field (may be an empty string) */
	const char *value;

} tagExtensionField;

/* This structure contains information about a specific tag. */
typedef struct {

	    /* name of tag */
	const char *name;

	    /* path of source file containing definition of tag */
	const char *file;

	    /* address for locating tag in source file */
	struct {
		    /* pattern for locating source line
		     * (may be NULL if not present) */
		const char *pattern;

		    /* line number in source file of tag definition
		     * (may be zero if not known) */
		unsigned long lineNumber;
	} address;

	    /* kind of tag (may by name, character, or NULL if not known) */
	const char *kind;

	    /* is tag of file-limited scope? */
	short fileScope;

	    /* miscellaneous extension fields */
	struct {
		    /* number of entries in `list' */
		unsigned short count;

		    /* list of key value pairs */
		tagExtensionField *list;
	} fields;

} tagEntry;


/*
*  FUNCTION PROTOTYPES
*/

/*
*  This function must be called before calling other functions in this
*  library. It is passed the path to the tag file to read and, optionally, a
*  pointer to a structure to populate with information about the tag file,
*  which may be null. The function will return TagSuccess if the file was
*  successfully opened, TagFailure if not.
*/
extern tagResult tagsOpen (const char *filePath, tagFileInfo *info);

/*
*  Step sequentially through each line of the tag file. It is passed a pointer
*  to a structure which will be populated with information about the next tag
*  file entry, which may be null. The function will return TagSuccess another
*  tag entry is found, or TagFailure if not (i.e. it reached end of file).
*/
extern tagResult tagsNext (tagEntry *entry);

/*
*  Retrieve the value associated with the extension field for a specified key.
*  It is passed a pointer to a structure already populated with values by a
*  previous call to tagsNext(), tagsFind(), or tagsFindNext(), and a string
*  containing the key of the desired extension field. If no such field of the
*  specified key exists, the function will return null.
*/
extern const char *tagsField (const tagEntry *entry, const char *key);

/*
*  Find the first tag associated with `name'. The structure pointed to by
*  `entry' will be populated with information about the tag file entry. The
*  function will return TagSuccess if a tag matching the name is found, or
*  TagFailure if not.
*/
extern tagResult tagsFind (tagEntry *entry, const char *name);

/*
*  Find the next tag matching the name last found by tagsFind(). The structure
*  pointed to by `entry' will be populated with information about the tag file
*  entry. The function will return TagSuccess if another tag matching the name
*  is found, or TagFailure if not.
*/
extern tagResult tagsFindNext (tagEntry *entry);

/*
*  Call tagsTerminate() at completion of reading the tag file, which will
*  close the file and free any internal memory allocated. The function will
*  return TagFailure is no file is currently open, TagSuccess otherwise.
*/
extern tagResult tagsClose (void);

/* vi:set tabstop=8 shiftwidth=4: */
