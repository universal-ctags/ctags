/*
*   Copyright (c) 2019, Red Hat, Inc.
*   Copyright (c) 2019, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_PARSER_XML_H
#define CTAGS_PARSER_XML_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "subparser.h"

/*
*	 DATA DECLARATIONS
*/

typedef struct sXmlSubparser xmlSubparser;
struct sXmlSubparser {
	subparser subparser;

	/* Similar to makeTagEntryNotify method of subparser.
	 * However, makeTagEntryWithNodeNotify passes the xml node
	 * just found to subparsers.
	 */
	void (* makeTagEntryWithNodeNotify) (xmlSubparser *s,
										 xmlNode *node, tagEntryInfo *xmlTag);
};

#endif /* CTAGS_PARSER_XML_H */
