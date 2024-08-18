/*
*   Copyright (c) 2020, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Tex base parser interface exported to subparsers
*/

#ifndef CTAGS_PARSER_TEX_H
#define CTAGS_PARSER_TEX_H

/*
*	 INCLUDE FILES
*/

#include "general.h"  /* must always come first */

#include "subparser.h"
#include "vstring.h"


/*
*	 DATA DEFINITIONS
*/

/* Parsing strategy */

enum TexNameFlag {
	/* Allow that the type of input token doesn't match
	 * the type of strategy. In stread of aborting,
	 * apply the next strategy to the same token. */
	TEX_NAME_FLAG_OPTIONAL             = (1 << 0),

	/* When reading tokens inside pair,
	 * whitespaces are considered as parts of a token or not. */
	TEX_NAME_FLAG_INCLUDING_WHITESPACE = (1 << 1),

	/* If a tag is created with this strategy, don't
	 * create a tag in its successor strategies. */
	TEX_NAME_FLAG_EXCLUSIVE            = (1 << 2),
};

struct TexParseStrategy {
	/* Expected token type '<', '[', '*', '{', and '\\' are supported.
	 * 0 means the end of strategies. '\\' means {} pair may be omitted.
	 *
	 * A string between <>, [], or {} (pairs) can be tagged or store to
	 * a vString. See kindIndex and name field of this structure.
	 */
	int type;

	/* Bits combination of enum TexNameFlag */
	unsigned int flags;

	/* Kind and role for making a tag for the string surrounded by one of pairs.
	 * If you don't need to make a tag for the string,
	 * specify KIND_GHOST_INDEX. */
	int kindIndex;
	int roleIndex;

	/* If a tag is made, Tex parser stores its cork index here. */
	int corkIndex;

	/* Store the string surrounded by one of paris.
	 * If you don't need to store the string, set NULL here. */
	vString *name;

	/* If true, make at most one tag for the name in the scope specified
	 * with scopeIndex. When making a tag, scopeIndex is set to
	 * extensionFields.scopeIndex only if unique is true.
	 * scopeIndex is never referred if unique if false. */
	bool unique;
	int scopeIndex;
};

typedef struct sTexSubparser texSubparser;
struct sTexSubparser {
	subparser subparser;

	/* When Tex parser reads an \begin{foo}, it calls
	 * this method.
	 *
	 * A subparser having interests in successor tokens may return strategies.
	 * If it doesn't, just return NULL; Tex base parser may call the next subparser.
	 */
	struct TexParseStrategy * (* readEnviromentBeginNotify) (texSubparser *s,
															 vString *env);
	/* When Tex parser reads an \end{foo}, it calls
	 * this method.
	 *
	 * If this method returns true, Tex base parser may call the next subparser.
	 * If it returns false, Tex base parser stops calling the rest of subparsers.
	 */
	bool (* readEnviromentEndNotify) (texSubparser *s, vString *env);

	/* When Tex parser reads an \identifier, it calls
	 * this method.
	 *
	 * A subparser having interests in successor tokens may return strategies.
	 * If it has no interest, just return NULL; Tex base parser may call next subparser.
	 */
	struct TexParseStrategy *(* readIdentifierNotify) (texSubparser *s,
													   vString *identifier);

	/* After Tex parser runs the strategies returned from readIdentifierNotify
	 * method, Tex parser calls this method to notify the subparser the result
	 * of running the strategies; corkIndex and/or name fields of strategies
	 * may be filled. */
	void (* reportStrategicParsing) (texSubparser *s,
									 const struct TexParseStrategy *strategy);
};

#endif	/* CTAGS_PARSER_TEX_H */
