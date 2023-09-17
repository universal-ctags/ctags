/*
 *   Copyright (c) 2023, Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for BibLaTeX source files
 *   https://ftp.yz.yamagata-u.ac.jp/pub/CTAN/macros/latex/contrib/biblatex/doc/biblatex.pdf
 *
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include "bibtex.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"

/*
 *   DATA DECLARATIONS
 */
struct bibLaTeXSubparser {
	bibTexSubparser bib;
};

enum BibLaTeXKind {
	K_ARTWORK,
	K_AUDIO,
	K_BIBNOTE,
	K_BOOKINBOOK,
	K_BOOKLET,
	K_COLLECTION,
	K_COMMENTARY,
	K_DATASET,
	K_IMAGE,
	K_INTERFERENCE,
	K_JURISDICTION,
	K_LEGISLATION,
	K_LEGAL,
	K_LETTER,
	K_MOVIE,
	K_MUSIC,
	K_MVBOOK,
	K_MVCOLLECTION,
	K_MVPROCEEDINGS,
	K_MVREFERENCE,
	K_ONLINE,
	K_PATENT,
	K_PERFORMANCE,
	K_PERIODICAL,
	K_REFERENCE,
	K_REPORT,
	K_REVIEW,
	K_SET,
	K_SOFTWARE,
	K_STANDARD,
	K_SUPPBOOK,
	K_SUPPCOLLECTION,
	K_SUPPPERIODICAL,
	K_THESIS,
	K_VIDEO,
	K_XDATA,
};

/*
 *	DATA DEFINITIONS
 */
static kindDefinition BibLaTeXKinds[] = {
	{ true, 'A', "artwork", "artworks"  },
	{ true, 'B', "audio", "audios"  },
	{ true, 'C', "bibnote", "bibnotes"  },
	{ true, 'D', "bookinbook", "bookinbooks"  },
	{ true, 'E', "Booklet", "Booklets"  },
	/* F is for reserved in main. */
	{ true, 'G', "collection", "collections"  },
	{ true, 'H', "commentary", "commentarys"  },
	{ true, 'I', "dataset", "datasets"  },
	{ true, 'J', "image", "images"  },
	{ true, 'K', "interference", "interferences"  },
	{ true, 'L', "jurisdiction", "jurisdictions"  },
	{ true, 'M', "legislation", "legislations"  },
	{ true, 'N', "legal", "legals"  },
	{ true, 'O', "letter", "letters"  },
	{ true, 'P', "movie", "movies"  },
	{ true, 'Q', "music", "musics"  },
	{ true, 'R', "mvbook", "mvbooks"  },
	{ true, 'S', "mvcollection", "mvcollections"  },
	{ true, 'T', "mvproceedings", "mvproceedings"  },
	{ true, 'U', "mvreference", "mvreferences"  },
	{ true, 'V', "online", "onlines"  },
	{ true, 'W', "patent", "patents"  },
	/* X, Y, Z are reserved. */
	/* a, b, c, d, e, f are reserved for custom[a-f]. */
	{ true, 'g', "performance", "performances"  },
	{ true, 'h', "periodical", "periodicals"  },
	{ true, 'i', "reference", "references"  },
	{ true, 'j', "report", "reports"  },
	{ true, 'k', "review", "reviews"  },
	{ true, 'l', "set", "sets"  },
	{ true, 'm', "software", "software"  },
	{ true, 'n', "standard", "standards"  },
	{ true, 'o', "suppbook", "suppbooks"  },
	{ true, 'p', "suppcollection", "suppcollections"  },
	{ true, 'q', "suppperiodical", "suppperiodicals"  },
	{ true, 'r', "thesis", "thesis"  },
	{ true, 's', "video", "videos"  },
	{ true, 't', "xdata", "xdatas"  },
	/* x, y, z are reserved. */
};

static const keywordTable BibLaTeXTable [] = {
	{ "artwork", K_ARTWORK },
	{ "audio", K_AUDIO },
	{ "bibnote", K_BIBNOTE },
	{ "bookinbook", K_BOOKINBOOK },
	{ "Booklet", K_BOOKLET },
	{ "collection", K_COLLECTION },
	{ "commentary", K_COMMENTARY },
	{ "dataset", K_DATASET },
	{ "image", K_IMAGE },
	{ "interference", K_INTERFERENCE },
	{ "jurisdiction", K_JURISDICTION },
	{ "legislation", K_LEGISLATION },
	{ "legal", K_LEGAL },
	{ "letter", K_LETTER },
	{ "movie", K_MOVIE },
	{ "music", K_MUSIC },
	{ "mvbook", K_MVBOOK },
	{ "mvcollection", K_MVCOLLECTION },
	{ "mvproceedings", K_MVPROCEEDINGS },
	{ "mvreference", K_MVREFERENCE },
	{ "online", K_ONLINE },
	{ "patent", K_PATENT },
	{ "performance", K_PERFORMANCE },
	{ "periodical", K_PERIODICAL },
	{ "reference", K_REFERENCE },
	{ "report", K_REPORT },
	{ "review", K_REVIEW },
	{ "set", K_SET },
	{ "software", K_SOFTWARE },
	{ "standard", K_STANDARD },
	{ "suppbook", K_SUPPBOOK },
	{ "suppcollection", K_SUPPCOLLECTION },
	{ "suppperiodical", K_SUPPPERIODICAL },
	{ "thesis", K_THESIS },
	{ "video", K_VIDEO },
	{ "xdata", K_XDATA },
};

static langType Lang_biblatex;

/*
 *	 FUNCTION DEFINITIONS
 */
static int isKeywordForTagging (bibTexSubparser *bibtex,
								const char *string)
{
	int kind = lookupKeyword (string, Lang_biblatex);
	if (kind == KEYWORD_NONE)
		return KIND_GHOST_INDEX;
	return kind;
}

static void findBibLaTeXTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static struct bibLaTeXSubparser bibLaTeXSubparser = {
	.bib = {
		.subparser = {
			.direction  = SUBPARSER_BI_DIRECTION,
		},
		.isKeywordForTagging = isKeywordForTagging,
	},
};

static void initialize (const langType language)
{
	Lang_biblatex = language;
}

extern parserDefinition* BibLaTeXParser (void)
{
	parserDefinition* const def = parserNew("BibLaTeX");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "BibTeX", &bibLaTeXSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->keywordTable = BibLaTeXTable;
	def->keywordCount = ARRAY_SIZE (BibLaTeXTable);

	def->kindTable = BibLaTeXKinds;
	def->kindCount = ARRAY_SIZE(BibLaTeXKinds);

	def->parser = findBibLaTeXTags;
	def->initialize = initialize;

	return def;
}

/*
(defconst biblatex-kinds '(
	("artwork")
	("audio")
	("bibnote")
	("bookinbook")
	("Booklet")
	("collection")
	("commentary")
	("dataset")
	("image")
	("interference")
	("jurisdiction")
	("legislation")
	("legal")
	("letter")
	("movie")
	("music")
	("mvbook")
	("mvcollection")
	("mvproceedings" . t)
	("mvreference")
	("online")
	("patent")
	("performance")
	("periodical")
	("reference")
	("report")
	("review")
	("set")
	("software" . t)
	("standard")
	("suppbook")
	("suppcollection")
	("suppperiodical")
	("thesis")
	("video")
	("xdata")))
(mapc (lambda (x)
	   ;(insert (format "	K_%s,\n" (upcase (car x))))
	   ;(insert (format "	{ true, '', \"%s\", \"%s\"  },\n" (car x) (if (cdr x) (car x) (concat (car x) "s"))))
	   (insert (format "	{ \"%s\", K_%s },\n" (car x) (upcase (car x))))
	)
 biblatex-kinds)
*/
