/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   The interface for subparsers of Markdown
*/
#ifndef CTAGS_PARSER_MARKDOWN_H
#define CTAGS_PARSER_MARKDOWN_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"
#include "vstring.h"

typedef struct sMarkdownSubparser markdownSubparser;

struct sMarkdownSubparser {
	subparser subparser;
	/* ```something
	   ---^ The rest of string is passed as LANGMARKER.

	   A sub parser analyses LANGMARKER.
	   If the sub parser can extract a name of language from LANGMARKER,
	   the parser puts the name to LANGNAME, and returns true.
	   If not, return false.

	   e.g.

	   ```{python}

	   Fot this input, ctags pases "{python}" as LANGMARKER. */
	bool (* extractLanguageForCodeBlock) (markdownSubparser *s,
										  const char *langMarker,
										  vString *langName);

	/* ```something
	   ...
	   <code block>
	   ...
	   ```

	   ctags passes each LINE in the code block to the sub parser
	   that returns true when ctags calls extractLanguageForCodeBlock()
	   with the sub parser. */
	void (* notifyCodeBlockLine) (markdownSubparser *s,
								  const unsigned char *line);

	/* ```something
	   ...
	   codeblock
	   ...
	   ```
	   ^ The end of code block.

	   ctags notifies the sub parser that the end of code block
	   is found.
	*/
	void (* notifyEndOfCodeBlock) (markdownSubparser *s);
};

#endif
