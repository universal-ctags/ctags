/*
 * Copyright (c) 2015, Dmitri Tikhonov
 *
 * This source code is released for free distribution under the terms of the
 * GNU General Public License version 2 or (at your option) any later version.
 *
 * selectors.h
 */

#ifndef CTAGS_MAIN_SELECTORS_H
#define CTAGS_MAIN_SELECTORS_H

#include "types.h"

const char *
selectByPickingPerlVersion (MIO *, langType *, unsigned int);

const char *
selectByObjectiveCAndMatLabKeywords (MIO *, langType *, unsigned int);

const char *
selectByObjectiveCKeywords(MIO *, langType *, unsigned int);

const char *
selectByArrowOfR (MIO *, langType *, unsigned int);

const char *
selectByRexxCommentAndDosbatchLabelPrefix (MIO *, langType *, unsigned int);

const char *
selectByXpathFileSpec (MIO *input, langType *candidates, unsigned int nCandidates);

#endif
