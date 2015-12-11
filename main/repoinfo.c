#include "general.h"
#include "ctags.h"
#ifdef HAVE_REPOINFO_H
#include "main/repoinfo.h"
#else
#define CTAGS_REPOINFO ((char*)0)
#endif
const char* ctags_repoinfo = CTAGS_REPOINFO;
