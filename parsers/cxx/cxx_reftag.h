#ifndef _cxxReftag_h_
#define _cxxReftag_h_
/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for extracting reference tags
*/

#include "general.h"

void cxxReftagEvalNewToken(void);

// Reset the kind and role of the tag specified with iCorkIndex.
// The scope of the tag is also updated with iScopeCorkIndex.
void cxxReftagReset(int iCorkIndex, int iScopeCorkIndex,
					int iKindIndex, int iRoleIndex, bool bUpdateValue);

#endif //!_cxxReftag_h_
