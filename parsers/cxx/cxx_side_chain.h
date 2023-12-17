#ifndef ctags_cxx_side_chain_h_
#define ctags_cxx_side_chain_h_
/*
*   Copyright (c) 2020-2022 Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for extracting information __attribute__
*   lists.
*/

#include "general.h"

#include "cxx_token_chain.h"

// cxxSideChainScan and cxxSideChainScanInRange are assumed to be called
// after calling cxxTagBegin and before calling cxxTagCommit.
// cxxSideChainScan scans PSIDECHAIN where __attribute__ sequence are stored
// to find something interesting.
void cxxSideChainScan(const CXXTokenChain * pSideChain);

// Collect all side chanins in token between PSTART to PEND, and append them to
// the side chain of PDEST.
void cxxSideChainCollectInRange(CXXToken *pStart, CXXToken *pEnd, CXXToken * pDest);

void cxxSideChainAppend(CXXToken * src, CXXToken * dest);
void cxxSideChainAppendChain(CXXTokenChain * pSideChain, CXXToken * dest);

// Return the side chain of PTOKEN, and fill NULL the side chain member of PTOKEN.
CXXTokenChain * cxxSideChainEject(CXXToken * pToken);

#endif //!ctags_cxx_side_chain_h_
