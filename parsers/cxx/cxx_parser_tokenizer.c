/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/
#include "cxx_parser.h"
#include "cxx_parser_internal.h"

#include "cxx_debug.h"
#include "cxx_keyword.h"
#include "cxx_token.h"
#include "cxx_token_chain.h"

#include "parse.h"
#include "vstring.h"
#include "lcpp.h"
#include "debug.h"
#include "keyword.h"
#include "read.h"
#include "options.h"

#include <string.h>

#define UINFO(c) (((c) < 0x80 && (c) >= 0) ? g_aCharTable[c].uType : 0)

static void cxxParserSkipToNonWhiteSpace(void)
{
	if(!isspace(g_cxx.iChar))
		return;
	do
		g_cxx.iChar = cppGetc();
	while(isspace(g_cxx.iChar));
}

enum CXXCharType
{
	// Start of an identifier a-z A-Z _ and ~ since
	// it's part of the destructor name
	CXXCharTypeStartOfIdentifier = 1,
	// Part of identifier a-z a-Z 0-9 _
	CXXCharTypePartOfIdentifier = (1 << 1),
	// A decimal digit
	CXXCharTypeDecimalDigit = (1 << 2),
	// A hexadecimal digit
	CXXCharTypeHexadecimalDigit = (1 << 3),
	// Hex digits x X u U l L and .
	CXXCharTypeValidInNumber = (1 << 4),
	// A named single char token. (uInfo >> 16) & 0xff
	// contains the type of the token.
	CXXCharTypeNamedSingleCharToken = (1 << 5),
	// A named single or repeated char token. (uInfo >> 16) & 0xff
	// is single type, (uInfo >> 24) & 0xff is multi type
	CXXCharTypeNamedSingleOrRepeatedCharToken = (1 << 6),
	// An operator (we merge them)
	CXXCharTypeOperator = (1 << 7),
	// A named single or operator (when repeated).
	// (uInfo >> 16) & 0xff is single type
	CXXCharTypeNamedSingleOrOperatorToken = (1 << 8)
};

typedef struct _CXXCharTypeData
{
	unsigned int uType;
	unsigned int uSingleTokenType;
	unsigned int uMultiTokenType;
} CXXCharTypeData;


static CXXCharTypeData g_aCharTable[128] =
{
	// 000 (0x00) NUL
	{
		0,
		0,
		0
	},
	// 001 (0x01) SOH
	{
		0,
		0,
		0
	},
	// 002 (0x02) STX
	{
		0,
		0,
		0
	},
	// 003 (0x03) ETX
	{
		0,
		0,
		0
	},
	// 004 (0x04) EOT
	{
		0,
		0,
		0
	},
	// 005 (0x05) ENQ
	{
		0,
		0,
		0
	},
	// 006 (0x06) ACK
	{
		0,
		0,
		0
	},
	// 007 (0x07) BEL
	{
		0,
		0,
		0
	},
	// 008 (0x08) BS
	{
		0,
		0,
		0
	},
	// 009 (0x09) '\t' HT
	{
		0,
		0,
		0
	},
	// 010 (0x0a) '\n' LF
	{
		0,
		0,
		0
	},
	// 011 (0x0b) '\v' VT
	{
		0,
		0,
		0
	},
	// 012 (0x0c) FF
	{
		0,
		0,
		0
	},
	// 013 (0x0d) '\r' CR
	{
		0,
		0,
		0
	},
	// 014 (0x0e) 'SO'
	{
		0,
		0,
		0
	},
	// 015 (0x0f) 'SI'
	{
		0,
		0,
		0
	},
	// 016 (0x10) DLE
	{
		0,
		0,
		0
	},
	// 017 (0x11) DC1
	{
		0,
		0,
		0
	},
	// 018 (0x12) DC2
	{
		0,
		0,
		0
	},
	// 019 (0x13) DC3
	{
		0,
		0,
		0
	},
	// 020 (0x14) DC4
	{
		0,
		0,
		0
	},
	// 021 (0x15) NAK
	{
		0,
		0,
		0
	},
	// 022 (0x16) SYN
	{
		0,
		0,
		0
	},
	// 023 (0x17) ETB
	{
		0,
		0,
		0
	},
	// 024 (0x18) CAN
	{
		0,
		0,
		0
	},
	// 025 (0x19) EM
	{
		0,
		0,
		0
	},
	// 026 (0x1a) SUB
	{
		0,
		0,
		0
	},
	// 027 (0x1b) ESC
	{
		0,
		0,
		0
	},
	// 028 (0x1c) FS
	{
		0,
		0,
		0
	},
	// 029 (0x1d) GS
	{
		0,
		0,
		0
	},
	// 030 (0x1e) RS
	{
		0,
		0,
		0
	},
	// 031 (0x1f) US
	{
		0,
		0,
		0
	},
	// 032 (0x20) ' '
	{
		0,
		0,
		0
	},
	// 033 (0x21) '!'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 034 (0x22) '"'
	{
		0,
		0,
		0
	},
	// 035 (0x23) '#'
	{
		0,
		0,
		0
	},
	// 036 (0x24) '$'
	{
		0,
		0,
		0
	},
	// 037 (0x25) '%'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 038 (0x26) '&'
	{
		CXXCharTypeNamedSingleOrRepeatedCharToken,
		CXXTokenTypeAnd,
		CXXTokenTypeMultipleAnds
	},
	// 039 (0x27) '''
	{
		0,
		0,
		0
	},
	// 040 (0x28) '('
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeOpeningParenthesis,
		0
	},
	// 041 (0x29) ')'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeClosingParenthesis,
		0
	},
	// 042 (0x2a) '*'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeStar,
		0
	},
	// 043 (0x2b) '+'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 044 (0x2c) ','
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeComma,
		0
	},
	// 045 (0x2d) '-'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 046 (0x2e) '.'
	{
		CXXCharTypeValidInNumber | CXXCharTypeNamedSingleOrRepeatedCharToken,
		CXXTokenTypeDotOperator,
		CXXTokenTypeMultipleDots
	},
	// 047 (0x2f) '/'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 048 (0x30) '0'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 049 (0x31) '1'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 050 (0x32) '2'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 051 (0x33) '3'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 052 (0x34) '4'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 053 (0x35) '5'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 054 (0x36) '6'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 055 (0x37) '7'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 056 (0x38) '8'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 057 (0x39) '9'
	{
		CXXCharTypePartOfIdentifier | CXXCharTypeDecimalDigit |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 058 (0x3a) ':'
	{
		CXXCharTypeNamedSingleOrRepeatedCharToken,
		CXXTokenTypeSingleColon,
		CXXTokenTypeMultipleColons
	},
	// 059 (0x3b) ';'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeSemicolon,
		0
	},
	// 060 (0x3c) '<'
	{
		CXXCharTypeNamedSingleOrOperatorToken,
		CXXTokenTypeSmallerThanSign,
		0
	},
	// 061 (0x3d) '='
	{
		CXXCharTypeOperator | CXXCharTypeNamedSingleOrRepeatedCharToken,
		CXXTokenTypeAssignment,
		CXXTokenTypeOperator
	},
	// 062 (0x3e) '>' // We never merge two >>
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeGreaterThanSign,
		0
	},
	// 063 (0x3f) '?'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 064 (0x40) '@'
	{
		0,
		0,
		0
	},
	// 065 (0x41) 'A'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 066 (0x42) 'B'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 067 (0x43) 'C'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 068 (0x44) 'D'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 069 (0x45) 'E'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 070 (0x46) 'F'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 071 (0x47) 'G'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 072 (0x48) 'H'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0,
		0
	},
	// 073 (0x49) 'I'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 074 (0x4a) 'J'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 075 (0x4b) 'K'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 076 (0x4c) 'L'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0,
		0
	},
	// 077 (0x4d) 'M'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 078 (0x4e) 'N'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 079 (0x4f) 'O'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 080 (0x50) 'P'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 081 (0x51) 'Q'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0,
		0
	},
	// 082 (0x52) 'R'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 083 (0x53) 'S'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 084 (0x54) 'T'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 085 (0x55) 'U'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 086 (0x56) 'V'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 087 (0x57) 'W'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 088 (0x58) 'X'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 089 (0x59) 'Y'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 090 (0x5a) 'Z'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 091 (0x5b) '['
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeOpeningSquareParenthesis,
		0
	},
	// 092 (0x5c) '\'
	{
		0,
		0,
		0
	},
	// 093 (0x5d) ']'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeClosingSquareParenthesis,
		0
	},
	// 094 (0x5e) '^'
	{
		CXXCharTypeOperator,
		0,
		0
	},
	// 095 (0x5f) '_'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 096 (0x60) '`'
	{
		0,
		0,
		0
	},
	// 097 (0x61) 'a'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0,
		0
	},
	// 098 (0x62) 'b'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 099 (0x63) 'c'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 100 (0x64) 'd'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 101 (0x65) 'e'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 102 (0x66) 'f'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeHexadecimalDigit | CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 103 (0x67) 'g'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 104 (0x68) 'h'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 105 (0x69) 'i'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 106 (0x6a) 'j'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 107 (0x6b) 'k'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 108 (0x6c) 'l'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 109 (0x6d) 'm'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 110 (0x6e) 'n'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 111 (0x6f) 'o'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 112 (0x70) 'p'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 113 (0x71) 'q'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 114 (0x72) 'r'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 115 (0x73) 's'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 116 (0x74) 't'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 117 (0x75) 'u'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 118 (0x76) 'v'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 119 (0x77) 'w'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 120 (0x78) 'x'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier |
			CXXCharTypeValidInNumber,
		0 ,
		0
	},
	// 121 (0x79) 'y'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 122 (0x7a) 'z'
	{
		CXXCharTypeStartOfIdentifier | CXXCharTypePartOfIdentifier,
		0 ,
		0
	},
	// 123 (0x7b) '{'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeOpeningBracket,
		0
	},
	// 124 (0x7c) '|'
	{
		CXXCharTypeOperator,
		0 ,
		0
	},
	// 125 (0x7d) '}'
	{
		CXXCharTypeNamedSingleCharToken,
		CXXTokenTypeClosingBracket,
		0
	},
	// 126 (0x7e) '~'
	{
		CXXCharTypeStartOfIdentifier,
		0 ,
		0
	},
	// 127 (0x7f) ''
	{ 0, 0, 0 }
};

// The __attribute__((...)) sequence complicates parsing quite a lot.
// For this reason we attempt to "hide" it from the rest of the parser
// at tokenizer level.
static boolean cxxParserParseNextTokenCondenseAttribute(void)
{
	CXX_DEBUG_ENTER();

	CXX_DEBUG_ASSERT(
			cxxTokenIsKeyword(g_cxx.pToken,CXXKeyword__ATTRIBUTE__),
			"This function should be called only after we have parsed __attribute__"
		);

	// Kill it
	cxxTokenDestroy(cxxTokenChainTakeLast(g_cxx.pTokenChain));

	// And go ahead.

	if(!cxxParserParseNextToken())
	{
		CXX_DEBUG_LEAVE_TEXT("No next token after __attribute__");
		return FALSE;
	}

	if(!cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeOpeningParenthesis))
	{
		CXX_DEBUG_LEAVE_TEXT("Something that is not an opening parenthesis");
		return TRUE;
	}

	if(!cxxParserParseAndCondenseCurrentSubchain(
			CXXTokenTypeOpeningParenthesis |
				CXXTokenTypeOpeningSquareParenthesis |
				CXXTokenTypeOpeningBracket,
			FALSE
		))
	{
		CXX_DEBUG_LEAVE_TEXT("Failed to parse and condense subchains");
		return FALSE;
	}

	CXX_DEBUG_ASSERT(
			cxxTokenTypeIs(g_cxx.pToken,CXXTokenTypeParenthesisChain),
			"Should have a parenthesis chain as last token!"
		);

	// Try to make sense of certain kinds of __attribute__.
	// the proper syntax is __attribute__(()), so look at the inner chain

	CXXToken * pInner = cxxTokenChainFirst(g_cxx.pToken->pChain);
	if(pInner)
	{
		if(pInner->pNext && cxxTokenTypeIs(pInner->pNext,CXXTokenTypeParenthesisChain))
			pInner = cxxTokenChainFirst(pInner->pNext->pChain);

		while(pInner)
		{
			if(cxxTokenTypeIs(pInner,CXXTokenTypeIdentifier))
			{
				CXX_DEBUG_PRINT("Analyzing attribyte %s",vStringValue(pInner->pszWord));
				if(
						(strcmp(vStringValue(pInner->pszWord),"always_inline") == 0) ||
						(strcmp(vStringValue(pInner->pszWord),"__always_inline__") == 0)
					)
				{
					CXX_DEBUG_PRINT("Found attribute 'always_inline'");
					// assume "inline" has been seen.
					g_cxx.uKeywordState |= CXXParserKeywordStateSeenInline;
				} else if(
						(strcmp(vStringValue(pInner->pszWord),"deprecated") == 0) ||
						(strcmp(vStringValue(pInner->pszWord),"__deprecated__") == 0)
					)
				{
					CXX_DEBUG_PRINT("Found attribute 'deprecated'");
					// assume "inline" has been seen.
					g_cxx.uKeywordState |= CXXParserKeywordStateSeenAttributeDeprecated;
				}
			}

			// If needed, we could attach certain attributes to previous
			// identifiers. But note that __attribute__ may apply to a
			// following identifier too.

			pInner = pInner->pNext;
		}
	}

	// Now just kill the chain.
	cxxTokenDestroy(cxxTokenChainTakeLast(g_cxx.pTokenChain));

	// And finally extract yet another token.
	CXX_DEBUG_LEAVE();
	return cxxParserParseNextToken();
}

boolean cxxParserParseNextToken(void)
{
	CXXToken * t = cxxTokenCreate();

	// The token chain should not be allowed to grow arbitrairly large.
	// The token structures are quite big and it's easy to grow up to
	// 5-6GB or memory usage. However this limit should be large enough
	// to accomodate all the reasonable statements that could have some
	// information in them. This includes multiple function prototypes
	// in a single statement (ImageMagick has some examples) but probably
	// does NOT include large data tables.
	if(g_cxx.pTokenChain->iCount > 16384)
		cxxTokenChainDestroyLast(g_cxx.pTokenChain);

	cxxTokenChainAppend(g_cxx.pTokenChain,t);

	g_cxx.pToken = t;

	cxxParserSkipToNonWhiteSpace();

	// FIXME: this cpp handling is kind of broken:
	// it works only because the moon is in the correct phase.
	cppBeginStatement();

	// This must be done after getting char from input
	t->iLineNumber = getInputLineNumber();
	t->oFilePosition = getInputFilePosition();

	if(g_cxx.iChar == EOF)
	{
		t->eType = CXXTokenTypeEOF;
		t->bFollowedBySpace = FALSE;
		return FALSE;
	}

	unsigned int uInfo = UINFO(g_cxx.iChar);

	//printf("Char %c %02x info %u\n",g_cxx.iChar,g_cxx.iChar,uInfo);

	if(uInfo & CXXCharTypeStartOfIdentifier)
	{
		// word
		t->eType = CXXTokenTypeIdentifier;
		t->bFollowedBySpace = FALSE;

		vStringPut(t->pszWord,g_cxx.iChar);

		// special case for tile, which may actually be an operator
		if(g_cxx.iChar == '~')
		{
			// may be followed by space!
			g_cxx.iChar = cppGetc();
			if(isspace(g_cxx.iChar))
			{
				t->bFollowedBySpace = TRUE;
				g_cxx.iChar = cppGetc();
				while(isspace(g_cxx.iChar))
					g_cxx.iChar = cppGetc();
			}

			// non space
			uInfo = UINFO(g_cxx.iChar);
			if(!(uInfo & CXXCharTypeStartOfIdentifier))
			{
				// this is not an identifier after all
				t->eType = CXXTokenTypeOperator;
				if((!t->bFollowedBySpace) && g_cxx.iChar == '=')
				{
					// make ~= single token so it's not handled as
					// a separate assignment
					vStringPut(t->pszWord,g_cxx.iChar);
					g_cxx.iChar = cppGetc();
					t->bFollowedBySpace = isspace(g_cxx.iChar);
				}
				return TRUE;
			}
		} else {
			g_cxx.iChar = cppGetc();
		}

		for(;;)
		{
			uInfo = UINFO(g_cxx.iChar);
			if(!(uInfo & CXXCharTypePartOfIdentifier))
				break;
			vStringPut(t->pszWord,g_cxx.iChar);
			g_cxx.iChar = cppGetc();
		}

		int iCXXKeyword = lookupKeyword(t->pszWord->buffer,g_cxx.eLanguage);
		if(iCXXKeyword >= 0)
		{
			if(
					(
						(iCXXKeyword == CXXKeywordFINAL) &&
						(!g_cxx.bParsingClassStructOrUnionDeclaration)
					) || (
						(
							(iCXXKeyword == CXXKeywordPUBLIC) ||
							(iCXXKeyword == CXXKeywordPROTECTED) ||
							(iCXXKeyword == CXXKeywordPRIVATE)
						) &&
						(!g_cxx.bEnablePublicProtectedPrivateKeywords)
					)
				)
			{
				t->eType = CXXTokenTypeIdentifier;
			} else {
				t->eType = CXXTokenTypeKeyword;
				t->eKeyword = (enum CXXKeyword)iCXXKeyword;

				if(iCXXKeyword == CXXKeyword__ATTRIBUTE__)
				{
					// special handling for __attribute__
					return cxxParserParseNextTokenCondenseAttribute();
				}
			}
		} else {
			boolean bIgnoreParens = FALSE;
			const char * szReplacement = NULL;
			if(isIgnoreToken(
					vStringValue(t->pszWord),
					&bIgnoreParens,
					&szReplacement
				))
			{
				CXX_DEBUG_PRINT("Ignore token %s",vStringValue(t->pszWord));
				// FIXME: Handle ignore parens!
				if(szReplacement && *szReplacement)
				{
					vStringClear(t->pszWord);
					vStringCatS(t->pszWord,szReplacement);
				} else {
					// skip
					cxxTokenChainDestroyLast(g_cxx.pTokenChain);
					return cxxParserParseNextToken();
				}
			}
		}

		t->bFollowedBySpace = t->bFollowedBySpace | isspace(g_cxx.iChar);

		return TRUE;
	}

	if(g_cxx.iChar == '-')
	{
		// special case for pointer
		vStringPut(t->pszWord,g_cxx.iChar);
		g_cxx.iChar = cppGetc();
		if(g_cxx.iChar == '>')
		{
			t->eType = CXXTokenTypePointerOperator;
			vStringPut(t->pszWord,g_cxx.iChar);
			g_cxx.iChar = cppGetc();
		} else {
			t->eType = CXXTokenTypeOperator;
			if(g_cxx.iChar == '-')
			{
				vStringPut(t->pszWord,g_cxx.iChar);
				g_cxx.iChar = cppGetc();
			}
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

#if 0
	// As long as we use cppGetc() we don't need this

	if(g_cxx.iChar == '"')
	{
		// special case for strings
		t->eType = CXXTokenTypeStringConstant;
		vStringPut(t->pszWord,g_cxx.iChar);
		// We don't even care of storing the other chars: we don't need
		// them for parsing
		// FIXME: We might need them in signature:() tag.. maybe add
		// them up to a certain length only?
		for(;;)
		{
			g_cxx.iChar = cppGetc();
			if(g_cxx.iChar == EOF)
			{
				t->bFollowedBySpace = FALSE;
				return TRUE;
			}
			if(g_cxx.iChar == '\\')
			{
				// escape
				g_cxx.iChar = cppGetc();
				if(g_cxx.iChar == EOF)
				{
					t->bFollowedBySpace = FALSE;
					return TRUE;
				}
			} else if(g_cxx.iChar == '"')
			{
				g_cxx.iChar = cppGetc();
				break;
			}
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}
#else
	if(g_cxx.iChar == STRING_SYMBOL)
	{
		t->eType = CXXTokenTypeStringConstant;
		vStringPut(t->pszWord,'"');
		vStringPut(t->pszWord,'"');
		g_cxx.iChar = cppGetc();
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}
#endif

#if 0
	// As long as we use cppGetc() we don't need this
	if(g_cxx.iChar == '\'')
	{
		// special case for strings
		t->eType = CXXTokenTypeCharacterConstant;
		vStringPut(t->pszWord,g_cxx.iChar);
		// We don't even care storing the other chars: we don't
		// need them for parsing
		for(;;)
		{
			g_cxx.iChar = cppGetc();
			if(g_cxx.iChar == EOF)
			{
				t->bFollowedBySpace = FALSE;
				return TRUE;
			}
			if(g_cxx.iChar == '\\')
			{
				// escape
				g_cxx.iChar = cppGetc();
				if(g_cxx.iChar == EOF)
				{
					t->bFollowedBySpace = FALSE;
					return TRUE;
				}
			} else if(g_cxx.iChar == '\'')
			{
				g_cxx.iChar = cppGetc();
				break;
			}
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}
#else
	if(g_cxx.iChar == CHAR_SYMBOL)
	{
		t->eType = CXXTokenTypeCharacterConstant;
		vStringPut(t->pszWord,'\'');
		vStringPut(t->pszWord,'\'');
		g_cxx.iChar = cppGetc();
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}
#endif

	if(uInfo & CXXCharTypeDecimalDigit)
	{
		// number
		t->eType = CXXTokenTypeNumber;
		vStringPut(t->pszWord,g_cxx.iChar);

		for(;;)
		{
			g_cxx.iChar = cppGetc();
			uInfo = UINFO(g_cxx.iChar);
			if(!(uInfo & CXXCharTypeValidInNumber))
				break;
			vStringPut(t->pszWord,g_cxx.iChar);
		}

		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

	if(uInfo & CXXCharTypeNamedSingleOrRepeatedCharToken)
	{
		t->eType = g_aCharTable[g_cxx.iChar].uSingleTokenType;
		vStringPut(t->pszWord,g_cxx.iChar);
		int iChar = g_cxx.iChar;
		g_cxx.iChar = cppGetc();
		if(g_cxx.iChar == iChar)
		{
			t->eType = g_aCharTable[g_cxx.iChar].uMultiTokenType;
			// We could signal a syntax error with more than two colons
			// or equal signs...but we're tolerant
			do {
				vStringPut(t->pszWord,g_cxx.iChar);
				g_cxx.iChar = cppGetc();
			} while(g_cxx.iChar == iChar);
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

	if(uInfo & CXXCharTypeNamedSingleOrOperatorToken)
	{
		t->eType = g_aCharTable[g_cxx.iChar].uSingleTokenType;
		vStringPut(t->pszWord,g_cxx.iChar);
		g_cxx.iChar = cppGetc();
		uInfo = UINFO(g_cxx.iChar);
		if(uInfo & (CXXCharTypeOperator | CXXCharTypeNamedSingleOrOperatorToken))
		{
			t->eType = CXXTokenTypeOperator;
			do {
				vStringPut(t->pszWord,g_cxx.iChar);
				g_cxx.iChar = cppGetc();
				uInfo = UINFO(g_cxx.iChar);
			} while(
					uInfo &
						(CXXCharTypeOperator | CXXCharTypeNamedSingleOrOperatorToken)
				);
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

	if(uInfo & CXXCharTypeNamedSingleCharToken)
	{
		t->eType = g_aCharTable[g_cxx.iChar].uSingleTokenType;
		vStringPut(t->pszWord,g_cxx.iChar);
		g_cxx.iChar = cppGetc();
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

	if(uInfo & CXXCharTypeOperator)
	{
		t->eType = CXXTokenTypeOperator;
		vStringPut(t->pszWord,g_cxx.iChar);
		g_cxx.iChar = cppGetc();
		uInfo = UINFO(g_cxx.iChar);
		while(uInfo & CXXCharTypeOperator)
		{
			vStringPut(t->pszWord,g_cxx.iChar);
			g_cxx.iChar = cppGetc();
			uInfo = UINFO(g_cxx.iChar);
		}
		t->bFollowedBySpace = isspace(g_cxx.iChar);
		return TRUE;
	}

	t->eType = CXXTokenTypeUnknown;
	vStringPut(t->pszWord,g_cxx.iChar);
	g_cxx.iChar = cppGetc();
	t->bFollowedBySpace = isspace(g_cxx.iChar);

	return TRUE;
}
