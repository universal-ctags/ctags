#ifndef _cxxTag_h_
#define _cxxTag_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "kind.h"
#include "entry.h"

#include "cxx_token.h"

enum CXXTagKind
{
	CXXTagKindCLASS,
	CXXTagKindMACRO,
	CXXTagKindENUMERATOR,
	CXXTagKindFUNCTION,
	CXXTagKindENUM,
	CXXTagKindINCLUDE,
	CXXTagKindLOCAL,
	CXXTagKindMEMBER,
	CXXTagKindNAMESPACE,
	CXXTagKindPROTOTYPE,
	CXXTagKindSTRUCT,
	CXXTagKindTYPEDEF,
	CXXTagKindUNION,
	CXXTagKindVARIABLE,
	CXXTagKindEXTERNVAR,
	CXXTagKindPARAMETER,
	CXXTagKindLABEL,
	CXXTagKindNAME,
	CXXTagKindUSING
};

typedef enum _CXXTagCPPField
{
	CXXTagCPPFieldEndLine,
	CXXTagCPPFieldTemplate,
	CXXTagCPPFieldLambdaCaptureList,
	CXXTagCPPFieldProperties
} CXXTagCPPField;

typedef enum _CXXTagCField
{
	CXXTagCFieldEndLine,
	CXXTagCFieldProperties
} CXXTagCField;

fieldSpec * cxxTagGetCPPFieldSpecifiers(void);
int cxxTagGetCPPFieldSpecifierCount(void);
int cxxTagCPPFieldEnabled(CXXTagCPPField eField);

fieldSpec * cxxTagGetCFieldSpecifiers(void);
int cxxTagGetCFieldSpecifierCount(void);
int cxxTagCFieldEnabled(CXXTagCField eField);

kindOption * cxxTagGetKindOptions(void);
int cxxTagGetKindOptionCount(void);
boolean cxxTagKindEnabled(enum CXXTagKind eKindId);

// Begin composing a tag.
// Returns NULL if the tag should *not* be included in the output
// or the tag entry info that can be filled up with extension fields.
// Must be followed by cxxTagCommit() if it returns a non-NULL value.
// The pToken ownership is NOT transferred.
tagEntryInfo * cxxTagBegin(enum CXXTagKind eKindId,CXXToken * pToken);

// Set the type of the current tag from the specified token sequence
// (which must belong to the same chain!).
// Returns a token that must be destroyed after cxxTagCommit() has
// been called.
CXXToken * cxxTagSetTypeField(
		CXXToken * pTypeStart,
		CXXToken * pTypeEnd
	);

typedef enum _CXXTagProperty
{
	// Function is virtual
	CXXTagPropertyVirtual = 1,
	// Function/variable is static
	CXXTagPropertyStatic = (1 << 1),
	// Function is inline
	CXXTagPropertyInline = (1 << 2),
	// Function is explicit
	CXXTagPropertyExplicit = (1 << 3),
	// Function/variable is extern
	CXXTagPropertyExtern = (1 << 4),
	// Function is const
	CXXTagPropertyConst = (1 << 5),
	// Function is pure virtual
	CXXTagPropertyPure = (1 << 6),
	// Function is marked as override
	CXXTagPropertyOverride = (1 << 7),
	// Functoin is marked as default
	CXXTagPropertyDefault = (1 << 8),
	// Function is marked as final
	CXXTagPropertyFinal = (1 << 9),
	// Function is marked as delete
	CXXTagPropertyDelete = (1 << 10),
	// Variable is marked as mutable
	// (C++ treats "mutable" as storage class)
	CXXTagPropertyMutable = (1 << 11),
	// Function (note: NOT variable) is marked as volatile as in "int a() volatile"
	// (Because for variables it's treated as part of type)
	CXXTagPropertyVolatile = (1 << 12)
} CXXTagProperty;

// Set the modifiers field of the tag.
// Returns a string that you must destroy after the call to cxxTagCommit()
// or NULL if the modifiers weren't set for some reason (no modifiers, field
// not enabled or similar...)
vString * cxxTagSetProperties(unsigned int uProperties);

// Set a parser-local CPP field. The szValue pointer must persist
// until cxxTagCommit() is called.
void cxxTagSetCPPField(CXXTagCPPField eField,const char * szValue);

// Set a parser-local CPP field for a tag in cork queue.
// The szValue pointer is copied.
// Make sure that the field is enabled before calling this function.
void cxxTagSetCorkQueueCPPField(
		int iIndex,
		CXXTagCPPField eField,
		const char * szValue
	);

// Set a parser-local C field. The szValue pointer must persist
// until cxxTagCommit() is called.
void cxxTagSetCField(CXXTagCField eField,const char * szValue);

// Set a parser-local C field for a tag in cork queue.
// The szValue pointer is copied.
// Make sure that the field is enabled before calling this function.
void cxxTagSetCorkQueueCField(
		int iIndex,
		CXXTagCField eField,
		const char * szValue
	);


// Commit the composed tag. Must follow a succesfull cxxTagBegin() call.
// Returns the index of the tag in the cork queue.
int cxxTagCommit(void);

// Same as cxxTagBegin() eventually followed by cxxTagCommit()
void cxxTag(enum CXXTagKind eKindId,CXXToken * pToken);

typedef enum {
	CR_MACRO_UNDEF,
} cMacroRole;

typedef enum {
	CR_HEADER_SYSTEM,
	CR_HEADER_LOCAL,
} cHeaderRole;

#endif //!_cxxTag_h_
