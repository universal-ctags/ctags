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

// Tag kinds common to all (sub)languages this parser supports
enum CXXTagCommonKind
{
	CXXTagKindMACRO,
	CXXTagKindENUMERATOR,
	CXXTagKindFUNCTION,
	CXXTagKindENUM,
	CXXTagKindINCLUDE,
	CXXTagKindLOCAL,
	CXXTagKindMEMBER,
	CXXTagKindPROTOTYPE,
	CXXTagKindSTRUCT,
	CXXTagKindTYPEDEF,
	CXXTagKindUNION,
	CXXTagKindVARIABLE,
	CXXTagKindEXTERNVAR,
	CXXTagKindPARAMETER,
	CXXTagKindLABEL,

	CXXTagCommonKindCount
};

// Tags specific to the CPP language.
enum CXXTagCPPKind
{
	CXXTagCPPKindCLASS = CXXTagCommonKindCount,
	CXXTagCPPKindNAMESPACE,
	CXXTagCPPKindALIAS,
	CXXTagCPPKindNAME,
	CXXTagCPPKindUSING
};

// The fields common to all (sub)languages this parser supports.
enum CXXTagCommonField
{
	CXXTagFieldProperties,
	
	CXXTagCommonFieldCount
};

// The fields specific to the CPP language.
enum CXXTagCPPField
{
	CXXTagCPPFieldTemplate = CXXTagCommonFieldCount,
	CXXTagCPPFieldLambdaCaptureList,
	CXXTagCPPFieldAliasedName
};


fieldSpec * cxxTagGetCPPFieldSpecifiers(void);
int cxxTagGetCPPFieldSpecifierCount(void);

fieldSpec * cxxTagGetCFieldSpecifiers(void);
int cxxTagGetCFieldSpecifierCount(void);

boolean cxxTagFieldEnabled(unsigned int uField);

kindOption * cxxTagGetCKindOptions(void);
int cxxTagGetCKindOptionCount(void);

kindOption * cxxTagGetCPPKindOptions(void);
int cxxTagGetCPPKindOptionCount(void);

// Returns true if the specified tag kind is enabled in the current language
boolean cxxTagKindEnabled(unsigned int uTagKind);

// Begin composing a tag. The tag kind must correspond to the current language.
// Returns NULL if the tag should *not* be included in the output
// or the tag entry info that can be filled up with extension fields.
// Must be followed by cxxTagCommit() if it returns a non-NULL value.
// The pToken ownership is NOT transferred.
tagEntryInfo * cxxTagBegin(unsigned int uKind,CXXToken * pToken);

// Set the type of the current tag from the specified token sequence
// (which must belong to the same chain!).
// Before setting the type this function will check that the specified
// range of tokens looks reasonable for a type name and if it looks
// suspicious will refuse to emit it.
// If the type is effectively set then the return value is a token that must
// be destroyed after cxxTagCommit() has been called.
CXXToken * cxxTagCheckAndSetTypeField(
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
	CXXTagPropertyVolatile = (1 << 12),
	// Template specialization a<x>()
	CXXTagPropertyTemplateSpecialization = (1 << 13),
	// Template specialization of scope a<x>::b() (which implies TemplateSpec too)
	CXXTagPropertyScopeTemplateSpecialization = (1 << 14),
	// __attribute__((deprecated)) has been seen
	CXXTagPropertyDeprecated = (1 << 15),
} CXXTagProperty;

// Set the modifiers field of the tag.
// Returns a string that you must destroy after the call to cxxTagCommit()
// or NULL if the modifiers weren't set for some reason (no modifiers, field
// not enabled or similar...)
vString * cxxTagSetProperties(unsigned int uProperties);

// Set a parser-local field. The szValue pointer must persist
// until cxxTagCommit() is called.
void cxxTagSetField(unsigned int uField,const char * szValue);

// Set a parser-local CPP field for a tag in cork queue.
// The szValue pointer is copied.
// Make sure that the field is enabled before calling this function.
void cxxTagSetCorkQueueField(
		int iIndex,
		unsigned int uField,
		const char * szValue
	);

// Commit the composed tag. Must follow a succesfull cxxTagBegin() call.
// Returns the index of the tag in the cork queue.
int cxxTagCommit(void);

// Same as cxxTagBegin() eventually followed by cxxTagCommit()
void cxxTag(unsigned int uKind,CXXToken * pToken);

typedef enum {
	CR_MACRO_UNDEF,
} cMacroRole;

typedef enum {
	CR_HEADER_SYSTEM,
	CR_HEADER_LOCAL,
} cHeaderRole;

// Initialize the parser state for the specified language.
// Must be called before attempting to access the kind options.
void cxxTagInitForLanguage(langType eLanguage);

#endif //!_cxxTag_h_
