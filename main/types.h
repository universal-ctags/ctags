/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Private definitions for parsing support.
*/

#ifndef CTAGS_MAIN_TYPES_H
#define CTAGS_MAIN_TYPES_H

typedef int langType;

struct sTagEntryInfo;
typedef struct sTagEntryInfo tagEntryInfo;

struct sPtagDesc;
typedef struct sPtagDesc ptagDesc;

struct sRoleDefinition;
typedef struct sRoleDefinition roleDefinition;

struct sKindDefinition;
typedef struct sKindDefinition kindDefinition;

struct sParserDefinition;
typedef struct sParserDefinition parserDefinition;

struct _MIO;
typedef const char * (*selectLanguage) (struct _MIO *, langType *, unsigned int);

struct sSlaveParser;
typedef struct sSlaveParser slaveParser;

struct sSubparser;
typedef struct sSubparser subparser;

struct sParserDependency;
typedef struct sParserDependency parserDependency;

struct sFieldDefinition;
typedef struct sFieldDefinition fieldDefinition;

struct sXtagDefinition;
typedef struct sXtagDefinition xtagDefinition;

struct sParameterHandlerTable;
typedef struct sParameterHandlerTable parameterHandlerTable;

#endif	/* CTAGS_MAIN_TYPES_H */
