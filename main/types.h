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

struct sFieldDesc;
typedef struct sFieldDesc fieldDesc;

struct sPtagDesc;
typedef struct sPtagDesc ptagDesc;

struct sKindOption;
typedef struct sKindOption kindOption;

struct sParserDefinition;
typedef struct sParserDefinition parserDefinition;
#endif	/* CTAGS_MAIN_TYPES_H */
