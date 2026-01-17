/*
*
*   Copyright (c) 2023, Masatake YAMATO
*   Copyright (c) 2023, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for tagging input for I18n Ruby Gem.
*
*   References:
*   - https://guides.rubyonrails.org/i18n.html#setup-the-rails-application-for-internationalization
*   - https://github.com/ruby-i18n/i18n
*/

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include "x-yaml.h"

#include "entry.h"
#include "keyword.h"
#include "kind.h"
#include "numarray.h"
#include "read.h"
#include "parse.h"
#include "subparser.h"

/*
 *	 MACROS
 */

/*
 *	 DATA DECLARATIONS
 */

typedef enum {
	KIND_KEY,
	KIND_KEY_IN_MIDDLE,
	KIND_LOCALE,
} i18nRubyGemKind;

typedef enum {
	X_localeless,
	X_localeful,
} i18nRubyGemXtag;

struct sI18nRubyGemSubparser {
	yamlSubparser yaml;
	int depth;
	enum { LOCALE_NOTYET = -1, LOCALE_NONE, LOCALE_FOUND } localeFound;
	int corkIndex;
	intArray *keys;
};

enum {
	KEYWORD_LOCALE,
};

/*
 *	 FUNCTION DECLARATIONS
 */
static bool i18nRubyGemInitTagEntry(tagEntryInfo *e, yamlSubparser *yaml, char *key, void * data CTAGS_ATTR_UNUSED);

/*
 *	DATA DEFINITIONS
 */

static kindDefinition I18nRubyGemKind [] = {
	{ true,  'k', "key",         "translation keys at the leafs" },
	{ false, 'm', "keyInMiddle", "the middle component of keys"  },
	{ false, 'l', "locale",      "the root element representing a locale"},
};

static xtagDefinition I18nRubyGemXtagTable [] = {
	{
		.enabled = true,
		.name = "localeless",
		.description = "full qualifies translation key without locale component",
	},
	{
		.enabled = true,
		.name = "localeful",
		.description = "full qualifies translation key with locale component",
	},
};

static tagYpathTable ypathTables [] = {
#define E(PAT) { PAT, YPATH_DSTAT_LAST_KEY, KIND_GHOST_INDEX, \
		.initTagEntry = i18nRubyGemInitTagEntry, }

	/* TODO: This should be written in E("**"); */
	E("*"),
	E("*/*"),
	E("*/*/*"),
	E("*/*/*/*"),
	E("*/*/*/*/*"),
	E("*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*/*/*/*/*"),
	E("*/*/*/*/*/*/*/*/*/*/*/*/*/*/*"),
};

typedef int keywordId;
static const struct keywordGroup i18nRubyGemLocales;

static langType Lang_i18nrubygem;

/*
 *	 FUNCTION DEFINITIONS
 */
static bool isLocaleName (const char *key)
{
	return (lookupKeyword (key, Lang_i18nrubygem) == KEYWORD_LOCALE);
}

static bool i18nRubyGemInitTagEntry(tagEntryInfo *e, yamlSubparser *yaml, char *key, void * data CTAGS_ATTR_UNUSED)
{
	struct sI18nRubyGemSubparser *i18n = (struct sI18nRubyGemSubparser *)yaml;
	i18nRubyGemKind kindIndex = KIND_KEY;

	if (key[0] == ':')
		key = key + 1;

	if (i18n->localeFound == LOCALE_NOTYET)
	{
		int depth = (int)ypathGetTypeStackDepth(yaml);
		if (depth == 1)
		{
			if (isLocaleName (key))
			{
				kindIndex = KIND_LOCALE;
				i18n->localeFound = LOCALE_FOUND;
			}
			else
				i18n->localeFound = LOCALE_NONE;
		}
	}

	if (i18n->localeFound != LOCALE_FOUND)
		return false;

	initTagEntry (e, key, kindIndex);
	return true;
}

static void findI18nRubyGemTags (void)
{
	scheduleRunningBaseparser (0);
}

static void inputStart (subparser *s)
{
	struct sI18nRubyGemSubparser *i18n = (struct sI18nRubyGemSubparser *)s;
	i18n->depth = -1;
	i18n->corkIndex = CORK_NIL;
	i18n->keys = intArrayNew ();
	i18n->localeFound = LOCALE_NOTYET;
}

static int buildNameandReturnRoot (int corkIndex, vString *buf, int root)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);

	if (e)
	{
		int parent = e->extensionFields.scopeIndex;
		if (parent == CORK_NIL)
		{
			if (root == 0)
				vStringCatS(buf, e->name);
			return corkIndex;
		}

		int r = buildNameandReturnRoot (parent, buf, root);
		vStringJoinS (buf, '.', e->name);
		return r;
	}

	return CORK_NIL;
}

static void makeLocaleXTag (const tagEntryInfo *const e0, vString *buf, i18nRubyGemXtag xtag)
{
	tagEntryInfo e  = *e0;
	resetTagCorkState (&e, RESET_TAG_MEMBER_CLEAR, RESET_TAG_MEMBER_CLEAR);
	e.skipAutoFQEmission = 1;

	markTagExtraBit (&e, I18nRubyGemXtagTable[xtag].xtype);
	if (xtag == X_localeless)
		e.extensionFields.scopeIndex
			= buildNameandReturnRoot (e.extensionFields.scopeIndex,
									  buf, 1);
	else
		(void)buildNameandReturnRoot (e.extensionFields.scopeIndex,
									  buf, 0);

	vStringJoinS (buf, '.', e.name);
	e.name = vStringValue (buf);
	makeTagEntry (&e);
}

static void makeLocalefulXTag (const tagEntryInfo *const e0, vString *buf)
{
	makeLocaleXTag (e0, buf, X_localeful);
}

static void makeLocalelessXTag (const tagEntryInfo *const e0, vString *buf)
{
	makeLocaleXTag (e0, buf, X_localeless);
}

static void inputEnd (subparser *s)
{
	langType i18nrubygem = getInputLanguage ();
	struct sI18nRubyGemSubparser *i18n = (struct sI18nRubyGemSubparser *)s;
	size_t count = intArrayCount(i18n->keys);

	vString *buf = vStringNew ();
	for (unsigned int i = 0; i < count; i++)
	{
		tagEntryInfo *e = getEntryInCorkQueue(intArrayItem(i18n->keys, i));
		if (e->langType != i18nrubygem)
			continue;
		if (e->kindIndex == KIND_KEY_IN_MIDDLE || e->kindIndex == KIND_LOCALE)
			continue;

		if (isXtagEnabled (I18nRubyGemXtagTable[X_localeless].xtype))
		{
			makeLocalelessXTag (e, buf);
			vStringClear (buf);
		}

		if (isXtagEnabled (I18nRubyGemXtagTable[X_localeful].xtype))
		{
			makeLocalefulXTag (e, buf);
			vStringClear (buf);
		}
	}

	vStringDelete (buf);
	intArrayDelete (i18n->keys);
	i18n->keys = NULL;
}

static int getAncestor(int corkIndex, int upwardSteps)
{
	if (upwardSteps == 0)
		return corkIndex;

	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e == NULL)
		return corkIndex;

	return getAncestor (e->extensionFields.scopeIndex, upwardSteps - 1);
}

static void makeTagEntryNotifyViaYpath (yamlSubparser *s, int corkIndex)
{
	int currentDepth = (int)ypathGetTypeStackDepth(s);
	struct sI18nRubyGemSubparser *i18n = (struct sI18nRubyGemSubparser *)s;

	intArrayAdd (i18n->keys, corkIndex);
	if (i18n->depth < currentDepth)
	{
		tagEntryInfo *e = getEntryInCorkQueue(corkIndex);
		if (e)
			e->extensionFields.scopeIndex = i18n->corkIndex;

		tagEntryInfo *parent = getEntryInCorkQueue(i18n->corkIndex);
		if (parent && parent->kindIndex != KIND_LOCALE)
			parent->kindIndex = KIND_KEY_IN_MIDDLE;

		i18n->depth = currentDepth;
		i18n->corkIndex = corkIndex;
	}
	else if (i18n->depth == currentDepth)
	{
		tagEntryInfo *sibling = getEntryInCorkQueue(i18n->corkIndex);
		if (sibling)
		{
			tagEntryInfo *e = getEntryInCorkQueue(corkIndex);
			if (e)
				e->extensionFields.scopeIndex = sibling->extensionFields.scopeIndex;
			i18n->corkIndex = corkIndex;
		}
	}
	else
	{
		int lastCorkIndex = i18n->corkIndex;
		int lastDepth = i18n->depth;

		i18n->depth = currentDepth;
		i18n->corkIndex = corkIndex;

		tagEntryInfo *e = getEntryInCorkQueue(corkIndex);
		if (e)
		{
			int upwardSteps = lastDepth - currentDepth + 1;
			Assert(upwardSteps >= 0);
			e->extensionFields.scopeIndex = getAncestor (lastCorkIndex,
														 upwardSteps);
		}
	}
}

static void initializeI18nRubyGemTags (const langType language)
{
	Lang_i18nrubygem = language;
	addKeywordGroup (&i18nRubyGemLocales, language);
}

extern parserDefinition* I18nRubyGemParser (void)
{
	static struct sI18nRubyGemSubparser i18nRubyGemSubparser = {
		.yaml = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.inputStart = inputStart,
				.inputEnd = inputEnd,
			},
			.ypathTables = ypathTables,
			.ypathTableCount = ARRAY_SIZE (ypathTables),
			.makeTagEntryNotifyViaYpath = makeTagEntryNotifyViaYpath,
		},
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Yaml", &i18nRubyGemSubparser },
	};

	parserDefinition* const def = parserNew ("I18nRubyGem");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable	= I18nRubyGemKind;
	def->kindCount = ARRAY_SIZE (I18nRubyGemKind);
	def->xtagTable = I18nRubyGemXtagTable;
	def->xtagCount = ARRAY_SIZE (I18nRubyGemXtagTable);
	def->parser	= findI18nRubyGemTags;
	def->initialize = initializeI18nRubyGemTags;

	def->requestAutomaticFQTag = true;

	return def;
}



static const struct keywordGroup i18nRubyGemLocales = {
	.value = KEYWORD_LOCALE,
	.addingUnlessExisting = false,
	.keywords = {
		/* For generating this list, I did:
		 *
		 *	{ ls /usr/share/locale/ | xargs -n 1 printf '"%s",\n';
		 *	  ls /usr/share/locale/ | sed 's/_/-/g' | xargs -n 1 printf '"%s",\n' } \
		 *	  | sort | uniq; echo 'NULL'
		 *
		 *  on Fedora 39.
		 *
		 * rpm -qif /usr/share/locale
		 * Name        : filesystem
		 * Version     : 3.18
		 * Release     : 6.fc39
		 * Architecture: x86_64
		 * Install Date: Tue 05 Dec 2023 11:56:59 PM JST
		 * Group       : Unspecified
		 * Size        : 106
		 * License     : LicenseRef-Fedora-Public-Domain
		 * Signature   : RSA/SHA256, Sat 22 Jul 2023 12:38:40 AM JST, Key ID 75cf5ac418b8e74c
		 * Source RPM  : filesystem-3.18-6.fc39.src.rpm
		 * ...
		 * URL         : https://pagure.io/filesystem
		 * Bug URL     : https://bugz.fedoraproject.org/filesystem
		 * Summary     : The basic directory layout for a Linux system
		 * Description :
		 * The filesystem package is one of the basic packages that is installed
		 * on a Linux system. Filesystem contains the basic directory layout
		 * for a Linux operating system, including the correct permissions for
		 * the directories.
		 */
		"aa",
		"ab",
		"ace",
		"ach",
		"ada",
		"ady",
		"ae",
		"af",
		"afa",
		"afh",
		"af-ZA",
		"af_ZA",
		"agr",
		"ain",
		"ak",
		"akk",
		"ale",
		"alg",
		"aln",
		"alt",
		"am",
		"an",
		"ang",
		"anp",
		"apa",
		"ar",
		"arc",
		"ar-DZ",
		"ar_DZ",
		"ar-MA",
		"ar_MA",
		"arn",
		"arp",
		"ar-SY",
		"ar_SY",
		"art",
		"arw",
		"as",
		"ast",
		"ath",
		"aus",
		"av",
		"awa",
		"ay",
		"ayc",
		"aym",
		"az",
		"az-AZ",
		"az_AZ",
		"az-IR",
		"az_IR",
		"ba",
		"bad",
		"bai",
		"bal",
		"ban",
		"bar",
		"bas",
		"bat",
		"be",
		"bej",
		"be@latin",
		"bem",
		"ber",
		"bg",
		"bg-BG",
		"bg_BG",
		"bh",
		"bho",
		"bi",
		"bik",
		"bin",
		"bla",
		"bm",
		"bn",
		"bn-BD",
		"bn_BD",
		"bn-IN",
		"bn_IN",
		"bnt",
		"bo",
		"br",
		"bra",
		"brx",
		"bs",
		"bs-BA",
		"bs_BA",
		"btk",
		"bua",
		"bug",
		"byn",
		"ca",
		"ca-AD",
		"ca_AD",
		"cad",
		"ca-ES",
		"ca_ES",
		"ca-FR",
		"ca_FR",
		"cai",
		"ca-IT",
		"ca_IT",
		"cak",
		"car",
		"cau",
		"ca.us-ascii",
		"ca@valencia",
		"ce",
		"ceb",
		"cel",
		"cgg",
		"ch",
		"chb",
		"chg",
		"chk",
		"chm",
		"chn",
		"cho",
		"chp",
		"chr",
		"chy",
		"ckb",
		"cmc",
		"cmn",
		"cn",
		"cnr",
		"co",
		"cop",
		"cpe",
		"cpf",
		"cpp",
		"cr",
		"crh",
		"crp",
		"cs",
		"csb",
		"cs.cp1250",
		"cs-CZ",
		"cs_CZ",
		"cu",
		"cus",
		"cv",
		"cy",
		"da",
		"da-DK",
		"da_DK",
		"dak",
		"dar",
		"day",
		"de",
		"de-AT",
		"de_AT",
		"de-CH",
		"de_CH",
		"de-DE",
		"de_DE",
		"de@hebrew",
		"del",
		"den",
		"de.us-ascii",
		"dgr",
		"din",
		"doi",
		"dra",
		"dsb",
		"dua",
		"dum",
		"dv",
		"dyu",
		"dz",
		"ee",
		"efi",
		"egy",
		"eka",
		"el",
		"el-GR",
		"el_GR",
		"elx",
		"en",
		"en@arabic",
		"en-AU",
		"en_AU",
		"en@boldquot",
		"en-CA",
		"en_CA",
		"en@cyrillic",
		"en-CZ",
		"en_CZ",
		"en-FR",
		"en_FR",
		"en-GB",
		"en_GB",
		"en@greek",
		"en@hebrew",
		"en-HK",
		"en_HK",
		"en-IE",
		"en_IE",
		"enm",
		"en-NZ",
		"en_NZ",
		"en@piglatin",
		"en@quot",
		"en@shaw",
		"en-US",
		"en_US",
		"en-US@piglatin",
		"en_US@piglatin",
		"en-ZA",
		"en_ZA",
		"eo",
		"es",
		"es-AR",
		"es_AR",
		"es-CL",
		"es_CL",
		"es-CO",
		"es_CO",
		"es-CR",
		"es_CR",
		"es-DO",
		"es_DO",
		"es-EC",
		"es_EC",
		"es-ES",
		"es_ES",
		"es-EU",
		"es_EU",
		"es-GT",
		"es_GT",
		"es-HN",
		"es_HN",
		"es-MX",
		"es_MX",
		"es-NI",
		"es_NI",
		"es-PA",
		"es_PA",
		"es-PE",
		"es_PE",
		"es-PR",
		"es_PR",
		"es-PY",
		"es_PY",
		"es-SV",
		"es_SV",
		"es-US",
		"es_US",
		"es.us-ascii",
		"es-UY",
		"es_UY",
		"es-VE",
		"es_VE",
		"et",
		"et-EE",
		"et_EE",
		"eu",
		"eu-ES",
		"eu_ES",
		"ewo",
		"fa",
		"fa-AF",
		"fa_AF",
		"fa-IR",
		"fa_IR",
		"fan",
		"fat",
		"ff",
		"fi",
		"fi-FI",
		"fi_FI",
		"fil",
		"fiu",
		"fj",
		"fo",
		"fon",
		"fr",
		"fr-CA",
		"fr_CA",
		"fr-CH",
		"fr_CH",
		"fr-FR",
		"fr_FR",
		"frm",
		"fro",
		"frp",
		"frr",
		"frs",
		"fr.us-ascii",
		"fur",
		"fy",
		"ga",
		"gaa",
		"gay",
		"gba",
		"gd",
		"gem",
		"gez",
		"gil",
		"gl",
		"gl-ES",
		"gl_ES",
		"gmh",
		"gn",
		"goh",
		"gom",
		"gon",
		"gor",
		"gos",
		"got",
		"grb",
		"grc",
		"gsw",
		"gu",
		"guc",
		"gv",
		"gwi",
		"ha",
		"hai",
		"haw",
		"he",
		"he-IL",
		"he_IL",
		"hi",
		"hi-IN",
		"hi_IN",
		"hil",
		"him",
		"hit",
		"hmn",
		"hne",
		"ho",
		"hr",
		"hr-HR",
		"hr_HR",
		"hsb",
		"ht",
		"hu",
		"hu-HU",
		"hu_HU",
		"hup",
		"hus",
		"hy",
		"hy-AM",
		"hy_AM",
		"hye",
		"hz",
		"ia",
		"iba",
		"ibo",
		"id",
		"id-ID",
		"id_ID",
		"ie",
		"ig",
		"ii",
		"ijo",
		"ik",
		"ilo",
		"inc",
		"ine",
		"inh",
		"io",
		"ira",
		"iro",
		"is",
		"it",
		"it-IT",
		"it_IT",
		"iu",
		"ja",
		"ja.euc-jp",
		"ja-JP",
		"ja_JP",
		"jam",
		"jbo",
		"jpr",
		"jrb",
		"jv",
		"ka",
		"kaa",
		"kab",
		"kac",
		"ka-GE",
		"ka_GE",
		"kam",
		"kar",
		"kaw",
		"kbd",
		"kg",
		"kha",
		"khi",
		"kho",
		"ki",
		"kj",
		"kk",
		"kl",
		"km",
		"kmb",
		"km-KH",
		"km_KH",
		"kmr",
		"kn",
		"ko",
		"kok",
		"kok@latin",
		"ko-KR",
		"ko_KR",
		"kos",
		"kpe",
		"kr",
		"krc",
		"krl",
		"kro",
		"kru",
		"ks",
		"ks@aran",
		"ks@deva",
		"ks@devanagari",
		"ksw",
		"ku",
		"ku-IQ",
		"ku_IQ",
		"kum",
		"kut",
		"kv",
		"kw",
		"kw-GB",
		"kw_GB",
		"kw@kkcor",
		"kw@uccor",
		"ky",
		"l10n",
		"la",
		"lad",
		"lah",
		"lam",
		"lb",
		"lez",
		"lg",
		"li",
		"ln",
		"lo",
		"locale.alias",
		"lol",
		"lo-LA",
		"lo_LA",
		"loz",
		"lt",
		"ltg",
		"lt-LT",
		"lt_LT",
		"lu",
		"lua",
		"lui",
		"lun",
		"luo",
		"lus",
		"lv",
		"lv-LV",
		"lv_LV",
		"mad",
		"mag",
		"mai",
		"mak",
		"man",
		"map",
		"mas",
		"mdf",
		"mdr",
		"men",
		"mg",
		"mga",
		"mh",
		"mhr",
		"mi",
		"mic",
		"min",
		"mis",
		"mjw",
		"mk",
		"mkh",
		"mk-MK",
		"mk_MK",
		"ml",
		"ml-IN",
		"ml_IN",
		"mn",
		"mnc",
		"mni",
		"mni@beng",
		"mni@bengali",
		"mni@meiteimayek",
		"mnk",
		"mno",
		"mo",
		"moh",
		"mos",
		"mr",
		"mr-IN",
		"mr_IN",
		"ms",
		"ms-MY",
		"ms_MY",
		"mt",
		"mul",
		"mun",
		"mus",
		"mvo",
		"mwl",
		"mwr",
		"my",
		"my-MM",
		"my_MM",
		"myn",
		"myv",
		"na",
		"nah",
		"nai",
		"nan",
		"nap",
		"nb",
		"nb-NO",
		"nb_NO",
		"nd",
		"nds",
		"ne",
		"new",
		"ng",
		"nia",
		"nic",
		"niu",
		"nl",
		"nl-BE",
		"nl_BE",
		"nl-NL",
		"nl_NL",
		"nl.us-ascii",
		"nn",
		"nn-NO",
		"nn_NO",
		"no",
		"nog",
		"non",
		"no-NO",
		"no_NO",
		"no.us-ascii",
		"nqo",
		"nr",
		"nso",
		"nub",
		"nv",
		"nwc",
		"ny",
		"nym",
		"nyn",
		"nyo",
		"nzi",
		"oc",
		"oj",
		"om",
		"or",
		"or-IN",
		"or_IN",
		"os",
		"osa",
		"ota",
		"oto",
		"pa",
		"paa",
		"pag",
		"pal",
		"pam",
		"pap",
		"pa-PK",
		"pa_PK",
		"pau",
		"pbs",
		"peo",
		"phi",
		"phn",
		"pi",
		"pis",
		"pl",
		"pl-PL",
		"pl_PL",
		"pms",
		"pon",
		"pra",
		"pro",
		"ps",
		"pt",
		"pt-BR",
		"pt_BR",
		"pt-BR.us-ascii",
		"pt_BR.us-ascii",
		"pt-PT",
		"pt_PT",
		"pt.us-ascii",
		"qaa-qtz",
		"qu",
		"quy",
		"quz",
		"raj",
		"rap",
		"rar",
		"rm",
		"rn",
		"ro",
		"roa",
		"rom",
		"ro-MD",
		"ro_MD",
		"ro-RO",
		"ro_RO",
		"ru",
		"rue",
		"rup",
		"ru-RU",
		"ru_RU",
		"ru-RU.KOI8-R",
		"ru_RU.KOI8-R",
		"rw",
		"sa",
		"sad",
		"sah",
		"sai",
		"sal",
		"sam",
		"sas",
		"sat",
		"sat@deva",
		"sat@olchiki",
		"sc",
		"scn",
		"sco",
		"sd",
		"sd@deva",
		"se",
		"sel",
		"sem",
		"sg",
		"sga",
		"sgn",
		"shn",
		"shs",
		"si",
		"sid",
		"si-LK",
		"si_LK",
		"sio",
		"sit",
		"sk",
		"sk.cp1250",
		"sk-SK",
		"sk_SK",
		"sl",
		"sla",
		"sl-SI",
		"sl_SI",
		"sm",
		"sma",
		"smi",
		"smj",
		"smn",
		"sms",
		"sn",
		"snk",
		"so",
		"sog",
		"son",
		"sp",
		"sq",
		"sq-AL",
		"sq_AL",
		"sr",
		"sr-Cyrl",
		"sr_Cyrl",
		"srd",
		"sr@ije",
		"sr@ijekavian",
		"sr@ijekavianlatin",
		"sr@latin",
		"sr-Latn",
		"sr@Latn",
		"sr_Latn",
		"sr-ME",
		"sr_ME",
		"srn",
		"srr",
		"sr-RS",
		"sr_RS",
		"sr-RS@latin",
		"sr_RS@latin",
		"ss",
		"ssa",
		"st",
		"su",
		"suk",
		"sus",
		"sux",
		"sv",
		"sv-SE",
		"sv_SE",
		"sw",
		"syc",
		"syr",
		"szl",
		"ta",
		"tai",
		"ta-IN",
		"ta_IN",
		"ta-LK",
		"ta_LK",
		"te",
		"tem",
		"ter",
		"tet",
		"tg",
		"th",
		"th-TH",
		"th_TH",
		"ti",
		"tig",
		"tiv",
		"tk",
		"tkl",
		"tl",
		"tlh",
		"tli",
		"tl-PH",
		"tl_PH",
		"tmh",
		"tn",
		"to",
		"tog",
		"tok",
		"ton",
		"tpi",
		"tr",
		"tr-TR",
		"tr_TR",
		"ts",
		"tsi",
		"tt",
		"tt@iqtelif",
		"tt-RU",
		"tt_RU",
		"tum",
		"tup",
		"tut",
		"tvl",
		"tw",
		"ty",
		"tyv",
		"tzm",
		"tzo",
		"ua",
		"udm",
		"ug",
		"uga",
		"uk",
		"uk-UA",
		"uk_UA",
		"umb",
		"und",
		"ur",
		"ur-PK",
		"ur_PK",
		"uz",
		"uz@cyrillic",
		"uz@Cyrl",
		"uz@Latn",
		"vai",
		"ve",
		"vec",
		"ven",
		"vi",
		"vi-VN",
		"vi_VN",
		"vo",
		"vot",
		"wa",
		"wae",
		"wak",
		"wal",
		"war",
		"was",
		"wba",
		"wen",
		"wo",
		"xal",
		"xh",
		"yao",
		"yap",
		"yi",
		"yo",
		"ypk",
		"yue",
		"za",
		"zam",
		"zap",
		"zbl",
		"zen",
		"zgh",
		"zh",
		"zh-CN",
		"zh_CN",
		"zh-CN.GB2312",
		"zh_CN.GB2312",
		"zh-Hans",
		"zh_Hans",
		"zh-Hans-CN",
		"zh_Hans_CN",
		"zh-Hant",
		"zh_Hant",
		"zh-Hant-TW",
		"zh_Hant_TW",
		"zh-HK",
		"zh_HK",
		"zh-TW",
		"zh_TW",
		"zh-TW.Big5",
		"zh_TW.Big5",
		"znd",
		"zu",
		"zun",
		"zxx",
		"zza",
		NULL
	},
};
