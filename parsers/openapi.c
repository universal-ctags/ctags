/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*   Copyright (c) 2022, Vasily Kulikov
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Documentation on the schemas:
*   https://github.com/OAI/OpenAPI-Specification/tree/main/versions
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "kind.h"
#include "x-yaml.h"
#include "parse.h"
#include "subparser.h"
#include "read.h"


typedef enum {
	KIND_SCHEMA,
	KIND_PATH,
	KIND_RESPONSE,
	KIND_PARAMETER,
	KIND_TITLE,
	KIND_SERVER,
	KIND_TAG,
} openapiKind;

static kindDefinition OpenAPIKinds [] = {
	{ true, 'd', "schema", "schemas" },
	{ true, 'p', "path", "paths" },
	{ true, 'R', "response", "responses" },
	{ true, 'P', "parameter", "parameters" },
	{ true, 't', "title", "titles" },
	{ true, 's', "server", "servers (or hosts in swagger)" },
	{ true, 'T', "tag", "tags"},
};

struct sOpenAPISubparser {
	yamlSubparser yaml;
};

static tagYpathTable ypathTables [] = {
	{ "paths/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_PATH,      },
	{ "components/responses/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_RESPONSE,  },
	{ "responses/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_RESPONSE,  },
	{ "components/parameters/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_PARAMETER, },
	{ "parameters/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_PARAMETER, },
	{ "components/schemas/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_SCHEMA,    },
	{ "definitions/*",
	  YPATH_DSTAT_LAST_KEY,   KIND_SCHEMA,    },
	{ "info/title",
	  YPATH_DSTAT_LAST_VALUE, KIND_TITLE,     },
	{ "servers/*/url",
	  YPATH_DSTAT_LAST_VALUE, KIND_SERVER,    },
	{ "host",
	  YPATH_DSTAT_LAST_VALUE, KIND_SERVER,    },
	{ "tags/*/name",
	  YPATH_DSTAT_LAST_VALUE, KIND_TAG,       },
};

static void findOpenAPITags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* OpenAPIParser (void)
{
	static const char *const patterns [] = { "openapi.yaml", NULL };
	static struct sOpenAPISubparser openapiSubparser = {
		.yaml = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
			},
			.ypathTables = ypathTables,
			.ypathTableCount = ARRAY_SIZE (ypathTables),
		},
	};
	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Yaml", &openapiSubparser },
	};

	parserDefinition* const def = parserNew ("OpenAPI");

	def->patterns   = patterns;

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable	= OpenAPIKinds;
	def->kindCount = ARRAY_SIZE (OpenAPIKinds);
	def->parser	= findOpenAPITags;
	return def;
}
