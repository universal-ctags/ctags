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
#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "yaml.h"
#include "parse.h"
#include "subparser.h"
#include "keyword.h"
#include "read.h"
#include "trace.h"


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

/* - name: "THE NAME" */
enum openapiPlayDetectingState {
	DSTAT_LAST_KEY,
	DSTAT_LAST_VALUE,
	DSTAT_INITIAL,
};


struct sOpenAPISubparser {
	yamlSubparser yaml;
	enum openapiPlayDetectingState play_detection_state;
};

static tagYpathTable ypathTables [] = {
	{ "paths/*",
	  DSTAT_LAST_KEY,   KIND_PATH,      },
	{ "components/responses/*",
	  DSTAT_LAST_KEY,   KIND_RESPONSE,  },
	{ "responses/*",
	  DSTAT_LAST_KEY,   KIND_RESPONSE,  },
	{ "components/parameters/*",
	  DSTAT_LAST_KEY,   KIND_PARAMETER, },
	{ "parameters/*",
	  DSTAT_LAST_KEY,   KIND_PARAMETER, },
	{ "components/schemas/*",
	  DSTAT_LAST_KEY,   KIND_SCHEMA,    },
	{ "definitions/*",
	  DSTAT_LAST_KEY,   KIND_SCHEMA,    },
	{ "info/title",
	  DSTAT_LAST_VALUE, KIND_TITLE,     },
	{ "servers/*/url",
	  DSTAT_LAST_VALUE, KIND_SERVER,    },
	{ "host",
	  DSTAT_LAST_VALUE, KIND_SERVER,    },
	{ "tags/*/name",
	  DSTAT_LAST_VALUE, KIND_TAG,       },
};

static void	openapiPlayStateMachine (struct sOpenAPISubparser *openapi,
									 yaml_token_t *token)
{
#ifdef DO_TRACING
	ypathPrintTypeStack (YAML(openapi));
#endif

	switch (token->type)
	{
	case YAML_KEY_TOKEN:
		openapi->play_detection_state = DSTAT_LAST_KEY;
		break;
	case YAML_SCALAR_TOKEN:
		switch (openapi->play_detection_state)
		{
		case DSTAT_LAST_KEY:
			ypathFillKeywordOfTokenMaybe (YAML(openapi), token, getInputLanguage ());
			/* FALL THROUGH */
		case DSTAT_LAST_VALUE:
			TRACE_PRINT("token-callback: %s: %s",
						(openapi->play_detection_state == DSTAT_LAST_KEY)? "key": "value",
						(char*)token->data.scalar.value);
			ypathHandleToken (YAML(openapi), token, openapi->play_detection_state,
							  ypathTables, ARRAY_SIZE (ypathTables));
			break;
		default:
			break;
		}

		openapi->play_detection_state = DSTAT_INITIAL;

		break;
	case YAML_VALUE_TOKEN:
		openapi->play_detection_state = DSTAT_LAST_VALUE;
		break;

	default:
		openapi->play_detection_state = DSTAT_INITIAL;
		break;
	}
}

static void newTokenCallback (yamlSubparser *s, yaml_token_t *token)
{
	if (token->type == YAML_BLOCK_SEQUENCE_START_TOKEN
		|| token->type == YAML_BLOCK_MAPPING_START_TOKEN)
		ypathPushType (s, token);

	openapiPlayStateMachine ((struct sOpenAPISubparser *)s, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		ypathPopType (s);
	else if (token->type == YAML_STREAM_END_TOKEN)
		ypathPopAllTypes (s);
}

static void inputStart(subparser *s)
{
	((struct sOpenAPISubparser*)s)->play_detection_state = DSTAT_INITIAL;
	((yamlSubparser*)s)->ypathTypeStack = NULL;
}

static void inputEnd(subparser *s)
{
	Assert (((yamlSubparser*)s)->ypathTypeStack == NULL);
}

static void findOpenAPITags (void)
{
	scheduleRunningBaseparser (0);
}

static void initialize (langType language)
{
	ypathCompileTables (language, ypathTables, ARRAY_SIZE (ypathTables), 0);
}

static void finalize (langType language, bool initialized)
{
	if (initialized)
		ypathCompiledCodeDelete (ypathTables, ARRAY_SIZE (ypathTables));
}

extern parserDefinition* OpenAPIParser (void)
{
	static const char *const patterns [] = { "openapi.yaml", NULL };
	static struct sOpenAPISubparser openapiSubparser = {
		.yaml = {
			.subparser = {
				.direction = SUBPARSER_BI_DIRECTION,
				.inputStart = inputStart,
				.inputEnd = inputEnd,
			},
			.newTokenNotfify = newTokenCallback
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
	def->initialize = initialize;
	def->finalize = finalize;
	return def;
}
