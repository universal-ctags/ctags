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
} openapiKind;

static kindDefinition OpenAPIKinds [] = {
	{ true, 'd', "schema", "schemas" },
	{ true, 'p', "path", "paths" },
	{ true, 'R', "response", "responses" },
	{ true, 'P', "parameter", "parameters" },
	{ true, 't', "title", "titles" },
	{ true, 's', "server", "servers (or hosts in swagger)" },
};

#define KEY_UNKNOWN KEYWORD_NONE
enum openapiKeys {
	KEY_PATHS,
	KEY_COMPONENTS,
	KEY_SCHEMAS,
	KEY_PARAMETERS,
	KEY_RESPONSES,
	KEY_DEFINITIONS,
	KEY_INFO,
	KEY_TITLE,
	KEY_SERVERS,
	KEY_URL,
	KEY_HOST,
};

static const keywordTable OpenAPIKeywordTable[] = {
	{ "paths",       KEY_PATHS       },
	{ "components",  KEY_COMPONENTS  },
	{ "schemas",     KEY_SCHEMAS     },
	{ "parameters",  KEY_PARAMETERS  },
	{ "responses",   KEY_RESPONSES   },
	{ "definitions", KEY_DEFINITIONS },
	{ "info",        KEY_INFO },
	{ "title",       KEY_TITLE },
	{ "servers",     KEY_SERVERS },
	{ "url",         KEY_URL },
	{ "host",        KEY_HOST },
};

struct yamlBlockTypeStack {
	yaml_token_type_t type;
	enum openapiKeys key;
	struct yamlBlockTypeStack *next;
};

/* - name: "THE NAME" */
enum openapiPlayDetectingState {
	DSTAT_LAST_KEY,
	DSTAT_LAST_VALUE,
	DSTAT_INITIAL,
};


struct sOpenAPISubparser {
	yamlSubparser yaml;
	struct yamlBlockTypeStack *type_stack;
	enum openapiPlayDetectingState play_detection_state;
};

static void pushBlockType (struct sOpenAPISubparser *openapi, yaml_token_type_t t)
{
	struct yamlBlockTypeStack *s;

	s = xMalloc (1, struct yamlBlockTypeStack);

	s->next = openapi->type_stack;
	openapi->type_stack = s;

	s->type = t;
	s->key = KEY_UNKNOWN;
}

static void popBlockType (struct sOpenAPISubparser *openapi)
{
	struct yamlBlockTypeStack *s;

	s = openapi->type_stack;
	openapi->type_stack = s->next;

	s->next = NULL;

	eFree (s);
}

static void popAllBlockType (struct sOpenAPISubparser *openapi)
{
	while (openapi->type_stack)
		popBlockType (openapi);
}

static bool stateStackMatch (struct yamlBlockTypeStack *stack,
							 const enum openapiKeys *expectation,
							 unsigned int length)
{
	if (length == 0)
	{
		if (stack == NULL)
			return true;
		else
			return false;
	}

	if (stack == NULL)
		return false;

	if (stack->key == expectation[0])
		return stateStackMatch (stack->next, expectation + 1, length - 1);
	else
		return false;
}

static enum openapiKeys parseKey(yaml_token_t *token)
{
	static langType langType = LANG_IGNORE;
	if (langType == LANG_IGNORE)
		langType = getInputLanguage ();

	return lookupKeyword ((char *)token->data.scalar.value, langType);
}

#ifdef DO_TRACING
static void printStack(struct yamlBlockTypeStack* stack)
{
	if (!stack)
	{
		TRACE_PRINT_NEWLINE();
		return;
	}

	tracePrintFmt("[%d] - ", stack->key);
	printStack(stack->next);
}
#endif

struct tagSource {
	openapiKind kind;
	const enum openapiKeys* keys;
	size_t countKeys;
};

static const enum openapiKeys pathKeys[] = {
	KEY_UNKNOWN,
	KEY_PATHS,
};

static const enum openapiKeys responses3Keys[] = {
	KEY_UNKNOWN,
	KEY_RESPONSES,
	KEY_COMPONENTS,
};

static const enum openapiKeys responses2Keys[] = {
	KEY_UNKNOWN,
	KEY_RESPONSES,
};

static const enum openapiKeys parameters3Keys[] = {
	KEY_UNKNOWN,
	KEY_PARAMETERS,
	KEY_COMPONENTS,
};

static const enum openapiKeys parameters2Keys[] = {
	KEY_UNKNOWN,
	KEY_PARAMETERS,
};

static const enum openapiKeys schemas3Keys[] = {
	KEY_UNKNOWN,
	KEY_SCHEMAS,
	KEY_COMPONENTS,
};

static const enum openapiKeys definitions2Keys[] = {
	KEY_UNKNOWN,
	KEY_DEFINITIONS,
};

static const enum openapiKeys title3Keys[] = {
	KEY_TITLE,
	KEY_INFO,
};

static const enum openapiKeys server3Keys[] = {
	KEY_URL,
	KEY_UNKNOWN,
	KEY_SERVERS,
};

static const enum openapiKeys host2Keys[] = {
	KEY_HOST,
};

const struct tagSource tagSources[] = {
	{
		KIND_PATH,
		pathKeys,
		ARRAY_SIZE (pathKeys),
	},
	{
		KIND_RESPONSE,
		responses3Keys,
		ARRAY_SIZE (responses3Keys),
	},
	{
		KIND_RESPONSE,
		responses2Keys,
		ARRAY_SIZE (responses2Keys),
	},
	{
		KIND_PARAMETER,
		parameters3Keys,
		ARRAY_SIZE (parameters3Keys),
	},
	{
		KIND_PARAMETER,
		parameters2Keys,
		ARRAY_SIZE (parameters2Keys),
	},
	{
		KIND_SCHEMA,
		schemas3Keys,
		ARRAY_SIZE (schemas3Keys),
	},
	{
		KIND_SCHEMA,
		definitions2Keys,
		ARRAY_SIZE (definitions2Keys),
	},
};

const struct tagSource tagValueSources[] = {
	{
		KIND_TITLE,
		title3Keys,
		ARRAY_SIZE (title3Keys),
	},
	{
		KIND_SERVER,
		server3Keys,
		ARRAY_SIZE (server3Keys),
	},
	{
		KIND_SERVER,
		host2Keys,
		ARRAY_SIZE (host2Keys),
	}
};

static void handleToken(struct sOpenAPISubparser *openapi, yaml_token_t *token,
						const struct tagSource *tss, size_t ts_count)
{
	for (int i = 0; i < ts_count; i++)
	{
		const struct tagSource* ts = &tss[i];

		if (stateStackMatch(openapi->type_stack,
							ts->keys, ts->countKeys))
		{

			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value, ts->kind);
			attachYamlPosition (&tag, token, false);

			makeTagEntry (&tag);
			break;
		}
	}
}

static void handleKey(struct sOpenAPISubparser *openapi,
					  yaml_token_t *token)
{
	handleToken (openapi, token, tagSources, ARRAY_SIZE (tagSources));
}

static void handleValue(struct sOpenAPISubparser *openapi,
						yaml_token_t *token)
{
	handleToken (openapi, token, tagValueSources, ARRAY_SIZE (tagValueSources));
}

static void	openapiPlayStateMachine (struct sOpenAPISubparser *openapi,
									 yaml_token_t *token)
{
#ifdef DO_TRACING
	printStack(openapi->type_stack);
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
			TRACE_PRINT("  key: %s", (char*)token->data.scalar.value);
			if (openapi->type_stack)
			{
				openapi->type_stack->key = parseKey(token);
				handleKey (openapi, token);
			}
			break;
		case DSTAT_LAST_VALUE:
			TRACE_PRINT("  value: %s", (char*)token->data.scalar.value);
			if (openapi->type_stack)
				handleValue (openapi, token);
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
		pushBlockType ((struct sOpenAPISubparser *)s, token->type);

	openapiPlayStateMachine ((struct sOpenAPISubparser *)s, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		popBlockType ((struct sOpenAPISubparser *)s);
	else if (token->type == YAML_STREAM_END_TOKEN)
		popAllBlockType ((struct sOpenAPISubparser *)s);
}

static void inputStart(subparser *s)
{
	((struct sOpenAPISubparser*)s)->play_detection_state = DSTAT_INITIAL;
	((struct sOpenAPISubparser*)s)->type_stack = NULL;
}

static void inputEnd(subparser *s)
{
	Assert (((struct sOpenAPISubparser*)s)->type_stack == NULL);
}

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

	def->keywordTable = OpenAPIKeywordTable;
	def->keywordCount = ARRAY_SIZE (OpenAPIKeywordTable);

	def->kindTable	= OpenAPIKinds;
	def->kindCount = ARRAY_SIZE (OpenAPIKinds);
	def->parser	= findOpenAPITags;
	return def;
}
