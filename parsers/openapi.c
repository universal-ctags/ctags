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

#include <stdio.h>

typedef enum {
	KIND_SCHEMA,
	KIND_PATH,
	KIND_RESPONSE,
	KIND_PARAMETER,
	KIND_INFO,
	KIND_SERVER,
	KIND_SECURITY,
	KIND_TAG,
	KIND_EXAMPLE,
	KIND_EXTERNAL_DOCS,
	KIND_REQUEST_BODY,
	KIND_HEADER,
	KIND_SECURITY_SCHEME,
	KIND_LINK,
	KIND_CALLBACK,
	KIND_PATH_ITEM,
} openapiKind;

static kindDefinition OpenApiKinds [] = {
	{ true, 'd', "schema", "schemas" },
	{ true, 'p', "path", "paths" },
	{ true, 'R', "response", "responses" },
	{ true, 'P', "parameter", "parameters" },
	{ true, 'I', "info", "info"},
	{ true, 'S', "server", "servers"},
	{ true, 's', "security", "security"},
	{ true, 't', "tag", "tags"},
	{ true, 'e', "example", "examples"},
	{ true, 'D', "doc", "docs"},
	{ true, 'B', "requestBody", "requestBodies"},
	{ true, 'h', "header", "headers"},
	{ true, 'C', "securityScheme", "securitySchemes"},
	{ true, 'l', "link", "links"},
	{ true, 'c', "callback", "callbacks"},
	{ true, 'A', "pathItem", "pathItems"},
};

enum openapiKeys {
	KEY_UNKNOWN, // 0
	KEY_PATHS,
	KEY_COMPONENTS,
	KEY_SCHEMAS,
	KEY_PARAMETERS,
	KEY_RESPONSES, //5
	KEY_DEFINITIONS,
	KEY_INFO,
	KEY_SERVERS,
	KEY_SECURITY,
	KEY_TAG, // 10
	KEY_EXTERNAL_DOCS,
	KEY_NAME,
	KEY_URL,
	KEY_TYPE,
	KEY_EXAMPLES, // 15
	KEY_REQUEST_BODIES,
	KEY_HEADERS,
	KEY_SECURITY_SCHEMES,
	KEY_LINKS,
	KEY_CALLBACKS, // 20
	KEY_PATH_ITEMS,
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


struct sOpenApiSubparser {
	yamlSubparser yaml;
	struct yamlBlockTypeStack *type_stack;
	enum openapiPlayDetectingState play_detection_state;
};

static void pushBlockType (struct sOpenApiSubparser *openapi, yaml_token_type_t t)
{
	struct yamlBlockTypeStack *s;

	s = xMalloc (1, struct yamlBlockTypeStack);

	s->next = openapi->type_stack;
	openapi->type_stack = s;

	s->type = t;
	s->key = KEY_UNKNOWN;
}

static void popBlockType (struct sOpenApiSubparser *openapi)
{
	struct yamlBlockTypeStack *s;

	s = openapi->type_stack;
	openapi->type_stack = s->next;

	s->next = NULL;

	eFree (s);
}

static void popAllBlockType (struct sOpenApiSubparser *openapi)
{
	while (openapi->type_stack)
		popBlockType (openapi);
}

static bool stateStackMatch (struct yamlBlockTypeStack *stack,
							 const enum openapiKeys *expectation,
							 unsigned int length
							 )
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

static bool scalarNeq (yaml_token_t *token, unsigned int len, const char* val)
{
	if ((token->data.scalar.length == len)
		&& (strncmp (val, (char *)token->data.scalar.value, len) == 0))
		return true;
	else
		return false;
}

static enum openapiKeys parseKey(yaml_token_t *token)
{
	if (scalarNeq(token, 10, "components"))
		return KEY_COMPONENTS;
	else if (scalarNeq(token, 7, "schemas"))
		return KEY_SCHEMAS;
	else if (scalarNeq(token, 5, "paths"))
		return KEY_PATHS;
	else if (scalarNeq(token, 11, "definitions"))
		return KEY_DEFINITIONS;
	else if (scalarNeq(token, 10, "parameters"))
		return KEY_PARAMETERS;
	else if (scalarNeq(token, 9, "responses"))
		return KEY_RESPONSES;
	else if (scalarNeq(token, 4, "info"))
		return KEY_INFO;
	else if (scalarNeq(token, 7, "servers"))
		return KEY_SERVERS;
	else if (scalarNeq(token, 8, "security"))
		return KEY_SECURITY;
	else if (scalarNeq(token, 4, "tags"))
		return KEY_TAG;
	else if (scalarNeq(token, 12, "externalDocs"))
		return KEY_EXTERNAL_DOCS;
	else if (scalarNeq(token, 3, "url"))
		return KEY_URL;
	else if (scalarNeq(token, 4, "name"))
		return KEY_NAME;
	else if (scalarNeq(token, 4, "type"))
		return KEY_TYPE;
	else if (scalarNeq(token, 8, "examples"))
		return KEY_EXAMPLES;
	else if (scalarNeq(token, 13, "requestBodies"))
		return KEY_REQUEST_BODIES;
	else if (scalarNeq(token, 5, "links"))
		return KEY_LINKS;
	else if (scalarNeq(token, 9, "pathItems"))
		return KEY_PATH_ITEMS;
	else if (scalarNeq(token, 9, "callbacks"))
		return KEY_CALLBACKS;
	else if (scalarNeq(token, 7, "headers"))
		return KEY_HEADERS;
	else if (scalarNeq(token, 15, "securitySchemes"))
		return KEY_SECURITY_SCHEMES;
	else
		return KEY_UNKNOWN;
}

static void printStack(struct yamlBlockTypeStack* stack)
{
  if (!stack)
  {
	  puts("");
	  return;
  }

  printf("[%d] - ", stack->key);
  printStack(stack->next);
}


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

static const enum openapiKeys links3Keys[] = {
	KEY_UNKNOWN,
	KEY_LINKS,
	KEY_COMPONENTS,
};

static const enum openapiKeys callbacks3Keys[] = {
	KEY_UNKNOWN,
	KEY_CALLBACKS,
	KEY_COMPONENTS,
};

static const enum openapiKeys pathItems3Keys[] = {
	KEY_UNKNOWN,
	KEY_PATH_ITEMS,
	KEY_COMPONENTS,
};

static const enum openapiKeys securitySchemes3Keys[] = {
	KEY_UNKNOWN,
	KEY_SECURITY_SCHEMES,
	KEY_COMPONENTS,
};

static const enum openapiKeys headers3Keys[] = {
	KEY_UNKNOWN,
	KEY_HEADERS,
	KEY_COMPONENTS,
};

static const enum openapiKeys requestBodies3Keys[] = {
	KEY_UNKNOWN,
	KEY_REQUEST_BODIES,
	KEY_COMPONENTS,
};

static const enum openapiKeys examples3Keys[] = {
	KEY_UNKNOWN,
	KEY_EXAMPLES,
	KEY_COMPONENTS,
};

static const enum openapiKeys info3Keys[] = {
	KEY_UNKNOWN,
	KEY_INFO,
};

static const enum openapiKeys server3Keys[] = {
	KEY_URL,
	KEY_UNKNOWN,
	KEY_SERVERS,
};

// The tag "type" is used as a poor man's tag title.
// That's because "name" can be set with type=apiKey only :(
static const enum openapiKeys security3Keys[] = {
	KEY_UNKNOWN,
	KEY_UNKNOWN,
	KEY_SECURITY,
};

static const enum openapiKeys tags3Keys[] = {
	KEY_NAME,
	KEY_UNKNOWN,
	KEY_TAG,
};

static const enum openapiKeys externalDocs3Keys[] = {
	KEY_URL,
	KEY_EXTERNAL_DOCS,
};

const struct tagSource tagSources[] = {
	{
		KIND_INFO,
		info3Keys,
		ARRAY_SIZE (info3Keys),
	},
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
	// 5:
	{
		KIND_PARAMETER,
		parameters2Keys,
		ARRAY_SIZE (parameters2Keys),
	},
	{
		KIND_EXAMPLE,
		examples3Keys,
		ARRAY_SIZE (examples3Keys),
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
	{
		KIND_REQUEST_BODY,
		requestBodies3Keys,
		ARRAY_SIZE (requestBodies3Keys),
	},
	// 10:
	{
		KIND_HEADER,
		headers3Keys,
		ARRAY_SIZE (headers3Keys),
	},
	{
		KIND_PATH_ITEM,
		pathItems3Keys,
		ARRAY_SIZE (pathItems3Keys),
	},
	{
		KIND_SECURITY_SCHEME,
		securitySchemes3Keys,
		ARRAY_SIZE (securitySchemes3Keys),
	},
	{
		KIND_LINK,
		links3Keys,
		ARRAY_SIZE (links3Keys),
	},
	{
		KIND_CALLBACK,
		callbacks3Keys,
		ARRAY_SIZE (callbacks3Keys),
	},
	// 15:
	{
		KIND_SECURITY,
		security3Keys,
		ARRAY_SIZE (security3Keys),
	},
};

const struct tagSource tagValueSources[] = {
	{
		KIND_SERVER,
		server3Keys,
		ARRAY_SIZE (server3Keys),
	},
	{
		KIND_TAG,
		tags3Keys,
		ARRAY_SIZE (tags3Keys),
	},
	{
		KIND_EXTERNAL_DOCS,
		externalDocs3Keys,
		ARRAY_SIZE (externalDocs3Keys),
	},
};

static void handleKey(struct sOpenApiSubparser *openapi,
											 yaml_token_t *token)
{
	int i;
	for (i = 0; i < ARRAY_SIZE(tagSources); i++)
	{
		const struct tagSource* ts = &tagSources[i];

		if (stateStackMatch(
					openapi->type_stack,
					ts->keys,
					ts->countKeys
			))
		{
			// printf("match! i=%d, value=%s\n", i, token->data.scalar.value);

			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value, ts->kind);
			attachYamlPosition (&tag, token, false);

			makeTagEntry (&tag);
			break;
		}
	}
}


static void handleValue(struct sOpenApiSubparser *openapi,
						yaml_token_t *token)
{
	int i;
	for (i = 0; i < ARRAY_SIZE(tagValueSources); i++)
	{
		const struct tagSource* ts = &tagValueSources[i];

		if (stateStackMatch(
					openapi->type_stack,
					ts->keys,
					ts->countKeys
			))
		{
			// printf("match value! i=%d\n", i);

			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value, ts->kind);
			attachYamlPosition (&tag, token, false);

			makeTagEntry (&tag);
			break;
		}
	}
}

static void openapiPlayStateMachine (struct sOpenApiSubparser *openapi,
									 yaml_token_t *token)
{
	// printStack(openapi->type_stack);

	switch (token->type)
	{
	case YAML_KEY_TOKEN:
		openapi->play_detection_state = DSTAT_LAST_KEY;
		break;
	case YAML_SCALAR_TOKEN:
		switch (openapi->play_detection_state)
		{
			case DSTAT_LAST_KEY:
				openapi->type_stack->key = parseKey (token);
				// printf ("  key: %s\n", (char*)token->data.scalar.value);
				handleKey (openapi, token);
				break;
			case DSTAT_LAST_VALUE:
				// printf ("  value: %s\n", (char*)token->data.scalar.value);
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
		pushBlockType ((struct sOpenApiSubparser *)s, token->type);

	openapiPlayStateMachine ((struct sOpenApiSubparser *)s, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		popBlockType ((struct sOpenApiSubparser *)s);
	else if (token->type == YAML_STREAM_END_TOKEN)
		popAllBlockType ((struct sOpenApiSubparser *)s);
}

static void inputStart(subparser *s)
{
	((struct sOpenApiSubparser*)s)->play_detection_state = DSTAT_INITIAL;
	((struct sOpenApiSubparser*)s)->type_stack = NULL;
}

static void inputEnd(subparser *s)
{
	popAllBlockType ((struct sOpenApiSubparser *)s);
}

static void
findOpenApiTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* OpenApiParser (void)
{
	static struct sOpenApiSubparser openapiSubparser = {
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


	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable	= OpenApiKinds;
	def->kindCount = ARRAY_SIZE (OpenApiKinds);
	def->parser	= findOpenApiTags;
	return def;
}
