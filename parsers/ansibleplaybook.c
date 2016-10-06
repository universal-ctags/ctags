/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"	/* must always come first */
#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "meta-yaml.h"
#include "parse.h"


#include <stdio.h>

typedef enum {
	K_PLAY
} ansiblePlaybookKind;

static kindOption AnsiblePlaybookKinds [] = {
	{ true,  'p', "play", "plays" },
};

struct yamlBlockTypeStack {
	yaml_token_type_t type;
	int associatedCorkIndex;
	struct yamlBlockTypeStack *next;
};

/* - name: "THE NAME" */
enum ansiblePlaybookPlayDetectingState {
	DSTAT_PLAY_NAME_INITIAL,
	DSTAT_PLAY_NAME_KEY,
	DSTAT_PLAY_NAME_KEY_SCALAR,
	DSTAT_PLAY_NAME_VALUE,
};


struct ansiblePlaybookState {
	struct yamlBlockTypeStack *type_stack;
	enum ansiblePlaybookPlayDetectingState play_detection_state;
};

static void pushBlockType (struct ansiblePlaybookState *state, yaml_token_type_t t)
{
	struct yamlBlockTypeStack *s;

	s = xMalloc (1, struct yamlBlockTypeStack);

	s->next = state->type_stack;
	state->type_stack = s;

	s->type = t;
	s->associatedCorkIndex = CORK_NIL;
}

static void popBlockType (struct ansiblePlaybookState *state,
						  yaml_token_t *token)
{
	struct yamlBlockTypeStack *s;

	s = state->type_stack;
	state->type_stack = s->next;

	s->next = NULL;
	if (s->associatedCorkIndex != CORK_NIL)
	{
		tagEntryInfo *tag;

		tag = getEntryInCorkQueue (s->associatedCorkIndex);
		attachYamlPosition (tag, token, true);
	}

	eFree (s);
}

static void popAllBlockType (struct ansiblePlaybookState *state,
							 yaml_token_t *token)
{
	while (state->type_stack)
		popBlockType (state, token);
}

static bool stateStackMatch (struct yamlBlockTypeStack *stack,
							 yaml_token_type_t *expectation,
							 unsigned int length,
							 bool partial)
{
	if (length == 0)
	{
		if (stack == NULL)
			return true;
		else if (partial)
			return true;
		else
			return false;
	}

	if (stack == NULL)
		return false;

	if (stack->type == expectation[0])
		return stateStackMatch (stack->next, expectation + 1, length - 1, partial);
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

static void	ansiblePlaybookPlayStateMachine (struct ansiblePlaybookState *state,
											 yaml_token_t *token)
{
	yaml_token_type_t play_expect[] = {
		YAML_BLOCK_MAPPING_START_TOKEN,
		YAML_BLOCK_SEQUENCE_START_TOKEN,
	};

	switch (token->type)
	{
	case YAML_KEY_TOKEN:
		if (stateStackMatch (state->type_stack,
							 play_expect, ARRAY_SIZE (play_expect),
							 false))
			state->play_detection_state = DSTAT_PLAY_NAME_KEY;
		else
			state->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	case YAML_SCALAR_TOKEN:
		if ((state->play_detection_state == DSTAT_PLAY_NAME_KEY)
			&& scalarNeq (token, 4, "name"))
			state->play_detection_state = DSTAT_PLAY_NAME_KEY_SCALAR;
		else if (state->play_detection_state == DSTAT_PLAY_NAME_VALUE)
		{
			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value,
						  AnsiblePlaybookKinds + K_PLAY);
			attachYamlPosition (&tag, token, false);

			Assert (state->type_stack->associatedCorkIndex == CORK_NIL);
			state->type_stack->associatedCorkIndex = makeTagEntry (&tag);
			state->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		}
		else
			state->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	case YAML_VALUE_TOKEN:
		if (state->play_detection_state == DSTAT_PLAY_NAME_KEY_SCALAR)
			state->play_detection_state = DSTAT_PLAY_NAME_VALUE;
		else
			state->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	default:
		state->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	}
}

static void ansiblePlaybook (yaml_token_t *token, void *data)
{
	struct ansiblePlaybookState *state = data;

	if (token->type == YAML_BLOCK_SEQUENCE_START_TOKEN
		|| token->type == YAML_BLOCK_MAPPING_START_TOKEN)
		pushBlockType (state, token->type);

	ansiblePlaybookPlayStateMachine (state, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		popBlockType (state, token);
	else if (token->type == YAML_STREAM_END_TOKEN)
		popAllBlockType (data, token);
}

static void *ansiblePlaybookInputStart(void)
{
	static struct ansiblePlaybookState state;

	state.play_detection_state = DSTAT_PLAY_NAME_INITIAL;

	return &state;
}

static void ansiblePlaybookInputEnd(void *data)
{
	struct ansiblePlaybookState *state CTAGS_ATTR_UNUSED = data;

	Assert (state->type_stack == NULL);
}

static void
findAnsiblePlaybookTags (void)
{
	void *data = ansiblePlaybookInputStart ();
	runYamlParser (ansiblePlaybook, data);
}

static struct yamlParserClient AnsiblePlaybookYamlClient = {
	.inputStart = ansiblePlaybookInputStart,
	.callback = ansiblePlaybook,
	.inputEnd = ansiblePlaybookInputEnd,
};

static void initializeAnsiblePlaybookParser (const langType language)
{
	AnsiblePlaybookYamlClient.lang = language;
	registerYamlParserClient (&AnsiblePlaybookYamlClient);
}

extern parserDefinition* AnsiblePlaybookParser (void)
{
	parserDefinition* const def = parserNew ("AnsiblePlaybook");

	static parserDependency dependencies [] = {
		{ DEPTYPE_SUBPARSER, "Yaml" },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->initialize    = initializeAnsiblePlaybookParser;
	def->kinds         = AnsiblePlaybookKinds;
	def->kindCount     = ARRAY_SIZE (AnsiblePlaybookKinds);
	def->parser        = findAnsiblePlaybookTags;
	def->method        = METHOD_YAML;
	def->useCork       = true;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
