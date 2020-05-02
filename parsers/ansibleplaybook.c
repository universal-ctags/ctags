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
#include "yaml.h"
#include "parse.h"
#include "subparser.h"

#include <stdio.h>

typedef enum {
	K_PLAY
} ansiblePlaybookKind;

static kindDefinition AnsiblePlaybookKinds [] = {
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


struct sAnsiblePlaybookSubparser {
	yamlSubparser yaml;
	struct yamlBlockTypeStack *type_stack;
	enum ansiblePlaybookPlayDetectingState play_detection_state;
};

static void pushBlockType (struct sAnsiblePlaybookSubparser *ansible, yaml_token_type_t t)
{
	struct yamlBlockTypeStack *s;

	s = xMalloc (1, struct yamlBlockTypeStack);

	s->next = ansible->type_stack;
	ansible->type_stack = s;

	s->type = t;
	s->associatedCorkIndex = CORK_NIL;
}

static void popBlockType (struct sAnsiblePlaybookSubparser *ansible,
						  yaml_token_t *token)
{
	struct yamlBlockTypeStack *s;

	s = ansible->type_stack;
	ansible->type_stack = s->next;

	s->next = NULL;
	tagEntryInfo *tag = getEntryInCorkQueue (s->associatedCorkIndex);
	if (tag)
		attachYamlPosition (tag, token, true);

	eFree (s);
}

static void popAllBlockType (struct sAnsiblePlaybookSubparser *ansible,
							 yaml_token_t *token)
{
	while (ansible->type_stack)
		popBlockType (ansible, token);
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

static void	ansiblePlaybookPlayStateMachine (struct sAnsiblePlaybookSubparser *ansible,
											 yaml_token_t *token)
{
	yaml_token_type_t play_expect[] = {
		YAML_BLOCK_MAPPING_START_TOKEN,
		YAML_BLOCK_SEQUENCE_START_TOKEN,
	};

	switch (token->type)
	{
	case YAML_KEY_TOKEN:
		if (stateStackMatch (ansible->type_stack,
							 play_expect, ARRAY_SIZE (play_expect),
							 false))
			ansible->play_detection_state = DSTAT_PLAY_NAME_KEY;
		else
			ansible->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	case YAML_SCALAR_TOKEN:
		if ((ansible->play_detection_state == DSTAT_PLAY_NAME_KEY)
			&& scalarNeq (token, 4, "name"))
			ansible->play_detection_state = DSTAT_PLAY_NAME_KEY_SCALAR;
		else if (ansible->play_detection_state == DSTAT_PLAY_NAME_VALUE)
		{
			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value,
						  K_PLAY);
			attachYamlPosition (&tag, token, false);

			Assert (ansible->type_stack->associatedCorkIndex == CORK_NIL);
			ansible->type_stack->associatedCorkIndex = makeTagEntry (&tag);
			ansible->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		}
		else
			ansible->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	case YAML_VALUE_TOKEN:
		if (ansible->play_detection_state == DSTAT_PLAY_NAME_KEY_SCALAR)
			ansible->play_detection_state = DSTAT_PLAY_NAME_VALUE;
		else
			ansible->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	default:
		ansible->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
		break;
	}
}

static void newTokenCallback (yamlSubparser *s, yaml_token_t *token)
{
	if (token->type == YAML_BLOCK_SEQUENCE_START_TOKEN
		|| token->type == YAML_BLOCK_MAPPING_START_TOKEN)
		pushBlockType ((struct sAnsiblePlaybookSubparser *)s, token->type);

	ansiblePlaybookPlayStateMachine ((struct sAnsiblePlaybookSubparser *)s, token);

	if (token->type == YAML_BLOCK_END_TOKEN)
		popBlockType ((struct sAnsiblePlaybookSubparser *)s, token);
	else if (token->type == YAML_STREAM_END_TOKEN)
		popAllBlockType ((struct sAnsiblePlaybookSubparser *)s, token);
}

static void inputStart(subparser *s)
{
	((struct sAnsiblePlaybookSubparser*)s)->play_detection_state = DSTAT_PLAY_NAME_INITIAL;
	((struct sAnsiblePlaybookSubparser*)s)->type_stack = NULL;
}

static void inputEnd(subparser *s)
{
	Assert (((struct sAnsiblePlaybookSubparser*)s)->type_stack == NULL);
}

static void
findAnsiblePlaybookTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* AnsiblePlaybookParser (void)
{
	static struct sAnsiblePlaybookSubparser ansiblePlaybookSubparser = {
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
		{ DEPTYPE_SUBPARSER, "Yaml", &ansiblePlaybookSubparser },
	};

	parserDefinition* const def = parserNew ("AnsiblePlaybook");


	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable         = AnsiblePlaybookKinds;
	def->kindCount     = ARRAY_SIZE (AnsiblePlaybookKinds);
	def->parser        = findAnsiblePlaybookTags;
	def->useCork       = CORK_QUEUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
