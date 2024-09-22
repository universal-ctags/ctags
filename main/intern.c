/*
*
*   Copyright (c) 2024, Red Hat, Inc.
*   Copyright (c) 2024, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"

#include "htable.h"
#include "intern.h"
#include "routines.h"

#include <string.h>

struct obentry {
	struct obentry *next;
	char quark [];
};

/* $ find linux -type f | wc -l
   86267 */
#define OBARRAY_SIZE    87719
static struct obentry *obarray [OBARRAY_SIZE];

const char *intern (const char *name)
{
	unsigned int hv = hashCstrhash (name) % OBARRAY_SIZE;
	struct obentry* tmp = obarray [hv];

	while (tmp)
	{
		if (!strcmp (tmp->quark, name))
			return tmp->quark;
		tmp = tmp->next;
	}

	size_t len = strlen (name);
	struct obentry *s = eMalloc (sizeof (struct obentry) + len + 1);
	s->next = obarray[hv];
	memcpy (s->quark, name, len + 1);

	obarray[hv] = s;
	return s->quark;
}
