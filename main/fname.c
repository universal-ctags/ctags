/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#include "general.h"
#include "fname.h"

#include <string.h>

#include "routines.h"
#include "vstring.h"


struct comp {
	char *str;
	size_t len;
	struct comp *parent;
	struct comp *child;
};


struct canonFnameCacheTable {
	hashTable *table;
	char *input_last;
	char *return_last;
};

extern struct canonFnameCacheTable *canonFnameCacheTableNew (void)
{
	struct canonFnameCacheTable *r = xMalloc (1, struct canonFnameCacheTable);
	r->table = hashTableNew (7, hashCstrhash, hashCstreq,
				 eFree, eFree);
	r->input_last = NULL;
	r->return_last = NULL;
	return r;
}

extern void canonFnameCacheTableDelete (struct canonFnameCacheTable *cache_table)
{
	hashTableDelete (cache_table->table);
	eFree (cache_table);
}

static void strcpy_comps (char *buf, struct comp *comp)
{
	while (comp)
	{
		buf[0] = '/';
		memcpy (buf + 1, comp->str, comp->len);
		buf += (1 + comp->len);
		comp = comp->child;
	}
	buf[0] = '\0';
}

static size_t strlen_comps (struct comp *comp)
{
	size_t n = 0;

	while (comp)
	{
		n += (1 + comp->len);
		comp = comp->child;
	}
	return n;
}

static char *fsimplify_absz (struct comp *comp)
{
	struct comp *root = comp;

	while (comp)
	{
		// fprintf (stderr, "[stack] -%s-\n", comp->str);
		root = comp;
		comp = comp->parent;
	}

	if (root->child && root->len == 0)
		root = root->child;

	size_t len = strlen_comps (root);
	char *buf = xMalloc (len + 1, char);

	strcpy_comps(buf, root);
	return buf;
}

static char *fsimplify_abs0 (char *fname, struct comp *parent)
{
	char *next = strchr (fname, '/');
	struct comp comp = {
		.str = fname,
		.len = next? (next - fname): strlen (fname),
		.parent = parent,
		.child = NULL
	};

	if (comp.len  == 0
		|| (comp.len == 1 && fname [0] == '.'))
	{
		parent->child = NULL;
		if (next == NULL)
			return fsimplify_absz (parent);
		*next = '\0';
		return fsimplify_abs0 (next + 1, parent);
	}

	if (comp.len == 2 && fname [0] == '.' && fname [1] == '.')
	{
		if (next == NULL)
		{
			if (parent->parent)
			{
				parent->parent->child = NULL;
				return fsimplify_absz (parent->parent);
			}
			return strdup("/");
		}
		*next = '\0';
		if (parent->parent)
		{
			parent->parent->child = NULL;
			return fsimplify_abs0 (next + 1, parent->parent);
		}
		comp.parent = NULL;
		comp.str[0] = '\0';
		comp.len = 0;
		return fsimplify_abs0 (next + 1, &comp);
	}

	parent->child = &comp;
	if (next == NULL)
		return fsimplify_absz (&comp);
	*next = '\0';
	return fsimplify_abs0 (next + 1, &comp);
}

extern char *canonicalizeAbsoluteFileName (char *fname)
{
	char  *next = strchr (fname, '/');
	if (next == NULL)
	{
		if (!strcmp (fname, "..") || !strcmp (fname, "."))
		{
			fname [0] = '/';
			fname [1] = '\0';
			return strdup (fname);
		}
		char *r = xMalloc (strlen (fname) + 2, char);
		r[0] = '/';
		strcpy (r + 1, fname);
		return r;
	}

	*next = '\0';
	struct comp comp = {
		.str = fname,
		.len = next - fname,
		.parent = NULL,
		.child = NULL
	};
	if (!strcmp (comp.str, "..") || !strcmp (comp.str, "."))
	{
		comp.str[0] = '\0';
		comp.len = 0;
	}
	return fsimplify_abs0 (next + 1, &comp);
}

static char *canonicalizePathNew(const char *dir, size_t dir_len, const char *rela)
{
	bool relative = false;
	vString *buf = vStringNew ();

	if (rela == NULL)
		vStringNCopyS (buf, dir, dir_len);
	else if (rela[0] == '/')
		vStringCopyS (buf, rela);
	else
	{
		vStringNCopyS (buf, dir, dir_len);
		vStringPut (buf, '/');
		vStringCatS (buf, rela);
		relative = true;
	}

	char *r = canonicalizeAbsoluteFileName (vStringValue (buf));
	if (relative)
	{
		if (strncmp (dir, r, dir_len) == 0)
		{
			if (r[dir_len] == '/')
			{
				if (r[dir_len + 1] == '\0')
					vStringCopyS (buf, ".");
				else
					vStringCopyS (buf, r + dir_len + 1);
				eFree (r);
				return vStringDeleteUnwrap (buf);
			}
			else if (r[dir_len] == '\0')
			{
				vStringCopyS (buf, ".");
				eFree (r);
				return vStringDeleteUnwrap (buf);
			}
			else
			{
				vStringDelete (buf);
				return r;
			}
		}
	}

	vStringDelete (buf);
	return r;
}

extern const char *canonicalizeRelativeFileName (const char *cwd, size_t cwd_len, const char *input,
						 struct canonFnameCacheTable *cache_table)
{
	if (cache_table->input_last)
	{
		if (strcmp (input, cache_table->input_last) == 0)
			return cache_table->return_last;
		cache_table->input_last = NULL;
	}

	char *r = hashTableGetItem (cache_table->table, input);
	if (r)
		return r;

	r = canonicalizePathNew (cwd, cwd_len, input);

	cache_table->input_last = eStrdup (input);
	cache_table->return_last = r;
	hashTablePutItem (cache_table->table, cache_table->input_last, cache_table->return_last);
	return cache_table->return_last;
}
