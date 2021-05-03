/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#include "general.h"

#include "optscript.h"
#include "mio.h"
#include "routines.h"

#include <string.h>
#include <stdlib.h>

static void
help (const char* progname, int exit_status)
{
	FILE *out = stderr;
	if (exit_status == 0)
		out = stdout;

	fprintf (out, "Usage: %s [options] [file|-]\n\n", progname);
	fputs   (     "  -h|--help             Print this option summary\n", out);
	fputs   (     "  -l|--list-operators   List built-in operators\n", out);
	fputs   (     "  -e|--eval CODE        Evaluate the CODE\n", out);
	fputc   ('\n', out);
	exit (exit_status);
}

static EsObject*
op_renew (OptVM *vm, EsObject *name)
{
	bool *app_data = opt_vm_get_app_data (vm);
	*app_data = true;
	return OPT_ERR_QUIT;
}

int
optscript_run (OptVM *vm, char *prompt, void *app_data)
{
	int r = 0;
	void *old_app_data = opt_vm_set_app_data (vm, app_data);
	char *old_prompt = opt_vm_set_prompt (vm, prompt);

	opt_vm_print_prompt (vm);

	while (true)
	{
		EsObject *o = opt_vm_read (vm, NULL);
		if (es_object_equal (o, ES_READER_EOF))
		{
			es_object_unref (o);
			break;
		}
		EsObject *e = opt_vm_eval (vm, o);
		es_object_unref (o);

		if (es_error_p (e))
		{
			if (!es_object_equal (e, OPT_ERR_QUIT))
			{
				opt_vm_report_error (vm, e, NULL);
				r = 1;
			}
			break;
		}
	}

	opt_vm_set_prompt (vm, old_prompt);
	opt_vm_set_app_data (vm, old_app_data);
	return r;
}

int
main(int argc, char **argv)
{
	MIO *in = NULL;
	const char *file = NULL;
	bool help_ops = false;

	for (int i = 1; i < argc; i++)
	{
		if (strcmp (argv[i], "-h") == 0
			|| strcmp (argv[i], "--help") == 0)
			help (argv[0], 0);
		else if (strcmp (argv[i], "-l") == 0
				 || strcmp (argv[i], "--list-operators") == 0)
			help_ops = true;
		else if (strcmp (argv[i], "-e") == 0
				 || strcmp (argv[i], "--eval") == 0)
		{
			if (file)
			{
				fprintf (stderr, "Don't specify multiple input: %s, %s\n",
						 file, argv[i]);
				exit (2);
			}

			if (i == argc -1)
			{
				fprintf (stderr, "no code for %s\n", argv[i]);
				exit (2);
			}
			file = "<cmdline>";
			i++;
			in = mio_new_memory ((unsigned char *)eStrdup(argv[i]), strlen (argv[i]), eRealloc, eFree);
			if (!in)
			{
				fprintf (stderr, "failed to create mio from memory\n");
				exit (1);
			}
		}
		else if (argv[i][0] == '-' && argv[i][1] == '\0')
		{
			if (file)
			{
				fprintf (stderr, "Don't specify multiple input: %s, %s\n",
						 file, argv[i]);
				exit (2);
			}

			file = "<stdin>";
			in = mio_new_fp (stdin, NULL);
			if (!in)
			{
				fprintf (stderr, "failed to use <stdin>\n");
				exit (1);
			}
		}
		else if (argv[i][0] == '-')
		{
			fprintf (stderr, "unknown option: %s\n", argv[i]);
			exit (2);
		}
		else if (in)
		{
			fprintf (stderr, "too many arugments\n");
			mio_unref (in);
			exit (2);
		}
		else
		{
			if (file)
			{
				fprintf (stderr, "Don't specify multiple input: %s, %s\n",
						 file, argv[i]);
				exit (2);
			}

			file = argv[i];
			in = mio_new_file (file, "r");
			if (!in)
			{
				fprintf (stderr, "failed to open: %s\n", file);
				exit (1);
			}
		}
	}

	opt_init ();

	if (!in)
		in = mio_new_fp (stdin, NULL);
	if (in == NULL)
	{
		fprintf (stderr, "failed to open input stream\n");
		exit (1);
	}

	MIO *out = mio_new_fp (stdout, NULL);
	if (!out)
	{
		mio_unref (in);
		fprintf (stderr, "failed to open output stream\n");
		exit (1);
	}
	MIO *err = mio_new_fp (stderr, NULL);
	if (!err)
	{
		mio_unref (out);
		mio_unref (in);
		fprintf (stderr, "failed to open error stream\n");
		exit (1);
	}

	OptVM *vm = opt_vm_new (in, out, err);

	int r;

	EsObject *dict = opt_dict_new (47);
	{
		EsObject *op;
		EsObject *sym;

		op = opt_operator_new (op_renew, "_renew", 0, ":clear the state of vm%- _RENEW -");
		sym = es_symbol_intern ("_renew");
		opt_dict_def (dict, sym, op);
		es_object_unref (op);
	}

	opt_vm_dstack_push (vm, dict);
	if (help_ops)
		r = opt_vm_help (vm, NULL, NULL, NULL);
	else
	{
		bool renew = false;
	renew:
		r = optscript_run (vm, file? NULL: "OPT", &renew);
		if (renew)
		{
			opt_vm_clear (vm);
			opt_vm_dstack_push (vm, dict);
			renew = false;
			goto renew;
		}
	}
	es_object_unref (dict);

	opt_vm_delete (vm);

	mio_unref (err);
	mio_unref (out);
	mio_unref (in);

	return r;
}
