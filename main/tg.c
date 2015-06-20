/*
 *
 *  Copyright (c) 2014, Red Hat, Inc.
 *  Copyright (c) 2014, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *  This source code is released for free distribution under the terms of the
 *  GNU General Public License. It is provided on an as-is basis and no
 *  responsibility is accepted for its failure to perform as expected.
 *
 */

#include "general.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "tg.h"

#ifndef xCalloc
#define xCalloc(n,Type)    (Type *)calloc((size_t)(n), sizeof (Type))
#endif
#ifndef eFree
#define eFree(x) free(x)
#endif



static void            tg_load_full   (unsigned char *mini_table,
				       FILE *fp, int (* get_char) (FILE*, void* ),
				       void *data);
static int             tg_nonalnum_get_char (FILE *fp, void *dump);


#ifdef TG_MAIN
static void            tg_emit   (void * data, unsigned char *mini_table, FILE *fp,
				  void (* emit_header) (void*, FILE*),
				  void (* emit_trailer)(void*, FILE*),
				  void (* emit_entry)  (void*, FILE*, unsigned short, unsigned char, unsigned char, unsigned char));
static void tg_c_emit_header     (void * data, FILE *fp);
static void tg_c_emit_trailer    (void * data, FILE *fp);
static void tg_c_emit_entry      (void * data, FILE* fp,
				  unsigned short index, unsigned char value, unsigned char c0, unsigned char c1);
#endif	/* TG_MAIN */

static unsigned short  tg_mini_size      (const unsigned char *mini_table);
static unsigned short* tg_big_create     (const unsigned char *a, const unsigned char *b, float ratio);
static unsigned long   tg_big_score      (unsigned short *t);
static void            tg_big_destroy    (unsigned short *a);



unsigned char*  tgCreate (void)
{
	return xCalloc(256 * 256, unsigned char);
}

void tgDestroy(unsigned char *mini_table)
{
	eFree(mini_table);
}


void tgLoad (unsigned char *mini_table, FILE *fp)
{
	int dump = 0;
	tg_load_full(mini_table, fp, tg_nonalnum_get_char, &dump);
}

static void tg_load_full   (unsigned char *mini_table,
			    FILE *fp, int (* get_char) (FILE*, void* ),
			    void *data)
{
	int  a;
	unsigned char c[2];
	unsigned char C;

	a = (* get_char)(fp, data);
	if (a == EOF)
		return;
	c[0] = (unsigned char)a;

	while ((a = (* get_char)(fp, data)) != EOF)
	{
		c[1] = (unsigned char)a;


		if (!((c[0] == '\n') && (c[1] == '\n')))
		{
			C = mini_table[c[0] * 256 + c[1]];
			if (C != 255)
				mini_table[c[0] * 256 + c[1]] = C + 1;
		}
		c[0] = c[1];
	}
}

static int tg_nonalnum_get_char (FILE *fp, void *dump)
{
	int c;

	/* Ignore alphabet, number, space.

	   This stage improves the language
	   detection drastically. */
	while (1)
	{
		c = getc(fp);
		if (c == EOF)
			break;
		else if (c == '\r')
		{
			c = '\n';
			break;
		}
		else if (c == '\n')
			break;
		else if (!isascii(c))
			continue;
		else if (c == '1'
			 || c == '0')
			break;
		else if (isalnum (c))
			continue;
		else if (isblank(c))
			continue;

		break;
	}

	if (*(int *)dump)
	{
		if (c != EOF)
			printf("%c\n", c);
		else
			printf("EOF\n");
	}
	return c;
}


#ifdef TG_MAIN
static void tg_c_emit_header(void * data, FILE *fp)
{
	fprintf(fp, "static unsigned char %s_tg_table [%u] =\n{\n", (char *)data, 256 * 256);
}
static void tg_c_emit_trailer(void * data, FILE *fp)
{
	fprintf(fp, "};\n");
}
static void tg_c_emit_entry(void * data, FILE* fp,
			    unsigned short index, unsigned char value, unsigned char c0, unsigned char c1)
{
	fprintf(fp, "	[%8u] = %8u, /* %c %c */\n",
		index, value,
		(c0 == '\n')? 'N': c0,
		(c1 == '\n')? 'N': c1)	;
}

void tg_emit   (void * data, unsigned char *mini_table, FILE *fp,
		void (* emit_header) (void*, FILE*),
		void (* emit_trailer)(void*, FILE*),
		void (* emit_entry)  (void*, FILE*, unsigned short, unsigned char, unsigned char, unsigned char))
{
	unsigned int c[2];
	unsigned int i;
	unsigned char v;

	emit_header(data, fp);
	for (c[0] = 0; c[0] < 256; c[0]++)
		for(c[1] = 0; c[1] < 256; c[1]++)
		{
			i = c[0] * 256 + c[1];
			v = mini_table[i];
			if (v != 0)
				emit_entry(data, fp, i, v, c[0], c[1]);
		}
	emit_trailer(data, fp);
}
#endif	/* TG_MAIN */

static unsigned short  tg_mini_size   (const unsigned char *mini_table)
{
	unsigned short t;
	unsigned int i;

	for (t = 0, i = 0; i < 255 * 255; i++)
		t += mini_table[i];
	/* TODO: we can cache the result
	   mini_table[0] = t; */
	return t;
}

static unsigned short* tg_big_create (const unsigned char *a, const unsigned char *b, float ratio)
{
	unsigned int i;
	unsigned short* big_table;

	big_table  = xCalloc(256 * 256, unsigned short);

	/* TODO: Maybe sqrt is needed. */
	for (i = 0; i < 255 * 255; i++)
		big_table[i] = (unsigned short)(ratio * a[i] * b[i]);

	return big_table;
}

#ifdef TG_MAIN
static void tg_dump (unsigned short *big_table, FILE *fp)
{
	unsigned int   c[2];
	unsigned short i;
	unsigned short v;

	for (c[0] = 0; c[0] < 256; c[0]++)
		for(c[1] = 0; c[1] < 256; c[1]++)
		{
			i = c[0] * 256 + c[1];
			v = big_table[i];
			if (v != 0)
				fprintf(fp, "	[%5u] = %5u, /* %c %c */\n",
					i, v,
					(c[0] == '\n')? 'N': c[0],
					(c[1] == '\n')? 'N': c[1]
				       );
		}
}
#endif

static unsigned long   tg_big_score (unsigned short *big_table)
{
	unsigned short i;
	unsigned long t;

	for (t = 0, i = 0; i < 255 * 255; i++)
		t += big_table[i];
	return t;
}

static void tg_big_destroy (unsigned short *big_table)
{
	eFree(big_table);
}

int tgCompare(const unsigned char *a, const unsigned char *b, const unsigned char *t)
{
	unsigned long asz, bsz, max;
	unsigned short *abt, *bbt;
	unsigned long ascore, bscore;
	int r;

	asz = tg_mini_size(a);
	bsz = tg_mini_size(b);
	if (asz > bsz)
		max = asz;
	else
		max = bsz;

	abt = tg_big_create(a, t, (1.0f * max) / (1.0f * asz));
	bbt = tg_big_create(b, t, (1.0f * max) / (1.0f * bsz));
	ascore = tg_big_score(abt);
	bscore = tg_big_score(bbt);

	if (ascore > bscore)
		r = -1 * ascore;
	else if (ascore < bscore)
		r = bscore;
	else
		r = 0;

	tg_big_destroy(abt);
	tg_big_destroy(bbt);

	return r;
}
#ifdef TG_MAIN
#include <errno.h>

/*
 *
 *
 * 0. help
 * ----------------------
 *
 *    $ tg help
 *    $ tg --help|-h
 *
 *
 * 1. Generate table
 * -----------------
 *
 *    $ tg [--debug] [--prefix SOMETHING] generate FILE
 *
 *
 *    The output C code must be able to use as tg_field of parser def.
 *
 *
 * 2. Compare files
 * ----------------------
 *
 *    $ tg [--debug] compare A B C
 *
 *
 *    Print A if file A is similar than file C.
 *    Print B if file B is similar than file C.
 *
 *
 */
static int   M_debug;
static char* G_prefix = "_";

static void help(const char* const progname, FILE *out)
{
	fprintf(out,
		"/* Usage of %s\n"
		" *\n"
		" *\n"
		" * 0. help\n"
		" * ----------------------\n"
		" *\n"
		" *    $ tg help\n"
		" *    $ tg --help|-h\n"
		" *\n"
		" *\n"
		" * 1. Generate table\n"
		" * -----------------\n"
		" *\n"
		" *    $ tg [--debug] [--prefix SOMETHING] generate FILE\n"
		" *\n"
		" *\n"
		" *    The output C code must be able to use as tg_field of parser def.\n"
		" *\n"
		" *\n"
		" * 2. Compare files\n"
		" * ----------------------\n"
		" *\n"
		" *    $ tg [--debug] compare A B C\n"
		" *\n"
		" *\n"
		" *    Print A if file A is similar to file C than file B. \n"
		" *    Print B if file B is similar to file C than file A. \n"
		" *    Print nothing if no difference about similarities.\n"
		" *\n"
		" *\n"
		"*/\n",
		progname);
 }

 static int generate_table_main(int argc, char **argv)
 {
	 const char *fname;
	 FILE *fp;
	 unsigned char*  mini_table;

	 if (argc != 2)
	 {
		 fprintf(stderr, "generate: unexpected number of arguments\n");
		 return 1;
	 }

	 fname = argv[1];
	 fp = fopen(fname, "r");
	 if (fp == NULL)
	 {
		 fprintf(stderr, "generate: failed in fopen\n");
		 perror(fname);
		 return 1;
	 }
	 mini_table = tgCreate();

	 tg_load_full(mini_table, fp, tg_nonalnum_get_char, (void *)&M_debug);
	 fclose(fp);

	 tg_emit(G_prefix, mini_table, stdout,
		 tg_c_emit_header,
		 tg_c_emit_trailer,
		 tg_c_emit_entry);

	return 0;
}

static int compare_files_main(int argc, char **argv)
{
	int i;
	const char *fname[3];
	FILE *fp[3];
	unsigned char*  mini_table[3];

	unsigned long size[2], max;
	float ratio[2];
	unsigned short *big_table[2];
	unsigned long score[2];
	char resultc;

	if (argc != 4)
	{
		fprintf(stderr, "compare: unexpected number of arguments\n");
		return 1;
	}

	for (i = 1; i < argc; i++)
		fname[i - 1] = argv[i];

	for (i = 0; i < 3; i++)
	{
		fp[i] = fopen(fname[i], "r");
		if (fp [i] == NULL)
		{
			fprintf(stderr, "compare: failed in fopen\n");
			perror(fname[i]);
			return 1;
		}
		mini_table[i] = tgCreate();
		tg_load_full(mini_table[i], fp[i], tg_nonalnum_get_char, (void *)&M_debug);
		fclose(fp[i]);
	}

	size[0] = tg_mini_size(mini_table[0]);
	size[1] = tg_mini_size(mini_table[1]);
	max = size[1] > size[0] ? size[1]: size[0];
	ratio[0] = (1.0 * max)/(1.0 * size[0]);
	ratio[1] = (1.0 * max)/(1.0 * size[1]);
	if (M_debug)
	{
		fprintf(stderr, "/* %s size: %lu ratio: %f */\n",
			fname[0], size[0],  ratio[0]);
		fprintf(stderr, "/* %s size: %lu ratio: %f */\n",
			fname[1], size[1],  (1.0 * max)/(1.0 * size[1]));
	}

	big_table[0] = tg_big_create(mini_table[0], mini_table[2], (1.0 * max)/(1.0 * size[0]));
	big_table[1] = tg_big_create(mini_table[1], mini_table[2], (1.0 * max)/(1.0 * size[1]));

	score[0] = tg_big_score(big_table[0]);
	score[1] = tg_big_score(big_table[1]);

	if (M_debug)
	{
		fprintf(stderr, "/* FINAL DUMP: %s x %s */\n", fname[0], fname[2]);
		tg_dump(big_table[0], stderr);
		fprintf(stderr, "/* FINAL DUMP: %s x %s */\n", fname[1], fname[2]);
		tg_dump(big_table[1], stderr);
	}

	if (score[0] > score[1])
	{
		resultc = '>';
		fprintf(stdout, "%s\n", fname[0]);
	}
	else if (score[0] < score[1])
	{
		resultc = '<';
		fprintf(stdout, "%s\n", fname[1]);
	}
	else
		resultc = '=';

	if (M_debug)
		fprintf(stderr, "%f (%lu:%s %c %lu:%s) %s\n",
			1.0*(score[0] - score[1])/(score[0] + score[1]),
			score[0], fname[0],
			resultc,
			score[1], fname[1],
			fname[2]);
	return 0;
}


int main(int argc, char** argv)
{
	int i;

	for (i = 1; i < argc; i++)
	{
		if (strcmp(argv[i], "--debug") == 0)
			M_debug++;
		else if (strcmp(argv[i], "--prefix") == 0)
		{
			i++;
			G_prefix = argv[i];
		}
		else if ((strcmp(argv[i], "--help") == 0)
			 || (strcmp(argv[i], "-h") == 0))
		{
			help(argv[0], stdout);
			return 0;
		}
		else if (argv[i][0] == '-')
		{
			fprintf(stderr, "Unknown option: %s\n", argv[i]);
			help(argv[0], stderr);
			return 1;
		}
		else
			break;
	}

	if (i == argc)
	{
		fprintf(stderr, "No subcommand given\n");
		help(argv[0], stderr);
		return 1;
	}

	if (strcmp (argv[i], "help") == 0)
	{
		help(argv[0], stdout);
		return 0;
	}
	else if (strcmp (argv[i], "generate") == 0)
		return generate_table_main(argc - i, &(argv[i]));
	else if (strcmp (argv[i], "compare") == 0)
		return compare_files_main(argc - i, &(argv[i]));
	else
	{
		fprintf(stderr, "Unknown subcommand given: %s\n", argv[i]);
		help(argv[0], stderr);
		return 1;
	}
}

#endif	/* TG_MAIN */
