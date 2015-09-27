/* badinput.c: do bisect-quest to find minimal input which breaks the target command execution

   Copyright (C) 2014 Masatake YAMATO

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   Build
   =======================================================================

	$ gcc -Wall badinput.c -o badinput

   Usage
   =======================================================================

	$ badinput CMDLINE_TEMPLATE INPUT OUTPUT

   Description
   =======================================================================

   Consider a situation that a process execve'd from CMDLINE_TEMPLATE crashes or
   enters into an infinite-loop when the process deals with INPUT file.

   This program truncates both the head and tail of the INPUT file and
   runs CMDLINE_TEMPLATE repeatedly till the process exits normally(==
   0); and reports the shortest input which causes the crash or infinite-loop.

   Here is an example:

	$ misc/badinput "timeout 1 ./ctags -o - --language-force=Ada %s > /dev/null" Test/1880687.js /tmp/output.txt

   Ada parser of ctags enters an infinite-loop when Test/1880687.js is given.
   The size of original Test/1880687.js is 2258 bytes.

	$ misc/badinput "timeout 1 ./ctags -o - --language-force=Ada %s > /dev/null" Test/1880687.js /tmp/output.txt
	[0, 2448]...31744
	[0, 0]...0
	step(end): 0 [0, 2448]...31744
	step(end): 1 [0, 1224]...31744
	step(end): 2 [0, 612]...0
	step(end): 3 [0, 918]...0
	step(end): 4 [0, 1071]...0
	step(end): 5 [0, 1147]...31744
	step(end): 6 [0, 1109]...0
	step(end): 7 [0, 1128]...31744
	step(end): 8 [0, 1119]...0
	step(end): 9 [0, 1123]...31744
	step(end): 10 [0, 1121]...0
	step(end): 11 [0, 1122]...31744
	step(start): 0 [0, 1122]...31744
	step(start): 1 [561, 1122]...31744
	step(start): 2 [841, 1122]...31744
	step(start): 3 [981, 1122]...31744
	step(start): 4 [1051, 1122]...31744
	step(start): 5 [1086, 1122]...0
	step(start): 6 [1069, 1122]...31744
	step(start): 7 [1077, 1122]...31744
	step(start): 8 [1081, 1122]...31744
	step(start): 9 [1083, 1122]...0
	step(start): 10 [1082, 1122]...0
	Minimal bad input:
	function baz() {
	    }
	}

	function g(
	$

   New shorter input, only 38 bytes, which can reproduce the issue is reported at the end.
   This new input is useful for debugging.

   The result is shown in stdout and is recorded to the file specified as OUTPUT. */

#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


static void
print_help(const char *prog, FILE *fp, int status)
{
	fprintf(fp, "Usage:\n");
	fprintf(fp, "	%s --help|-h\n", prog);
	fprintf(fp, "	%s CMDLINE_TEMPLATE INPUT OUTPUT\n", prog);
	exit (status);
}

static void
load (const char* input_file, char** input, size_t* len)
{
	int input_fd;
	struct stat stat_buf;

	input_fd = open (input_file, O_RDONLY);
	if (input_fd < 0)
	{
		perror ("open(input)");
		exit(1);
	}

	if (fstat(input_fd, &stat_buf) < 0)
	{
		perror ("fstat");
		exit(1);
	}

	*len = stat_buf.st_size;
	*input = malloc (*len);
	if (!*input)
	{
		fprintf(stderr, "memory exhausted\n");
		exit (1);
	}

	if (read (input_fd, *input, *len) != *len)
	{
		perror ("read");
		exit (1);
	}
}

static void
prepare(int output_fd, char * input, size_t len)
{
	if (lseek (output_fd, 0, SEEK_SET == -1))
	{
		perror("lseek");
		exit (1);
	}

	if (ftruncate(output_fd, 0) == -1)
	{
		perror ("truncate");
		exit (1);
	}

	if (write (output_fd, input, len) != len)
	{
		perror ("write");
		exit (1);
	}
}

static int
test (char* cmdline, char * input, off_t start, size_t len, int output_fd)
{
	int r;

	prepare (output_fd, input + start, len);
	fprintf (stderr, "[%lu, %lu]...", start, start + len);
	r = system(cmdline);
	fprintf(stderr, "%d\n", r);

	return r;
}

static int
bisect(char* cmdline, char * input, size_t len, int output_fd)
{
	off_t end;
	off_t start;

	unsigned int step;
	int delta;

	off_t failed = len;
	off_t successful = 0;

	end = len;
	failed = len;
	successful = 0;
	for (step = 0; 1; step++)
	{
		fprintf(stderr, "step(end): %d ", step);
		delta = (len >> (step + 1));
		if (delta == 0)
			delta = 1;

		if (test (cmdline, input, 0, end, output_fd) == 0)
		{
			successful = end;
			if (end + 1 == failed)
			{
				end = failed;
				break;
			}
			else
				end += delta;
		}
		else
		{
			failed = end;
			if (successful + 1 == end)
				break;
			else
				end -= delta;
		}
	}

	len = end;
	start = 0;
	failed = 0;
	successful = end;
	for (step = 0; 1; step++)
	{
		fprintf(stderr, "step(start): %d ", step);
		delta = (len >> (step + 1));
		if (delta == 0)
			delta = 1;
		if (test (cmdline, input, start, end - start, output_fd) == 0)
		{
			successful = start;
			if (start - 1 == failed)
			{
				start--;
				break;
			}
			else
				start -= delta;
		}
		else
		{
			failed = start;
			if (successful - 1 == start)
				break;
			else
				start += delta;
		}

	}

	len = end - start;
	fprintf(stderr, "Minimal bad input:\n");
	fwrite(input + start, 1, len, stdout);
	prepare (output_fd, input + start, len);
	printf("\n");

	return 0;
}

int
main(int argc, char** argv)
{
	char* cmdline_template;
	char* input_file;
	char* output_file;

	char* cmdline;
	char * input;
	size_t len;
	int output_fd;


	if (argc == 2
	    && ((!strcmp(argv[2], "--help"))
		|| (!strcmp(argv[2], "-h"))))
		print_help(argv[0], stdout, 0);
	else if (argc != 4)
	{
		fprintf(stderr,"wrong number of arguments\n");
		exit (1);
	}

	cmdline_template = argv[1];
	input_file = argv[2];
	output_file = argv[3];

	if (!strstr (cmdline_template, "%s"))
	{
		fprintf(stderr, "no %%s is found in command line template\n");
		exit (1);
	}

	load (input_file, &input, &len);

	output_fd = open (output_file, O_WRONLY|O_CREAT, 0666);
	if (output_fd < 0)
	{
		perror ("open(output)");
		exit (1);
	}

	if (asprintf (&cmdline, cmdline_template, output_file) == -1)
	{
		fprintf(stderr, "error in asprintf\n");
		exit (1);
	}

	if (test (cmdline, input, 0, len, output_fd) == 0)
	{
		fprintf(stderr, "the target command line exits normally against the original input\n");
		exit (1);
	}

	if (test (cmdline, input, 0, 0, output_fd) != 0)
	{
		fprintf(stderr, "the target command line exits normally against the empty input\n");
		exit (1);
	}

	return bisect(cmdline, input, len, output_fd);
}
