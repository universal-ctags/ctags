/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "main_p.h"
#include <stdio.h>

/*
*   FUNCTION DEFINITIONS
*/
int main(int argc, char **argv)
{
	fprintf(stderr, "%s %s:%d\n", __FUNCTION__, __FILE__, __LINE__);
	return ctags_cli_main (argc, argv);
}
