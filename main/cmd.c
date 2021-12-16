/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
*   INCLUDE FILES
*/
#include <config.h>

#include <regex.h>

/*
*   FUNCTION DEFINITIONS
*/
int main(int argc, char **argv)
{
	regex_t code;
	return regcomp (&code, "a", 5);
}
