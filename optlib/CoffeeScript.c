/*
 * Generated by ./misc/optlib2c from optlib/CoffeeScript.ctags, Don't edit this manually.
 */
#include "general.h"
#include "parse.h"
#include "routines.h"


static void initializeCoffeeScriptParser (const langType language)
{
	addLanguageXcmd (language, "coffeetags");
}

extern parserDefinition* CoffeeScriptParser (void)
{
	static const char *const extensions [] = {
		"coffee",
		NULL
	};

	static const char *const aliases [] = {
		NULL
	};

	static const char *const patterns [] = {
		NULL
	};


	parserDefinition* const def = parserNew ("CoffeeScript");

	def->enabled       = TRUE;
	def->extensions    = extensions;
	def->patterns      = patterns;
	def->aliases       = aliases;
	def->method        = METHOD_NOT_CRAFTED|METHOD_XCMD;
	def->initialize    = initializeCoffeeScriptParser;

	return def;
}

/*
 * Editor modelines  -  https://www.wireshark.org/tools/modelines.html
 *
 * Local variables:
 * c-basic-offset: 4
 * tab-width: 4
 * End:
 *
 * vi: set shiftwidth=4 tabstop=4:
 * :indentSize=4:tabSize=4:
 */
