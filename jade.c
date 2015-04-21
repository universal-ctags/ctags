#include "general.h"
#include "parse.h"

static void installJadeParser (const langType language)
{
  addTagRegex(language, "^mixin[ \t]*([a-sA-Z0-9_]+)", "\\1", "d,definition", NULL);
}


extern parserDefinition* JadeParser (void)
{
  static const char* extensions[] = { "jade", NULL };
  parserDefinition* def = parserNew("Jade");
  def->extensions       = extensions;
  def->initialize       = installJadeParser;
  def->method           = METHOD_REGEX;
  return def;
}
