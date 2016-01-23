
static const char* str1 = R"blah(
lots
of text
)blah";

struct typ1 { int memb1; };

static const char* str2 = R"blah(
lots
of text including a quote"
)blah";

struct typ2 { int memb2; };

/* check we don't get confused by string concatenation */
#define FOUR "four"

static const char* str3 = FOUR"f(iv)e";

struct typ3 { int memb3; };
