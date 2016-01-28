
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

/* check for prefixes */
static const char* str4 = LR"blah(";int bug4;)blah";
struct typ4 { int memb4; };

static const char* str5 = u8R"blah(";int bug5;)blah";
struct typ5 { int memb5; };

static const char* str6 = uR"blah(";int bug6;)blah";
struct typ6 { int memb6; };

static const char* str7 = UR"blah(";int bug7;)blah";
struct typ7 { int memb7; };
