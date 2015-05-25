
/* simple trigraphs */
??=define A 1
??=define B 2
??=define STRINGIFY_INTERN(x) ??=x
??=define STRINGIFY(x) STRINGIFY_INTERN(x)

/* doesn't expand to anything that makes sense, but as "???" is not a valid
 * trigraph it should not prevent "??/" to match */
??=define D 4 ???/
#define bug1
??=define E ?????/
#define bug2

/* \ isn't interpreted for trigraphs */
??=define F ???\??/
extern int bug3 = ??-0;

??=define M3_INIT(a, b, c) ??< a, b, c ??>
typedef int matrix3??(3??);

struct str ??<
  char *buf;
  unsigned int len, size;
??>;

int main(void)
??<
  const char *hello = STRINGIFY(hello);
  matrix3 m = M3_INIT(1, 2, 3);

  return m??(2??);
??>

/* FIXME: how to test "??'" ("^"), "??!" ("|") and "??-" ("~")?
 *        I can't think of a construct CTags cares about using those */

??=if 0
#define bug4
??=endif


/* test the same with untaken preprocessor paths (as they are then not read by
 * the C parser but get.c) */
#if 0

??=define if0d_A 1
??=define if0d_B 2
??=define if0d_C 4 ???/
#define bug5
??=define I ?????/
#define bug6
??=define I ??????????/
#define bug7

#endif
