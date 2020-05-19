/*
 *  u-ctags --fields=S --kinds-C++=+p -o output.tags input.hxx
 */
int f ( int,  int,  int);
int f (char,  int,  int);
int f (char, char,  int);
int f (char,  int, char);
int f (char, char, char);
int f ( int, char,  int);
int f ( int, char, char);
int f ( int,  int, char);

typedef char CHAR;
typedef int  INT;

int g ( INT,  INT,  INT);
int g (CHAR,  INT,  INT);
int g (CHAR, CHAR,  INT);
int g (CHAR,  INT, CHAR);
int g (CHAR, CHAR, CHAR);
int g ( INT, CHAR,  INT);
int g ( INT, CHAR, CHAR);
int g ( INT,  INT, CHAR);
