#include "a.h"
#embed "data.bin"
#embed <font.ttf> limit(1024)
#   embed "spaced.bin"
#embed"nospace.bin"
#embed <sys/res.dat> prefix(0x1,) suffix(,0)

static const unsigned char data[] = {
#embed "inline.bin"
};

/* #embed "in-comment.bin" */
