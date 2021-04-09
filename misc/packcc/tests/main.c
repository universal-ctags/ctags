#include <stddef.h>

#include "parser.h"

#define PRINT(X) printf("%s\n", X);
#define PRINT_L(LBL, X) printf("%s: %s\n", LBL, X);

#ifndef RET_TYPE
#define RET_TYPE int
#endif

#include "parser.c"

int main(int argc, char **argv) {
    RET_TYPE ret;
    pcc_context_t *ctx = pcc_create(NULL);
    while (pcc_parse(ctx, &ret));
    pcc_destroy(ctx);
    return 0;
}
