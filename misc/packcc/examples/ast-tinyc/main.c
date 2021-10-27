/*
 * This code is hereby placed in the public domain.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "system.h"
#include "parser.h"

#include <stdio.h>

int main(int argc, char **argv) {
    if (argc > 2) {
        fprintf(stderr, "ERROR: Too many arguments\n");
        return 1; /* usage error */
    }
    const char *path = (argc > 1) ? argv[1] : NULL;
    int ret = 0;
    system_t system;
    parser_context_t *parser = NULL;
    if (setjmp(system.jmp) == 0) {
        system__initialize(&system);
        system__open_source_file(&system, path);
        parser = parser_create(&system);
        ast_node_t *ast;
        const int b = parser_parse(parser, &ast);
        if (system.source.ecount > 0) longjmp(system.jmp, 1); /* never returns */
        if (b) {
            ret = 10; /* internal error */
            fprintf(stderr, "FATAL: Internal error\n");
                /* <-- input text remaining due to incompleteness of the grammar */
        }
        else {
            system__dump_ast(&system, ast);
        }
    }
    else {
        ret = 2; /* error during parsing */
    }
    parser_destroy(parser);
    system__finalize(&system); /* all system resources are freed */
    return ret;
}
