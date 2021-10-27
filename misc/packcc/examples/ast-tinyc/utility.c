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

#include "utility.h"

#include <stdlib.h>

#ifndef CHAR_ARRAY_MIN
#define CHAR_ARRAY_MIN 256 /* the minimum number of char_array_t elements to be allocated */
#endif

#ifndef SIZE_T_ARRAY_MIN
#define SIZE_T_ARRAY_MIN 16 /* the minimum number of size_t_array_t elements to be allocated */
#endif

void char_array__initialize(char_array_t *obj) {
    obj->m = 0;
    obj->n = 0;
    obj->p = NULL;
}

void char_array__finalize(char_array_t *obj) {
    free(obj->p);
}

bool_t char_array__resize(char_array_t *obj, size_t size) {
    if (obj->m < size) {
        size_t m = obj->m;
        if (m == 0) m = CHAR_ARRAY_MIN;
        while (m < size && m != 0) m <<= 1;
        if (m == 0) m = size; /* in case of shift overflow */
        char *const p = (char *)realloc(obj->p, m);
        if (p == NULL) return BOOL_FALSE;
        obj->p = p;
        obj->m = m;
    }
    obj->n = size;
    return BOOL_TRUE;
}

void size_t_array__initialize(size_t_array_t *obj) {
    obj->m = 0;
    obj->n = 0;
    obj->p = NULL;
}

void size_t_array__finalize(size_t_array_t *obj) {
    free(obj->p);
}

bool_t size_t_array__resize(size_t_array_t *obj, size_t size) {
    if (obj->m < size) {
        size_t m = obj->m;
        if (m == 0) m = SIZE_T_ARRAY_MIN;
        while (m < size && m != 0) m <<= 1;
        if (m == 0) m = size; /* in case of shift overflow */
        size_t *const p = (size_t *)realloc(obj->p, sizeof(size_t) * m);
        if (p == NULL) return BOOL_FALSE;
        obj->p = p;
        obj->m = m;
    }
    obj->n = size;
    return BOOL_TRUE;
}
