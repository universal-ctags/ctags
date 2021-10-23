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

#ifndef INCLUDED_UTILITY_H
#define INCLUDED_UTILITY_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum bool_tag {
    BOOL_FALSE = 0,
    BOOL_TRUE
} bool_t;

typedef struct range_tag {
    size_t min; /* the start position (inclusive) */
    size_t max; /* the end position (exclusive) */
} range_t;

typedef struct char_array_tag {
    size_t m; /* the allocated length */
    size_t n; /* the actual length */
    char *p; /* the buffer */
} char_array_t;

typedef struct size_t_array_tag {
    size_t m; /* the allocated length */
    size_t n; /* the actual length */
    size_t *p; /* the buffer */
} size_t_array_t;

void char_array__initialize(char_array_t *obj);
void char_array__finalize(char_array_t *obj);
bool_t char_array__resize(char_array_t *obj, size_t size);

void size_t_array__initialize(size_t_array_t *obj);
void size_t_array__finalize(size_t_array_t *obj);
bool_t size_t_array__resize(size_t_array_t *obj, size_t size);

#ifdef __cplusplus
}
#endif

inline static range_t range__void(void) {
    const range_t obj = { 0, 0 };
    return obj;
}

inline static range_t range__new(size_t min, size_t max) {
    const range_t obj = { min, max };
    return obj;
}

#endif /* !INCLUDED_UTILITY_H */
