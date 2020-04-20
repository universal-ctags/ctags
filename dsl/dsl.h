/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef DSL_H
#define DSL_H

/*
 * INCLUDES
 */
#include "es-lang-c-stdc99.h"


/*
 * TYPES
 */

#define DSL_ERR_UNBOUND_VARIABLE    (es_error_intern("unbound-variable"))
#define DSL_ERR_TOO_FEW_ARGUMENTS   (es_error_intern("too-few-arguments"))
#define DSL_ERR_TOO_MANY_ARGUMENTS  (es_error_intern("too-many-arguments"))
#define DSL_ERR_NUMBER_REQUIRED     (es_error_intern("number-required"))
#define DSL_ERR_WRONG_TYPE_ARGUMENT (es_error_intern("wrong-type-argument"))


/*
 * MACROS
 */
#define dsl_throw(e,o)               return es_error_set_object(DSL_ERR_##e, o)


#endif
