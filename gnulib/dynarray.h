/* Type-safe arrays which grow dynamically.
   Copyright 2021 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert and Bruno Haible, 2021.  */

#ifndef _GL_DYNARRAY_H
#define _GL_DYNARRAY_H

/* Before including this file, you need to define:

   DYNARRAY_STRUCT
      The struct tag of dynamic array to be defined.

   DYNARRAY_ELEMENT
      The type name of the element type.  Elements are copied
      as if by memcpy, and can change address as the dynamic
      array grows.

   DYNARRAY_PREFIX
      The prefix of the functions which are defined.

   The following parameters are optional:

   DYNARRAY_ELEMENT_FREE
      DYNARRAY_ELEMENT_FREE (E) is evaluated to deallocate the
      contents of elements. E is of type  DYNARRAY_ELEMENT *.

   DYNARRAY_ELEMENT_INIT
      DYNARRAY_ELEMENT_INIT (E) is evaluated to initialize a new
      element.  E is of type  DYNARRAY_ELEMENT *.
      If DYNARRAY_ELEMENT_FREE but not DYNARRAY_ELEMENT_INIT is
      defined, new elements are automatically zero-initialized.
      Otherwise, new elements have undefined contents.

   DYNARRAY_INITIAL_SIZE
      The size of the statically allocated array (default:
      at least 2, more elements if they fit into 128 bytes).
      Must be a preprocessor constant.  If DYNARRAY_INITIAL_SIZE is 0,
      there is no statically allocated array at, and all non-empty
      arrays are heap-allocated.

   DYNARRAY_FINAL_TYPE
      The name of the type which holds the final array.  If not
      defined, is PREFIX##finalize not provided.  DYNARRAY_FINAL_TYPE
      must be a struct type, with members of type DYNARRAY_ELEMENT and
      size_t at the start (in this order).

   These macros are undefined after this header file has been
   included.

   The following types are provided (their members are private to the
   dynarray implementation):

     struct DYNARRAY_STRUCT

   The following functions are provided:
 */

/* Initialize a dynamic array object.  This must be called before any
   use of the object.  */
#if 0
static void
       DYNARRAY_PREFIX##init (struct DYNARRAY_STRUCT *list);
#endif

/* Deallocate the dynamic array and its elements.  */
#if 0
static void
       DYNARRAY_PREFIX##free (struct DYNARRAY_STRUCT *list);
#endif

/* Return true if the dynamic array is in an error state.  */
#if 0
static bool
       DYNARRAY_PREFIX##has_failed (const struct DYNARRAY_STRUCT *list);
#endif

/* Mark the dynamic array as failed.  All elements are deallocated as
   a side effect.  */
#if 0
static void
       DYNARRAY_PREFIX##mark_failed (struct DYNARRAY_STRUCT *list);
#endif

/* Return the number of elements which have been added to the dynamic
   array.  */
#if 0
static size_t
       DYNARRAY_PREFIX##size (const struct DYNARRAY_STRUCT *list);
#endif

/* Return a pointer to the first array element, if any.  For a
   zero-length array, the pointer can be NULL even though the dynamic
   array has not entered the failure state.  */
#if 0
static DYNARRAY_ELEMENT *
       DYNARRAY_PREFIX##begin (const struct DYNARRAY_STRUCT *list);
#endif

/* Return a pointer one element past the last array element.  For a
   zero-length array, the pointer can be NULL even though the dynamic
   array has not entered the failure state.  */
#if 0
static DYNARRAY_ELEMENT *
       DYNARRAY_PREFIX##end (const struct DYNARRAY_STRUCT *list);
#endif

/* Return a pointer to the array element at INDEX.  Terminate the
   process if INDEX is out of bounds.  */
#if 0
static DYNARRAY_ELEMENT *
       DYNARRAY_PREFIX##at (struct DYNARRAY_STRUCT *list, size_t index);
#endif

/* Add ITEM at the end of the array, enlarging it by one element.
   Mark *LIST as failed if the dynamic array allocation size cannot be
   increased.  */
#if 0
static void
       DYNARRAY_PREFIX##add (struct DYNARRAY_STRUCT *list,
                             DYNARRAY_ELEMENT item);
#endif

/* Allocate a place for a new element in *LIST and return a pointer to
   it.  The pointer can be NULL if the dynamic array cannot be
   enlarged due to a memory allocation failure.  */
#if 0
static DYNARRAY_ELEMENT *
       DYNARRAY_PREFIX##emplace (struct DYNARRAY_STRUCT *list);
#endif

/* Change the size of *LIST to SIZE.  If SIZE is larger than the
   existing size, new elements are added (which can be initialized).
   Otherwise, the list is truncated, and elements are freed.  Return
   false on memory allocation failure (and mark *LIST as failed).  */
#if 0
static bool
       DYNARRAY_PREFIX##resize (struct DYNARRAY_STRUCT *list, size_t size);
#endif

/* Remove the last element of LIST if it is present.  */
#if 0
static void
       DYNARRAY_PREFIX##remove_last (struct DYNARRAY_STRUCT *list);
#endif

/* Remove all elements from the list.  The elements are freed, but the
   list itself is not.  */
#if 0
static void
       DYNARRAY_PREFIX##clear (struct DYNARRAY_STRUCT *list);
#endif

#if defined DYNARRAY_FINAL_TYPE
/* Transfer the dynamic array to a permanent location at *RESULT.
   Returns true on success on false on allocation failure.  In either
   case, *LIST is re-initialized and can be reused.  A NULL pointer is
   stored in *RESULT if LIST refers to an empty list.  On success, the
   pointer in *RESULT is heap-allocated and must be deallocated using
   free.  */
#if 0
static bool
       DYNARRAY_PREFIX##finalize (struct DYNARRAY_STRUCT *list,
                                  DYNARRAY_FINAL_TYPE *result);
#endif
#else /* !defined DYNARRAY_FINAL_TYPE */
/* Transfer the dynamic array to a heap-allocated array and return a
   pointer to it.  The pointer is NULL if memory allocation fails, or
   if the array is empty, so this function should be used only for
   arrays which are known not be empty (usually because they always
   have a sentinel at the end).  If LENGTHP is not NULL, the array
   length is written to *LENGTHP.  *LIST is re-initialized and can be
   reused.  */
#if 0
static DYNARRAY_ELEMENT *
       DYNARRAY_PREFIX##finalize (struct DYNARRAY_STRUCT *list,
                                  size_t *lengthp);
#endif
#endif

/* A minimal example which provides a growing list of integers can be
   defined like this:

     struct int_array
     {
       // Pointer to result array followed by its length,
       // as required by DYNARRAY_FINAL_TYPE.
       int *array;
       size_t length;
     };

     #define DYNARRAY_STRUCT dynarray_int
     #define DYNARRAY_ELEMENT int
     #define DYNARRAY_PREFIX dynarray_int_
     #define DYNARRAY_FINAL_TYPE struct int_array
     #include <malloc/dynarray-skeleton.c>

   To create a three-element array with elements 1, 2, 3, use this
   code:

     struct dynarray_int dyn;
     dynarray_int_init (&dyn);
     for (int i = 1; i <= 3; ++i)
       {
         int *place = dynarray_int_emplace (&dyn);
         assert (place != NULL);
         *place = i;
       }
     struct int_array result;
     bool ok = dynarray_int_finalize (&dyn, &result);
     assert (ok);
     assert (result.length == 3);
     assert (result.array[0] == 1);
     assert (result.array[1] == 2);
     assert (result.array[2] == 3);
     free (result.array);

   If the elements contain resources which must be freed, define
   DYNARRAY_ELEMENT_FREE appropriately, like this:

     struct str_array
     {
       char **array;
       size_t length;
     };

     #define DYNARRAY_STRUCT dynarray_str
     #define DYNARRAY_ELEMENT char *
     #define DYNARRAY_ELEMENT_FREE(ptr) free (*ptr)
     #define DYNARRAY_PREFIX dynarray_str_
     #define DYNARRAY_FINAL_TYPE struct str_array
     #include <malloc/dynarray-skeleton.c>
 */


/* The implementation is imported from glibc.  */

/* Avoid possible conflicts with symbols exported by the GNU libc.  */
#define __libc_dynarray_at_failure gl_dynarray_at_failure
#define __libc_dynarray_emplace_enlarge gl_dynarray_emplace_enlarge
#define __libc_dynarray_finalize gl_dynarray_finalize
#define __libc_dynarray_resize_clear gl_dynarray_resize_clear
#define __libc_dynarray_resize gl_dynarray_resize

#if defined DYNARRAY_STRUCT || defined DYNARRAY_ELEMENT || defined DYNARRAY_PREFIX

# ifndef _GL_LIKELY
/* Rely on __builtin_expect, as provided by the module 'builtin-expect'.  */
#  define _GL_LIKELY(cond) __builtin_expect ((cond), 1)
#  define _GL_UNLIKELY(cond) __builtin_expect ((cond), 0)
# endif

/* Define auxiliary structs and declare auxiliary functions, common to all
   instantiations of dynarray.  */
# include <malloc/dynarray.gl.h>

/* Define the instantiation, specified through
     DYNARRAY_STRUCT
     DYNARRAY_ELEMENT
     DYNARRAY_PREFIX
   etc.  */
# include <malloc/dynarray-skeleton.gl.h>

#else

/* This file is being included from one of the malloc/dynarray_*.c files.  */
# include <malloc/dynarray.h>

#endif

#endif /* _GL_DYNARRAY_H */
