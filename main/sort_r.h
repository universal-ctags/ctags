/* Taken from
 * https://raw.githubusercontent.com/noporpoise/sort_r/refs/heads/master/sort_r.h
 * 1275373d990162f7d142de2fcaf43b535061466c */
/* Isaac Turner 29 April 2014 Public Domain */
#ifndef SORT_R_H_
#define SORT_R_H_

#include <stdlib.h> /* qsort_r(), qsort_s() */
#include <string.h> /* needed for memcpy() */

/*

sort_r function to be exported.

Parameters:
  base is the array to be sorted
  nel is the number of elements in the array
  width is the size in bytes of each element of the array
  compar is the comparison function
  arg is a pointer to be passed to the comparison function

void sort_r(void *base, size_t nel, size_t width,
            int (*compar)(const void *_a, const void *_b, void *_arg),
            void *arg);

*/

#define _SORT_R_INLINE inline

#if (defined __APPLE__ || defined __MACH__ || defined __DARWIN__ || \
     (defined __FreeBSD__ && !defined(qsort_r)) || defined __DragonFly__)
#  define _SORT_R_BSD
#elif (defined __GLIBC__ || (defined (__FreeBSD__) && defined(qsort_r)))
#  define _SORT_R_LINUX
#elif (defined _WIN32 || defined _WIN64 || defined __WINDOWS__ || \
       defined __MINGW32__ || defined __MINGW64__)
#  define _SORT_R_WINDOWS
#  undef _SORT_R_INLINE
#  define _SORT_R_INLINE __inline
#else
  /* Using our own recursive quicksort sort_r_simple() */
#endif

#if (defined NESTED_QSORT && NESTED_QSORT == 0)
#  undef NESTED_QSORT
#endif

#define SORT_R_SWAP(a,b,tmp) ((tmp) = (a), (a) = (b), (b) = (tmp))

/* swap a and b */
/* a and b must not be equal! */
static _SORT_R_INLINE void sort_r_swap(char *__restrict a, char *__restrict b,
                                       size_t w)
{
  char tmp, *end = a+w;
  for(; a < end; a++, b++) { SORT_R_SWAP(*a, *b, tmp); }
}

/* swap a, b iff a>b */
/* a and b must not be equal! */
/* __restrict is same as restrict but better support on old machines */
static _SORT_R_INLINE int sort_r_cmpswap(char *__restrict a,
                                         char *__restrict b, size_t w,
                                         int (*compar)(const void *_a,
                                                       const void *_b,
                                                       void *_arg),
                                         void *arg)
{
  if(compar(a, b, arg) > 0) {
    sort_r_swap(a, b, w);
    return 1;
  }
  return 0;
}

/*
Swap consecutive blocks of bytes of size na and nb starting at memory addr ptr,
with the smallest swap so that the blocks are in the opposite order. Blocks may
be internally re-ordered e.g.

  12345ab  ->   ab34512
  123abc   ->   abc123
  12abcde  ->   deabc12
*/
static _SORT_R_INLINE void sort_r_swap_blocks(char *ptr, size_t na, size_t nb)
{
  if(na > 0 && nb > 0) {
    if(na > nb) { sort_r_swap(ptr, ptr+na, nb); }
    else { sort_r_swap(ptr, ptr+nb, na); }
  }
}

/* Implement recursive quicksort ourselves */
/* Note: quicksort is not stable, equivalent values may be swapped */
static _SORT_R_INLINE void sort_r_simple(void *base, size_t nel, size_t w,
                                         int (*compar)(const void *_a,
                                                       const void *_b,
                                                       void *_arg),
                                         void *arg)
{
  char *b = (char *)base, *end = b + nel*w;

  /* for(size_t i=0; i<nel; i++) {printf("%4i", *(int*)(b + i*sizeof(int)));}
  printf("\n"); */

  if(nel < 10) {
    /* Insertion sort for arbitrarily small inputs */
    char *pi, *pj;
    for(pi = b+w; pi < end; pi += w) {
      for(pj = pi; pj > b && sort_r_cmpswap(pj-w,pj,w,compar,arg); pj -= w) {}
    }
  }
  else
  {
    /* nel > 6; Quicksort */

    int cmp;
    char *pl, *ple, *pr, *pre, *pivot;
    char *last = b+w*(nel-1), *tmp;

    /*
    Use median of second, middle and second-last items as pivot.
    First and last may have been swapped with pivot and therefore be extreme
    */
    char *l[3];
    l[0] = b + w;
    l[1] = b+w*(nel/2);
    l[2] = last - w;

    /* printf("pivots: %i, %i, %i\n", *(int*)l[0], *(int*)l[1], *(int*)l[2]); */

    if(compar(l[0],l[1],arg) > 0) { SORT_R_SWAP(l[0], l[1], tmp); }
    if(compar(l[1],l[2],arg) > 0) {
      SORT_R_SWAP(l[1], l[2], tmp);
      if(compar(l[0],l[1],arg) > 0) { SORT_R_SWAP(l[0], l[1], tmp); }
    }

    /* swap mid value (l[1]), and last element to put pivot as last element */
    if(l[1] != last) { sort_r_swap(l[1], last, w); }

    /*
    pl is the next item on the left to be compared to the pivot
    pr is the last item on the right that was compared to the pivot
    ple is the left position to put the next item that equals the pivot
    ple is the last right position where we put an item that equals the pivot

                                           v- end (beyond the array)
      EEEEEELLLLLLLLuuuuuuuuGGGGGGGEEEEEEEE.
      ^- b  ^- ple  ^- pl   ^- pr  ^- pre ^- last (where the pivot is)

    Pivot comparison key:
      E = equal, L = less than, u = unknown, G = greater than, E = equal
    */
    pivot = last;
    ple = pl = b;
    pre = pr = last;

    /*
    Strategy:
    Loop into the list from the left and right at the same time to find:
    - an item on the left that is greater than the pivot
    - an item on the right that is less than the pivot
    Once found, they are swapped and the loop continues.
    Meanwhile items that are equal to the pivot are moved to the edges of the
    array.
    */
    while(pl < pr) {
      /* Move left hand items which are equal to the pivot to the far left.
         break when we find an item that is greater than the pivot */
      for(; pl < pr; pl += w) {
        cmp = compar(pl, pivot, arg);
        if(cmp > 0) { break; }
        else if(cmp == 0) {
          if(ple < pl) { sort_r_swap(ple, pl, w); }
          ple += w;
        }
      }
      /* break if last batch of left hand items were equal to pivot */
      if(pl >= pr) { break; }
      /* Move right hand items which are equal to the pivot to the far right.
         break when we find an item that is less than the pivot */
      for(; pl < pr; ) {
        pr -= w; /* Move right pointer onto an unprocessed item */
        cmp = compar(pr, pivot, arg);
        if(cmp == 0) {
          pre -= w;
          if(pr < pre) { sort_r_swap(pr, pre, w); }
        }
        else if(cmp < 0) {
          if(pl < pr) { sort_r_swap(pl, pr, w); }
          pl += w;
          break;
        }
      }
    }

    pl = pr; /* pr may have gone below pl */

    /*
    Now we need to go from: EEELLLGGGGEEEE
                        to: LLLEEEEEEEGGGG

    Pivot comparison key:
      E = equal, L = less than, u = unknown, G = greater than, E = equal
    */
    sort_r_swap_blocks(b, ple-b, pl-ple);
    sort_r_swap_blocks(pr, pre-pr, end-pre);

    /*for(size_t i=0; i<nel; i++) {printf("%4i", *(int*)(b + i*sizeof(int)));}
    printf("\n");*/

    sort_r_simple(b, (pl-ple)/w, w, compar, arg);
    sort_r_simple(end-(pre-pr), (pre-pr)/w, w, compar, arg);
  }
}


#if defined NESTED_QSORT

  static _SORT_R_INLINE void sort_r(void *base, size_t nel, size_t width,
                                    int (*compar)(const void *_a,
                                                  const void *_b,
                                                  void *aarg),
                                    void *arg)
  {
    int nested_cmp(const void *a, const void *b)
    {
      return compar(a, b, arg);
    }

    qsort(base, nel, width, nested_cmp);
  }

#else /* !NESTED_QSORT */

  /* Declare structs and functions */

  #if defined _SORT_R_BSD

    /* Ensure qsort_r is defined */
    extern void qsort_r(void *base, size_t nel, size_t width, void *thunk,
                        int (*compar)(void *_thunk,
                                      const void *_a, const void *_b));

  #endif

  #if defined _SORT_R_BSD || defined _SORT_R_WINDOWS

    /* BSD (qsort_r), Windows (qsort_s) require argument swap */

    struct sort_r_data
    {
      void *arg;
      int (*compar)(const void *_a, const void *_b, void *_arg);
    };

    static _SORT_R_INLINE int sort_r_arg_swap(void *s,
                                              const void *a, const void *b)
    {
      struct sort_r_data *ss = (struct sort_r_data*)s;
      return (ss->compar)(a, b, ss->arg);
    }

  #endif

  #if defined _SORT_R_LINUX

    typedef int(* __compar_d_fn_t)(const void *, const void *, void *);
    extern void (qsort_r)(void *base, size_t nel, size_t width,
                          __compar_d_fn_t __compar, void *arg)
      __attribute__((nonnull (1, 4)));

  #endif

  /* implementation */

  static _SORT_R_INLINE void sort_r(void *base, size_t nel, size_t width,
                                    int (*compar)(const void *_a,
                                                  const void *_b, void *_arg),
                                    void *arg)
  {
    #if defined _SORT_R_LINUX

      #if defined __GLIBC__ && ((__GLIBC__ < 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 8))

        /* no qsort_r in glibc before 2.8, need to use nested qsort */
        sort_r_simple(base, nel, width, compar, arg);

      #else

        qsort_r(base, nel, width, compar, arg);

      #endif

    #elif defined _SORT_R_BSD

      struct sort_r_data tmp;
      tmp.arg = arg;
      tmp.compar = compar;
      qsort_r(base, nel, width, &tmp, sort_r_arg_swap);

    #elif defined _SORT_R_WINDOWS

      struct sort_r_data tmp;
      tmp.arg = arg;
      tmp.compar = compar;
      qsort_s(base, nel, width, sort_r_arg_swap, &tmp);

    #else

      /* Fall back to our own quicksort implementation */
      sort_r_simple(base, nel, width, compar, arg);

    #endif
  }

#endif /* !NESTED_QSORT */

#undef _SORT_R_INLINE
#undef _SORT_R_WINDOWS
#undef _SORT_R_LINUX
#undef _SORT_R_BSD

#endif /* SORT_R_H_ */
