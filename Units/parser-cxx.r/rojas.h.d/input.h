/* Problem reported by Emil Rojas <emil@lapel.com> on 22 May 2002 */
/* Fixed by reinitializing statement when either of "extern", "static", or
 * "typedef" keywords are read.
 */
# ifdef NOT_DEFINED
    typedef unsigned long uint32 //; // remove comment before ";" to make ctags work
# endif

extern "C" {

typedef void * FooBar;
FooBar * afunc (const char * aparam);
struct astruct
{
    int m_member;
};
typedef struct astruct astruct;
typedef uint32 (*FFunc) (const astruct * pP, int n);

}
