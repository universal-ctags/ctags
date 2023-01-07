// Derrived from glibc
# define __attribute_copy__(arg) __attribute__ ((__copy__ (arg)))
# define weak_alias(name, aliasname) _weak_alias (name, aliasname)
# define _weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name))) \
    __attribute_copy__ (name);

int
__brk (void *addr)
{
	/* ... */
	return 0;
}
weak_alias (__brk, brk)
