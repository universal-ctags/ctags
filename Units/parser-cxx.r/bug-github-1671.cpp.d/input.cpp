/* Bug #1671 reported by davisking on 2018.01.28 */

template <long x, typename enabled=void>
struct tabs { const static long value = x; };

// specialize tabs
template <long x>
struct tabs<x,typename enable_if_c<(x<0)>::type> { const static long value = -x; };

void my_function()
{
}
