// Bug reported by hierabyss on github.com.
// funb() was not present in the ctags output.

bool funa()
{
    if (first) return false;

#if defined (MACRO)
    if (a)
    {
        if (second)
#else               
    if (a)
    {
#endif              
        return false;
    }

    return true;
}

int funb()
{
}