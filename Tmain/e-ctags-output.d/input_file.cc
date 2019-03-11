/* Could NOT be parsed well.
   The signature should be  recorded well. */
int32 test(int32 a)
{
    return 0;
}
/* Can be parsed */
int32 test2(void)
{
    return 0;
}

/* A tab is included in signature.
   However, it should be recorded well after converting
   the tab to a whitespace. */
int32 test3(int32	a)
{
    return 0;
}

/* A newline is included in signature.
   However, it should be recorded well after converting
   the newline to a whitespace. */
int32 test4(int32
a)
{
    return 0;
}
