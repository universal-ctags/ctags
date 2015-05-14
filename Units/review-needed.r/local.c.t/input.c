main ()
{
    int a;
    int b = 3;
    a = 2;
}

static boolean isContextualKeyword (const tokenInfo *const token)
{
    boolean result;
label:
    goto label;
    switch (token->keyword)
    {
	case KEYWORD_UNION:
	    result = TRUE;
	    break;

	default: result = FALSE; break;
    }
    return result;
}
