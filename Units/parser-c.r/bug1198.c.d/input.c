/*
 Bug 1198@github reported by lvc on 26/11/2016.
 
 The latest universal ctags 015eae7 doesn't detect function XESetCopyEventCookie
 from libX11-1.3 library (include/X11/Xlibint.h):
*/

extern Bool (*XESetCopyEventCookie(
    Display*            /* display */,
    int                 /* extension */,
    Bool (*) (
               Display*                 /* display */,
               XGenericEventCookie*     /* in */,
               XGenericEventCookie*     /* out */
             )          /* proc */
))(
    Display*, XGenericEventCookie*, XGenericEventCookie*
);

/* also adding simplified output found by masatake */

int (* f (char c)) (void * a);

static int g (void *a)
{
	return 1;
}

int (* f (char c)) (void * a)
{
	return g;
}