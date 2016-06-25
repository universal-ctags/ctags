#include <a.h>
#include "b.h"

#include <sys/c.h>
#include "sys/d.h"

/* this is actually a syntax error */
"
#include       <K.h>
#include       \"L.h\"
"

#include <../e.h>
#include "../f.h"

int
M(void)
{
  return 0;
}

#define D 1

#include<g.h>
#include"h.h"

#include       <i.h>
#include       "j.h"

   #    	include <stdio.h>
/* objc constant String */
@"#include<y.h>"
