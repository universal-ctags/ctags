/*
Bugs item #665086, was opened at 2003-01-09 15:30
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=665086&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Welti Marco (cider101)
Assigned to: Nobody/Anonymous (nobody)
Summary: nested namespaces

Initial Comment:
hi

it seems that ctags has ommits the scope for nested 
namespaces.
*/
namespace N1
{
  namespace N2
  {
    class C12{}
  }
}
/*
N1	test.h	/^namespace N1$/;"	namespace	line:1
N2	test.h	/^namespace N2$/;"	namespace	line:3
C12	test.h	/^  class C12{};$/;"	class	line:5	namespace:N1::N2
*/
