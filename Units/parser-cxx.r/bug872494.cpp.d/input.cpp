/*
Bugs item #872494, was opened at 2004-01-07 16:33
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=872494&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Igor Proskuriakov (proskig)
Assigned to: Nobody/Anonymous (nobody)
Summary: C++ class template specialization

Initial Comment:
Hi, 
some time ago I posted a bug related to C++ member 
template, which was fixed using patch bug849591.diff. 
Many thanks - it really fixed the problem ! 
Now about a different one.
When parsing C++ file ctags seems to ignore template 
specialization, but I think that it maybe useful to add it 
as a configurable feature. Another problem is that 
template specialization seems to confuse patched 
version of ctags (patch bug849591.diff). When running a 
patched version against the following file
*/
template<class T> class TemplClass { double i;};

template<> class TemplClass< char* > { int i;};

class FooClass2{};
/*
it does not generate tag for FooClass2 while ctags 5.5.2 
does generate tag for FooClass2. Neither of them 
generate tag for specialization TemplClass<char *>.

thanks in advance and let me know should you need 
more info!

Igor
*/
