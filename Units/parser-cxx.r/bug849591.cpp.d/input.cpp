/*
Bugs item #849591, was opened at 2003-11-26 11:35
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=849591&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Igor Proskuriakov (proskig)
Assigned to: Nobody/Anonymous (nobody)
Summary: C++ Member templates 

Initial Comment:
Hi,
There seems to be a problem with how ctags treats C++ 
member templates. Example:
*/
void MainClass< ParamClass1&, ParamClass2>::Foo()
{
  exit(0);
};
/*
Generates erroneous tag
ParamClass1	test.cpp	/^void MainClass< ParamClass1&, ParamClass2>::Foo()$/;"	v

Full ctags file:
!_TAG_FILE_FORMAT	2	/extended format; --format=1 will not append ;" to lines/
!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted, 2=foldcase/
!_TAG_PROGRAM_AUTHOR	Darren Hiebert	/dhiebert@users.sourceforge.net/
!_TAG_PROGRAM_NAME	Exuberant Ctags	//
!_TAG_PROGRAM_URL	http://ctags.sourceforge.net	/official site/
!_TAG_PROGRAM_VERSION	5.5.2	//
Foo	test.cpp	/^void MainClass< const ParamClass1&, ParamClass2>::Foo()$/;"	f	class:ParamClass2
ParamClass1	test.cpp	/^void MainClass< ParamClass1&, ParamClass2>::Foo()$/;"	v

-----

Removing space after first angle bracket:
*/
void MainClass<ParamClass1&, ParamClass2>::Foo()
{
  exit(0);
};
/*
makes ctags to drop first letter from parameter class 
name:

aramClass1	test.cpp	/^void MainClass<ParamClass1&, ParamClass2>::Foo()$/;"	v


Let me know if you need any clarification with this issue.
Many thanks in advance!
*/
