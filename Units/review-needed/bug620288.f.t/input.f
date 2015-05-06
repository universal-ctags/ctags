C Bugs item #620288, was opened at 2002-10-08 08:15
C You can respond by visiting: 
C https://sourceforge.net/tracker/?func=detail&atid=106556&aid=620288&group_id=6556
C 
C Category: None
C Group: None
C Status: Open
C Resolution: None
C Priority: 5
C Submitted By: Nobody/Anonymous (nobody)
C Assigned to: Nobody/Anonymous (nobody)
C Summary: fortran function definition
C 
C Initial Comment:
C 
C System Information:
C --------------
C ctags version: 
C Exuberant Ctags 5.3.1, Copyright (C) 1996-2002 Darren 
C Hiebert
C   Compiled: Sep 12 2002, 10:22:42
C   Addresses: <dhiebert@users.sourceforge.net>, 
C http://ctags.sourceforge.net
C   Optional compiled features: +wildcards, +regex
C 
C Unix:
C HP-UX B.11.00 A 9000/800 551726527
C --------------
C 
C Symptoms:
C --------------
C I have a fortran file that has a single function.  This 
C function has a
C return type of double precision.  I type $ctags bar.f .  
C This produces the expected 
C tags file.  However, tags does not contain the function 
C definition.
C 
C This does not occur when the same function's return 
C type has been changed to 
C a integer or character data return type.
C --------------
C 
C 
C Examples in which I can repeat the experience with their 
C respective tags files:
C --------------
C 
C 
C foo.f
C --------------
	integer function foo(a)

	integer a
	foo = a
	end
C --------------
C 
C 
C tags
C --------------
C !_TAG_FILE_FORMAT	2	/extended 
C format; --format=1 will not append ;" to lines/
C !_TAG_FILE_SORTED	1
C 	/0=unsorted, 1=sorted, 2=foldcase/
C !_TAG_PROGRAM_AUTHOR	Darren Hiebert
C 	/dhiebert@users.sourceforge.net/
C !_TAG_PROGRAM_NAME	Exuberant Ctags	//
C !_TAG_PROGRAM_URL
C 	http://ctags.sourceforge.net	/official site/
C !_TAG_PROGRAM_VERSION	5.3.1	//
C foo	foo.f	/^	integer function foo(/;"	f
C --------------
C 
C 
C 
C bar.f	
C ---------------
	double precision function bar(a)
	
	double precision a
	bar = a
	end
C --------------	
C 
C tags
C ---------------
C !_TAG_FILE_FORMAT	2	/extended 
C format; --format=1 will not append ;" to lines/
C !_TAG_FILE_SORTED	1
C 	/0=unsorted, 1=sorted, 2=foldcase/
C !_TAG_PROGRAM_AUTHOR	Darren Hiebert
C 	/dhiebert@users.sourceforge.net/
C !_TAG_PROGRAM_NAME	Exuberant Ctags	//
C !_TAG_PROGRAM_URL
C 	http://ctags.sourceforge.net	/official site/
C !_TAG_PROGRAM_VERSION	5.3.1	//
C ---------------
