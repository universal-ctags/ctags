/*
From noreply@sourceforge.net Wed May 29 23:11:25 2002
Date: Wed, 15 May 2002 23:25:52 -0700
From: noreply@sourceforge.net
To: noreply@sourceforge.net
Subject: [ ctags-Bugs-556645 ] Some typedef can not be tagged in C code

Bugs item #556645, was opened at 2002-05-16 14:25
You can respond by visiting: 
http://sourceforge.net/tracker/?func=detail&atid=106556&aid=556645&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Howard Wu (howardhbwu)
Assigned to: Nobody/Anonymous (nobody)
Summary: Some typedef can not be tagged in C code

Initial Comment:
My Ctags version:5.2.3

The typedef of "Qtype" as the following,

*/
#define A1(_type, _length)                  \
struct  {                                   \
    	    unsigned int    head;           \
    	    unsigned int    tail;           \
    	    bool            is_full;        \
    	    _type           queue[_length]; \
	}

typedef A1(ilm_struct, 1) Qtype;

/*
As using ctags, the "Qtype" can not be tagged by it.
*/
