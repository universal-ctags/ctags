/*
Bugs item #823000, was opened at 2003-10-13 21:56
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=823000&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Jozsef Nagy (brutal)
Assigned to: Nobody/Anonymous (nobody)
Summary: PL/SQL functions and procedures

Initial Comment:
I've tried to use ctags (version 5.5.2) to navigate
PL/SQL source codes in vim but jumping to
function/procedure definitions worked not properly. I
figured out that the problem was that I've defined
these functions/procedures in multiline format, for
example:

test.pkb:
*/
CREATE OR REPLACE PACKAGE TEST IS

PROCEDURE TestFunc1
(
  arg1  IN NUMBER,
  arg2  IN NUMBER
)
IS
BEGIN
  NULL;
END TestFunc1;

PROCEDURE TestFunc2
(
  arg1  IN NUMBER,
  arg2  IN NUMBER
)
IS
BEGIN
  NULL;
END TestFunc2;

END TEST;
/
/*
ctags creates a regexp type pattern for my
functions/procedures but unfortunately the pattern was
/^IS$/ for each function/procedure, that's why the
jumping method worked really crappy. (see the output of
ctags 5.5.2 on this example code below)

ctags -f - --language-force=sql test.pkb
TEST    test.pkb        /^CREATE OR REPLACE PACKAGE
TEST IS$/;" P
TestFunc1       test.pkb        /^IS$/;"        p
TestFunc2       test.pkb        /^IS$/;"        p

I've looked into the ctags source code and I saw that
in parseSubProgram() in sql.c called makeSqlTag() only
when KEYWORD_is reached. I have modified this function
 so that makeSqlTag() is called immediately after a
function/procedure keyword and name is read (I have
attached the output of diff sql.c.5.5.2 sql.c). I don't
know much of ctags internals so I am not sure if this
is the best solution but it worked for me. (see the
output of my modified ctags below)

ctags -f - --language-force=sql test.pkb
TEST    test.pkb        /^CREATE OR REPLACE PACKAGE
TEST IS$/;" P
TestFunc1       test.pkb        /^PROCEDURE
TestFunc1$/;"       p
TestFunc2       test.pkb        /^PROCEDURE
TestFunc2$/;"       p
*/
