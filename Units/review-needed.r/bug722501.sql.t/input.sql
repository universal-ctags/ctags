/*
The PL/SQL parser (v1.6) does not parse a standalone
procedure or function (i.e., not part of a package) when
the SQL is in the form
*/
CREATE OR REPLACE PROCEDURE foo
AS /* or IS*/
    BEGIN
	DECLARE
	    l_foo NUMBER;
	BEGIN
	    l_foo := 1;
	END;
    END; 
/*
When this is processed the only tag reported is
Procedure foo. If you remove the line with AS, the
variable l_foo is seen, but then the procedure will not
compile in Oracle.

Functions seem to have similar problems in that the
parser will not see inside the function, but removing the
IS or AS does not remedy the problem for a function.
*/
