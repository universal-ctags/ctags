CREATE OR REPLACE PACKAGE demo_pkg
IS

FUNCTION func1_proto( value in varchar ) RETURNS number;
FUNCTION func2_proto( value in varchar ) RETURN number;

FUNCTION func1( value in varchar ) RETURNS number IS
BEGIN
	RETURN 1;
END func1;

FUNCTION func2( value in varchar ) RETURN number IS
BEGIN
	RETURN 2;
END func2;

END demo_pkg;
/
