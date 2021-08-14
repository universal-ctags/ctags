-- Derived from a comment of https://github.com/universal-ctags/ctags/issues/3006
-- submitted by @bagl.
CREATE OR REPLACE FUNCTION fun1() RETURNS VARCHAR AS '
DECLARE
	test1_var1 VARCHAR(64) := $$ABC$$;
	test1_var2 VARCHAR(64) := $xyz$XYZ$xyz$;
	test1_var3     INTEGER := 1;
BEGIN
	RETURN  TO_CHAR(test_var3, ''000'') || test1_var1 || test1_var2;
END;
' LANGUAGE plpgsql;

-- Derived from a comment of https://github.com/universal-ctags/ctags/issues/3006
-- submitted by @bagl.
CREATE OR REPLACE FUNCTION fun2() RETURNS VARCHAR LANGUAGE plpgsql AS $$
DECLARE
	test2_var1 VARCHAR(64) := 'ABC2';
	test2_var2 VARCHAR(64) := 'XYZ2';
	test2_var3        INTEGER := 2;
BEGIN
	RETURN  TO_CHAR(test2_var3, '000') || test2_var1 || test2_var2;
END;
$$;

-- Derived from https://github.com/plv8/plv8/blob/r3.0alpha/sql/plv8.sql
CREATE FUNCTION test(keys text[], vals text[]) RETURNS text AS
$$
	var o = {};
	for (var i = 0; i < keys.length; i++)
		o[keys[i]] = vals[i];
	return JSON.stringify(o);
$$
LANGUAGE plv8 IMMUTABLE STRICT;

CREATE FUNCTION test2(keys text[], vals text[]) RETURNS text AS $ABCDEFGHIJKLMNOPQRSTUVWXYZ$
var q={}; return "{}";
 $ABCDEFGHIJKLMNOPQRSTUVWXYZ$ LANGUAGE plv8 IMMUTABLE STRICT;
-- The fist whitespace is needed to trigger the original bug.
