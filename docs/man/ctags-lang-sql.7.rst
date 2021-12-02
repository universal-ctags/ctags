.. _ctags-lang-sql(7):

==============================================================
ctags-lang-sql
==============================================================

The man page of the SQL parser for Universal Ctags

:Version: 5.9.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... [--extras={guest}] --languages=+SQL ...


DESCRIPTION
-----------
The SQL parser supports various SQL dialects. PostgreSQL is one of them.

PostgreSQL allows user-defined functions to be written in other
languages (*procedural languages*) besides SQL and C [PL]_.

The SQL parser makes tags for language objects in the user-defined
functions written in the procedural languages if the ``guest`` extra
is enabled.

The SQL parser looks for a token coming after ``LANGUAGE`` keyword in
the source code to choose a proper guest parser.

.. code-block:: SQL

   ... LANGUAGE plpythonu AS '... user-defined function ' ...
   ... AS $$ user-defined function $$ LANGUAGE plv8 ...

In the above examples, ``plpythonu`` and ``plv8`` are the names of
procedural languages. The SQL parser trims `pl` at the start and `u`
at the end of the name before finding a ctags parser.  For
``plpythonu`` and ``plv8``, the SQL parser extracts ``python`` and
``v8`` as the candidates of guest parsers.

For ``plpythonu``, ctags can run its Python parser.  ctags doesn't
have a parser named ``v8``. However, the JavaScript parser in ctags has
``v8`` as an alias. So ctags can run the JavaScript parser as the
guest parser for ``plv8``.

EXAMPLES
--------
tagging code including a user-defined function in a string literal [GH3006]_:

"input.sql"

.. code-block:: SQL

	CREATE OR REPLACE FUNCTION fun1() RETURNS VARCHAR AS '
	DECLARE
		test1_var1 VARCHAR(64) := $$ABC$$;
		test1_var2 VARCHAR(64) := $xyz$XYZ$xyz$;
		test1_var3     INTEGER := 1;
	BEGIN
		RETURN  TO_CHAR(test_var3, ''000'') || test1_var1 || test1_var2;
	END;
	' LANGUAGE plpgsql;

"output.tags"
with "--options=NONE -o - --sort=no --extras=+{guest} input.sql"

.. code-block:: tags

	fun1	input.sql	/^CREATE OR REPLACE FUNCTION fun1() RETURNS VARCHAR AS '$/;"	f
	test1_var1	input.sql	/^	test1_var1 VARCHAR(64) := $$ABC$$;$/;"	v
	test1_var2	input.sql	/^	test1_var2 VARCHAR(64) := $xyz$XYZ$xyz$;$/;"	v
	test1_var3	input.sql	/^	test1_var3     INTEGER := 1;$/;"	v

tagging code including a user-defined function in a dollar quote [GH3006]_:

"input.sql"

.. code-block:: SQL

	CREATE OR REPLACE FUNCTION fun2() RETURNS VARCHAR LANGUAGE plpgsql AS $$
	DECLARE
		test2_var1 VARCHAR(64) := 'ABC2';
		test2_var2 VARCHAR(64) := 'XYZ2';
		test2_var3        INTEGER := 2;
	BEGIN
		RETURN  TO_CHAR(test2_var3, '000') || test2_var1 || test2_var2;
	END;
	$$;

"output.tags"
with "--options=NONE -o - --sort=no --extras=+{guest} input.sql"

.. code-block:: tags

	fun2	input.sql	/^CREATE OR REPLACE FUNCTION fun2() RETURNS VARCHAR LANGUAGE plpgsql AS $\$$/;"	f
	test2_var1	input.sql	/^	test2_var1 VARCHAR(64) := 'ABC2';$/;"	v
	test2_var2	input.sql	/^	test2_var2 VARCHAR(64) := 'XYZ2';$/;"	v
	test2_var3	input.sql	/^	test2_var3        INTEGER := 2;$/;"	v

tagging code including a user-defined written in JavaScript:

.. code-block:: SQL

	-- Derived from https://github.com/plv8/plv8/blob/r3.0alpha/sql/plv8.sql
	CREATE FUNCTION test(keys text[], vals text[]) RETURNS text AS
	$$
		var o = {};
		for (var i = 0; i < keys.length; i++)
			o[keys[i]] = vals[i];
		return JSON.stringify(o);
	$$
	LANGUAGE plv8 IMMUTABLE STRICT;

"output.tags"
with "--options=NONE -o - --sort=no --extras=+{guest} input.sql"

.. code-block:: tags

	test	input.sql	/^CREATE FUNCTION test(keys text[], vals text[]) RETURNS text AS$/;"	f
	o	input.sql	/^	var o = {};$/;"	v

KNOWN BUGS
----------
Escape sequences (`''`) in a string literal may make a guest parser confused.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`

REFERENCES
----------

.. [PL] PostgreSQL 9.5.25 Documentation, "Chapter 39. Procedural Languages", https://www.postgresql.org/docs/9.5/xplang.html

.. [GH3006] @bagl's comment submitted to https://github.com/universal-ctags/ctags/issues/3006
