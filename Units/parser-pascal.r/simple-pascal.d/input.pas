PROGRAM hello;

TYPE
	simpletype = RECORD
					one: INTEGER;
				 END;


PROCEDURE helloproc(param1: STRING; param2: BYTE);
BEGIN
	writeln('Hello World!');
END;


FUNCTION max(num1, num2: INTEGER): INTEGER;
VAR
   result: INTEGER;
BEGIN
   if (num1 > num2) then
      result := num1

   else
      result := num2;
   max := result;
END;


FUNCTION noargs: STRING;
BEGIN
   noargs := 'functon without arguments';
END;

FUNCTION emptyargs(): STRING;
BEGIN
   emptyargs := 'functon without arguments';
END;


VAR result : INTEGER;
BEGIN
	helloproc('ignored', 1);
	result := max(73, 42);
	writeln('Result: ', result);
END.
