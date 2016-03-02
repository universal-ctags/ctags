CREATE FUNCTION X RETURNS void AS $$
BEGIN
	IF 0 = 0 THEN
	   ;
	 END IF;
END;
$$ LANGUAGE plpgsql;

-- Taken from https://www.postgresql.jp/document/9.2/html/sql-createfunction.html
CREATE OR REPLACE FUNCTION Y(i integer) RETURNS integer AS $$
        BEGIN
                RETURN i + 1;
        END;
$$ LANGUAGE plpgsql;
