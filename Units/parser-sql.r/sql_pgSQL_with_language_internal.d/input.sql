CREATE FUNCTION isnlt(ean13, ean13)
        RETURNS boolean
        AS 'int8lt'
        LANGUAGE 'internal'
        IMMUTABLE STRICT
        PARALLEL SAFE;
CREATE FUNCTION isnle(ean13, ean13)
        RETURNS boolean
        AS 'int8le'
        LANGUAGE 'internal'
        IMMUTABLE STRICT
        PARALLEL SAFE;
