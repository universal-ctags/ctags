CREATE CAST (type_a AS type_b)
    WITH FUNCTION type_conversion_function(type_a)
    AS IMPLICIT;

CREATE OR REPLACE VIEW my_view AS
SELECT 1;
