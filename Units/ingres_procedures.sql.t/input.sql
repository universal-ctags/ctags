CREATE PROCEDURE db0001 (short_name varchar(12) not null)  
BEGIN
END;

PROCEDURE db0002 (short_name varchar(12) not null)  =
DECLARE
x = varchar(12) not null; 
n = i4 not null; 
err = varchar(80) not null not default; 
BEGIN 

    if short_name = '' then 
        err := 'db0001: Pusty short_name!'; 
        raise error 8001 :err; 
        return -1; 
    endif; 

    select :x = bank_sh_name from "cafa". banks_directory_066 
    where bank_sh_name = :short_name; 
    if iierrornumber != 0 THEN rollback; 
        err := 'db0001: Podczas sprawdzania unikalnosci short_name 
        banku wystapil blad!'; 
        raise error 8001 :err; 
        return -1; 
    endif; 

    n := iirowcount; 
    commit; 
    return n; 
END;  

-- OR with AS instead of =:


PROCEDURE db0003 (short_name varchar(12) not null)  
AS
DECLARE
x = varchar(12) not null; 
n = i4 not null; 
err = varchar(80) not null not default; 
BEGIN 

    if short_name = '' then 
        err := 'db0001: Pusty short_name!'; 
        raise error 8001 :err; 
        return -1; 
    endif; 

    select :x = bank_sh_name from "cafa". banks_directory_066 
    where bank_sh_name = :short_name; 
    if iierrornumber != 0 THEN rollback; 
        err := 'db0001: Podczas sprawdzania unikalnosci short_name 
        banku wystapil blad!'; 
        raise error 8001 :err; 
        return -1; 
    endif; 

    n := iirowcount; 
    commit; 
    return n; 
END  


