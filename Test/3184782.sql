create or replace package body p_test is

procedure do_this_stuff is begin
if true then
    for idx in z+1 .. myarr.last loop
        if myarr.exists(idx) then
            null;
        end if;
    end loop;

elsif true then
    for idx in myarr.first .. myarr.last loop
        if myarr.exists(idx) then
            null;
        end if;
    end loop;
end if;
end do_this_stuff;

procedure process_this (
    p_flag in boolean
) is
begin
    null;
end process_this;

procedure myfn1 (
    p_str1 in varchar2,
    p_str2 in varchar2
) is begin
process_this(false);
end myfn1;

procedure myfn2 (
    p_str1 in varchar2,
    p_str2 in varchar2
) is begin
process_this(true);
end myfn2;

end p_test;
