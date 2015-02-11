package P is
    type T;
    type T is record
        One: access T;
        -- Missing semicolon used to choke the Ada parser
        Two: access T
    end record;
end P;
