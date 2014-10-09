        program uninitialized_structure
        structure /weather/
        integer        month, day, year
        character*20   clouds
        real           rainfall
        end structure
        record /weather/ latest
        end

        program initialized_structure
        structure /weather/
                integer*1     month /08/, day /10/, year /89/
                character*20  clouds /' overcast'/
                real   rainfall /3.12/
        end structure
        record /weather/ latest
        print *, latest.month, latest.day, latest.year, 
      + latest.clouds, latest.rainfall
        end program

        program nested_structure
        structure /top/
            structure /nested/ level2
                structure level3a, level3b
                    integer a
                end structure
            end structure
        end structure
        end program
