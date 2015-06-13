        program writedate
        structure /start/
            union
                map
                    character*2 month
                    character*2 day
                    character*2 year
                end map
                map
                    character*6 date
                end map
            end union
        end structure
        record /start/ sdate
        sdate.month = '08'
        sdate.day =   '10'
        sdate.year =  '89'
        write (*, 10) sdate.date
10      format (a)
        stop
        end
