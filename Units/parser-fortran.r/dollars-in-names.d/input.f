C     Taken from #4033 submitted by @navinp0304.
      function name$()
      integer name$
      name$ = 42
      end function

      program main
      integer ret$
      ret$=name$()
      print *,ret$
      end program
