! this is a comment and continuation must not be handled in it &
! this is another line of comment

#if dummy
function &
                                ! just a test for support of comments after a line continuation
                                ! below is a preprocessor line, also just to test continuation
#endif
                                ! still testing
  & do_stuff(a)
end function

  function &
                                ! actually the ampersand isn't required at the start of the continuation line
#pragma stuff
  do_stuff_again(b)
end function do_stuff_again
