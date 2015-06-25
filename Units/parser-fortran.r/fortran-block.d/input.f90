module test_block

contains

  SUBROUTINE suba()
    INTEGER :: a
    
    a = 5
    BLOCK
      INTEGER :: b
      b = a + 2
    END BLOCK
  END SUBROUTINE

  subroutine subb
  
  end subroutine

end module test_block
