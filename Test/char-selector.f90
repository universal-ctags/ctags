! Test for bug in parsing of char-selector
      MODULE OUT_RD5
      USE inmdmx
      IMPLICIT NONE
      SAVE

      CHARACTER*(MXPATHLNGTH) temp
      PRIVATE :: temp
      INTEGER(4), PRIVATE, PARAMETER :: MXDDI=45
      CHARACTER*40 titles(MXDDI)
      PRIVATE :: titles
      CHARACTER*12 units(MXDDI)
      PRIVATE :: units
      INTEGER(4), PRIVATE, DIMENSION (MXDDI+1) :: list
      INTEGER(4), PRIVATE :: nout
      END MODULE OUT_RD5
