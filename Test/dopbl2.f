! Bug reported by Brian Helinski <bjh@absoft.com> on 4 Feb 2003
      DOUBLE PRECISION FUNCTION DOPBL2( SUBNAM, M, N, KKL, KKU )
*
*  -- LAPACK timing routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*6        SUBNAM
      INTEGER            KKL, KKU, M, N
*     ..
*
*  Purpose
*  =======
*
*  DOPBL2 computes an approximation of the number of floating point
*  operations used by a subroutine SUBNAM with the given values
*  of the parameters M, N, KL, and KU.
*
*  This version counts operations for the Level 2 BLAS.
*
*  Arguments
*  =========
*
*  SUBNAM  (input) CHARACTER*6
*          The name of the subroutine.
*
*  M       (input) INTEGER
*          The number of rows of the coefficient matrix.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the coefficient matrix.
*          If the matrix is square (such as in a solve routine) then
*          N is the number of right hand sides.  N >= 0.
*
*  KKL     (input) INTEGER
*          The lower band width of the coefficient matrix.
*          KL is set to max( 0, min( M-1, KKL ) ).
*
*  KKU     (input) INTEGER
*          The upper band width of the coefficient matrix.
*          KU is set to max( 0, min( N-1, KKU ) ).
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        C3
      DOUBLE PRECISION   ADDS, EK, EM, EN, KL, KU, MULTS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. .NOT.( LSAME( SUBNAM, 'S' ) .OR. LSAME( SUBNAM,
     $    'D' ) .OR. LSAME( SUBNAM, 'C' ) .OR. LSAME( SUBNAM, 'Z' ) ) )
     $     THEN
         DOPBL2 = 0
         RETURN
      END IF
*
      C1 = SUBNAM( 1: 1 )
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      MULTS = 0
      ADDS = 0
      KL = MAX( 0, MIN( M-1, KKL ) )
      KU = MAX( 0, MIN( N-1, KKU ) )
      EM = M
      EN = N
      EK = KL
*
*     -------------------------------
*     Matrix-vector multiply routines
*     -------------------------------
*
      IF( LSAMEN( 3, C3, 'MV ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
            MULTS = EM*( EN+1.D0 )
            ADDS = EM*EN
*
*        Assume M <= N + KL and KL < M
*               N <= M + KU and KU < N
*        so that the zero sections are triangles.
*
         ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
            MULTS = EM*( EN+1.D0 ) - ( EM-1.D0-KL )*( EM-KL ) / 2.D0 -
     $              ( EN-1.D0-KU )*( EN-KU ) / 2.D0
            ADDS = EM*( EN+1.D0 ) - ( EM-1.D0-KL )*( EM-KL ) / 2.D0 -
     $             ( EN-1.D0-KU )*( EN-KU ) / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 )
            ADDS = EM*EM
*
         ELSE IF( LSAMEN( 2, C2, 'SB' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHB' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) - ( EM-1.D0-EK )*( EM-EK )
            ADDS = EM*EM - ( EM-1.D0-EK )*( EM-EK )
*
         ELSE IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) )
     $             THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 -
     $              ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0 -
     $             ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
*
         END IF
*
*     ---------------------
*     Matrix solve routines
*     ---------------------
*
      ELSE IF( LSAMEN( 3, C3, 'SV ' ) ) THEN
*
         IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0
*
         ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 -
     $              ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
            ADDS = ( EM-1.D0 )*EM / 2.D0 -
     $             ( EM-EK-1.D0 )*( EM-EK ) / 2.D0
*
         END IF
*
*     ----------------
*     Rank-one updates
*     ----------------
*
      ELSE IF( LSAMEN( 3, C3, 'R  ' ) ) THEN
*
         IF( LSAMEN( 3, SUBNAM, 'SGE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'DGE' ) ) THEN
*
            MULTS = EM*EN + MIN( EM, EN )
            ADDS = EM*EN
*
         ELSE IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $            LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) / 2.D0 + EM
            ADDS = EM*( EM+1.D0 ) / 2.D0
*
         END IF
*
      ELSE IF( LSAMEN( 3, C3, 'RC ' ) .OR. LSAMEN( 3, C3, 'RU ' ) ) THEN
*
         IF( LSAMEN( 3, SUBNAM, 'CGE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZGE' ) ) THEN
*
            MULTS = EM*EN + MIN( EM, EN )
            ADDS = EM*EN
*
         END IF
*
*     ----------------
*     Rank-two updates
*     ----------------
*
      ELSE IF( LSAMEN( 3, C3, 'R2 ' ) ) THEN
         IF( LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'CHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'CHP' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHE' ) .OR.
     $       LSAMEN( 3, SUBNAM, 'ZHP' ) ) THEN
*
            MULTS = EM*( EM+1.D0 ) + 2.D0*EM
            ADDS = EM*( EM+1.D0 )
*
         END IF
      END IF
*
*     ------------------------------------------------
*     Compute the total number of operations.
*     For real and double precision routines, count
*        1 for each multiply and 1 for each add.
*     For complex and complex*16 routines, count
*        6 for each multiply and 2 for each add.
*     ------------------------------------------------
*
      IF( LSAME( C1, 'S' ) .OR. LSAME( C1, 'D' ) ) THEN
*
         DOPBL2 = MULTS + ADDS
*
      ELSE
*
         DOPBL2 = 6*MULTS + 2*ADDS
*
      END IF
*
      RETURN
*
*     End of DOPBL2
*
      END
