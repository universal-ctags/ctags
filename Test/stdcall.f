* Obtained from http://www.nag.com/local/nagping/np006a3.asp
*     D02CJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      CHARACTER        RELABS*15                                        !NEW
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=21*N+28)
*     .. Scalars in Common ..
      DOUBLE PRECISION H, XEND
      INTEGER          K
*     .. Local Scalars ..
      DOUBLE PRECISION PI, TOL, X
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION D02CJW, G, X01AAF
      STDCALL EXTERNAL D02CJW, G, X01AAF                                !CHANGE
*     .. External Subroutines ..
      STDCALL EXTERNAL D02CJF, D02CJX, FCN, OUT                         !CHANGE
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           XEND, H, K
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02CJF Example Program Results'
      RELABS = "Default"                                                !NEW
      XEND = 10.0D0
      PI = X01AAF(0.0D0)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 1: intermediate output, root-finding'
      DO 20 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         K = 4
         H = (XEND-X)/DBLE(K+1)
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         IFAIL = 0
*
         CALL D02CJF(X,XEND,N,Y,FCN,TOL,VAL(LOC(RELABS)),VAL(7),OUT,G,W,!CHANGE
     +               IFAIL)
*
         WRITE (NOUT,99998) '  Root of Y(1) = 0.0 at', X
         WRITE (NOUT,99997) '  Solution is', (Y(I),I=1,N)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 2: no intermediate output, root-finding'
      DO 40 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         IFAIL = 0
*
         CALL D02CJF(X,XEND,N,Y,FCN,TOL,VAL(LOC(RELABS)),VAL(7),D02CJX, !CHANGE
     +               G,W,IFAIL)
*
         WRITE (NOUT,99998) '  Root of Y(1) = 0.0 at', X
         WRITE (NOUT,99997) '  Solution is', (Y(I),I=1,N)
   40 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 3: intermediate output, no root-finding'
      DO 60 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         K = 4
         H = (XEND-X)/DBLE(K+1)
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         IFAIL = 0
*
         CALL D02CJF(X,XEND,N,Y,FCN,TOL,VAL(LOC(RELABS)),VAL(7),OUT,    CHANGE
     +               D02CJW,W,IFAIL)
*
   60 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'Case 4: no intermediate output, no root-finding ( integrate to XE
     +ND)'
      DO 80 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         WRITE (NOUT,*) '     X         Y(1)         Y(2)         Y(3)'
         WRITE (NOUT,99996) X, (Y(I),I=1,N)
         IFAIL = 0
*
         CALL D02CJF(X,XEND,N,Y,FCN,TOL,VAL(LOC(RELABS)),VAL(7),D02CJX, !CHANGE
     +               D02CJW,W,IFAIL)
*
         WRITE (NOUT,99996) X, (Y(I),I=1,N)
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D8.1)
99998 FORMAT (1X,A,F7.3)
99997 FORMAT (1X,A,3F13.5)
99996 FORMAT (1X,F8.2,3F13.5)
      END
*
      STDCALL SUBROUTINE OUT(X,Y)                                       !CHANGE
*     .. Parameters ..
      INTEGER        NOUT
      PARAMETER      (NOUT=6)
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION Y(N)
*     .. Scalars in Common ..
      DOUBLE PRECISION H, XEND
      INTEGER        I
*     .. Local Scalars ..
      INTEGER        J
*     .. Intrinsic Functions ..
      INTRINSIC      DBLE
*     .. Common blocks ..
      COMMON         XEND, H, I
*     .. Executable Statements ..
      WRITE (NOUT,99999) X, (Y(J),J=1,N)
      X = XEND - DBLE(I)*H
      I = I - 1
      RETURN
*
99999 FORMAT (1X,F8.2,3F13.5)
      END
*
      STDCALL SUBROUTINE FCN(T,Y,F)                                     !CHANGE
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
      F(3) = -0.032D0/Y(2)**2
      RETURN
      END
*
      STDCALL DOUBLE PRECISION FUNCTION G(T,Y)                          !CHANGE
*     .. Parameters ..
      INTEGER                     N
      PARAMETER                   (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            T
*     .. Array Arguments ..
      DOUBLE PRECISION            Y(N)
*     .. Executable Statements ..
      G = Y(1)
      RETURN
      END
