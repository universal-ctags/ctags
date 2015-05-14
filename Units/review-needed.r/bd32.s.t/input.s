* From: http://www.programmersheaven.com/zone5/cat462/3642.htm
* For 68000 assembler manual, see http://www.ece.iit.edu/ftp/242/Asm.doc
*
* test driver for BD32 programming command
* Scott Howard February 1992
* Format: TEST <parameter 1> [<parameter 2> ...]
* simply echoes the command line parameters back to the user

                opt     nol
                include ipd.inc
                opt     l
                dc.l    TEST            execution address
signon  dc.b    'TEST PROGRAM for BD32 programming drivers'
CRLF            dc.b    13,10,0         <cr>, <lf>, null
                even

* following is the mainline routine for the driver

TEST            move.l  a0,a1           get argv in a1
                move.l  d0,d2           get argc
                lea.l   signon(PC),a0   print signon string
                moveq.l #BD_PUTS,d0     use 'putstring' function in BD32
		bgnd
                cmpi.l  #2,d2           arg count < 2?
                bcc     test_1
                moveq.l #1,d1           bail out - error code 1
                bra     test_error
test_1          moveq.l #BD_PUTS,d0     puts () system call
                movea.l (a1)+,a0        point to next string
		bgnd
                moveq.l #' ',d1         print space between each one
                moveq.l #BD_PUTCHAR,d0
		bgnd
                subq    #1,d2
                bne     test_1          loop till done
		lea.l	CRLF(PC),a0	point to <cr> <lf> string
                moveq.l #BD_PUTS,d0     display it on user screen
		bgnd
                clr.l   d1              indicate 'no error' to BD32
test_error      moveq.l #BD_QUIT,d0     all done - quit
		bgnd

                END     TEST
