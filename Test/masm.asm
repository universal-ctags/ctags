.MODEL medium
.DATA
Msg	DB "Text string"
.CODE 
QUACK  PROC PUBLIC
        enter   0, 0             ; May be an instruction or a macro
        mov     bx, OFFSET DGROUP:Msg
        mov     bx, [bp+6]
        mov     WORD PTR [bx], ax
        leave
        ret    2
QUACK  ENDP
END

myequ	EQU    3
myequal = 4

; http://www.xploiter.com/mirrors/asm/asm_1.htm
hllequal := 4

BYTE_BUFFER    LABEL     BYTE
WORD_BUFFER    DW        512 dup (?)

 mov     bx, ax
LabelMaker1:
        xor     ax, ax
LabelMaker2:

mymacro macro args
endm

mystruct struct
ends
