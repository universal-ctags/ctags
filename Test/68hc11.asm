INTERRUPTS:
;
PORTD       EQU  $1008
;
SP          EQU  $20
;
TRAPP       FCB CR,LF
            FCB CR,LF
            FCC  '    ******** ILLEGAL OPCODE TRAP !!! ********'
            FCB CR,LF
            FCB 0
PROMPT      FCB CR,LF
            FCC  '               68Mon V1.2 (C) 1992 Keith Vasilakes'
            FCB CR,LF
            FCB 0
COLD        LDS  #STACK
            LDAA #$20
