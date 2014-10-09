Gosub start

Global a.l
Dim b.l(100)

Structure test
    a.l
    b.l
EndStructure

Procedure f.l()
    a = 3
EndProcedure

start:
    f()
Return

