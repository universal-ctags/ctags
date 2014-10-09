Gosub start

Const one# = 1.0

Global a
Dim b(100)

Type test
    Field a
    Field b
End Type

Function f()
    a = 3
End Function

.start
    f()
Return

