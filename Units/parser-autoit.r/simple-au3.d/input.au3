; Taken from https://www.autoitscript.com/autoit3/docs/intro/lang_functions.htm

#include <Constants.au3>
#include<GUIConstantsEx.au3>
#include "WindowsConstants.au3"

Local $iNumber = 10
Local $iDoubled = 0

For $i = 1 To 10
    $iDoubled = MyDouble($iNumber)
    MsgBox($MB_OK, "", $iNumber & " doubled is " & $iDoubled)
    $iNumber = $iDoubled
Next
Exit

#cs
Func Ignored()
EndFunc
#comments-start
Func Ignored0()
EndFunc
#comments-end
Func Ignored1()
EndFunc
#ce

#Region All functions
Func MyDouble($iValue)
    $iValue = $iValue * 2
    Return $iValue
EndFunc   ;==>MyDouble

func MyDouble0($iValue)
    Local $iSomething = 42
    $iValue = $iValue * 2
    Return $iValue
EndFunc   ;==>MyDouble

    	FUNC MyDouble1($iValue)
    $iValue = $iValue * 2
    Return $iValue
EndFunc

Volatile Func MyVolatileDouble ($iValue)
    Return $iValue * 2;
EndFunc

FUNC MyDummy($iValue)
    LOCAL STATIC $iFirst = 0
    STATIC LOCAL $iSecond = 1
    RETURN $iValue * $iSecond + $iFirst
ENDFUNC
#EndRegion All functions
