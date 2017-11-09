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

#Region All functions
Func MyDouble($iValue)
    $iValue = $iValue * 2
    Return $iValue
EndFunc   ;==>MyDouble

func MyDouble0($iValue)
    $iValue = $iValue * 2
    Return $iValue
EndFunc   ;==>MyDouble

    	FUNC MyDouble1($iValue)
    $iValue = $iValue * 2
    Return $iValue
EndFunc
#EndRegion All functions
