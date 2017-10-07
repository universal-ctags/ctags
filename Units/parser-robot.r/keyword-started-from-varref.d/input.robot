#
# This test input is taken from #1572 opened by @mMontu.
#
*** Variables ***

${myvar}    variable_value

*** Keywords ***

My Regular Keyword
    Sleep  5s

${embedded arg} Starting Single
    No Operation

${e} Starting Single Letter
    No Operation


${embedded arg:value1|value2} Starting Single With Regex
    No Operation

${embedded arg1} Starting ${embedded arg2} Multiple
    No Operation

${embedded arg1} Starting ${embedded arg2:value1|value2} Multiple With Regex
    No Operation

${embedded arg1} Starting And ${embedded arg2} Ending ${embedded arg3}
    No Operation

Middle ${embedded arg} Single Arguments
    No Operation

Middle ${embedded arg1} Multiple ${embedded arg2} Arguments
    No Operation

Middle ${embedded arg:value1|value2} Single Arguments With Regex
    No Operation

Middle ${e} Single Letter Arguments
    No Operation

Ending Single ${embedded arg}
    No Operation

Ending Multiple ${embedded arg1} And ${embedded arg2}
    No Operation

My Keyword_with_underscore-and-dashes
    No Operation

My Keyword_With $ dollar sign
    No Operation

*** Test Cases ***

My Test With Template1        My Kw 1        My Kw 2
