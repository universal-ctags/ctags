#
# This test input is taken from #1570 opened by @mMontu.
#
*** Variables ***
${myvar}    variable_value

*** Keywords ***

My Regular Keyword
    Sleep  5s

${embedded arg} Starting Single
    No Operation

${embedded arg1} Starting ${embedded arg2} Multiple
    No Operation

${embedded arg1} Starting And ${embedded arg2} Ending ${embedded arg3}
    No Operation

Middle ${embedded arg} Single Arguments
    No Operation

Middle ${embedded arg1} Multiple ${embedded arg2} Arguments
    No Operation

Ending Single ${embedded arg}
    No Operation

Ending Multiple ${embedded arg1} And ${embedded arg2}
    No Operation
