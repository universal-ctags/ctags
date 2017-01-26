* Settings ***
Library           test.py

*** Variables ***
${var 1}  a
${var_2}   b
@{list 1}   a  b  c
@{list_2}  d  e  f

*** Test Cases ***
Test case 1
    Keyword 1  ${var 1}
    Keyword_2  ${var_1}
    Keyword 3  ${var 2}
    Keyword 3  ${var_2}
    Keyword 3  @{list 1}
    Keyword 3  @{list_1}
    Keyword 3  @{list 2}
    Keyword 3  @{list_2}
    Keyword 4  @{list_2}
    Keyword with a variable name
    it's ok to be correct

*** Keywords ***
Keyword 1
    [Arguments]  ${arg}
    python keyword 1

Keyword 2
    [Arguments]  ${arg}
    python_keyword_2

Keyword_3
    [Arguments]  @{arg1}
    Python_keyword_2

Keyword_4    [Arguments]  @{arg1}
    Python_keyword_2

Keyword ${with variable} name
    Python_keyword_2

it's ok to be correct
    Python_keyword_2

KeywordWithNoSpace
    [Arguments]  ${arg}
    python keyword 1
