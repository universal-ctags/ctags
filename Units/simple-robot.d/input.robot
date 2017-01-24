*** Settings ***
Library           test.py

*** Variables ***
${var 1}  a
${var_2}   b
@{list 1}   a  b  c
@{list_2}  d  e  f

*** Test Case ***
Test case 1
    Keyword 1  ${var 1}
    Keyword_2  ${var_1}
    Keyword 3  ${var 2}
    Keyword 3  ${var_2}
    Keyword 3  @{list 1}
    Keyword 3  @{list_1}
    Keyword 3  @{list 2}
    Keyword 3  @{list_2}
    Keyword with a variable name
    Keyword two with a variable name
    it's ok to be correct

*** Keyword ***
Keyword 1
    python keyword 1

Keyword 2
    python_keyword_2

Keyword_3
    Python_keyword_2

Keyword ${with variable} name
    Python_keyword_2

Keyword two ${with variable} name
    Python_keyword_2

it's ok to be correct
    Python_keyword_2
