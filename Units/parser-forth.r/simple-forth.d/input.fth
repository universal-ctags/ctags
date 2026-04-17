
( https://rosettacode.org/wiki/FizzBuzz#Forth / https://skilldrick.github.io/easyforth/ ) 

\ Works in gforth and other ANS forth. Other Forth may differ.

10 constant DUMMY1
12 CONSTANT DUMMY2
  13 CONSTANT DUMMY_INDENTED
  
variable dummy3
variable dummy4
VARIABLE dummy5
  VARIABLE dummy_indented2
11 dummy3 !
13 dummy4 !
42 dummy_indented2 !

7 constant SEVERAL variable _DEFINITIONS : IN_ONE_LINE 7 emit ;

: fizz?  \ a simple comment 
3 mod 0 = dup if ." Fizz" then ;
: buzz?  
5 mod 0 = dup if ." Buzz" then ;
  : baz_indented ( - )
    10 0 do 
      i . cr
    loop ;
\ colon : in comment

: fizz-buzz?  
dup fizz? swap buzz? or invert ;

: do-fizz-buzz  
25 1 do cr i fizz-buzz? if i . then loop ;

do-fizz-buzz
cr
baz_indented
SEVERAL _DEFINITIONS ! IN_ONE_LINE

quit
\ bye
