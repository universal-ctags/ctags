
( https://rosettacode.org/wiki/FizzBuzz#Forth / https://skilldrick.github.io/easyforth/ ) 

\ Works in gforth and other ANS forth. Other Forth may differ.

10 constant DUMMY1
12 constant DUMMY2

variable dummy3
variable dummy4

11 dummy3 !
13 dummy4 !

: fizz?  
3 mod 0 = dup if ." Fizz" then ;
: buzz?  
5 mod 0 = dup if ." Buzz" then ;

: fizz-buzz?  
dup fizz? swap buzz? or invert ;

: do-fizz-buzz  
25 1 do cr i fizz-buzz? if i . then loop ;

do-fizz-buzz
cr

quit
\ bye