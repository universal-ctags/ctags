
( https://rosettacode.org/wiki/FizzBuzz#Forth / https://skilldrick.github.io/easyforth/ ) 



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

bye