# a

## b

### c

#### d

##### e

###### f

# g #
# h ##

## i #
## j ##
## k ###

### l #
### m ##
### n ###
### o ###

#### p #
#### q #####

##### r #
##### s ######

```sh
# A COMMENT LINE in SHELL SYNTAX
function x
{
	echo 'Hello, World!'
}
```
###### t #
```sh
# THE OTHER COMMENT LINE in SHELL SYNTAX
function y
{
	cat <<<EOF
	~~~
# ANOTHER COMMENT LINE in SHELL SYNTAX
	~~~
~~~
# ANOTHER COMMENT LINE in SHELL SYNTAX
~~~
EOF
}
```

``` sh
z()
{
}
```

###### u #######

A
=

B
==

~~~perl
# A COMMENT LINE in PERL SYNTAX
sub f {
    my ($line, $opts) = @_;
	return $line;
}
~~~
C
===

~~~perl
# THE OTHER COMMENT LINE in PERL SYNTAX
sub g
{
    print <<EOF;
	```
# ANOTHER COMMENT LINE in PERL SYNTAX
	```
```
# ANOTHER COMMENT LINE in PERL SYNTAX
```
EOF
}
~~~

~~~	perl
sub h
{
}
~~~	
D
-

E
--

F
---

 G
-

  H
-

   I
-

    indented
-

	indented_with_tab
-

 	indented_with_space_and_tab
-

text
- ignored
-

# C\#

J
 -

K
  -

L
   -

ignored
    -

```foo```

> quoted
---
