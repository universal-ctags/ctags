Tests for entering and leaving PHP mode

Expected output is

functions:
	a
	b
	c
	d
	e
	f
	g
	h
	i
	j


function bug0() {
	// this should not appear
}

<?php // entering PHP mode, classic method

function a() {}

?> // PHP mode left

function bug1() {}

<? // entering PHP mode, short open tag

function b() {}

?> // PHP mode left

function bug2() {}

<?php // entering PHP mode

// this WILL leave PHP mode: ?>

function bug3() {}

<? // entering PHP mode

function c() {
	?> // PHP mode left
	function bug4() {
	}
	<? // back to PHP mode, still inside function c()
}

function d() {
	?> // same here
	function bug5() {
	}
	<?php // back to PHP mode, still inside function d()
}

// any open tag matches any close tag, so this is valid
</script> // leaves PHP mode

function bug4() {}

?> <!-- just in case -->

<script language="php"> // enetered PHP mode

function e() {
	return 42;
}

?> // left PHP mode

function bug5() {}

// some valid long tag opening with inner whitespaces

<script
	language
	=
	php
> // entered
function f() {}
</script
	> // left

function bug6() {}

<script	language=	'php'	> // enter
function g() {}
</script > // leave

function bug7() {}

<?php
// this WON'T leave PHP mode, it's in a comment  </script>
function h() {}
?>

function bug8() {}

<?php

function i() {}
// any open tag matches any close tag, so this is valid
</script         	  	 
 	  	        >

function bug9() {}

// this won't enter PHP, no spaces are allowed between the "<" and "script"
< script language = php >

function bug10() {}

// does nothing, just resets mode for some tools not aware of the "script" thing
?>

<!-- <script> is OK anywhere, even in XML strings -->
<p attr="<script language=php>
function j() {}
</script>">

</p>
