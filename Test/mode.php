Tests for entering and leaving PHP mode

Expected output is

functions:
	a
	b
	c
	d


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
