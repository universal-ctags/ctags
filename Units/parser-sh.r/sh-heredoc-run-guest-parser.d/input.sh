#!/bin/sh

function sh0
{
	:
}

/usr/bin/env php<<EOF1
<?php
$x = 5 /* + 15 */ + 5;
echo $x;
?>
EOF1

php<<EOF2
<?php
$y = 5 /* + 15 */ + 5;
echo $y;
?>
EOF2

node <<JSCODE1
var pr1 = function (msg) {
	console.log(msg);
}
pr1 ('hello');
JSCODE1

/usr/bin/node <<JSCODE2
var pr2 = function (msg) {
	console.log(msg);
}
pr2 ('hello');
JSCODE2

cat /usr/bin/node <<JSCODE3
var n_pr0 = function (msg) {
	console.log(msg);
}
n_pr0 ('hello');
JSCODE3

/usr/bin/env /usr/bin/node <<JSCODE3
var pr3 = function (msg) {
	console.log(msg);
}
pr3 ('hello');
JSCODE3

/usr/bin/env dummy node <<JSCODE4
var n_pr1 = function (msg) {
	console.log(msg);
}
n_pr1 ('hello');
JSCODE4

cat > foo.php <<EOF3
<?php
$z = 5 /* + 15 */ + 5;
echo $z;
?>
EOF3

cat <<EOF4 > foo.php
<?php
$a = 5 /* + 15 */ + 5;
echo $a;
?>
EOF4

function sh1
{
	:
}
