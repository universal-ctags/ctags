
var a = `he$llo${`lala`/*+{}*///;var bug1=2;
}
var bug2 = 3;
function bug3() {}
guys\`${2}`;

var b = 2;

function f() {
    return `hello
people\` of \` the $world$
;
function bug4() {
    return 1;
}`;
}

function f2() {}

var c = `
\${42`;

var d = `$\{42`;

var e = `${{_:{}} || 1}`;

// just to be sure the last element is not eaten
var z = 42;
