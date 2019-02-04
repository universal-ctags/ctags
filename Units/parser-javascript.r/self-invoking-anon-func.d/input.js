(function(x) {
    var a = function (o) { return o; };
    return x + a (1);
})(1);

(function(y) {
    const b = function (p) { return p; };
    return y - b (1);
}(1));

!function(z) {
    const c = function (r) { return r; };
    return z * c (1);
}(1);

console.log("a" + (function (A) {
    return A;
}((function (B) {
    return B;
}(!function(C) {
    var f = function (t) {
	return !t;
    }
    return f(C);
}(true)
 )))));
