
function A() {
    this.a = 1;
}

A.prototype = {
    m1 : function() {
        this.a = 2;

        foo.bar.baz.hello(1);
        foo.bar.baz.hello(2);
        foo.bar.baz.hello(3);
    },
    
    m2: function() {
        return this.a;
    },
};
