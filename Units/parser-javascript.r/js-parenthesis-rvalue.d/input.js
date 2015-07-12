
// plain values
var a1 = 42;
var a2 = (42);

// functions
var b1 = function(){
  function b1sub(){}
};
var b2 = (function(){
  function b2sub(){}
});
var b3 = ((function(){
  function b3sub(){}
}));

// objects
var c1 = {};
var c2 = ({});
var d1 = {a:'hello',b:'hi'};
var d2 = ({a:'hello',b:'hi'});

// function expressions called straight away
var e1 = function(){
  function e1sub(){}
  return 42;
}();
var e2 = (function(){
  function e2sub(){}
  return 42
})();
var e3 = ((function(){
  function e3sub(){}
  return 42
})());
