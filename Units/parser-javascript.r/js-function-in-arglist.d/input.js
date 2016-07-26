// Taken from #1014 submitted by @caneta
AUI().use(
  function funcGen(A){
    var Test;
    Test.func1 = function func1(){ console.log('func1'); };
    Test.func2 = function func2(){ console.log('func2'); };
  }
);
