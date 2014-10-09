/*
 * ctags should return the following for parsing this file using:
 * ctags -f - simple.js
 *
 * functions
 *   extra.validFunctionFour
 *   getHalfOf
 *   getHalfOf.calculate
 *   testlib.core.validFunctionSix
 *   testlib.validFunctionFive
 *   validFunctionOne
 *   validFunctionThree
 *   validFunctionThree.innerThree
 *   validFunctionTwo
 * classes
 *   Database
 *   ValidClassTwo
 *   testlib.extras.ValidClassOne
 * methods
 *   Database.executeQueryString
 *   Database.getTodaysDate
 *   ValidClassTwo.validMethodFour
 *   ValidClassTwo.validMethodThree
 *   testlib.extras.ValidClassOne.validMethodOne
 *   testlib.extras.ValidClassOne.validMethodTwo
 * variables
 *   my_global_var1
 *   my_global_var2
 *   my_global_var3
 *   my_global_var4
 */

validFunctionOne = function(a,b) {}

function validFunctionTwo(a,b) {}

function validFunctionThree(a,b) {
    var innerThree = function(a,b) {}
}

var my_global_var1 = 33;

function extra.validFunctionFour(a,b) {}

//pseudo-module setup
testlib = {}
testlib.core = {}
testlib.extras = {}

var my_global_var2 = "Something";

testlib.validFunctionFive = function(a,b) {}

    var invalidInnerFunction = function(a,b) {}

testlib.core.validFunctionSix = function(a,b) {}

testlib.extras.ValidClassOne = function(a,b) { 
    this.a = a; 
}

testlib.extras.ValidClassOne.prototype = {
    'validMethodOne' : function(a,b) {},
    'validMethodTwo' : function(a,b) {}
}

ValidClassTwo = function () 
{
    this.validMethodThree = function() {}

    // unnamed method
    this.validMethodFour = () {}
}

var my_global_var4 = document.getElementsByTagName("input");
for (var i = 0; i < my_global_var4.length; i++) {
    var originalvalue = my_global_var4[i].value;
    my_global_var4[i].onchange = function () {
        alert(this.value + " == " + originalvalue);
    }
}

function getHalfOf(num1, num2, num3)     
{ 
  function calculate(number)
  {
    return number/2;
  }

  var result="";
  result+=calculate(num1)+" ";
  result+=calculate(num2)+" ";
  result+=calculate(num3);
}         

var my_global_var3;

Database.prototype.getTodaysDate = Database_getTodaysDate;
Database.prototype.executeQueryString = Db_executeQueryString;


