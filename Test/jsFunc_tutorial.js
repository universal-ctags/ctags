
/*
 * These samples were retrieved from this website:
 * http://www.permadi.com/tutorial/jsFunc/
 *
 * This the output you should see from running:
 * ctags -f - Test/jsFunc_tutorial.js
 *    functions
 *      Ball1
 *      Ball3
 *      D1
 *      D2
 *      D2A
 *      D3
 *      D4
 *      D5
 *      DT1
 *      DT2
 *      DT2A
 *      DT3
 *      PT1
 *      calculate8
 *      getHalfOf7
 *      getHalfOf7.calculate
 *      getHalfOf8
 *      getSalaryFunctionDT9
 *      myFunction4
 *      myFunction5
 *      myFunction6
 *      myFunction6A
 *      myFunction6AE
 *      myFunction6B
 *      myFunction6E
 *      myObject.add
 *      savedFunc6B
 *      sayName4A
 *      theAdd
 *    classes
 *      DT4
 *      DT5
 *      DT6
 *      DT7
 *      DT7A
 *      DT8
 *      DT9
 *      PT2
 *      PT3
 *      addSalaryFunction
 *      addSalaryFunctionDT9
 *    methods
 *      DT7.getSalary
 *      DT7A.getSalary
 *      DT8.getSalary
 *      PT2.livesIn
 *      PT2.price
 *      PT3.addSalary
 *      PT3.getSalary
 *    variables
 *      my_global_var1
 */

// Example D1

function D1(a, b) 
{                     
  return a+b;
}      
alert(D1(1,2));        // produces 3

var my_global_var1 = 'global';

// Example D2

var D2=function(a, b) 
{                     
  return a+b;
}                     
alert(D2(1,2));        // produces 3


// Example D2A

var D2A=function theAdd(a, b) 
{                     
  return a+b;
}                     
alert(D2A(1,2));           // produces 3
alert(theAdd(1,2));        // also produces 3

var myObject=new Object();
myObject.add=function(a,b){return a+b};  
// myObject now has a property/a method named "add"
// and I can use it like below
myObject.add(1, 2);


// Example D3

var D3=new Function("a", "b", "return a+b;");
alert(D3(3,4));        // produces 7


// Example D4

var D4=new Function("a", "b", 
  "alert" +                      // chop string using "+"
  "('adding '+a+' and ' +b);\    // separate string using "\"
   return a+b;");
alert(D4(3,4));        // produces 7


// Example D5

function D5(myOperator)
{
  return new Function("a", "b", "return a" + myOperator + "b;");
}

var add=D5("+");                // creates "add" function
var subtract=D5("-");           // creates "subtract" function
var multiply=D5("*");           // created "multiply" function

// test the functions
alert("result of add="+add(10,2));            // result is 12
alert("result of substract="+subtract(10,2)); // result is 8
alert("result of multiply="+multiply(10,2));  // result is 20
alert(add);


// Example 1

function Ball1()       // it may seem odd, but this declaration
{                     // creates a object named Ball
  i=1;
}                     
alert(typeof Ball1);     // produces "function"


// Example 3

function Ball3()       // it may seem odd, but declaration
{                     // creates an object named Ball, and you can 
}                     // refer to it or add properties to it like below
Ball3.callsign="The Ball"; // add property to Ball
alert(Ball3.callsign); // produces "The Ball"


// Example 4

function myFunction4(message) 
{ 
  alert(message);
}
var ptr=myFunction4;  // ptr points to myFunction
ptr("hello");	     // executes myFunction which will prints "hello"


// Example 4A

function sayName4A(name) 
{ 
  alert(name);
}

var object1=new Object();      // creates 3 objects
var object2=new Object();
var object3=new Object();
object1.sayMyName4A=sayName;       // assign the function to all objects
object2.sayMyName4A=sayName;
object3.sayMyName4A=sayName;
  
object1.sayMyName4A("object1");    // prints "object1"
object2.sayMyName4A("object2");    // prints "object2"
object3.sayMyName4A("object3");    // prints "object3"


// Example 5

function myFunction5() 
{ 
  alert(myFunction.message);
}
myFunction5.message="old";
var ptr1=myFunction5;                 // ptr1 points to myFunction
var ptr2=myFunction5;                 // ptr2 also points to myFunction

ptr1();				     // prints "old"
ptr2();                              // prints "old"

myFunction5.message="new";

ptr1();				     // prints "new"
ptr2();                              // prints "new"


//Example 6:

function myFunction6() 
{ 
  alert("Old");
}
myFunction6(); // prints "Old"

myFunction6E=function()
{
  alert("New");
};
myFunction6E(); // prints "New"


//Example 6A:

function myFunction6A() 
{ 
  alert("Old");
}
var savedFunction=myFunction6A;

myFunction6AE=function()
{
  alert("New");
};
myFunction6AE();    // prints "New"
savedFunction();  // printf "Old"

//Example 6B:

function myFunction6B() 
{ 
  alert("Old");
}
var savedFunc=myFunction6B;
savedFunc6B=function()
{
  alert("New");
};
myFunction6B();            // prints "Old"
savedFunc6B();             // prints "New"

// Example 7

function getHalfOf7(num1, num2, num3)     
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
var resultString=getHalfOf7(10,20,30);
alert(resultString);         // prints "5 10 15"

// Example 8

function calculate8(number)
{
  return number/3;
}

function getHalfOf8(num1, num2, num3)     
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
var resultString=getHalfOf8(10,20,30);
alert(resultString);         // prints "5 10 15"

// Example DT1
function DT1()
{
}
var ball0=new DT1(); // ball0 now points to a new object

alert(ball0);         // prints "Object" because ball0 is now an Object


// Example DT2

function DT2(message)
{
  alert(message);
}
var ball1=new DT2("creating new Ball");  // creates object & 
                                          // prints the message
ball1.name="ball-1";                      // ball0 now has a "name" property
alert(ball1.name);                        // prints "ball-0"


// Example DT2A

function DT2A(message)
{
  alert(message);
}
var ball2=new Object();
ball2.construct=DT2A;
ball2.construct("creating new ball2");  // executes ball0.Ball("creating..");
ball2.name="ball-2";                      
alert(ball2.name);                        


// Example DT3 (creates 3 ball objects)

function DT3()
{
}
var ball3=new DT3(); // ball0 now points to a new instance of type Ball
ball3.name="ball-3";  // ball0 now has a "name" property

var ball4=new DT3();
ball4.name="ball-4";

var ball5=new DT3();

alert(ball0.name);    // prints "ball-0"
alert(ball1.name);    // prints "ball-1"
alert(ball2.name);    // oops, I forgot to add "name" to ball2!


// Example DT4

function DT4(message, specifiedName)
{
  alert(message);
  this.name=specifiedName;                
}
var ball6=new DT4("creating new Ball", "Soccer Ball");  
alert(ball6.name);                   // prints "Soccer Ball"


// Example DT5

function DT5(color, specifiedName, owner, weight)
{
  this.name=specifiedName;                
  this.color=color;
  this.owner=owner;
  this.weight=weigth;
}
var ball7=new DT5("black/white", "Soccer Ball", "John", 20);  
var ball8=new DT5("gray", "Bowling Ball", "John", 30);  
var ball9=new DT5("yellow", "Golf Ball", "John", 55);  
var balloon=new DT5("red", "Balloon", "Pete", 10);  

alert(ball7.name);                        // prints "Soccer Ball"
alert(balloon.name);                      // prints "Balloon"
alert(ball9.weight);                      // prints "55"


// Example DT6

function DT6(name, salary, mySupervisor)
{
  this.name=name;                
  this.salary=salary;
  this.supervisor=mySupervisor;
}
var boss=new DT6("John", 200);

var manager=new DT6("Joan", 50, boss);  
var teamLeader=new DT6("Rose", 50, boss);  

alert(manager.supervisor.name+" is the supervisor of "+manager.name);
alert(manager.name+"\'s supervisor is "+manager.supervisor.name);  


// Example DT7

function DT7(name, salary)
{
  this.name=name;                
  this.salary=salary;

  this.addSalary=addSalaryFunction;

  this.getSalary=function()
                 {
                   return this.salary;
                 };
}

function addSalaryFunction(addition)
{
  this.salary=this.salary+addition;
}

var boss=new DT7("John", 200000);
boss.addSalary(10000);                    // boss gets 10K raise
alert(boss.getSalary());                  // print 210K



function DT7A(name, salary)
{
  this.name=name;                
  this.salary=salary;

  this.addSalary=addSalaryFunction;

  this.getSalary=function()
                 {
                   return this.salary;
                 };
}

function addSalaryFunction(addition)
{
  this.salary=this.salary+addition;
}

var boss=new DT7A("John", 200000);
var boss2=new DT7A("Joan", 200000);
var boss3=new DT7A("Kim", 200000);


// Example DT8

function DT8(name, salary)
{
  this.name=name;                
  this.salary=salary;

  this.addSalary=addSalaryFunction;
  this.getSalary=function()
                 {
                   return this.salary;
                 };
}

function addSalaryFunction(addition)
{
  this.salary=this.salary+addition;
}

var boss1=new DT8("John", 200000);
var boss2=new DT8("Joan", 200000);


// add properties to getSalary function object.
boss1.getSalary.owner="boss1";
boss2.getSalary.owner="boss2";
alert(boss1.getSalary.owner);   // prints "boss1"
alert(boss2.getSalary.owner);   // prints "boss2"
// if both objects are pointing to the same function object, then 
// both output above should have printed "boss2". 

// add properties to addSalary function object.
boss1.addSalary.owner="boss1";
boss1.addSalary.owner="boss2";
alert(boss1.addSalary.owner);   // prints "boss2"
alert(boss2.addSalary.owner);   // prints "boss2"
// since both objects are not pointing to the same function, 
// then changes in one, affects all instances (so, both prints "boss2"). 


// Example DT9

function DT9(name, salary)
{
  this.name=name;                
  this.salary=salary;

  this.addSalary=addSalaryFunctionDT9;
  this.getSalary=getSalaryFunctionDT9;
}

function getSalaryFunctionDT9()
{
  return this.salary;
}

function addSalaryFunctionDT9(addition)
{
  this.salary=this.salary+addition;
}


// Example PT1

function PT1()
{
}
alert(PT1.prototype);  // prints "Object"


// Example PT2

function PT2(name, color)
{
  this.name=name;
  this.color=color;
}
PT2.prototype.livesIn="water";
PT2.prototype.price=20;


// Example PT3

function PT3(name, salary)
{
  this.name=name;                
  this.salary=salary;
}

PT3.prototype.getSalary=function getSalaryFunction()
{
  return this.salary;
}

PT3.prototype.addSalary=function addSalaryFunction(addition)
{
  this.salary=this.salary+addition;
}
