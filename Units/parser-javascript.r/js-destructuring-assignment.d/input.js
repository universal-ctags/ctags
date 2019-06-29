// partially taken from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment
/* Array destructuring */
var [x,y] = [0,0];
var z = [0];
var [,a] = [0, 0];
var [b,...rest] = [0, 0];

var { d0, d1 } = { d0: 10, d1: 20 };
const { d2, d3 } = { d2: 10, d3: 20 };
let { d4, d5 } = { d4: 10, d5: 20 };

var [v0=5, v1=7] = [1, 2];

var {a: v2 = 10, b: v3 = 5} = {a: 3};


let key = 'z';

let {[key]: d6} = {z: 'bar'};
var {x: [a0, a1]} = {x: [0, 1]}


const foo = { 'fizz-buzz': true };
const { 'fizz-buzz': fizzBuzz } = foo;
const { [((x, y)=>x+y)(1,1)]: fuzzBizz } = foo;

const props = [
  { id: 1, name: 'Fizz'},
  { id: 2, name: 'Buzz'},
  { id: 3, name: 'FizzBuzz'}
];

const [,, { name }] = props;


const metadata = {
  title: 'Scratchpad',
  translations: [
    {
      locale: 'de',
      localization_tags: [],
      last_edit: '2014-04-14T08:43:37',
      url: '/de/docs/Tools/Scratchpad',
      title: 'JavaScript-Umgebung'
    }
  ],
  url: '/en-US/docs/Tools/Scratchpad'
};

let {
  title: englishTitle, // rename
  translations: [
    {
       title: localeTitle, // rename
    },
  ],
} = metadata;

