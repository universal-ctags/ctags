// Derrived from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment
const [x] = [1];
const [y, z] = [1, 2, 3, 4, 5];
let [a=5, b=7] = [1];

let [c, , d] = [1, 2, 3];
let [e, f = 0, , g] = [1, 2, 3, 4];

let [,,] = [1, 2, 3];
let [,]  = [1, 2, 3];
let []   = [1, 2, 3];

const [h, i, ...[j, k]] = [1, 2, 3, 4];
const [l, m, ...[n, o, ...[p, q]]] = [1, 2, 3, 4, 5, 6];

const [A, B, ...{ C, D }] = []

const user = {
    E: 42,
    F: true
};

const {E, F} = user;

const {E: G, F: H} = user;


const {I = 10, J = 5} = {I: 3};

let {a: K = 10, b: L = 5} = {a: 3};

let {M, N, ...O} = {M: 10, N: 20, c: 30, d: 40}


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
