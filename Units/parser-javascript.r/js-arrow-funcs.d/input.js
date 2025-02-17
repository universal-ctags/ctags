var f0 = (x) => {
    const ignoreme = 1;
    console.log('hello');
}

var f1 = (x, y) => {
    var ignoreme = 1;
    console.log('hello');
}

const f2 = x => {
    let ignoreme = 1;
    console.log('hello');
}

let f3 = () => {
    const ignoreme = 1;
    console.log('hello');
}

let f4 = (...x) => {
    const ignoreme = 1;
    console.log('hello');
}

let f5 = (x,...y) => {
    const ignoreme = 1;
    console.log('hello');
}

let f6 = (x,y,...z) => {
    const ignoreme = 1;
    console.log('hello');
}

// Make this input acceptable as the input for nodejs.
var a = {}
var b = {}

a.f7 = x => {
    const ignoreme = 1;
    console.log('hello');
}

a.f8 = async () => {
    const ignoreme = 1;
    console.log('hello');
}

b.f9 = (async () => {
    const ignoreme = 1;
    console.log('hello');
})

b.f10 = (async x => {
    const ignoreme = 1;
    console.log('hello');
})
