// Derrived from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment
function userDisplayName({displayName: dname}) {
  return dname;
}

function whois({displayName, fullName: {firstName: name}}) {
  return `${displayName} is ${name}`;
}

function drawChart({size = 'big', coords = {x: 0, y: 0}, radius = 25} = {}) {
  console.log(size, coords, radius);
  // do some chart drawing
}
