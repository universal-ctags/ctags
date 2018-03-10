/* test behavior of "naked" UTF-16 surrogates */
var object = {
  // high surrogate alone
  '\uD800': function() {},
  // low surrogate alone
  '\uDF00': function() {},
  // swapped high and low surrogates
  '\uDF00\uD800': function() {},
  // naked low surrogate, plus valid pair
  '\uDF00\uD800\uDF00': function() {},
  // naked high surrogate, plus valid pair
  '\uD800\uD800\uDF00': function() {},

/* same, but with extra stuff after */
  // high surrogate alone
  '\uD800a1': function() {},
  '\uD800\u{61}2': function() {},
  // low surrogate alone
  '\uDF00a1': function() {},
  '\uDF00\u{61}2': function() {},
  // swapped high and low surrogates
  '\uDF00\uD800a1': function() {},
  '\uDF00\uD800\u{61}2': function() {},
};
