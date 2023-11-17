//
// This test input is based on the comment submitted by @AdrienGiboire in #3435
//

const embedded = { foo: 'bar' }
const something = {
  ...embedded,
  baz: 'fox',
}

const mapGetters = function() {}
const computed = {
  ...mapGetters([
    'getAsylumAssistant',
    'getModel',
    'getType',
  ]),
}

const FNO = {
  fn: () => { /* doing something */ },
}
