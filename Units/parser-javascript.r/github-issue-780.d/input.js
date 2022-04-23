import Foo from './foo';

const Bar = Foo.create({
  jump() {
    return 123;
  }
})

export default Bar;

// https://github.com/universal-ctags/ctags/issues/780
