
class Foo {
  async async() {}
  static async async() {}
  static static() {}
  static staticProperty = 42;
  static {
    console.log('Class static initialization block called');
  }
}
