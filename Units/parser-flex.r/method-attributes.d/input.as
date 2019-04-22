/* Not sure it's really valid, but the goal is to check not choking on
 * attributes, so so long as it's valid attributes it's fine */
class C {
  public function f1():void {}
  private function f2():void {}
  protected function f3():void {}
  internal function f4():void {}
  public function f5():void {}
  public override function f6():void {}
  final function f7():void {}
  native function f8():void {}
}
