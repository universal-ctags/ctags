<?php

trait SomeTrait
{
    public function someMethod() {}
}

trait SomeOtherTrait
{
    public function someOtherMethod() {}
}

class Test1
{
    use SomeTrait;
    public int $id;
}

class Test2
{
    use SomeTrait, SomeOtherTrait;
    public int $id;
}

class Test3
{
    use SomeTrait { someMethod as private; }
    public int $id;
}

class Test4
{
    use SomeTrait, SomeOtherTrait { someOtherMethod as protected; }
    public int $id;
}

class Test5
{
    use \SomeTrait;
    public int $id;
}
