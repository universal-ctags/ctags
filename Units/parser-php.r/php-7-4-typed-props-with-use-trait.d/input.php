<?php

use App\SomeNamespacedClass;

class ClassWithIntProperty
{
    use SomeTrait;

    public int $id;
}

class ClassWithNullableIntProperty
{
    use SomeTrait;

    public ?int $id;
}

class ClassWithClassTypeProperty
{
    use SomeTrait;

    public \stdClass $id;
}

class ClassWithNamespacedClassProperty
{
    use SomeTrait;

    public SomeNamespacedClass $id;
}

trait SomeTrait
{
}
