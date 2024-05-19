<?php

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

trait SomeTrait
{
}
