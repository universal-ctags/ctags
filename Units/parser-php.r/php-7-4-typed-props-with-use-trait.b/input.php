<?php

trait SomeTrait
{
}

class User
{
    use SomeTrait;

    public int $id;
}
