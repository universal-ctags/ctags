<?php declare(strict_types=1);

class A
{
        public function f() {
                       $factory = new BuilderFactory;
                       $node = $factory->namespace('Name\Space')
                               ->addStmt($factory->use('Foo\Bar\SomeOtherClass'))
                               ;
               }
}
