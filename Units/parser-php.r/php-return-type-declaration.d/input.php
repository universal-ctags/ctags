<?php

namespace A {
    class T {}
}

namespace
{
    class Test
    {
        public function first(string $str) : Closure
        {
            /* closure with return type declaration and use() clause */
            $tmp = function() use ($str) : int {
                return strlen($str);
            };
            return $tmp;
        }

        public function second() : self
        {
            return $this;
        }
    }

    class SubTest extends Test
    {
        public function third() : parent
        {
            return $this;
        }

        /* returning a namespace-qualified type */
        public function fourth() : \A\T
        {
            return new \A\T;
        }

        /* spaces are allowed around elements of the namespace */
        public function fifth() : \ A \ T
        {
            return $this->fourth();
        }

        public function &sixth(\A\T &$t) : \A\T
        {
            return $t;
        }
    }

    class SubAT extends \A \ T
    {
        public function seventh() : parent
        {
            return $this;
        }
    }

    /* runtime test */
    $s = new SubTest;
    $t = $s->third();
    var_dump(($t->second()->first("hello"))());
    var_dump($s->fourth());
    var_dump($s->fifth());
    $at = $s->fourth();
    var_dump($s->sixth($at));

    var_dump((new SubAT)->seventh());
}
