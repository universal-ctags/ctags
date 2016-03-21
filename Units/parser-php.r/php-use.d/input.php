<?php

namespace NS {
  const FOO = 1;
  
  interface Iface {
    public function hello();
  }
  
  class Cls implements Iface {
    const CONST = 1;
    
    public function hello() {
      echo "hi\n";
    }
  }
    
  function test() {
    echo "test\n";
  }
}

namespace NS2 {
  use NS\Iface as NSIface, NS\Cls as NSCls;
  use const NS\FOO;
  
  interface Iface extends NSIface {
    
  }
  
  class Cls implements NSIface {
    private $foo = FOO;
    
    public function hello() {
      echo "hello ",$this->foo,"\n";
    }
  }
}

namespace {
  use NS as SomeAliasedNS;
  use NS\Cls;
  use NS2\Cls as Cls2;
  use const NS\FOO as MYCONST;
  use function NS\test;
  
  $cls = new Cls;
  echo $cls::CONST, "\n";
  echo MYCONST, "\n";
  
  $cls->hello();
  
  test();
  
  $cls = new Cls2;
  $cls->hello();
}
