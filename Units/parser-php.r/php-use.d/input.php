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

namespace NS2\NS30 {
  class Cls3 {}
}

namespace NS2\NS31 {
  class Cls4 {}
}

namespace NS2\NS30\NS301 {}
namespace NS2\NS30\NS302 {}

namespace {
  use NS as SomeAliasedNS;
  use NS\Cls;
  use NS2\Cls as Cls2;
  use const NS\FOO as MYCONST;
  use function NS\test;
  use NS2\{Cls as NS2Cls, Iface};
  use NS2\{NS30, NS31\Cls4};
  use NS2\{NS30 as NameSpaceTreePointO, NS31\Cls4 as ClassFour};
  use NS2\{NS30 as NameSpaceTreePointZero,};
  
  $cls = new Cls;
  echo $cls::CONST, "\n";
  echo MYCONST, "\n";
  
  $cls->hello();
  
  test();
  
  $cls = new Cls2;
  $cls->hello();
  
  new ClassFour;
  new NameSpaceTreePointO\Cls3;
}
