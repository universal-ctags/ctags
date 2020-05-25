# Skip public, protected, and private methods invocation
module Mod
  public def a()  end
  private def b()  end
  protected def c()  end
  public_class_method def self.d()  end
  private_class_method def self.e()  end
end
