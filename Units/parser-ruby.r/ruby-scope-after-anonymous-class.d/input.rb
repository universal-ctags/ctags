class C
  class << self
    def foo() end
  end
  
  def bar(); end
end

puts C.foo
puts C.new.bar

class D
  Class.new(C) do
    def f0()
    end
  end
  def f1()
  end
end
