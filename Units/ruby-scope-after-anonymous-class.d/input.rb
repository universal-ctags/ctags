class C
  class << self
    def foo() end
  end
  
  def bar(); end
end

puts C.foo
puts C.new.bar
