class Foo
  def f0() p :f0 end
  define_method(:f1, instance_method(:f0))
  define_method(:f2) {
    puts "f2"
  }
  define_method(:f3) do
    # Semantically, this doesn't make sense. However,
    # Syntactically, this is acceptable.
    define_method(:f30) do
      puts "f30"
    end
  end
  define_method :f4 do
    puts "f4"
  end

  define_method 'f5' do
    puts 'f5'
  end
end

Foo.new.f3


