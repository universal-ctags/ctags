class C
end

C.define_method :act do
  puts "hello\n"
end
c = C.new

c.act
