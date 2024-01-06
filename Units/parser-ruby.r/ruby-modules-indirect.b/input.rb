module B
  D =  Module.new() {
    |m|
    def m.fd0()
      puts "D.fd0"
    end
  }
  E =  Module.new() do
    |m|
    def m.fd1()
      puts "E.fd1"
    end
  end

end

B::D.fd0
B::E.fd1
