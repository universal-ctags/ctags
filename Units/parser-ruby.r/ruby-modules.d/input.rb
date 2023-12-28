module A
  def self.Fa()
    puts "A.Fa"
  end
end
A.Fa

module B
  def fb()
    puts "B.fb"
  end

  def B.Fb0()
    puts "B.Fb0"
  end

  def self.Fb1()
    puts "B.Fb1"
  end

  C = Module.new() do
    def self.fc0()
      puts 'C.fc0'
    end
    def fc1()
    end
  end

  E = Module.new()

  def E.fe()
    puts "E.fe"
  end

  def gb()
    puts "B.gb"
  end

  Module.new()
end

B.Fb0
B.Fb1
B::C.fc0
B::E.fe

class X
  include B
end

class Y
  extend A
end
