# https://sourceforge.net/p/geany/bugs/542/

def method_or_class
  for x in 0..1 do
    for y in 0..1 do
    end
  end
end

class X
end


# more tests

class C
  # do as separator
  def method1
    for x in 0..1 do
      puts x
    end
  end

  def method2
    until 0 == 1 do
      puts "hello"
      break
    end
  end

  def method3
    while 1 == 1 do
      puts "hello"
      break
    end
  end

  # semicolon as separator
  def method4
    for x in 1..2; [1,2,3].each do |y|
        puts x*y
      end
    end
  end

  def method5
    until 0 == 1; [1,2,3].each do |x|
        puts x
      end
      break
    end
  end

  def method6
    while 1 == 1; [1,2,3].each do |x|
        puts x
      end
      break
    end
  end

  # newline as separator
  def method7
    for x in 1..2
      [1,2,3].each do |y|
        puts x*y
      end
    end
  end

  def method8
    until 0 == 1
      [1,2,3].each do |x|
        puts x
      end
      break
    end
  end

  def method9
    while 1 == 1
      [1,2,3].each do |x|
        puts x
      end
      break
    end
  end
end


# check the code works
c = C.new
c.method1
c.method2
c.method3
c.method4
c.method5
c.method6
c.method7
c.method8
c.method9
