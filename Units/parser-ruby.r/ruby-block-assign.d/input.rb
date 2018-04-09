m = module Foo
end

c = class Bar
  x = def method_a
    if 1
    else
    end
  end

  def method_b
    x = if 1
    else
    end
  end

  def method_c
    x = while 1 do
      break
    end
  end

  def method_d
  end
end
