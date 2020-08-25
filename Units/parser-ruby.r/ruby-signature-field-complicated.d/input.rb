class X
  def cat (n = ')',              # comment
           m = "missing")
    return n + m
  end
end

p X.new.cat("a","b")
