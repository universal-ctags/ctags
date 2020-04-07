class X
  def cat (n = ")",              # comment
           m = "misssing")
    return n + m
  end
end

p X.new.cat("a","b")
