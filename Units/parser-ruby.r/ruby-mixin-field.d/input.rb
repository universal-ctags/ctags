# Taken from a comment in #2476 submitted by @AmaiKinono

module X
  def hi
    p "Calling 'hi' in X."
  end
end

module Y
  def hoi
    p "Calling 'hoi' in Y."
  end
end

module Z
  def zoo
    p "Calling 'zoo' in Z."
  end
end

class A
  include X
  def hi
    p "Calling 'hi' in A."
  end
  include Y
end

class B
  def self.prep
    include X
  end
end

class C
  prepend X
  def hi
    p "Calling 'hi' in C."
  end
  prepend Y
end

class D
  def self.prep
    prepend X
  end
end

class E
  extend X
  def hi
    p "Calling 'hi' in E."
  end
  extend Y
  extend Z
end

class F
  def self.prep
    extend X
  end
end

class G
  include(X)
  def self.prep
    prepend (Y)
  end
  extend ( Z)
end

class H
  if true
    unless false
      include(X)
    end
  end
  def self.prep
    if true
      unless false
        prepend (Y)
      end
    end
  end
  extend ( Z)
end
