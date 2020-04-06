# Taken from
# * https://docs.ruby-lang.org/en/master/syntax/modules_and_classes_rdoc.html, and
# * https://docs.ruby-lang.org/en/master/syntax/miscellaneous_rdoc.html

$old = 0
module MyModule
  def my_method
  end
end

module MyModule
  alias my_alias my_method
  alias $new $old
end

p $new # prints 0
