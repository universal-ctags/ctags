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
  alias_method :my_alias_method0, :my_method
  alias_method "my_alias_method1", :my_method
  alias_method(:my_alias_method2, :my_method)
  alias_method("my_alias_method3", :my_method)
  alias_method('my_alias_method4', :my_method)
end

p $new # prints 0
