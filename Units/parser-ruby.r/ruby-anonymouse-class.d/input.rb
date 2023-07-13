# Taken from https://github.com/universal-ctags/ctags/issues/408 commented by @mislav.
Class.new do
  def foo() end
end

D = Class.new
def bar()
  # This is not a part of D.
end
