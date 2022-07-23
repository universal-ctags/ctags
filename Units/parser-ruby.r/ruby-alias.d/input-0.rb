# Derived from https://github.com/tmm1/ripper-tags/blob/master/test/test_ripper_tags.rb
module M
  def get(); end
  def set(); end

  alias :"[]" :get
  alias :"[]=" :set
  alias :set :"[]="
end

def z(); end
