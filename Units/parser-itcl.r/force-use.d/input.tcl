# Assume this source file is loaded from
# another source file:
# 
#   package require Itcl
#   namespace import itcl::*
#   source input.tcl
#
class MyClass {
    public method foo {} {
    }
}

body MyClass::foo {} {
}
