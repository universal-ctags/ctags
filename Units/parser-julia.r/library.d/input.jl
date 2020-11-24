using Module1
using Module2: func1, func2
import Module3
import Module4.func1, Module4.func2
import Module5: func1, func2

module MyModule
using Module6,
      Module7,
      Module8

using Module9: func1,
               func2,
               func3
end
