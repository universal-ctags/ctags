using Module1
using Module2: func1, func2
import Module3
import Module4.func1, Module5, Module6.func2
import Module7: func1, func2

module MyModule
using Module8,
      Module9,
      Module10

using Module11: func1,
                func2,
                func3
end
