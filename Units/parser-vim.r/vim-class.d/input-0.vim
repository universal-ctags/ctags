vim9script
# Taken from https://vim-jp.org/vimdoc-en/vim9class.html#Vim9-simple-class

class OtherThing
   var size: number
   static var totalSize: number

   def new(this.size)
      totalSize += this.size
   enddef

   static var _sum: number
   public static var result: number

   static def ClearTotalSize(): number
      var prev = totalSize
      totalSize = 0
      return prev
   enddef
endclass


class YetOtherThing
   static def _Foo()
      echo "Foo"
   enddef
   def Bar()
      _Foo()
   enddef
endclass

class A
   final v1 = [1, 2]               # final object variable
   public final v2 = {x: 1}        # final object variable
   static final v3 = 'abc'         # final class variable
   public static final v4 = 0z10   # final class variable
endclass
