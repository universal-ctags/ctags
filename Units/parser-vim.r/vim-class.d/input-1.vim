vim9script
# Taken from https://vim-jp.org/vimdoc-en/vim9class.html#Vim9-simple-class

abstract class Shape
   var color = Color.Black
   var thickness = 10
endclass

class Square extends Shape
   var size: number

   def new(this.size)
   enddef
endclass

class Triangle extends Shape
   var base: number
   var height: number

   def new(this.base, this.height)
   enddef
endclass
