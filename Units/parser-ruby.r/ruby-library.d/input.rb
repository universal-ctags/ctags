require "a"
require("b")
require_relative "c"
require_relative("d")
load "e.rb"
load("f.rb")
autoload :C
autoload :D, "g"
autoload(:G)
autoload(:H, "h")

