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

require 'i'
require('j')
require_relative 'k'
require_relative('l')
load 'm.rb'
load('n.rb')
autoload :I
autoload :J, 'o'
autoload(:K)
autoload(:L, 'p')
