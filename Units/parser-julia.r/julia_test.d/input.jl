using Revise
import Distributions: Normal
using Random.randn

using Plots, Makie


const a::Int = 'c'  # struct Struct_wrong3 end

macro test_macro() end

"""
    test_fun(a::Int)
For test only
"""
function test_fun(a::Int, b::T) where #
        T <:Array{S} where S <: Number
    println(a)
    println("foo")
end


function Base.ifelse(a::Int)
    println("bar")
end
function lone_function end

function run_test(a::T) where T<:Int; a::Int end

eq(c=4; b=(1,2,3), c=:a=>5) = a == b
eq(a::T, b::T, c=4; b=(1,2,3), c=:a=>5)::T where T<:Real = (a == b; a)

#=
  Structs
=#
abstract type ATest end

mutable struct STest <: ATest; a::Int end


struct Test1 <: ATest
    a::Int
    α::Real

    Test1() = new(0)
    Test1(a) = new(1)
    Test1(a, b) = begin new(2) end
end
