#= Julia syntax highlighting test.

Modified from https://github.com/JuliaEditorSupport/julia-syntax-test-cases

This file derives from https://gist.github.com/Wilfred/f1aca44c61ed6e1df603
whose author is [@Wilfred](https://github.com/Wilfred). @Wilfred has put it in
the public domain:
  https://gist.github.com/Wilfred/f1aca44c61ed6e1df603#gistcomment-2948526

Changes from the original are governed by the license of the repository in
which this file is found.

This file is designed to test various corner cases of Julia
syntax highlighting.
=#

baremodule Mod1
    # Nothing here
end

module Mod2
    # Here neither
end

## Simple function definitions.
# Expected: `function` should be highlighted, as should `foo_bar!`.
function foo_bar!(x,y)
    x + y + 1
end

# Expected: `foo_bar!` should be highlighted.
foo_bar!(x,y) = x + y

# Expected: `foo` should be highlighted.
Baz.foo(x) = 1

# Expected: `foo` should be highlighted.
foo(x::(Int,)) = 1

# Expected: `foo` should be highlighted.
foo(x, y=length(x))

## Function definitions in namespaces.
# Expected: `bar` should be highlighted.
function Foo.bar(x, y)
    x + 1
end

## Function definitions with type variables.
# Expected: `elsize` should be highlighted.
elsize(::AbstractArray{T}) where {T} = sizeof(T)

function elsize(::AbstractArray{T}) where T
    sizeof(T)
end

## Nested brackets in function definitions.
# Expected: `cell` should be highlighted.
cell(dims::(Integer...)) = Array(Any, convert((Int...), dims))

# TODO: find an example with a nested type expression.

## Macro usage
# Expected: `@hello_world!` should be highlighted.
@hello_world! foo

# Expected: highlight `myfun`
@inline myfun() = println("myfun")

## Builtin functions.
# Expected: `throw`, `error` and `super` should not be highlighted. There are
# too many built-in functions for this to be useful.
# https://github.com/JuliaLang/julia/commit/134867c69096fcb52afa5d5a7433892b5127e981
# https://github.com/JuliaLang/julia/pull/7963#issuecomment-52586261
throw(foo)
error("foo", bar, "baz")
super(Int)

## Strings
# Expected: highlight the string.
x = "foo \"bar\" baz"

# Expected: highlight the whole string.
x = "foo
bar"

# Expected: highlight the whole triple-quoted string.
x = """hello "world" foobar"""
y = """foo\\"""
z = """bar\""""
w = """"bar"""

# Expected: highlight `$user`
x = "hello $user"

# Expected: don't highlight `$user`
x = "hello \$user"

# Expected: highlight `$val`
x = """(a="$val")"""

# Expected: treat r as part of the string, so `r"a"` is highlighted.
x = r"0.1"

# Expected: treat ismx as part of the string, so `r"a"ismx` is highlighted.
x = r"a"ismx

# Expected: highlight `r"""a "b" c"""`
x = r"""a "b" c"""

# Expected: treat v as part of the string, so `v"0.1"` is highlighted.
x = v"0.1"

# Expected: treat b as part of the string, so `b"a"` is highlighted.
x = b"a"

# Bonus points:
# Expected: highlight the interpolation brackets `$(` and `)`
x = "hello $(user * user)"

# Bonus points:
# Expected: highlight regexp metacharacters `[` and `]`
x = r"[abc]"

# Bonus points:
# Expected: highlight escape sequences `\xff` and `\u2200`
x = b"DATA\xff\u2200"

# Bonus points:
# Expected: don't highlight `$user`
x = raw"hello $user"

## Characters
# Expected: highlight the character.
x = 'a'
y = '\u0'
z = '\U10ffff'
w = '\x41'
a = ' '
b = '"'
c = '''
d = '\''
e = '\\'

# Expected: don't highlight, as ' is an operator here, not a character delimiter.
a = b' + c'
A'''

# Bonus points:
# Expected: don't highlight the character
x = 'way too long so not a character'
x = ''

## Comments
# Expected: highlight `# foo`
# foo

# Expected: highlight `#= foo\n bar =#`
#= foo
bar =#

# Expected: highlight `#= #= =# =#` (comments can nest).
#= #= =# =#

# Expected: highlight `'` as adjoint operator
A#==#'
(A)#==#'
A[1]#==#'

## Type declarations

# Expected highlight `Foo` and `Bar`
mutable struct Foo
    x::Bar
end

# Expected highlight `Foo` and `Bar`
struct Foo
    x::Bar
end

# Expected: highlight `Foo` and `Bar`
abstract type Foo <: Bar end

# Expected: don't highlight x or y
x <: y

## Type annotations

# Expected: highlight `FooBar`
f(x::FooBar) = x

# Expected: highlight `Int8`
function foo()
    local x::Int8 = 5
    x
end

# Expected: highlight `Point` and `Real` as types
function norm(p::Point{<:Real})
    sqrt(p.x^2 + p.y^2)
end

# Expected: highlight `g` as function and `Int8` as type
function g(x, y)::Int8
   return x * y
end
       
# Expected: highlight `T` and `Number`
same_type_numeric(x::T, y::T) where {T <: Number} = true
same_type_numeric(x::T, y::T) where T = false

## Parametric type declaration

# Expected: highlight `Pointy` and `T`
abstract type Pointy{T} end

# Expected: highlight `Point`, `Pointy` and `T`
struct Point{T} <: Pointy{T}
    x::T
    y::T
end

## Variable declarations

# Expected: highlight `x` and `y`
global x = "foo, bar = 2", y = 3

# Expected: highlight `x` and `y`
global x = foo(a, b), y = 3

# Expected: highlight `y`
const y = "hello world"

# Expected: highlight `x` and `y`
function foo()
    local x = f(1, 2), y = f(3, 4)
    x + y
end

# Expected: highlight `x` and `y`
let x = f(1, 2), y = f(3, 4)
    x + y
end

## Colons and end

# Expected: highlight `:foo`, `:end` and `:function`
:foo
x = :foo
y = :function
z = :end

# Expected: highlight index `[end]` differently to block delimiter `end`
if foo[end]
end

# Expected: highlight as index `end`
foo[bar:end]

# Expected: highlight as index `begin`
foo[begin:4]

# Expected: don't highlight `:123`
x = :123

# Expected: don't highlight `:baz`
foo[bar:baz]

# Expected: highlight `:baz`
foo[:baz]

# Expected: highlight both `:baz`
foo(:baz,:baz)

# Note that `: foo` is currently a valid quoted symbol, this will hopefully
# change in 0.4: https://github.com/JuliaLang/julia/issues/5997

# Expected: highlight `:foo`
[1 :foo]

# Expected: highlight `:end`
[1 :end]

# Expected: highlight `:two`
@eval :one+:two

# Expected: don't highlight `:end` but `end` as index
[(1+1):end]

# Expected: don't highlight `:end` but `end` as index
[a[1]:end]

# Expected: don't highlight `:foo`
for x=1:foo
    print(x)
end

## Range detection

# Bonus points:
# Expected: don't highlight `:s2`
push!(a, s1 :s2)

# Bonus points:
# Expected: don't highlight `:end`
a[begin :end]

## Expression evaluation

# Expected: highlight `:` as operator
a = :(x = 2)

# Expected: highlight `:call` and `:b` as symbols
# Debatable: highlight `:+` as operator
ex = Expr(:call, :+, a, :b)

## Number highlighting

# Expected: highlight all these as numbers
x = 123
x = 1.1
x = .5
x = 0x123abcdef
x = 0o7
x = 0b1011
x = 2.5e-4
x = 2.5E-4
x = 1e+00
x = 2.5f-4
x = 0x.4p-1
x = 1_000

# Expected: highlight these as numbers or built-ins
x = Inf
x = NaN

# Expected: highlight `123`, not the letter
y = 123x
y = 123e

# Expected: highlight `1e+1` and `1e-1`
1e+1+1e-1

# Expected: highlight `1.` and `.1`
1. +.1
# Note that `1.+1` is currently ambiguous and gives an error

# Bonus points:
# Expected: don't highlight `..`
x = 1..3

# Bonus points:
# Debatable: highlight the first two digits, not the following digits
# or show an error
x = 0o1291
x = 0b1091

# Debatable: highlight `π` as a number or built-in
# (note that `πx` is a single symbol, not `π * x`)
x = π

## List comprehension
# Expected: highlight `for` and `if` without the `end` keyword
[i for i in 1:5 if i%2==0]

## Broadcasting
# Expected: highlight `.+` as operator
a.+1

## Command
# Expected: highlight "`echo 1`"
c = `echo 1`

# Expected: highlight "```echo `hello 1` ```"
c = ```echo `hello 1` ```

# Expected: highlight "raw`echo $1`"
c = raw`echo $1`

## Non-standard identifiers
# Bonus points:
# Expected: highlight `var"##"` as a function
function var"##"(x)
    println(x)
end

# Bonus points:
# Expected: highlight `var"%%"` as a function
var"%%"(x) = println(x)

# Bonus points:
# Expected: highlight `$var` as string and `##""` as comment
"$var"##""

# Bonus points:
# Expected: highlight `$(var")(")` as string interpolation
"$(var")(")"

# Bonus points:
# Expected: highlight `'` as adjoint operator
var"##mat"'

## Code folding: for and if in list comprehension
# Expected: fold between function and last end
function test(x)
    a = (if x; 0 else 1 end)
    println(a)
end
