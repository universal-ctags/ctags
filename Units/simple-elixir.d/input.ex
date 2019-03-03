# Most of this code is taken from the official Elixir documentation.
# With comments and modifications
# by Ivan Gonzalez Polanco <ivan14polanco@gmail.com>
#
# This code doesn't compile, since it's supposed to test de ctags elixir
# parser, not the elixir elixir parser.

#
# f functions (def ...)
#
defmodule MyString do
  def one_liner_func, do: :baz

  def func_no_params do
    #
  end

  # Function head
  def join(string1, string2 \\ nil, separator \\ " ")

  # Function with 1 arity
  def join(string1, nil, _separator) do
    private_function(string1)
  end

  # Normal function
  def join(string1, string2, separator) do
    string1 <> separator <> string2
  end

  # Private function
  defp private_func(a), do: a <> " alone"

  defp private_func_no_params do
    #
  end
end

#
# c callbacks (defcallback ...)
#
defmodule URI.MyParser do
  use Behaviour
  # This is the new callback syntax
  @callback default_port() :: integer

  # This is the old (deprecated) callback syntax
  defcallback parse(uri_info :: URI.t()) :: URI.t()
end

#
# d delegates (defdelegate ...)
#
defmodule MyList do
  defdelegate reverse(list), to: Enum
  defdelegate other_reverse(list), to: Enum, as: :reverse
end

# TODO: I don't know how to identificate/differenciate an exception since they
# don't _actually_ have name. Instead are defined like a struct.
#
# e exceptions      (defexception ...)
#
# defmodule MyAppError do
#   defexception [:message]
#
#   @impl true
#   def exception(value) do
#     msg = "did not get what was expected, got: #{inspect(value)}"
#     %MyAppError{message: msg}
#   end
# end

#
# p protocols (defprotocol ...)
#
defprotocol Size do
  @doc "Calculates the size (and not the length!) of a data structure"
  def size(data)
end

#
# g guards (defguard ...)
#
defmodule Integer.MyGuards do
  defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
  defguardp is_odd(value) when is_integer(value) and rem(value, 2) != 0
end

#
# i implementations (defimpl ...)
#
defimpl Size, for: BitString do
  def size(binary), do: byte_size(binary)
end

#
# a macros (defmacro ...)
#
defmodule MyLogic do
  defmacro macro(expr, opts) do
    #
  end

  defmacro macro_no_params do
    #
  end

  defmacrop private_macro(expr, opts) do
    #
  end

  defmacrop private_macro_no_params do
    #
  end
end

#
# o operators (e.g. "defmacro a <<< b")
#
defmodule MyOperators do
  def a + b, do: max(a, b)
  def a - b, do: max(a, b)
  def a * b, do: max(a, b)
  def a / b, do: max(a, b)
  def a = b, do: max(a, b)
  def a . b, do: max(a, b)

  # TODO: Fix the operators regex and then test these definitions
  # def a and b, do: max(a, b)
  # def a or b, do: max(a, b)
  # def a not b, do: max(a, b)
  # def a in b, do: max(a, b)
  # def a not in b, do: max(a, b)

  # The 13 operators bellow are ALL the operators that Elixir is CAPABLE of
  # parsing and are not used by default, so the user can
  def a | b, do: max(a, b)
  def _ ||| b, do: max(a, b)
  def a &&& _, do: max(a, b)
  def a <<< b, do: max(a, b)
  defp a >>> b, do: max(a, b)
  defp _ <<~ _, do: max(a, b)
  defp a ~>> b, do: max(a, b)
  defmacro a <~ b, do: max(a, b)
  defmacro _ ~> b, do: max(a, b)
  defmacro a <~> _, do: max(a, b)
  defmacrop a <|> b, do: max(a, b)
  defmacrop _ ^^^ _, do: max(a, b)
  defmacrop a ~~~ b, do: max(a, b)
end

#
# m modules (defmodule ...)
#
defmodule Nest do
  defmodule Of do
    defmodule Modules do
    end
  end
end

#
# r records (defrecord ...)
#
defmodule MyRecords do
  require Record
  Record.defrecord(:user1, name: "megan", age: "25")
  Record.defrecordp :user2, name: "ivan", age: "23"
  # This is not a typo but an intentional bad test, used to test the parser
  Record.defrecordp:bad, name: "ivan", age: "23"
end

#
# t tests (test ...)
#
defmodule CallbackModule do
  use ExUnit.Case, async: true

  test "with spaces" do
    assert 1 + 1 == 2
  end

  test("with parens") do
    assert 1 + 1 == 2
  end
  test"with bad spaces" do
    assert 1 + 1 == 2
  end
end
