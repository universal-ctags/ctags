# Most of this code is taken from the official Elixir documentation.
# With comments and modifications
# by Ivan Gonzalez Polanco <ivan14polanco@gmail.com>
#
# This code doesn't compile, since it's supposed to test de ctags elixir
# parser, not the elixir elixir parser.

#
# d delegates (defdelegate ...)
#
defmodule MyList do
  defdelegate reverse(list), to: Enum
  defdelegate other_reverse(list), to: Enum, as: :reverse
end

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
