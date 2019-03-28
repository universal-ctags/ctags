defmodule FunctionModule do
  def one_liner_func, do: :baz

  def func_no_params do
    #
  end

  # Function head
  def func_head(string1, string2 \\ nil, separator \\ " ")

  # Function with 1 arity
  def func_one_arity(string1, nil, _separator) do
    private_function(string1)
  end

  # Normal function
  def normal_func(string1, string2, separator) do
    string1 <> separator <> string2
  end

  # Private function
  defp private_func(a), do: a <> " alone"

  defp private_func_no_params do
    #
  end

  def function_with_comma,
    do: :some_constant
end
