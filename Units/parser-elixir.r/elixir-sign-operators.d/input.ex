defmodule OperatorModule do
  def a + b, do: max(a, b)
  def a - b, do: max(a, b)
  def a * b, do: max(a, b)
  def a / b, do: max(a, b)
  def a = b, do: max(a, b)
  def a . b, do: max(a, b)

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
