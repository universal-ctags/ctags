defmodule OperatorModule do
  def a and b, do: max(a, b)
  def a or b, do: max(a, b)
  defp a not b, do: max(a, b)
  defmacro a in b, do: max(a, b)
  defmacrop a not in b, do: max(a, b)
  defmacrop a when b, do: max(a, b)
end
