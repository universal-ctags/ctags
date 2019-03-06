defmodule OperatorModule do
  def a and b, do: max(a, b)
  def a or b, do: max(a, b)
  def a not b, do: max(a, b)
  def a in b, do: max(a, b)
  def a when b, do: max(a, b)
end
