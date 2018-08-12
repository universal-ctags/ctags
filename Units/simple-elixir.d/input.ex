#
# Taken from https://elixir-lang.org/
#

def drive(%User{age: age}) when age >= 16 do
  # Code that drives a car
end

drive(User.get("John Doe"))
#=> Fails if the user is under 16

defmodule MathTest do
  use ExUnit.Case, async: true

  test "can add two numbers" do
    assert 1 + 1 == 2
  end
end
