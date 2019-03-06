defmodule TestModule do
  use ExUnit.Case, async: true

  test "good with spaces" do
    assert 1 + 1 == 2
  end

  test("good with parens") do
    assert 1 + 1 == 2
  end
  test"bad without spaces" do
    assert 1 + 1 == 2
  end

  test "bad without 'do' word"
    assert 1 + 1 == 2
  end

  test 'bad with single quotes' do
    assert 1 + 1 == 2
  end
end
