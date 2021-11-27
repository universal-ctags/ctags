defmodule ModuleError do
  defexception [:message]

  @impl true
  def exception(value) do
    msg = "did not get what was expected, got: #{inspect(value)}"
    %ModuleError{message: msg}
  end
end
