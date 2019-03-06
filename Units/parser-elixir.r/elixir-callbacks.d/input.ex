defmodule CallbackModule do
  use Behaviour
  # This is the new callback syntax
  @callback new_callback() :: integer

  # This is the old (deprecated) callback syntax
  defcallback old_callback(info :: integer) :: integer
end
