defmodule TypeModule do
  @opaque t :: pid

  @type credentials :: {atom, password}

  @typep password :: binary
end
