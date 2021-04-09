defmodule YEnc.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import YEnc.TestCase
    end
  end

  setup tags do

    unless tags[:async] do
    end

    :ok
  end
end
