defmodule YEnc.PropertyTest do
  use YEnc.TestCase, async: true
  use ExUnitProperties

  property "symmetric" do
    check all bin <- binary() do
      assert decode(encode(bin)) == bin
    end
  end

  defp encode(data), do: YEnc.encode(data)
  defp decode(data), do: YEnc.decode(data)

end
