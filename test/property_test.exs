defmodule YEnc.PropertyTest do
  use YEnc.TestCase, async: true
  use ExUnitProperties

  import :binary, only: [
    bin_to_list: 1,
  ]

  import YEnc

  # These characters will be encoded to critical characters.
  @criticals [
    256 - 42 + 0, # = 214
    256 - 42 + ?\n, # = 224
    256 - 42 + ?\r, # = 227
    ?= - 42, # = 19
  ]

  property "encode byte to 8-bit ascii" do
    check all byte <- filter(byte(), &(&1 not in @criticals))  do
      <<encoded>> = encode(<<byte::integer>>)
      assert encoded === rem(byte + 42, 256)
    end
  end

  property "escape critical characters" do
    check all char <- member_of(@criticals) do
      # These characters will be encoded to critical characters, which
      # are prepended with the escape character `=`, then increased by
      # 64, modulo 256
      assert <<61, encoded::integer>> = encode(<<char::integer>>)
      critical = rem(char + 42, 256)
      assert encoded === rem(critical + 64, 256)
    end
  end

  property "encode binary to 8-bit ascii" do
    check all binary <- binary() do
      bytes = bin_to_list(binary)
      encoded = encode(binary)

      assert encoded === Enum.reduce(bytes, <<>>, fn(byte, acc) ->
        encoded_byte = encode(<<byte::integer>>)
        <<acc::binary, encoded_byte::binary>>
      end)
    end
  end

  property "symmetric" do
    check all binary <- binary() do
      assert decode(encode(binary)) === binary
    end
  end

end
