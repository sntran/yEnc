defmodule YEnc.PropertyTest do
  use YEnc.TestCase, async: true
  use ExUnitProperties

  import :binary, only: [
    bin_to_list: 1,
    match: 2,
  ]

  import YEnc

  # These characters will be encoded to critical characters.
  @criticals [
    256 - 42 + 0, # = 214
    256 - 42 + ?\n, # = 224
    256 - 42 + ?\r, # = 227
    ?= - 42, # = 19
  ]
  @tab (256 - 42 + ?\t)
  @space (256 - 42 + ?\s)

  property "encode byte to 8-bit ascii" do
    check all byte <- filter(byte(), &(&1 not in [@tab, @space | @criticals]))  do
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

  property "escape single TAB (09h) and SPACE (20h)" do
    check all char <- member_of([@tab, @space]) do
      assert <<61, encoded::integer>> = encode(<<char::integer>>)
      critical = rem(char + 42, 256)
      assert encoded === rem(critical + 64, 256)
    end
  end

  property "encode binary with leading TAB (09h) and SPACE (20h)" do
    check all char <- member_of([@tab, @space]),
              binary <- filter(binary(), &(match(&1, [<<@tab>>, <<@space>>]) === :nomatch))
    do
      binary = <<char::integer, binary::binary>>
      bytes = bin_to_list(binary)
      encoded = encode(binary)

      assert encoded === Enum.reduce(bytes, <<>>, fn(byte, acc) ->
        # Encoding TAB or SPACE when it's the leading character is
        # the same as encoding it individually.
        encoded_byte = encode(<<byte::integer>>)
        <<acc::binary, encoded_byte::binary>>
      end)
    end
  end

  property "encode binary with TAB (09h) or SPACE (20h) inside" do
    check all char <- member_of([@tab, @space]),
              # First part does not have TAB or SPACE and not empty.
              bin1 <- filter(binary(), &(&1 !== "" and match(&1, [<<@tab>>, <<@space>>]) === :nomatch)),
              # Second part can have any characters.
              bin2 <- binary()
    do
      binary = <<bin1::binary, char::integer, bin2::binary>>
      encoded = encode(binary)

      # The result should not have the special char escaped.
      assert(
        match(encoded, [encode(<<char::integer>>)]) === :nomatch,
        "The result should not have the special character escaped"
      )
    end
  end

  property "symmetric" do
    check all binary <- binary() do
      assert decode(encode(binary)) === binary
    end
  end

  property "encode post" do
    check all filename <- string(:alphanumeric),
              binary <- binary()
    do
      size = byte_size(binary)
      crc32 = crc32(binary)
      encoded = post(filename, binary)

      assert encoded === Enum.join([
        "=ybegin line=128 size=#{size} name=#{filename}\r\n",
        encode(binary),
        "\r\n=yend size=#{size} crc32=#{crc32}"
      ])
    end
  end

end
