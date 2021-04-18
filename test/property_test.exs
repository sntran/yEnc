defmodule YEnc.PropertyTest do
  use YEnc.TestCase, async: true
  use ExUnitProperties

  import YEnc

  # These characters will be encoded to critical characters.
  @criticals [
    256 - 42 + 0, # = 214
    256 - 42 + ?\n, # = 224
    256 - 42 + ?\r, # = 227
    ?= - 42, # = 19
  ]
  @tab (256 - 42 + ?\t) # = 223
  @space (256 - 42 + ?\s) # = 246

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
              # All binary, but with TAB and SPACE removed.
              # Set max_length to avoid line breaks.
              binary <- binary_without([<<@tab>>, <<@space>>], [max_length: 100])
    do
      binary = <<char::integer, binary::binary>>
      bytes = :binary.bin_to_list(binary)
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
              # Set max_length to avoid line breaks.
              bin1 <- filter(binary_without([<<@tab>>, <<@space>>], max_length: 100), &(&1 !== "")),
              # Second part can have any characters.
              # Set max_length to avoid line breaks.
              bin2 <- binary(max_length: 100)
    do
      binary = <<bin1::binary, char::integer, bin2::binary>>
      encoded = encode(binary)

      # The result should not have the special char escaped.
      assert(
        :binary.match(encoded, [encode(<<char::integer>>)]) === :nomatch,
        "The result should not have the special character escaped"
      )
    end
  end

  property "line breaks after default 128 characters" do
    check all binary <- binary(length: 129) do
      encoded = encode(binary)
      assert(
        :binary.match(encoded, "\r\n") !== :nomatch,
        "#{byte_size(encoded)}-byte encoded binary should have CRLF pairs"
      )
    end

    check all binary <- binary() do
      encoded = encode(binary)
      lines = :binary.split(encoded, "\r\n", [:global])
      # Here we calculate the size of the encoded binary without CRLF pair.
      single_line_encoded = Enum.join(lines, "")
      size = byte_size(single_line_encoded)

      case size do
        0 ->
          assert length(lines) === 1
        _ ->
          assert(
            length(lines) === ceil(size / 128),
            """
            assert length(lines) === ceil(size / 128)
            left: #{length(lines)}
            right: #{ceil(size / 128)}
            yEnc: #{inspect(encoded, limit: :infinity)}
            size: #{size}
            """
          )
      end
    end
  end

  property "nth escaped sequence is kept at the same line" do
    criticals = Enum.map([@tab, @space | @criticals], &(<<&1>>))

    # Our test binary has the first 127 bytes safe, i.e., no critical
    # characters, nor leading TAB or spaces. This ensures the encoded
    # yEnc for that first 127 bytes also has length of 127 bytes.
    check all bin1 <- binary_without(criticals, length: 127),
              char <- member_of(@criticals),
              bin2 <- binary(min_length: 1)
    do
      binary = <<bin1::binary, char::integer, bin2::binary>>
      encoded = encode(binary)

      assert <<_::binary-size(127), ?=, _, ?\r, ?\n, _::binary>> = encoded
    end
  end

  property "custom nth option to break lines" do
    criticals = Enum.map([@tab, @space | @criticals], &(<<&1>>))

    check all line_size <- integer(2..256),
              pre_break = line_size - 1,
              bin1 <- binary_without(criticals, length: pre_break),
              char <- member_of(@criticals),
              bin2 <- binary(min_length: 1)
    do
      binary = <<bin1::binary, char::integer, bin2::binary>>
      encoded = encode(binary, line_size: line_size)

      assert(
        Kernel.match?(<<_::binary-size(pre_break), ?=, _, ?\r, ?\n, _::binary>>, encoded),
        """
        code: <<_::binary-size(#{pre_break}), ?=, _, ?\\r, ?\\n, _::binary>> = encoded
        left: <<_::binary-size(#{pre_break}), ?=, _, ?\\r, ?\\n, _::binary>>
        right: #{inspect(encoded, limit: :infinity)},
        """
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

  # Binary generator without characters matching pattern.
  defp binary_without(patterns, options) do
    map(
      binary(options),
      # Replaces characters in patterns with an empty space.
      &(:binary.replace(&1, patterns, <<" ">>, [:global]))
    )
  end

end
