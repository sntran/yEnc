defmodule YEncTest do
  use YEnc.TestCase, async: true

  doctest YEnc, tags: []

  setup do
    file = "fixtures/binaries/1k.bin"
    line_or_bytes = 64
    stream = File.stream!(file, [], line_or_bytes)

    [stream: stream, line_or_bytes: line_or_bytes]
  end

  describe "decode/1" do
    test "", _context do

    end
  end

end
