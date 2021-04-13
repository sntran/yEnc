defmodule YEnc do
  @moduledoc ~S"""
  The Erlang yEnc decoder and encoder.

  yEnc is a binary-to-text encoding scheme for transferring binary files in
  messages on Usenet or via e-mail. It reduces the overhead over previous
  US-ASCII-based encoding methods by using an 8-bit encoding method. yEnc's
  overhead is often (if each byte value appears approximately with the same
  frequency on average) as little as 1–2%, compared to 33%–40% overhead
  for 6-bit encoding methods like uuencode and Base64. yEnc was initially
  developed by Jürgen Helbing and its first release was early 2001. By 2003
  yEnc became the de facto standard encoding system for binary files on Usenet.
  The name yEncode is a wordplay on "Why encode?", since the idea is to only
  encode characters if it is absolutely required to adhere to the message
  format standard.

  ## Examples

      iex> YEnc.encode(<<"ERLANG">>)
      "o|vkxq"

      iex> YEnc.decode("o|vkxq")
      "ERLANG"

      iex> YEnc.post("0b.bin", <<>>)
      "=ybegin line=128 size=0 name=0b.bin\r\n\r\n=yend size=0 crc32=00000000"

      iex> YEnc.post("Erlang.txt", <<"ERLANG">>)
      "=ybegin line=128 size=6 name=Erlang.txt\r\no|vkxq\r\n=yend size=6 crc32=8a5c101d"

  ## Encoding Principle

  The encoding process represents each octet of input data with a single
  corresponding encoded output character.  The ASCII value of each output
  character is derived by the following simple formula:

  O = (I+42) % 256

  That is, the output value is equal to the ASCII value of each input
  character plus 42, all modulo 256.  This reduces overhead by reducing the
  number of NULL characters (ASCII 00) that would otherwise have had needed
  to be escaped, since many binaries contain a disproportionately large
  number of NULLs).

  Under special circumstances, a single escape character (ASCII 3Dh, "=") is
  used to indicate that the following output character is "critical", and
  requires special handling.

  Critical characters include the following:

  ASCII 00h (NULL)
  ASCII 0Ah (LF)
  ASCII 0Dh (CR)
  ASCII 3Dh (=)
  ASCII 09h (TAB)

  These characters should always be escaped.  Additionally, technique used to
  encode critical characters (described in the next section) provides for any
  character to be escaped; yDecoder implementations should be capable of
  decoding any character following an escape sequence.

  The probability of occurance of these 4 characters in binary input data is
  approximately 0.4%.  On average, escape sequences cause approximately 1.6%
  overhead when only these 4 characters are escaped.

  The carriage return/linefeed overhead for every line depends on the
  developer-defined line length.  Header and trailer lines are relatively
  small, and cause negligible impact on output size.

  ## Encoding Technique

  A typical encoding process might look something like this:

  1. Fetch a character from the input stream.
  2. Increment the character's ASCII value by 42, modulo 256
  3. If the result is a critical character (as defined in the previous
      section), write the escape character to the output stream and increment
      character's ASCII value by 64, modulo 256.
  4. Output the character to the output stream.
  5. Repeat from start.

  """
  @moduledoc authors: ["Sơn Trần-Nguyễn"]

  @doc ~S"""
  Performs raw yEnc encoding on data returning the result.

  ## Examples

      iex> YEnc.encode("")
      ""

      iex> YEnc.encode(<<>>)
      ""

      iex> YEnc.encode("ERLANG")
      "o|vkxq"

      iex> YEnc.encode(<<69, 82, 76, 65, 78, 71>>)
      "o|vkxq"
  """
  @spec encode(binary()) :: binary()
  defdelegate encode(data), to: :yEnc

  @doc ~S"""
  Performs raw yEnc decoding on data returning the result.

  ## Examples

      iex> YEnc.decode("")
      ""

      iex> YEnc.decode("o|vkxq")
      "ERLANG"

      iex> YEnc.decode(<<111, 124, 118, 107, 120, 113>>)
      "ERLANG"
  """
  @spec decode(binary()) :: binary()
  defdelegate decode(text), to: :yEnc

  @doc ~S"""
  Returns a single yEnc encoded post, suitable for posting.

  ## Examples

      iex> YEnc.post("0b.bin", "")
      "=ybegin line=128 size=0 name=0b.bin\r\n\r\n=yend size=0 crc32=00000000"
  """
  @spec post(Path.t(), binary()) :: binary()
  defdelegate post(filename, data), to: :yEnc

  @doc ~S"""
  Computes and returns the crc32 (hexadecimal style) checksum for `data`.

  ## Examples

    iex> YEnc.crc32("")
    "00000000"

    iex> YEnc.crc32("ERLANG")
    "8a5c101d"
  """
  @spec crc32(binary) :: binary()
  defdelegate crc32(data), to: :yEnc

end
