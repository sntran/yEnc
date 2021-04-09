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

      iex> YEnc.encode(<<>>)
      {:ok, ""}

      iex> YEnc.decode("")
      {:ok, ""}

      iex> filename = "0b.bin"
      iex> {:ok, yEncodedPost} = YEnc.post(filename, <<>>)
      iex> yEncodedPost
      "=ybegin line=128 size=0 name=0b.bin\r\n\r\n=yend size=0 crc32=00000000"
  """
  @moduledoc authors: ["Sơn Trần-Nguyễn"]

  @doc ~S"""
  Performs raw yEnc encoding on data returning the result.

  ## Examples

      iex> YEnc.encode("")
      {:ok, ""}

      iex> YEnc.encode(<<>>)
      {:ok, ""}
  """
  @spec encode(binary()) :: {:ok, binary()}
  defdelegate encode(data), to: :yEnc

  @doc ~S"""
  Performs raw yEnc decoding on data returning the result.

  ## Examples

      iex> YEnc.decode("")
      {:ok, ""}
  """
  @spec decode(binary()) :: {:ok, binary()}
  defdelegate decode(text), to: :yEnc

  @doc ~S"""
  Returns a single yEnc encoded post, suitable for posting.

  ## Examples

      iex> {:ok, yEncodedPost} = YEnc.post("0b.bin", "")
      iex> yEncodedPost
      "=ybegin line=128 size=0 name=0b.bin\r\n\r\n=yend size=0 crc32=00000000"
  """
  @spec post(Path.t(), binary()) :: {:ok, binary()}
  defdelegate post(filename, data), to: :yEnc

end
