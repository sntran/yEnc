# Usage: mix run bench/encode.exs [optional switches]
# Optional switches:
#   -z, --size <bytes>        - Binary size in bytes to encode (default is 76800)
#   -s, --sleep <seconds>     - warmup time before real benchmark, in seconds (default is 2)
#   -j, --jobs <number>       - number of processes to be used for each benchmarking job (default is 1)
#   -x, --extended_statistics - whether to show extended statistics
#
# Hit Ctrl+C twice to stop it.

{options, _argv, _invalid} = OptionParser.parse(System.argv(),
  aliases: [z: :size, s: :sleep, x: :extended_statistics],
  strict: [size: :integer, sleep: :integer, extended_statistics: :boolean]
)

size = options[:size] || 768000

jobs = %{
  "yEnc" => &:yEnc.encode/1,
  "YEnc" => &YEnc.encode/1,
}

{_microseconds, bufWorst} = :timer.tc(fn() ->
  :binary.copy(<<224>>, size)
end)

{_microseconds, bufBest} = :timer.tc(fn() ->
  :binary.copy(<<0>>, size)
end)

inputs = %{
  "Worst (all escaping)" => bufWorst,
  "Best (no escaping)" => bufBest,
  "Random" => Enum.at(StreamData.binary(length: size), 1)
}

Benchee.run(jobs,
  parallel: options[:jobs] || 1,
  warmup: options[:sleep] || 2,
  time: 1,
  # memory_time: 1,
  inputs: inputs,
  formatters: [
    {
      Benchee.Formatters.Console,
      extended_statistics: options[:extended_statistics]
    }
  ]
)
