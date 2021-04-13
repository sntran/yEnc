use Mix.Config

config :logger, :console,
  format: "$time $metadata[$level] $levelpad$message\n"

if Mix.env == :dev do
  config :mix_test_watch,
    clear: true
end

if Mix.env == :test do
  config :stream_data,
    max_runs: if System.get_env("CI"), do: 1_024, else: 256
end
