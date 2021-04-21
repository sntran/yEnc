defmodule YEnc.MixProject do
  use Mix.Project

  @version "0.5.0"
  @repo "https://github.com/sntran/yEnc"

  def project do
    [
      app: :yEnc,
      version: @version,
      description: """
      The Erlang yEnc decoder and encoder.
      """,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      package: package(),

      # Docs
      name: "yEnc",
      source_url: @repo,
      homepage_url: "https://sntran.github.io/yEnc",
      docs: docs(),
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:stream_data, "~> 0.5", only: [:dev, :test]},
      {:benchee, "~> 1.0", only: :dev},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:ex_doc, "~> 0.24", only: :dev, runtime: false},
    ]
  end

  defp package do
    [
      maintainers: [
        "Son Tran-Nguyen"
      ],
      licenses: ["Apache 2.0"],
      links: %{github: @repo},
      files:
        ~w(include lib priv src) ++
          ~w(.formatter.exs mix.exs CHANGELOG.md LICENSE README.md),
      exclude_patterns: [".DS_Store"]
    ]
  end

  defp docs do
    [
      main: "YEnc",
      source_ref: "v#{@version}"
    ]
  end
end
