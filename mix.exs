defmodule Neurnet.MixProject do
  use Mix.Project

  def project do
    [
      app: :neurnet,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :mnesia, :datalog, :enn, :eevo],
      mod: {Neurnet.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:jiffy, git: "git://github.com/aboroska/jiffy.git", branch: "rebar3", override: true},
      {:datalog,
       git: "git://github.com/BorjaEst/datalog.git", branch: "jiffy-rebar3", override: true},
      {:numerl, git: "git://github.com/BorjaEst/numerl.git", branch: "master"},
      {:enn, git: "git://github.com/BorjaEst/enn.git", branch: "master"},
      {:nnet, git: "git://github.com/BorjaEst/nnet.git", branch: "master"},
      {:eevo, git: "git://github.com/BorjaEst/eevo.git", branch: "master"}
    ]
  end
end
