defmodule Neurnet.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    true = Database.new_table(:group)
    true = Database.new_table(:actuator)
    true = Database.new_table(:sensor)
    true = Database.new_table(:genotype)

    children = [
      # Starts a worker by calling: Neurnet.Worker.start_link(arg)
      # {Neurnet.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Neurnet.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
