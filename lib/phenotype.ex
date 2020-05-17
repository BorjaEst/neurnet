defmodule Phenotype do
  @moduledoc """
  """
  defstruct id: nil, network: nil, actuators: [], sensors: []

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an phenotype structure.
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Phenotype{}) |> Map.keys()

  @doc """
  Builds a phenotype from a genotype
  """
  @spec from(%Genotype{}) :: %Phenotype{}
  def from(%Genotype{} = genotype) do
    %Phenotype{
      id: Database.id(:phenotype),
      network: :enn.compile(model(genotype)),
      actuators: select(genotype.actuators),
      sensors: select(genotype.sensors)
    }
  end

  @doc """
  TBD
  """
  def mutate(%Phenotype{} = phenotype) do
    %Phenotype{
      # id: Database.id(:phenotype),
      # network: :enn.compile(model(genotype)),
      # actuators: select(genotype.actuators),
      # sensors: select(genotype.sensors)
    }
  end

  # Add to actuators the mutation info
  def mutate(%Actuator{}), do: :ok
  def mutate(%Sensor{}), do: :ok
  def mutate(network_id), do: :ok

  @doc """
  TBD
  """
  def controller(phenotype_id), do: :ok

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  defp model(%Genotype{} = genotype) do
    architecture = Database.dirty_read!(genotype.architecture, :architecture)
    inputs = :layer.input(length(genotype.sensors))
    hidden = Enum.map(architecture.dim, fn n -> :layer.dense(n) end)
    outputs = :layer.output(length(genotype.actuators))
    apply(:model, architecture.type, [[inputs] ++ hidden ++ [outputs]])
  end

  defp select(groups) when is_list(groups) do
    for g_id <- groups, do: select(g_id)
  end

  defp select(g_id) do
    group = Database.dirty_read!(g_id, :group)
    Enum.random(group.members)
  end
end
