defmodule Neurnet do
  @moduledoc """
  Documentation for `Neurnet`.
  """

  @type genotype :: atom()
  @type phenotype :: Phenotype.id()

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Loads the actuators from the indicated modules.
  """
  @spec load(modules :: [module :: atom]) :: {:atomic, [Genotype]}
  def load(modules) do
    Database.run(fn -> Genotype.load(modules) end)
  end

  @doc """
  Creates a phenotype from a genotype.
  """
  @spec phenotype_from(genotype) :: :agent.id()
  def phenotype_from(genotype) do
    genotype
    |> Database.dirty_read!(:genotype)
    |> Phenotype.from()
    |> Database.dirty_write(:phenotype)
  end

  @doc """
  Returns the phenotype information.
  """
  @spec info(phenotype()) :: %Phenotype{}
  def info({_, :phenotype} = id) do
    Database.dirty_read!(id, :phenotype)
  end

  @doc """
  Creates an eevo agent from a phenotype to run in a population.
  """
  @spec agent_from(phenotype()) :: :agent.id()
  def agent_from(phenotype_id) do
    :eevo.agent(%{
      function: &Phenotype.controller/1,
      mutation: &Neurnet.mutate/1,
      arguments: [phenotype_id]
    })
  end

  @doc """
  Mutates a list of phenotypes returning their childs id
  """
  @spec mutate([phenotype()]) :: phenotype()
  def mutate(list) when is_list(list) do
    Enum.map(list, &mutate/1)
  end

  def mutate({reference, :phenotype}) do
    reference
    |> Database.dirty_read!(:phenotype)
    |> Phenotype.clone()
    |> Phenotype.mutate()
    |> Database.dirty_write(:phenotype)
  end

  @doc """
  Runs a training evolutionary algorithm with phenotypes
  """
  @spec run(atom(), genotype(), number(), function()) :: :eevo.results()
  def run(name, genotype, parallel, stop_condition) do
    phenotypes = for _ <- 1..parallel, do: phenotype_from(genotype)
    agents = for p <- phenotypes, do: agent_from(p)
    :eevo.run(name, agents, parallel, stop_condition)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end