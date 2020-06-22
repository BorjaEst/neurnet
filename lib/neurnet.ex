defmodule Neurnet do
  @moduledoc """
  Documentation for `Neurnet`.
  """

  @type genotype() :: atom()
  @type phenotype() :: :eevo.agent()

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
  @spec phenotype_from(genotype()) :: phenotype()
  def phenotype_from(name) do
    genotype = Database.dirty_read!({:genotype, name})

    :eevo.agent(%{
      function: &Phenotype.controller/1,
      mutation: &Phenotype.mutate/1,
      arguments: [Phenotype.from(genotype)]
    })
  end

  @doc """
  Returns the phenotype information.
  """
  @spec info(phenotype()) :: phenotype()
  def info(phenotype) do
    %{arguments: [struct]} = :eevo.info(phenotype)
    struct
  end

  @doc """
  Runs a training evolutionary algorithm with phenotypes
  """
  @spec run(atom(), genotype(), number(), function()) :: :eevo.results()
  def run(name, genotype, parallel, stop_condition) do
    phenotypes = for _ <- 1..parallel, do: phenotype_from(genotype)
    :eevo.run(name, phenotypes, parallel, stop_condition)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
