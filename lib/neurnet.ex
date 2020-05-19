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
  Creates an eevo agent from a phenotype to run in a population.
  """
  @spec agent_from(phenotype) :: :agent.id()
  def agent_from(phenotype_id) do
    :eevo.agent(%{
      function: &Phenotype.controller/1,
      mutation: &Phenotype.mutate/1,
      arguments: [phenotype_id]
    })
  end

  def mutate({reference, :phenotype}) do
    reference
    |> Database.dirty_read!(:phenotype)
    |> Phenotype.clone()
    |> Phenotype.mutate()
    |> Database.dirty_write(:phenotype)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
