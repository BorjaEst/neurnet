defmodule Neurnet do
  @moduledoc """
  Documentation for `Neurnet`.
  """

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
  @spec phenotype_from(genotype :: atom) :: :agent.id()
  def phenotype_from(gref) do
    genotype = Database.dirty_read!(:genotype, gref)
    phenotype = Phenotype.from(genotype)
    Database.dirty_write(:phenotype, phenotype)
  end

  @doc """
  Creates an eevo agent from a phenotype to run in a population.
  """
  @spec agent_from(phenotype :: Phenotype.id()) :: :agent.id()
  def agent_from(phenotype_id) do
    :eevo.agent(%{
      function: &Phenotype.controller/1,
      mutation: &Phenotype.mutate/1,
      arguments: [phenotype_id]
    })
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
