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
  @spec load(modules :: [module :: atom]) :: [Genotype]
  def load(modules) do
    Genotype.load(modules)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
