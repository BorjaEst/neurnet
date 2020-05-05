defmodule TestArchitectures do
  @moduledoc """
  """

  ### =================================================================
  ###  Defined Architectures
  ### =================================================================
  use Architecture

  @doc """
  Simple architecture, only 1 initial layer and sequential.
  """
  defarchitecture "simple" do
    defdimensions([2])
    defmodel_type(:sequential)
  end

  @doc """
  Complex architecture, 2 initial layer and allows recurrent links.
  """
  defarchitecture "complex" do
    defdimensions([3, 3])
    defmodel_type(:recurrent)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
