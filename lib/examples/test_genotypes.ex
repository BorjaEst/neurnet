defmodule TestGenotypes do
  @moduledoc """
  """

  ### =================================================================
  ###  Modules to preload
  ### =================================================================

  def architectures, do: [TestArchitectures]
  def actuators, do: [TestActuators]
  def sensors, do: [TestSensors]

  ### =================================================================
  ###  Defined genotypes
  ### =================================================================
  use Genotype

  @doc """
  Simple genotype with low probability of success.
  """
  defgenotype "dummy_gate" do
    architecture(:simple)
    actuators([:gate_or_null])
    sensors([:bool_input1, :bool_input2])
  end

  @doc """
  Complex genotype with medium probability of success.
  """
  defgenotype "complex_gate" do
    architecture(:complex)
    actuators([:gate_score])
    sensors([:bool_input1, :bool_input2])
  end
end
