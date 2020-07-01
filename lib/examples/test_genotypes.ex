defmodule TestGenotypes do
  @moduledoc """
  """

  ### =================================================================
  ###  Modules to preload
  ### =================================================================

  def actuators, do: [TestActuators]
  def sensors, do: [TestSensors]

  ### =================================================================
  ###  Defined genotypes
  ### =================================================================
  use Genotype

  @doc """
  Simple genotype with low probability of success.
  """
  genotype "dummy_gate" do
    inputs([:bool_input1, :bool_input2], :outputs)
    outputs([:gate_or_null])
  end

  @doc """
  Complex genotype with medium probability of success
  """
  genotype "simple_gate" do
    inputs([:bool_input1, :bool_input2], :hidden1)
    outputs([:gate_score])

    layers(%{
      hidden1: :layer.tanh(2, %{outputs: :sequential})
    })
  end

  @doc """
  Complex genotype with medium probability of success
  """
  genotype "complex_gate" do
    inputs([:bool_input1, :bool_input2], :hidden1)
    outputs([:gate_score])

    layers(%{
      hidden1: :layer.dense(3, %{hidden2: :sequential}),
      hidden2: :layer.dense(3, %{outputs: :sequential})
    })
  end
end
