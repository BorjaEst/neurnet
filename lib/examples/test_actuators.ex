defmodule TestActuators do
  @moduledoc """
  An actuator must return one of the following values:
  @spec actuator_name(Signal, State) ->
                      {  ok,                NextState} |
                      {  ok,  Error, Score, NextState} |
                      {stop, Reason,        NextState} |
                      {stop, Reason, Score, NextState} |
  """

  ### =================================================================
  ###  Groups
  ### =================================================================
  @doc """
  Function: groups() -> [Group]
  Group = {GroupName, [ActuatorName :: atom()]
  """
  def groups() do
    [
      {:gate_score, [:xor_score]},
      {:gate_or_null, [:xor_score, :null]}
    ]
  end

  ### =================================================================
  ###  Defined Actuators
  ### =================================================================

  @doc """
  Scores and agent according to the xor error. Backpropagation.
  """
  def xor_score(), do: []

  @train_cycles 400
  @test_cycles 4
  @spec xor_score(number, map) :: Actuator.result()
  def xor_score(signal, %{cycle: cycle} = state) do
    error = num_xor(state.i1, state.i2) - signal

    cond do
      cycle <= @train_cycles ->
        {:ok, error, 0.0, %{state | cycle: cycle + 1}}

      cycle < @test_cycles + @train_cycles ->
        {:ok, error, score(error), %{state | cycle: cycle + 1}}

      true ->
        {:stop, :normal, score(error)}
    end
  end

  def xor_score(signal, %{} = state) do
    xor_score(signal, Map.put(state, :cycle, 1))
  end

  @doc """
  This does nothing (to deactivate actuator).
  """
  def null(), do: [{:xor_score, 0.9}]

  @spec null(number, map) :: Actuator.result()
  def null(_signal, state) do
    case :rand.uniform() do
      x when x > 0.1 -> {:ok, 0.0, state}
      _ -> {:stop, :normal, 0.001}
    end
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Applies the numeric xor function --------------------------------
  defp num_xor(-1, -1), do: -1
  defp num_xor(+1, -1), do: +1
  defp num_xor(-1, +1), do: +1
  defp num_xor(+1, +1), do: -1

  # Scores according to the error -----------------------------------
  defp score(err) do
    250 - 250 * (abs(err) / 2)
  end
end
