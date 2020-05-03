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

  @spec xor_score(number, map) :: Actuator.result()
  def xor_score(signal, state) do
    error = signal - num_xor(state.i1, state.i2)
    {:ok, error, score(error), %{state | :bool_in => []}}
  end

  @doc """
  This does nothing (to deactivate actuator).
  """
  def null(), do: [:xor_score]

  @spec null(number, map) :: Actuator.result()
  def null(_signal, state) do
    {:ok, state}
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
    2000 - 1000 * abs(err)
  end
end
