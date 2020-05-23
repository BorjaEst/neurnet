defmodule TestSensors do
  @moduledoc """
  An sensor must return one of the following values:
  @spec sensor_name(State) ->
                    {  ok, Signal, NextState} |
                    {stop, Reason, NextState}
  """

  ### =================================================================
  ###  Groups
  ### =================================================================
  @doc """
  Function: groups() -> [Group]
  Group = {GroupName, [SensorName :: atom()]
  """
  def groups() do
    [
      {:bool_input1, [:seq_1]},
      {:bool_input2, [:seq_2]}
    ]
  end

  ### =================================================================
  ###  Defined Sensors
  ### =================================================================

  @doc """
  Returns a boolean in an specific sequence.
  """
  def seq_1(), do: []

  @xor_seq [-1, +1, -1, +1]
  @spec seq_1(Sensor.state()) :: Sensor.result()
  def seq_1(%{seq_1: [signal | sx], i1: _} = state) do
    {:ok, signal, %{state | i1: signal, seq_1: sx}}
  end

  def seq_1(%{seq_1: []} = state) do
    {:stop, "end of training", state}
  end

  def seq_1(%{i1: _} = state) do
    seq_1(Map.put(state, :seq_1, @xor_seq))
  end

  def seq_1(%{} = state) do
    seq_1(Map.put(state, :i1, []))
  end

  @doc """
  Returns a boolean in an specific sequence.
  """
  def seq_2(), do: []

  @xor_seq [-1, -1, +1, +1]
  @spec seq_2(Sensor.state()) :: Sensor.result()
  def seq_2(%{seq_2: [signal | sx], i2: _} = state) do
    {:ok, signal, %{state | i2: signal, seq_2: sx}}
  end

  def seq_2(%{seq_2: []} = state) do
    {:stop, "end of training", state}
  end

  def seq_2(%{s_in: _} = state) do
    seq_2(Map.put(state, :seq_2, @xor_seq))
  end

  def seq_2(%{} = state) do
    seq_2(Map.put(state, :i2, []))
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
