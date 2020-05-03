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
  def seq_1(%{:seq_1 => [signal | sx]} = state) do
    {:ok, signal, %{state | :in => [signal | state.in], :seq_1 => sx}}
  end

  def seq_1(%{:seq_1 => []} = state) do
    {:stop, "end of training", state}
  end

  def seq_1(%{} = state) do
    seq_1(%{state | :seq_1 => @xor_seq})
  end

  @doc """
  Returns a boolean in an specific sequence.
  """
  def seq_2(), do: []

  @xor_seq [-1, -1, +1, +1]
  @spec seq_2(Sensor.state()) :: Sensor.result()
  def seq_2(%{:seq_2 => [signal | sx]} = state) do
    {:ok, signal, %{state | :in => [signal | state.in], :seq_2 => sx}}
  end

  def seq_2(%{:seq_2 => []} = state) do
    {:stop, "end of training", state}
  end

  def seq_2(%{} = state) do
    seq_2(%{state | :seq_2 => @xor_seq})
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
