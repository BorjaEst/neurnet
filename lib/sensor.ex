defmodule Sensor do
  @moduledoc """
  """
  defstruct id: nil, function: nil, evolution: []

  @type data() :: map()
  @type signal() :: number()
  @type reason() :: term()
  @type result() ::
          {:ok, signal(), data()}
          | {:stop, reason()}

  defmacro sensor_id(name), do: Database.id(name, :sensor)

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an sensor structure
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Sensor{}) |> Map.keys()

  @doc """
  Loads the sensors from the indicated modules.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec load(atom | [atom]) :: any
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    sensors = Group.load(module) |> from_groups()

    for name <- sensors do
      Database.write(new_sensor(name, module))
    end
  end

  @doc """
  Performs a random evaluation and if true, mutates the sensor
  """
  @spec mutate(atom) :: atom
  def mutate(name) do
    sensor = Database.dirty_read!(name, :sensor)

    case :ltools.rand_scale(sensor.evolution) do
      {} -> name
      new -> new
    end
  end

  @doc """
  Executes a sensor function
  """
  @spec run(%Sensor{}, data()) :: result()
  def run(%Sensor{function: function}, data) do
    apply(function, [data])
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates an sensor with the defined id and function ------------
  defp new_sensor(function_name, module) do
    %Sensor{
      id: sensor_id(function_name),
      function: Function.capture(module, function_name, 1),
      evolution: apply(module, function_name, [])
    }
  end

  # Adds the group members to a set ---------------------------------
  defp from_groups(groups) do
    from_groups(MapSet.new(), groups) |> MapSet.to_list()
  end

  defp from_groups(set, [%Group{} = group | groups]) do
    MapSet.union(set, group.members) |> from_groups(groups)
  end

  defp from_groups(set, []), do: set
end