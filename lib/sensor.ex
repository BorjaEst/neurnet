defmodule Sensor do
  @moduledoc """
  """
  defstruct function: nil, evolution: []

  @type data() :: map()
  @type signal() :: number()
  @type reason() :: term()
  @type result() ::
          {:ok, signal(), data()}
          | {:stop, reason()}

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
  @spec load(atom() | [atom()]) :: any()
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    groups = Group.load(module)
    sensors = :lists.append(for g <- groups, do: Group.members(g))

    for name <- Enum.uniq(sensors) do
      Database.new(:sensor, name, new(name, module))
      name
    end
  end

  @doc """
  Performs a random evaluation and if true, mutates the sensor
  """
  @spec mutate(atom()) :: atom()
  def mutate(name) do
    sensor = Database.dirty_read!({:sensor, name})

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
  defp new(function_name, module) do
    %Sensor{
      function: Function.capture(module, function_name, 1),
      evolution: apply(module, function_name, [])
    }
  end
end
