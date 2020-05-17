defmodule Sensor do
  @moduledoc """
  """
  defstruct id: nil, function: nil

  @type state() :: map()
  @type next_state() :: state()
  @type signal() :: number()
  @type reason() :: term()
  @type result() ::
          {:ok, signal, next_state}
          | {:stop, reason, next_state}

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

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates an sensor with the defined id and function ------------
  defp new_sensor(function_name, module) do
    fun = Function.capture(module, function_name, 1)
    %Sensor{:id => sensor_id(function_name), :function => fun}
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
