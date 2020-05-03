defmodule Actuator do
  @moduledoc """
  """
  defstruct id: nil, function: nil

  @type state() :: map()
  @type nextState() :: state()
  @type score() :: number()
  @type error() :: number()
  @type reason() :: term()
  @type result() ::
          {:ok, nextState}
          | {:ok, error, score, nextState}
          | {:stop, reason, nextState}
          | {:stop, reason, score, nextState}

  defmacro actuator_id(name), do: Database.id(:actuator, name)

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an actuator structure
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Actuator{}) |> Map.keys()

  @doc """
  Loads the actuators from the indicated modules.
  """
  @spec load(atom | [atom]) :: any
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    actuators = Group.load(module) |> from_groups()

    for name <- actuators do
      Database.write(new_actuator(name, module))
    end
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates an actuator with the defined id and function ------------
  defp new_actuator(function_name, module) do
    # %Actuator{:id => actuator_id(name), :function => &module.name/2}
    fun = Function.capture(module, function_name, 2)
    %Actuator{:id => actuator_id(function_name), :function => fun}
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
