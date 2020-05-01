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

  defmacro actuator_id(name), do: {name, :actuator}

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
  @spec load(modules :: [module :: atom]) :: [Actuator]
  def load(modules) do
    actuators_set = set_modules(modules, MapSet.new())
    # Write in mnesia
    MapSet.to_list(actuators_set)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  #  Loads the groups and returns a set with the collected actuators
  defp set_modules([module | modules], set) do
    groups = Group.load(module)
    set = Enum.reduce(groups, set, &Group.add_members_to_set/2)
    set_modules(modules, set)
  end

  defp set_modules([], actuators_set) do
    actuators_set
  end
end
