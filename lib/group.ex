defmodule Group do
  @moduledoc """
  """
  defstruct id: nil, members: nil

  defmacro group_id(name), do: {name, :group}

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of a group structure
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Group{}) |> Map.keys()

  @doc """
  Loads the members of an specific group from a module.
  """
  @spec load(module) :: [group :: struct]
  def load(module) do
    groups = for {n, m} <- module.groups(), do: new_group(n, m)
    # Write in mnesia
    groups
  end

  @doc """
  Adds the group members to a set.
  """
  @spec add_members_to_set(%Group{}, MapSet) :: any
  def add_members_to_set(group, set) do
    Enum.reduce(group.members, set, fn x, acc -> MapSet.put(acc, x) end)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates a group with the defined id and members -----------------
  defp new_group(name, members) do
    %Group{:id => name, :members => members}
  end
end
