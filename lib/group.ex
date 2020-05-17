defmodule Group do
  @moduledoc """
  """
  defstruct id: nil, members: nil

  defmacro group_id(name), do: Database.id(name, :group)

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
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec load(module) :: [group :: struct]
  def load(module) do
    groups = for {n, m} <- module.groups(), do: new_group(n, m)
    for g <- groups, do: Database.write(g)
    groups
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates a group with the defined id and members -----------------
  defp new_group(name, members) do
    %Group{:id => group_id(name), :members => MapSet.new(members)}
  end
end
