defmodule Group do
  @moduledoc """
  """
  defstruct members: nil

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
    for {name, members} <- module.groups() do
      Database.new(:group, name, new(members))
      name
    end
  end

  @doc """
  Returns the group members.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec members(atom()) :: [term()]
  def members(name) do
    group = read!(name)
    group.members
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates a group with the defined id and members -----------------
  defp new(members) do
    %Group{:members => Enum.uniq(members)}
  end

  # Reads a group from the database ---------------------------------
  defp read!(name) do
    Database.read!({:group, name})
  end
end
