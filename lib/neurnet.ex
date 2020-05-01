defmodule Neurnet do
  @moduledoc """
  Documentation for `Neurnet`.
  """

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns a list of tuples with the record name and attributes
  list. This is mainly used to prepare the tables in mnesia.
  """
  @spec attributes_table() :: [{name :: atom, [attributes :: atom]}]
  def attributes_table() do
    [
      {:actuator, Actuator.fields()},
      {:group, Group.fields()}
    ]
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
