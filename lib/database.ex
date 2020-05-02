defmodule Database do
  @moduledoc """
  """
  @type tname :: atom
  @type id :: {tname, term}
  @type entry :: %{required(:id) => id, optional(term) => term}
  @type operations :: function
  @attributes [:id, :data]

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Creates a new table in mnesia
  """
  @spec new_table(tname) :: {:aborted, any} | true
  def new_table(name) do
    case :mnesia.create_table(name, attributes: @attributes) do
      {:atomic, :ok} -> true
      {:aborted, {:already_exists, :test}} -> check_table(name)
      other -> other
    end
  end

  @doc """
  Creates entry id
  """
  @spec id(tname) :: id
  def id(tname), do: {tname, make_ref()}

  @spec id(tname, term) :: id
  def id(tname, ref), do: {tname, ref}

  @doc """
  Writes data in mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec write(entry) :: :ok
  def write(%{:id => {tname, _}} = data) do
    :mnesia.write({tname, data.id, data})
  end

  @doc """
  Reads data from mnesia
  Should run inside :mnesia transaction (or run/1)
  """
  @spec read(id) :: term
  def read({tname, _} = id) do
    case :mnesia.read(tname, id) do
      [{^tname, _, data}] -> data
      [] -> nil
    end
  end

  @doc """
  Executes the mnesia transactions
  """
  @spec run(operations) :: {:aborted, any} | {:atomic, any}
  def run(operations) do
    :mnesia.transaction(operations)
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Checks the table has the correct attributes ---------------------
  defp check_table(name) do
    case :mnesia.table_info(name, :attributes) do
      @attributes -> true
      _ -> raise "table #{name} already exists using invalid attributtes"
    end
  end
end
