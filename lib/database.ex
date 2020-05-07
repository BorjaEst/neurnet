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
  @spec write(entry) :: id
  def write(%{:id => {tname, _}} = data) do
    :ok = :mnesia.write({tname, data.id, data})
    data.id
  end

  @spec write(tname, term) :: id
  def write(tname, data) do
    id = id(tname)
    :ok = :mnesia.write({tname, id, data})
    id
  end

  @doc """
  Reads data from mnesia
  Should run inside :mnesia transaction (or run/1)
  """
  @spec read!(id) :: term
  def read!({tname, _} = id) do
    case :mnesia.read(tname, id) do
      [{^tname, _, data}] -> data
      [] -> raise "not found #{id}"
    end
  end

  @spec read!(tname, term) :: term
  def read!(tname, ref) do
    case :mnesia.read(tname, id(tname, ref)) do
      [{^tname, _, data}] -> data
      [] -> raise "not found #{ref} in table #{tname}"
    end
  end

  @doc """
  Executes the mnesia transactions
  """
  @spec run(operations) :: {:aborted, any} | {:atomic, any}
  def run(operations) do
    :mnesia.transaction(operations)
  end

  @doc """
  Dirty data write in mnesia.
  """
  @spec dirty_write(entry) :: id
  def dirty_write(%{:id => {tname, _}} = data) do
    :ok = :mnesia.dirty_write({tname, data.id, data})
    data.id
  end

  @spec dirty_write(tname, term) :: id
  def dirty_write(tname, data) do
    id = id(tname)
    :ok = :mnesia.dirty_write({tname, id, data})
    id
  end

  @doc """
  Dirty data read from mnesia
  """
  @spec dirty_read!(id) :: term
  def dirty_read!({tname, _} = id) do
    case :mnesia.dirty_read(tname, id) do
      [{^tname, _, data}] -> data
      [] -> raise "not found #{id}"
    end
  end

  @spec dirty_read!(tname, term) :: term
  def dirty_read!(tname, ref) do
    case :mnesia.dirty_read(tname, id(tname, ref)) do
      [{^tname, _, data}] -> data
      [] -> raise "not found #{ref} in table #{tname}"
    end
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
