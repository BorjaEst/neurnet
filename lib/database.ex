defmodule Database do
  @moduledoc """
  """
  @type key() :: term()
  @type tname() :: atom()
  @type id() :: {tname(), key()}
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
  Executes the mnesia transactions
  """
  @spec run(function()) :: {:aborted, any} | {:atomic, any}
  def run(operations) do
    :mnesia.transaction(operations)
  end

  @doc """
  Writes new data in mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec new(tname(), term()) :: id()
  def new(tname, data) do
    key = make_ref()
    :ok = :mnesia.write({tname, key, data})
    {tname, key}
  end

  @spec new(tname(), key(), term()) :: id()
  def new(tname, key, data) do
    :ok = :mnesia.write({tname, key, data})
    {tname, key}
  end

  @doc """
  Writes new data in mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec dirty_new(tname(), term()) :: id()
  def dirty_new(tname, data) do
    key = make_ref()
    :ok = :mnesia.dirty_write({tname, key, data})
    {tname, key}
  end

  @doc """
  Reads data from mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec read!(id()) :: term()
  def read!({tname, key}) do
    case :mnesia.read(tname, key) do
      [{_, _, data}] -> data
      [] -> raise "not found '#{key}' in table '#{tname}'"
    end
  end

  @doc """
  Dirty data read from mnesia
  """
  @spec dirty_read!(id) :: term
  def dirty_read!({tname, key}) do
    case :mnesia.dirty_read(tname, key) do
      [{_, _, data}] -> data
      [] -> raise "not found '#{key}' in table '#{tname}'"
    end
  end

  @doc """
  Overwrites data in mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec write(id(), term()) :: term()
  def write({tname, key}, data) do
    :ok = :mnesia.write({tname, key, data})
    {tname, key}
  end

  @doc """
  Dirty data write in mnesia.
  """
  @spec dirty_write(id(), term()) :: term()
  def dirty_write({tname, key}, data) do
    :ok = :mnesia.dirty_write({tname, key, data})
    {tname, key}
  end

  @doc """
  Updates an id data with using function data from mnesia.
  Should run inside :mnesia transaction (or run/1)
  """
  @spec updt!(id(), function()) :: term()
  def updt!({tname, key} = id, fun) do
    case :mnesia.wread({tname, key}) do
      [{_, _, data}] -> write(id, fun.(data))
      [] -> raise "not found #{id}"
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
