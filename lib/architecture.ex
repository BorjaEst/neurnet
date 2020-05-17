defmodule Architecture do
  @moduledoc """
  """
  defstruct id: nil, dim: nil, type: nil

  @doc false
  defmacro __using__(_opts) do
    quote do
      import Architecture

      # Initialize @architectures to an empty list
      @architectures []

      # Invoke Architecture.__before_compile__/1 before the module is compiled
      @before_compile Architecture
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      # Creates a function to return the architectures
      def architectures, do: @architectures
    end
  end

  @doc """
  Defines a new architecture
  """
  defmacro defarchitecture(name, do: block) do
    architecture_name = String.to_atom(name)
    var_architecture = Macro.var(:architecture, nil)

    def_architecture =
      Macro.escape(%Architecture{
        id: Database.id(architecture_name, :architecture)
      })

    quote do
      @architectures [unquote(architecture_name) | @architectures]
      @spec unquote(architecture_name)() :: Architecture.t()
      def unquote(architecture_name)() do
        unquote(var_architecture) = unquote(def_architecture)
        unquote(block)
        unquote(var_architecture)
      end
    end
  end

  @doc """
  Defines the dimensions for an architecture. Should be placed inside
  a defarchitecture ... end instance
  """
  defmacro defdimensions(dimensions) do
    quote do
      var!(architecture) = Map.put(var!(architecture), :dim, unquote(dimensions))
    end
  end

  @doc """
  Defines the model type for an architecture. Should be placed inside
  a defarchitecture ... end instance
  """
  defmacro defmodel_type(model_type) do
    quote do
      var!(architecture) = Map.put(var!(architecture), :type, unquote(model_type))
    end
  end

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an architecture structure.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Architecture{}) |> Map.keys()

  @doc """
  Loads the architectures from the indicated modules.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec load(atom | [atom]) :: any
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    for name <- module.architectures do
      architecture = %Architecture{} = apply(module, name, [])
      Database.write(architecture)
    end
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
