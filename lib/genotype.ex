defmodule Genotype do
  @moduledoc """
  """
  defstruct id: nil, architecture: nil, actuators: nil, sensors: nil

  @doc false
  defmacro __using__(_opts) do
    quote do
      import Genotype

      # Initialize @genotypes to an empty list
      @genotypes []

      # Invoke Genotype.__before_compile__/1 before the module is compiled
      @before_compile Genotype
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      # Creates a function to return the genotypes
      def genotypes, do: @genotypes
    end
  end

  @doc """
  Defines a new genotype
  """
  defmacro defgenotype(name, do: block) do
    genotype_name = String.to_atom(name)
    var_genotype = Macro.var(:genotype, nil)

    def_genotype =
      Macro.escape(%Genotype{
        id: Database.id(:genotype, genotype_name)
      })

    quote do
      @genotypes [unquote(genotype_name) | @genotypes]
      @spec unquote(genotype_name)() :: Genotype.t()
      def unquote(genotype_name)() do
        unquote(var_genotype) = unquote(def_genotype)
        unquote(block)
        unquote(var_genotype)
      end
    end
  end

  @doc """
  Defines the architecture for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro architecture(name) do
    quote do
      var!(genotype) = Map.put(var!(genotype), :architecture, unquote(name))
    end
  end

  @doc """
  Defines the actuators for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro actuators(name) do
    quote do
      var!(genotype) = Map.put(var!(genotype), :actuators, unquote(name))
    end
  end

  @doc """
  Defines the sensors for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro sensors(name) do
    quote do
      var!(genotype) = Map.put(var!(genotype), :sensors, unquote(name))
    end
  end

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an genotype structure
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Genotype{}) |> Map.keys()

  @doc """
  Loads the genotypes from the indicated modules.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec load(atom | [atom]) :: any
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    _ = Architecture.load(module.architectures())
    _ = Actuator.load(module.actuators())
    _ = Sensor.load(module.sensors())

    for name <- module.genotypes do
      genotype = %Genotype{} = apply(module, name, [])
      Database.write(genotype)
    end
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
