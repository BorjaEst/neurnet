defmodule Genotype do
  @moduledoc """
  """
  defstruct model: nil, actuators: [], sensors: []

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
  defmacro genotype(name, do: block) do
    genotype_name = String.to_atom(name)
    var_genotype = Macro.var(:v_genotype, nil)
    var_model = Macro.var(:v_model, nil)

    quote do
      @genotypes [unquote(genotype_name) | @genotypes]
      @spec unquote(genotype_name)() :: Genotype.t()
      def unquote(genotype_name)() do
        unquote(var_genotype) = Genotype.new()
        unquote(var_model) = Genotype.empty_map()
        unquote(block)
        Map.put(var!(v_genotype), :model, var!(v_model))
      end
    end
  end

  def new(), do: %Genotype{}
  def empty_map(), do: %{}

  @doc """
  Defines the sensors for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro inputs(list, next) do
    quote do
      var!(v_genotype) = Map.put(var!(v_genotype), :sensors, unquote(list))
      var!(v_model) = Map.merge(var!(v_model), Genotype.din(unquote(list), unquote(next)))
    end
  end

  def din(l, n), do: %{inputs: :layer.input(length(l), %{n => :sequential})}

  @doc """
  Defines the actuators for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro outputs(list) do
    quote do
      var!(v_genotype) = Map.put(var!(v_genotype), :actuators, unquote(list))
      var!(v_model) = Map.merge(var!(v_model), Genotype.dout(unquote(list)))
    end
  end

  def dout(list), do: %{outputs: :layer.output(length(list), %{})}

  @doc """
  Defines the architecture for a genotype. Should be placed inside
  a defgenotype ... end instance
  """
  defmacro layers(lmap) do
    quote do
      var!(v_model) = Map.merge(var!(v_model), unquote(lmap))
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
  @spec load(atom | [atom]) :: [%Genotype{}]
  def load(modules) when is_list(modules) do
    :lists.append(for m <- modules, do: load(m))
  end

  def load(module) do
    _ = Actuator.load(module.actuators())
    _ = Sensor.load(module.sensors())

    for name <- module.genotypes do
      genotype = %Genotype{} = apply(module, name, [])
      Database.new(:genotype, name, genotype)
      name
    end
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================
end
