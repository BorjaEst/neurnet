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

  @doc """
  Mutates the neuronal network
  """
  @spec mutate(:enn.network()) :: :enn.network()
  def mutate(network) do
    {:atomic, :ok} = :enn_edit.transaction(network, &mutate_network/1)
    network
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Randomly mutates a neural network -------------------------------
  defp mutate_network(enn) do
    enn
    |> maybe_divide_neuron
    |> maybe_increasse_connections
    |> mutate_neurons
  end

  defp mutate_neurons(enn) do
    neurons = :network.neurons(enn)
    sf = :math.sqrt(Map.get(:network.info(enn), :size))
    Enum.reduce(:ltools.rand(neurons, 1 / sf), enn, &mutate_neuron/2)
  end

  @chance 0.05
  defp maybe_divide_neuron(enn) do
    maybe(@chance, enn, fn enn ->
      neurons = :network.neurons(enn)
      neuron = :ltools.randnth(neurons)
      :enn_edit.divide_neuron(enn, neuron)
    end)
  end

  @chance 0.05
  defp maybe_increasse_connections(enn) do
    maybe(@chance, enn, fn enn ->
      neurons = :network.neurons(enn)
      sf = :math.sqrt(Map.get(:network.info(enn), :size))
      from = :ltools.rand(neurons, 1 / sf)
      to = :ltools.rand(neurons, 1 / sf)
      :enn_edit.connect_allowed(enn, from, to)
    end)
  end

  # Randomly mutates a neuron network -------------------------------
  defp mutate_neuron(neuron, enn) do
    enn
    |> maybe_reinitialise_bias(neuron)
    |> maybe_switch_activation(neuron)
  end

  @chance 0.15
  defp maybe_reinitialise_bias(enn, neuron) do
    maybe(@chance, enn, fn enn ->
      :enn_edit.reinitialise_bias(enn, neuron)
    end)
  end

  @chance 0.15
  @functions [:direct, :sigmoid, :tanh, :elu]
  defp maybe_switch_activation(enn, neuron) do
    maybe(@chance, enn, fn enn ->
      func = :ltools.randnth(@functions)
      :enn_edit.switch_activation(enn, neuron, func)
    end)
  end

  # Maybe applies the function over enn or returns the enn ----------
  defp maybe(probability, enn, fun) do
    case :rand.uniform() do
      x when x < probability -> apply(fun, [enn])
      _ -> enn
    end
  end
end
