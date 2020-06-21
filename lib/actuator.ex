defmodule Actuator do
  @moduledoc """
  """
  defstruct function: nil, evolution: []

  @type data() :: map()
  @type score() :: number()
  @type error() :: number()
  @type reason() :: term()
  @type result() ::
          {:ok, score(), data()}
          | {:ok, error(), score(), data()}
          | {:stop, reason()}
          | {:stop, reason(), score()}

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an actuator structure
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Actuator{}) |> Map.keys()

  @doc """
  Loads the actuators from the indicated modules.
  Should run inside :mnesia transaction (or Database.run/1)
  """
  @spec load(atom | [atom]) :: any
  def load(modules) when is_list(modules) do
    for m <- modules, do: load(m)
  end

  def load(module) do
    groups = Group.load(module)
    actuators = :lists.append(for g <- groups, do: Group.members(g))

    for name <- Enum.uniq(actuators) do
      Database.new(:actuator, name, new(name, module))
    end
  end

  @doc """
  Performs a random evaluation and if true, mutates the actuator
  """
  @spec mutate(atom) :: atom
  def mutate(name) do
    actuator = Database.read!({:actuator, name})

    case :ltools.rand_scale(actuator.evolution) do
      {} -> name
      new -> new
    end
  end

  @doc """
  Executes an actuator function
  """
  @spec run(%Actuator{}, number(), data()) :: result()
  def run(%Actuator{function: function}, signal, data) do
    apply(function, [signal, data])
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Creates an actuator with the defined id and function ------------
  defp new(function_name, module) do
    %Actuator{
      function: Function.capture(module, function_name, 2),
      evolution: apply(module, function_name, [])
    }
  end
end
