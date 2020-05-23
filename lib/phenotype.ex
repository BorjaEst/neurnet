defmodule Phenotype do
  @moduledoc """
  """
  defstruct id: nil, network: nil, actuators: [], sensors: []

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an phenotype structure.
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Phenotype{}) |> Map.keys()

  @doc """
  Clones a phenotype
  """
  @spec from(%Phenotype{}) :: %Phenotype{}
  def clone(%Phenotype{} = phenotype) do
    %{phenotype | id: Database.id(:phenotype)}
    %{phenotype | network: :enn.clone(phenotype.network)}
  end

  @doc """
  Builds a phenotype from a genotype
  """
  @spec from(%Genotype{}) :: %Phenotype{}
  def from(%Genotype{} = genotype) do
    %Phenotype{
      id: Database.id(:phenotype),
      network: :enn.compile(model(genotype)),
      actuators: select(genotype.actuators),
      sensors: select(genotype.sensors)
    }
  end

  @doc """
  Mutates the phenotype modifying the network, sensors and actuators
  """
  @spec mutate(%Phenotype{}) :: %Phenotype{}
  def mutate(%Phenotype{} = phenotype) do
    %Phenotype{
      network: Architecture.mutate(phenotype.network),
      actuators: for(x <- phenotype.actuators, do: Actuator.mutate(x)),
      sensors: for(x <- phenotype.sensors, do: Sensor.mutate(x))
    }
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  @spec controller(Database.id()) :: :eevo.agent_return()
  @doc """
  Starts the phenotype loop
  """
  def controller(phenotype_id) do
    phenotype = Database.dirty_read!(phenotype_id)
    :enn.start(phenotype.network)
    :enn.link(phenotype.network)

    state = %{
      cortex: :enn.cortex(phenotype.network),
      actuators: for(x <- phenotype.actuators, do: Database.dirty_read!(x, :actuator)),
      sensors: for(x <- phenotype.sensors, do: Database.dirty_read!(x, :sensor)),
      data: %{}
    }

    {:next, &enter_sensors/1, [state]}
  end

  @doc """
  Gets the signals from the phenotype sensors
  """
  def enter_sensors(state) do
    run_sensors([], state.sensors, state)
  end

  def run_sensors(signals, [sensor | sx], state) do
    case Sensor.run(sensor, state.data) do
      {:ok, signal, data} ->
        run_sensors([signal | signals], sx, %{state | data: data})

      {:stop, reason} ->
        {:stop, reason}
    end
  end

  def run_sensors(signals, [], state) do
    {:next, &enter_actuators/2, [signals, state]}
  end

  @doc """
  Converts the sensors signals into actuator inputs and executes the actuators
  """
  def enter_actuators(signals, state) do
    predictions = :cortex.predict(state.cortex, signals)
    run_actuators([], 0.0, predictions, state.actuators, state)
  end

  def run_actuators(errs, s_acc, [pred | px], [actuator | ax], state) do
    case Actuator.run(actuator, pred, state.data) do
      {:ok, score, data} ->
        s_acc = score + s_acc
        run_actuators([0.0 | errs], s_acc, px, ax, %{state | data: data})

      {:ok, err, score, data} ->
        s_acc = score + s_acc
        run_actuators([err | errs], s_acc, px, ax, %{state | data: data})

      {:stop, reason} ->
        {:stop, reason, []}

      {:stop, reason, score} ->
        {:stop, reason, [{:score, score}]}
    end
  end

  def run_actuators(errors, score_acc, [], [], state) do
    _bp_errors = :cortex.fit(state.cortex, errors)
    {:next, &enter_sensors/1, state, [{:score, score_acc}]}
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  defp model(%Genotype{} = genotype) do
    architecture = Database.dirty_read!(genotype.architecture, :architecture)
    inputs = :layer.input(length(genotype.sensors))
    hidden = Enum.map(architecture.dim, fn n -> :layer.dense(n) end)
    outputs = :layer.output(length(genotype.actuators))
    apply(:model, architecture.type, [[inputs] ++ hidden ++ [outputs]])
  end

  defp select(groups) when is_list(groups) do
    for g_id <- groups, do: select(g_id)
  end

  defp select(g_id) do
    group = Database.dirty_read!(g_id, :group)
    Enum.random(group.members)
  end
end
