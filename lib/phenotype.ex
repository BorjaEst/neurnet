defmodule Phenotype do
  @moduledoc """
  """
  defstruct network: nil, actuators: [], sensors: []

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Returns the fields of an phenotype structure.
  """
  @spec fields() :: [atom()]
  def fields(), do: Map.from_struct(%Phenotype{}) |> Map.keys()

  @doc """
  Builds a phenotype from a genotype
  """
  @spec from(%Genotype{}) :: %Phenotype{}
  def from(%Genotype{} = genotype) do
    {:atomic, network} = :enn.compile(genotype.model)

    %Phenotype{
      network: network,
      actuators: select(genotype.actuators),
      sensors: select(genotype.sensors)
    }
  end

  @doc """
  Clones a phenotype
  """
  @spec clone(%Phenotype{}) :: %Phenotype{}
  def clone(%Phenotype{} = phenotype) do
    {:atomic, clone} = :enn.clone(phenotype.network)
    %Phenotype{phenotype | network: clone}
  end

  @doc """
  Mutates the phenotype modifying the network, sensors and actuators
  """
  @spec mutate(%Phenotype{}) :: %Phenotype{}
  def mutate(%Phenotype{} = phenotype) do
    %Phenotype{
      network: Network.mutate(phenotype.network),
      actuators: for(x <- phenotype.actuators, do: Actuator.mutate(x)),
      sensors: for(x <- phenotype.sensors, do: Sensor.mutate(x))
    }
  end

  ### =================================================================
  ### Phenotype callbacks
  ### =================================================================

  @spec controller(%Phenotype{}) :: :eevo.agent_return()
  @doc """
  Starts the phenotype loop
  """
  def controller(%Phenotype{} = phenotype) do
    :enn.start(phenotype.network)
    :enn.link(phenotype.network)

    state = %{
      cortex: :enn.cortex(phenotype.network),
      actuators: for(x <- phenotype.actuators, do: Database.dirty_read!({:actuator, x})),
      sensors: for(x <- phenotype.sensors, do: Database.dirty_read!({:sensor, x})),
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
    {:next, &enter_sensors/1, [state], [{:score, score_acc}]}
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  defp select(groups) when is_list(groups) do
    for name <- groups, do: select(name)
  end

  defp select(name) do
    group = Database.dirty_read!({:group, name})
    Enum.random(group.members)
  end
end
