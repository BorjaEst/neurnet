defmodule Phenotype do
  @moduledoc """
  """
  defstruct network: nil, actuators: [], sensors: []
  require Logger

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
  @spec from(%Genotype{}) :: [%Phenotype{}]
  def from(%Genotype{} = genotype) do
    {:atomic, phenotype} =
      :mnesia.transaction(fn ->
        %Phenotype{
          network: :nnet.compile(genotype.model),
          actuators: select(genotype.actuators),
          sensors: select(genotype.sensors)
        }
      end)

    Logger.debug(what: "New phenotype", phenotype: phenotype)
    [phenotype]
  end

  @doc """
  Mutates the phenotype modifying the network, sensors and actuators
  """
  @spec mutate(%Phenotype{}) :: [%Phenotype{}]
  def mutate(%Phenotype{} = original) do
    mutation_function = fn ->
      %Phenotype{
        network: Network.mutate(original.network),
        actuators: for(x <- original.actuators, do: Actuator.mutate(x)),
        sensors: for(x <- original.sensors, do: Sensor.mutate(x))
      }
    end

    case :mnesia.transaction(mutation_function) do
      {:atomic, mutated} -> [mutated]
      {:aborted, _reasn} -> mutate(%Phenotype{} = original)
    end
  end

  ### =================================================================
  ### Phenotype callbacks
  ### =================================================================

  @spec controller(%Phenotype{}) :: :eevo.agent_return()
  @doc """
  Starts the phenotype loop
  """
  def controller(%Phenotype{} = phenotype) do
    {:atomic, network_info} = :mnesia.transaction(fn -> :nnet.info(phenotype.network) end)

    state = %{
      network: :enn.start_link(phenotype.network),
      net_info: network_info,
      score_f: score_factor(network_info),
      cortex: :enn.cortex(phenotype.network),
      actuators: for(x <- phenotype.actuators, do: Database.dirty_read!({:actuator, x})),
      sensors: for(x <- phenotype.sensors, do: Database.dirty_read!({:sensor, x})),
      data: %{}
    }

    Logger.debug(what: "Starting phenotype")
    {:next, &enter_sensors/1, [state]}
  end

  @doc """
  Gets the signals from the phenotype sensors
  """
  def enter_sensors(state) do
    Logger.debug(what: "Entering sensors")
    run_sensors([], state.sensors, state)
  end

  def run_sensors(signals, [sensor | sx], state) do
    case Sensor.run(sensor, state.data) do
      {:ok, signal, data} ->
        run_sensors([signal | signals], sx, %{state | data: data})

      {:stop, reason} ->
        terminate(reason, 0.0, state)
    end
  end

  def run_sensors(signals, [], state) do
    Logger.debug(what: "Exiting sensors", signals: signals)
    {:next, &enter_actuators/2, [signals, state]}
  end

  @doc """
  Converts the sensors signals into actuator inputs and executes the actuators
  """
  def enter_actuators(signals, state) do
    predictions = :cortex.predict(state.cortex, signals)
    Logger.debug(what: "Entering actuators", predictions: predictions)
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
        terminate(reason, s_acc, state)

      {:stop, reason, score} ->
        s_acc = score + s_acc
        terminate(reason, s_acc, state)
    end
  end

  def run_actuators(errors, score_acc, [], [], state) do
    _bp_errors = :cortex.fit(state.cortex, errors)
    score = score_acc * state.score_f
    Logger.debug(what: "Exiting actuators", errors: errors, score: score)
    {:next, &enter_sensors/1, [state], [{:score, score}]}
  end

  @doc """
  Terminates the phenotype
  """
  def terminate(reason, score_acc, state) do
    :ok = :enn.stop(state.network)
    score = score_acc * state.score_f
    Logger.debug(what: "Terminating agent", reason: reason, score: score)
    {:stop, reason, [{:score, score}]}
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  defp select(groups) when is_list(groups) do
    for name <- groups, do: select(name)
  end

  defp select(name) do
    group = Database.read!({:group, name})
    Enum.random(group.members)
  end

  @square_e 7.3890560989306495
  defp score_factor(network_info) do
    0.5 + 1 / :math.log(@square_e + network_info.size)
  end
end
