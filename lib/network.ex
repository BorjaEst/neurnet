defmodule Network do
  @moduledoc """
  """

  ### =================================================================
  ###  API
  ### =================================================================

  @doc """
  Mutates the neuronal network
  """
  @spec mutate(:enn.network()) :: :enn.network()
  def mutate(network) do
    {:atomic, _} = :nnet.edit(fn -> mutate_network(network) end)
    network
  end

  @doc """
  Mutates the neuronal network. Should run inside :nnet.edit/1
  """
  @spec mutate_network(:enn.network()) :: :enn.network()
  def mutate_network({:network, _} = network) do
    network
    |> maybe_divide_neuron
    |> maybe_del_connections
    |> maybe_add_connections
    |> maybe_mutate_neurons
  end

  @chance 0.05
  defp maybe_divide_neuron(network) do
    maybe(@chance, network, &divide_neuron/1)
  end

  @chance 0.25
  defp maybe_del_connections(network) do
    maybe(@chance, network, &del_connections/1)
  end

  @chance 0.25
  defp maybe_add_connections(network) do
    maybe(@chance, network, &add_connections/1)
  end

  @chance 0.75
  defp maybe_mutate_neurons(network) do
    maybe(@chance, network, &mutate_neurons/1)
  end

  @doc """
  Mutates the neuron. Should run inside :nnet.edit/1
  """
  @spec mutate_neuron(:enn.neuron()) :: :enn.neuron()
  def mutate_neuron({:nnode, _} = neuron) do
    neuron
    |> maybe_reinitialise_bias
    |> maybe_switch_activation
  end

  @chance 0.15
  defp maybe_reinitialise_bias(neuron) do
    maybe(@chance, neuron, &reinitialise_bias/1)
  end

  @chance 0.15
  defp maybe_switch_activation(neuron) do
    maybe(@chance, neuron, &switch_activation/1)
  end

  ### =================================================================
  ###  Mutation transactions
  ### =================================================================

  # Divides a neuron ------------------------------------------------
  defp divide_neuron(network) do
    nnodes = :network.nnodes(network)
    neuron = :ltools.randnth(Map.keys(nnodes))
    :nnet.divide(neuron, network)
  end

  # Deletes random connections from the network ---------------------
  defp del_connections(network) do
    nnodes = :network.nnodes(network)
    size_f = :math.sqrt(map_size(nnodes))
    rlinks = rcomb(Map.keys(nnodes), 1.0 / size_f)
    :nnet.disconnect(rlinks)
  end

  # Adds random connections to the network --------------------------
  defp add_connections(network) do
    nnodes = :network.nnodes(network)
    size_f = :math.sqrt(map_size(nnodes))
    rlinks = rcomb(Map.keys(nnodes), 1.0 / size_f)
    :nnet.connect(rlinks)
  end

  # Mutates randomly selected neurons -------------------------------
  defp mutate_neurons(network) do
    nnodes = :network.nnodes(network)
    size_f = :math.sqrt(map_size(nnodes))
    selected = :ltools.rand(Map.keys(nnodes), 1.0 / size_f)
    Enum.map(selected, &mutate_neuron/1)
  end

  # Reinitialises the bias ------------------------------------------
  defp reinitialise_bias(neuron) do
    :nnet.wnode(neuron, %{bias: :not_init})
  end

  # Randomly changes the activation ---------------------------------
  @functions [:direct, :sigmoid, :tanh, :elu]
  defp switch_activation(neuron) do
    func = :ltools.randnth(@functions)
    :nnet.wnode(neuron, %{activation: func})
  end

  ### =================================================================
  ###  Internal functions
  ### =================================================================

  # Maybe applies the function but always returns the arguments -----
  defp maybe(probability, arg, fun) do
    case :rand.uniform() do
      x when x < probability -> fun.(arg)
      _ -> :nothing
    end

    arg
  end

  # Returns a list of random combinations ---------------------------
  defp rcomb(list, x) do
    take? = fn -> :rand.uniform() < x end
    for n <- list, m <- list, take?.(), do: {n, m}
  end
end
