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
  def mutate({:network, _} = network), do: mutate_network(network)

  @doc """
  Mutates the neuronal network. Should run inside mnesia transaction.
  """
  @spec mutate_network(:enn.network()) :: :enn.network()
  def mutate_network(network) do
    :nnet.clone(network)
    |> maybe_split_neuron
    |> maybe_join_neuron
    |> maybe_del_connections
    |> maybe_add_connections
    |> maybe_mutate_neurons
  end

  @chance 0.10
  defp maybe_split_neuron(network) do
    maybe(@chance, network, &split_neuron/1)
  end

  @chance 0.10
  defp maybe_join_neuron(network) do
    maybe(@chance, network, &join_neurons/1)
  end

  @chance 0.10
  defp maybe_del_connections(network) do
    maybe(@chance, network, &del_connections/1)
  end

  @chance 0.10
  defp maybe_add_connections(network) do
    maybe(@chance, network, &add_connections/1)
  end

  @chance 0.25
  defp maybe_mutate_neurons(network) do
    maybe(@chance, network, &mutate_neurons/1)
  end

  @doc """
  Mutates the neuron. Should run inside mnesia transaction.
  """
  @spec mutate_neuron(:enn.neuron()) :: :enn.neuron()
  def mutate_neuron(neuron) do
    neuron
    |> maybe_edit_bias
    |> maybe_switch_activation
  end

  @chance 0.35
  defp maybe_edit_bias(neuron) do
    maybe(@chance, neuron, &edit_bias/1)
  end

  @chance 0.15
  defp maybe_switch_activation(neuron) do
    maybe(@chance, neuron, &switch_activation/1)
  end

  ### =================================================================
  ###  Mutation transactions
  ### =================================================================

  # Splits a neuron ------------------------------------------------
  defp split_neuron(network) do
    nnodes = :network.nnodes(network)
    neuron = :ltools.randnth(Map.keys(nnodes))
    :nnet.split(neuron, network)
  end

  # Joins 2 neurons -------------------------------------------------
  defp join_neurons(network) do
    nnodes = :network.nnodes(network)
    neuron1 = :ltools.randnth(Map.keys(nnodes))
    neuron2 = :ltools.randnth(Map.keys(nnodes))
    :nnet.join({neuron1, neuron2}, network)
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

  # Edits the bias value --------------------------------------------
  @reinitialise 0.25
  @saturate_up 0.50
  @saturate_down 0.75
  @set_to_0 1.00
  defp edit_bias(neuron) do
    # Note max glorot value is sqrt(3) = 1.7320508075688772
    case :rand.uniform() do
      x when x < @reinitialise -> :nnet.wnode(neuron, %{bias: :not_init})
      x when x < @saturate_up -> :nnet.wnode(neuron, %{bias: +1.7320508075688772})
      x when x < @saturate_down -> :nnet.wnode(neuron, %{bias: -1.7320508075688772})
      x when x < @set_to_0 -> :nnet.wnode(neuron, %{bias: 0.0})
    end
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
