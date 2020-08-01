defmodule NeurnetTest do
  use ExUnit.Case, async: true
  require Logger
  doctest Neurnet

  setup %{} do
    {:atomic, gx} = Neurnet.load([TestGenotypes])
    %{genotypes: gx}
  end

  test "Phenotype from genotype" do
    phenotype = Neurnet.phenotype_from(:dummy_gate)
    ph_info = Neurnet.info(phenotype)

    assert [] == ph_info.actuators -- [:xor_score, :null]
    assert [] == ph_info.sensors -- [:seq_1, :seq_2]
    # assert ph_info.network.nnodes
  end

  test "Phenotype mutation" do
    phenotype_1 = Neurnet.phenotype_from(:dummy_gate)
    ph1_info = Neurnet.info(phenotype_1)
    phenotype_2 = :eevo.mutate(phenotype_1)
    ph2_info = Neurnet.info(phenotype_2)

    assert ph1_info != ph2_info
    assert [] == ph2_info.actuators -- [:xor_score, :null]
    assert ph1_info.sensors == ph2_info.sensors
    assert ph1_info.network != ph2_info.network
  end

  test "Run a dummy gate" do
    phenotype = Neurnet.phenotype_from(:dummy_gate)
    {:next, function, [state]} = Neurnet.run_as(phenotype)

    assert is_function(function)
    assert is_map(state)
  end

  test "Run a simple gate" do
    phenotype = Neurnet.phenotype_from(:simple_gate)
    {:next, function, [state]} = Neurnet.run_as(phenotype)

    assert is_function(function)
    assert is_map(state)
  end

  test "Run a complex gate" do
    phenotype = Neurnet.phenotype_from(:complex_gate)
    {:next, function, [state]} = Neurnet.run_as(phenotype)

    assert is_function(function)
    assert is_map(state)
  end

  test "Run a broken gate" do
    phenotype = Neurnet.phenotype_from(:broken_gate)
    assert_raise(ErlangError, fn -> Neurnet.run_as(phenotype) end)
  end

  test "Run training of dummy gate" do
    stop_condition = fn %{generation: x} -> x > 25 end
    result = Neurnet.run(:test_dummy, :dummy_gate, 4, stop_condition)
    [champion | _] = result.top3

    assert result.population.run_data.generation >= 25
    IO.inspect(what: "Training results", results: result.population.run_data)
    IO.inspect(what: "Champio info", info: Neurnet.info(champion))
    assert is_map(result.tree)
    assert map_size(result.tree) > 0
  end

  test "Training of simple gate" do
    stop_condition = fn %{generation: x} -> x > 25 end
    result = Neurnet.run(:test_simple, :simple_gate, 4, stop_condition)
    [champion | _] = result.top3

    assert result.population.run_data.generation >= 25
    IO.inspect(what: "Training results", results: result.population.run_data)
    IO.inspect(what: "Champio info", info: Neurnet.info(champion))
    assert is_map(result.tree)
    assert map_size(result.tree) > 0
  end

  test "Training of complex gate" do
    stop_condition = fn %{generation: x} -> x > 25 end
    result = Neurnet.run(:test_complex, :complex_gate, 4, stop_condition)
    [champion | _] = result.top3

    assert result.population.run_data.generation >= 25
    IO.inspect(what: "Training results", results: result.population.run_data)
    IO.inspect(what: "Champio info", info: Neurnet.info(champion))
    assert is_map(result.tree)
    assert map_size(result.tree) > 0
  end
end
