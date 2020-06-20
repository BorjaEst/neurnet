defmodule NeurnetTest do
  use ExUnit.Case, async: true
  doctest Neurnet

  setup %{} do
    {:atomic, gx} = Neurnet.load([TestGenotypes])
    %{genotypes: gx}
  end

  test "Phenotype from genotype" do
    phenotype = Neurnet.phenotype_from(:dummy_gate)
    ph_struct = Neurnet.info(phenotype)

    assert [] == ph_struct.actuators -- [:xor_score, :null]
    assert [] == ph_struct.sensors -- [:seq_1, :seq_2]
    assert :network == elem(ph_struct.network, 0)
  end

  test "Phenotype mutation" do
    phenotype = Neurnet.phenotype_from(:dummy_gate)
    ph1_struct = Neurnet.info(phenotype)
    ph2_struct = Phenotype.mutate(phenotype)

    assert ph1_struct != ph1_struct
    assert [] == ph1_struct.actuators -- [:xor_score, :null]
    assert ph1_struct.sensors == ph1_struct.sensors
    assert ph1_struct.network != ph1_struct.network
  end

  # test "Run training of dummy gate" do
  #   stop_condition = fn %{runtime: x} -> x > 1000 end
  #   result = Neurnet.run(:test_dummy, :dummy_gate, 4, stop_condition)

  #   assert result.population.run_data.runtime >= 1000
  #   IO.inspect(result.population.run_data)
  #   assert is_map(result.tree)
  #   assert map_size(result.tree) > 0
  # end
end
