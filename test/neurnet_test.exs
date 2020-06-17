defmodule NeurnetTest do
  use ExUnit.Case, async: true
  doctest Neurnet

  setup %{} do
    {:atomic, gx} = Neurnet.load([TestGenotypes])
    %{genotypes: gx}
  end

  test "Phenotype from genotype" do
    id = Neurnet.phenotype_from(:dummy_gate)
    phenotype = Database.dirty_read!(id)

    assert [] == phenotype.actuators -- [:xor_score, :null]
    assert [] == phenotype.sensors -- [:seq_1, :seq_2]
    {:atomic, info} = :enn.info(phenotype.network)
    assert 2 == length(info.inputs)
    assert 1 == length(info.outputs)
    assert 3 == map_size(info.nnodes)
  end

  test "Phenotype mutation" do
    phenotype_1 = Neurnet.phenotype_from(:dummy_gate)
    phenotype_2 = Neurnet.mutate(phenotype_1)
    phenotype_1 = Database.dirty_read!(phenotype_1)
    phenotype_2 = Database.dirty_read!(phenotype_2)

    assert phenotype_1 != phenotype_2
    assert [] == phenotype_2.actuators -- [:xor_score, :null]
    assert phenotype_1.sensors == phenotype_2.sensors
    assert phenotype_1.network != phenotype_2.network
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
