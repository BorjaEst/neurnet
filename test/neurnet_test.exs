defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "Load test_genotypes genotypes" do
    {:atomic, gx} = Neurnet.load([TestGenotypes])
    assert MapSet.new(gx) == MapSet.new([:dummy_gate, :complex_gate])
  end

  test "Phenotype from genotype" do
    id = Neurnet.phenotype_from(:dummy_gate)
    phenotype = Database.dirty_read!(id)

    assert [] == phenotype.actuators -- [:xor_score, :null]
    assert [] == phenotype.sensors -- [:seq_1, :seq_2]
    assert %{type: :sequential, size: 5} == :enn.info(phenotype.network)
  end
end
