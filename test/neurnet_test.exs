defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "Load test_genotypes genotypes" do
    {:atomic, gx} = Neurnet.load([TestGenotypes])
    assert MapSet.new(gx) == MapSet.new([:dummy_gate, :complex_gate])
  end

  test "Phenotype from genotype" do
    id = Neurnet.phenotype_from(:dummy_gate)
    penotype = Database.dirty_read!(id)

    assert true
  end
end
