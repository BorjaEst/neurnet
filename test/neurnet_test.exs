defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "Neurnet fields" do
    assert Enum.sort(Actuator.fields()) ==
             Enum.sort([:id, :function, :evolution])

    assert Enum.sort(Sensor.fields()) ==
             Enum.sort([:id, :function, :evolution])

    assert Enum.sort(Group.fields()) ==
             Enum.sort([:id, :members])

    assert Enum.sort(Architecture.fields()) ==
             Enum.sort([:id, :dim, :type])

    assert Enum.sort(Genotype.fields()) ==
             Enum.sort([:id, :architecture, :actuators, :sensors])

    assert Enum.sort(Phenotype.fields()) ==
             Enum.sort([:id, :network, :actuators, :sensors])
  end

  test "Phenotype from genotype" do
    id = Neurnet.phenotype_from(:dummy_gate)
    phenotype = Database.dirty_read!(id)

    assert [] == phenotype.actuators -- [:xor_score, :null]
    assert [] == phenotype.sensors -- [:seq_1, :seq_2]
    assert %{type: :sequential, size: 5} == :enn.info(phenotype.network)
  end

  test "Phenotype mutation" do
    phenotype_1 = Neurnet.phenotype_from(:dummy_gate)
    phenotype_2 = Neurnet.mutate(phenotype_1)
    phenotype_1 = Database.dirty_read!(phenotype_1)
    phenotype_2 = Database.dirty_read!(phenotype_2)

    assert [] == phenotype_2.actuators -- [:xor_score, :null]
    assert phenotype_1.sensors == phenotype_2.sensors
    assert phenotype_1.network != phenotype_2.network
    assert :sequential == Map.get(:enn.info(phenotype_2.network), :type)
  end

  test "Run training of dummy gate" do
    stop_condition = fn %{runtime: x} -> x > 2000 end
    result = Neurnet.run(:test_dummy, :dummy_gate, 3, stop_condition)

    assert result == %{}
  end
end
