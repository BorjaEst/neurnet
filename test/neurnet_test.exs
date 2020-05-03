defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "Database write and read" do
    data = %{:info => :someinfo}

    assert {:atomic, data} ==
             Database.run(fn ->
               id = Database.write(:test, data)
               Database.read(id)
             end)
  end

  test "Neurnet fields" do
    assert Enum.sort(Actuator.fields()) ==
             Enum.sort([:id, :function])

    assert Enum.sort(Group.fields()) ==
             Enum.sort([:id, :members])
  end

  test "Load test_actuators groups" do
    {:atomic, _} = Database.run(fn -> Group.load(TestActuators) end)

    {:atomic, g1} = Database.run(fn -> Database.read(:group, :gate_score) end)
    assert g1.members == MapSet.new([:xor_score])

    {:atomic, g2} = Database.run(fn -> Database.read(:group, :gate_or_null) end)
    assert g2.members == MapSet.new([:xor_score, :null])
  end

  test "Load test_actuators actuators" do
    {:atomic, _} = Database.run(fn -> Actuator.load(TestActuators) end)

    {:atomic, a1} = Database.run(fn -> Database.read(:actuator, :xor_score) end)
    assert a1.function == (&TestActuators.xor_score/2)
    assert is_function(a1.function)

    {:atomic, a2} = Database.run(fn -> Database.read(:actuator, :null) end)
    assert a2.function == (&TestActuators.null/2)
    assert is_function(a2.function)
  end

  test "Load test_sensors groups" do
    {:atomic, _} = Database.run(fn -> Group.load(TestSensors) end)

    {:atomic, g1} = Database.run(fn -> Database.read(:group, :bool_input1) end)
    assert g1.members == MapSet.new([:seq_1])

    {:atomic, g2} = Database.run(fn -> Database.read(:group, :bool_input2) end)
    assert g2.members == MapSet.new([:seq_2])
  end

  test "Load test_sensors sensors" do
    {:atomic, _} = Database.run(fn -> Sensor.load(TestSensors) end)

    {:atomic, s1} = Database.run(fn -> Database.read(:sensor, :seq_1) end)
    assert s1.function == (&TestSensors.seq_1/1)
    assert is_function(s1.function)

    {:atomic, s2} = Database.run(fn -> Database.read(:sensor, :seq_2) end)
    assert s2.function == (&TestSensors.seq_2/1)
    assert is_function(s2.function)
  end
end
