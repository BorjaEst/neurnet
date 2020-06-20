defmodule DatabaseTest do
  use ExUnit.Case, async: true

  test "Database write and read" do
    data = %{:info => :someinfo}
    {:atomic, id} = Database.run(fn -> Database.new(:test, data) end)

    assert {:atomic, data} == Database.run(fn -> Database.read!(id) end)
    assert data == Database.dirty_read!(id)
  end

  test "Load test_actuators groups" do
    {:atomic, _groups} = Database.run(fn -> Group.load(TestActuators) end)

    g1 = Database.dirty_read!({:group, :gate_score})
    assert Enum.sort(g1.members) == Enum.sort([:xor_score])
    g2 = Database.dirty_read!({:group, :gate_or_null})
    assert Enum.sort(g2.members) == Enum.sort([:xor_score, :null])
  end

  test "Load test_actuators actuators" do
    {:atomic, _} = Database.run(fn -> Actuator.load(TestActuators) end)

    a1 = Database.dirty_read!({:actuator, :xor_score})
    assert a1.function == (&TestActuators.xor_score/2)
    assert is_function(a1.function)
    a2 = Database.dirty_read!({:actuator, :null})
    assert a2.function == (&TestActuators.null/2)
    assert is_function(a2.function)
  end

  test "Load test_sensors groups" do
    {:atomic, _} = Database.run(fn -> Group.load(TestSensors) end)

    g1 = Database.dirty_read!({:group, :bool_input1})
    assert Enum.sort(g1.members) == Enum.sort([:seq_1])
    g2 = Database.dirty_read!({:group, :bool_input2})
    assert Enum.sort(g2.members) == Enum.sort([:seq_2])
  end

  test "Load test_sensors sensors" do
    {:atomic, _} = Database.run(fn -> Sensor.load(TestSensors) end)

    s1 = Database.dirty_read!({:sensor, :seq_1})
    assert s1.function == (&TestSensors.seq_1/1)
    assert is_function(s1.function)
    s2 = Database.dirty_read!({:sensor, :seq_2})
    assert s2.function == (&TestSensors.seq_2/1)
    assert is_function(s2.function)
  end

  test "Load test_genotypes genotypes" do
    {:atomic, _} = Database.run(fn -> Genotype.load(TestGenotypes) end)

    g1 = Database.dirty_read!({:genotype, :dummy_gate})
    assert g1.actuators == [:gate_or_null]
    assert g1.sensors == [:bool_input1, :bool_input2]

    assert g1.model == %{
             inputs: %{
               units: 2,
               connections: %{outputs: :sequential},
               data: %{
                 activation: :direct,
                 aggregation: :direct,
                 bias: 0.0,
                 initializer: :ones
               }
             },
             outputs: %{
               units: 1,
               connections: %{},
               data: %{
                 activation: :direct,
                 aggregation: :dot_prod,
                 bias: 0.0,
                 initializer: :zeros
               }
             }
           }

    g2 = Database.dirty_read!({:genotype, :complex_gate})
    assert g2.actuators == [:gate_score]
    assert g2.sensors == [:bool_input1, :bool_input2]

    assert g2.model == %{
             hidden1: %{
               units: 3,
               connections: %{hidden1: :sequential},
               data: %{
                 activation: :direct,
                 aggregation: :dot_prod,
                 bias: :not_init,
                 initializer: :glorot
               }
             },
             hidden2: %{
               units: 3,
               connections: %{outputs: :sequential},
               data: %{
                 activation: :direct,
                 aggregation: :dot_prod,
                 bias: :not_init,
                 initializer: :glorot
               }
             },
             inputs: %{
               units: 2,
               connections: %{hidden1: :sequential},
               data: %{
                 activation: :direct,
                 aggregation: :direct,
                 bias: 0.0,
                 initializer: :ones
               }
             },
             outputs: %{
               units: 1,
               connections: %{},
               data: %{
                 activation: :direct,
                 aggregation: :dot_prod,
                 bias: 0.0,
                 initializer: :zeros
               }
             }
           }
  end
end
