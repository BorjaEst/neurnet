defmodule DatabaseTest do
  use ExUnit.Case, async: true

  test "Database write and read" do
    data = %{:info => :someinfo}

    assert {:atomic, data} ==
             Database.run(fn -> Database.write(data, :test) |> Database.read!() end)
  end

  test "Load test_actuators groups" do
    {:atomic, _} = Database.run(fn -> Group.load(TestActuators) end)

    {:atomic, g1} = Database.run(fn -> Database.read!(:gate_score, :group) end)
    assert g1.members == MapSet.new([:xor_score])

    {:atomic, g2} = Database.run(fn -> Database.read!(:gate_or_null, :group) end)
    assert g2.members == MapSet.new([:xor_score, :null])
  end

  test "Load test_actuators actuators" do
    {:atomic, _} = Database.run(fn -> Actuator.load(TestActuators) end)

    {:atomic, a1} = Database.run(fn -> Database.read!(:xor_score, :actuator) end)
    assert a1.function == (&TestActuators.xor_score/2)
    assert is_function(a1.function)

    {:atomic, a2} = Database.run(fn -> Database.read!(:null, :actuator) end)
    assert a2.function == (&TestActuators.null/2)
    assert is_function(a2.function)
  end

  test "Load test_sensors groups" do
    {:atomic, _} = Database.run(fn -> Group.load(TestSensors) end)

    {:atomic, g1} = Database.run(fn -> Database.read!(:bool_input1, :group) end)
    assert g1.members == MapSet.new([:seq_1])

    {:atomic, g2} = Database.run(fn -> Database.read!(:bool_input2, :group) end)
    assert g2.members == MapSet.new([:seq_2])
  end

  test "Load test_sensors sensors" do
    {:atomic, _} = Database.run(fn -> Sensor.load(TestSensors) end)

    {:atomic, s1} = Database.run(fn -> Database.read!(:seq_1, :sensor) end)
    assert s1.function == (&TestSensors.seq_1/1)
    assert is_function(s1.function)

    {:atomic, s2} = Database.run(fn -> Database.read!(:seq_2, :sensor) end)
    assert s2.function == (&TestSensors.seq_2/1)
    assert is_function(s2.function)
  end

  test "Load test_genotypes genotypes" do
    {:atomic, _} = Database.run(fn -> Genotype.load(TestGenotypes) end)

    {:atomic, g1} = Database.run(fn -> Database.read!(:dummy_gate, :genotype) end)
    assert g1.actuators == [:gate_or_null]
    assert g1.sensors == [:bool_input1, :bool_input2]

    assert g1.model == %{
             inputs: %{
               connections: %{outputs: :sequential},
               data: %{
                 activation: :direct,
                 aggregation: :direct,
                 bias: 0.0,
                 initializer: :ones
               },
               units: 2
             },
             outputs: %{
               connections: %{},
               data: %{
                 activation: :direct,
                 aggregation: :dot_prod,
                 bias: 0.0,
                 initializer: :glorot
               },
               units: 1
             }
           }

    {:atomic, g2} = Database.run(fn -> Database.read!(:complex_gate, :genotype) end)
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
                 initializer: :glorot
               }
             }
           }
  end
end
