defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "Database write and read" do
    id = Database.id(:test)
    data = %{:id => id, :info => :someinfo}

    assert {:atomic, data} ==
             Database.run(fn ->
               Database.write(data)
               Database.read(id)
             end)
  end

  test "Neurnet fields" do
    assert Enum.sort(Actuator.fields()) ==
             Enum.sort([:id, :function])

    assert Enum.sort(Group.fields()) ==
             Enum.sort([:id, :members])
  end

  test "Load test_actuators" do
    assert Enum.sort(Actuator.load([TestActuators])) ==
             Enum.sort([:xor_score, :null])
  end
end
