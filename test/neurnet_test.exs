defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

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
