defmodule NeurnetTest do
  use ExUnit.Case
  doctest Neurnet

  test "greets the world" do
    assert Neurnet.hello() == :world
  end
end
