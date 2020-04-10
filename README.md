# neurnet
Deep learning application based on Erlang and Python modules

# Important
If developing with parallel applications into release use _checkouts to solve dependencies

# Notes
[_] - Actuators and sensors can mutate (but following a path):
A1 -> [A2, A4];
A2 -> [A1, A3];
A3 -> [];
A4 -> [A2];
