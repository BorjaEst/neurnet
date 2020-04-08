%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_genotypes).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% Defined agent species
-export([]).


%%%===================================================================
%%% Defined architectures 
%%%===================================================================


% ....................................................................
% TODO: Define specs and comments
dummy_gate() ->
    #{   
        architecture => test_architectures:simple_edit_weights(),
        actuators    => [gate_or_null],
        sensors      => [bool_input, bool_input]
    }.


% ....................................................................
% TODO: Define specs and comments
complex_gate() ->
    #{   
        architecture => test_architectures:complex(),
        actuators    => [gate_score],
        sensors      => [bool_input, bool_input]
    }.
