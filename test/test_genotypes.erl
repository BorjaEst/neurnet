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
%%% Modules to preload 
%%%===================================================================

architectures() -> [test_architectures].
actuators()     -> [test_actuators].
sensors()       -> [test_sensors].
genotypes()     -> [dummy_gate, complex_gate].

%%%===================================================================
%%% Defined genotypes 
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
dummy_gate() ->
    _Genotype = #{   
        architecture => simple,
        actuators    => [gate_or_null],
        sensors      => [bool_input1, bool_input2]
    }.

% ....................................................................
% TODO: Define specs and comments
complex_gate() ->
    _Genotype = #{   
        architecture => complex,
        actuators    => [gate_score],
        sensors      => [bool_input1, bool_input2]
    }.
