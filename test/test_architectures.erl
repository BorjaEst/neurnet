%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_architectures).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% Defined agent species
-export([]).


%%%===================================================================
%%% Defined Architectures
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: all() -> [ArchitectureName :: atom()] 
%%--------------------------------------------------------------------
all() -> [simple, complex].


%%--------------------------------------------------------------------
%% @doc Simple architecture, only 1 initial layer and sequential.
%% @end
%%--------------------------------------------------------------------
-spec simple() -> Architecture :: neurnet:architecture().
simple() ->
    _Architecture = #{
        initial_dimensions => [2],
        model_type         => sequential
    }.

%%--------------------------------------------------------------------
%% @doc Complex architecture, 2 initial layer and allows recurrent 
%% links.
%% @end
%%--------------------------------------------------------------------
-spec complex() -> Architecture :: neurnet:architecture().
complex() ->
    _Architecture = #{
        initial_dimensions => [3, 3],
        model_type         => recurrent
    }.













% ....................................................................
% TODO: Define specs and comments
simple_edit_weights() ->
    _Mutation_algorithm = #{
        edit_random_weights => 0.01, % %_OfHit on each neuron
        edit_random_bias    => 0.01  % %_OfHit on each bias
    }.

% ....................................................................
% TODO: Define specs and comments
create_and_remove_links() ->
    _Mutation_algorithm = #{
        allow_recurrent_links => true, % Recurrent links available
        create_random_link    => 0.01, % %OfPossible possible links
        remove_random_link     => 0.01  % %OfPossible active links
    }.

% ....................................................................
% TODO: Define specs and comments
activation_and_aggregation() ->
    _Mutation_algorithm = #{
        available_af           => [{sigmoid, 1}, {tanh, 2}],    % [{function_name, function_weight}]
        change_to_random_af    => 0.1,                          % %_OfHit on each neuron
        available_aggrf        => [{dotprod, 1}],               % [{function_name, function_weight}]
        change_to_random_aggrf => 0.01                          % %_OfHit on each neuron
    }.

% ....................................................................
% TODO: Define specs and comments
change_architecture() ->
    _Mutation_algorithm = #{
        available_af    => [{sigmoid, 1}, {tanh, 2}], % [{function_name, function_weight}]
        available_aggrf => [{dotprod, 1}],            % [{function_name, function_weight}]
        create_neurons  => 0.05,                      % %_ofMaxNetwork extension
        remove_neurons  => 0.05                       % %_ofMaxNetwork reduction
    }.

% ....................................................................
% TODO: Define specs and comments
complex_morphology() ->
    _Mutation_algorithm = #{
        initial_layers          => [?dense(3), ?dense(3)],
        model_type              => recurrent,                   % Recurrent links available
        available_af            => [{sigmoid, 2}, {tanh, 1}],   % [{function_name, function_weight}]
        available_aggrf         => [{dotprod, 1}],              % [{function_name, function_weight}]
        remove_neurons          => 0.05,                        % %_ofMaxNetwork reduction
        create_neurons          => 0.10,                        % %_ofMaxNetwork extension
        edit_random_link        => 0.15,                        % %OfPossible possible links
        change_to_random_af     => 0.05,                        % %_OfHit on each neuron
        change_to_random_aggrf  => 0.05,                        % %_OfHit on each neuron
        edit_random_bias        => 0.10                         % %_OfHit on each bias
    }.




