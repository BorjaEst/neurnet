%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_algorithms).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("neurnet.hrl").

%% Defined agent species
-export([]).

-ifdef(debug_mode).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(LOG(X), true).
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined Actuators
%%%===================================================================

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
complex_architecture() ->
    _Mutation_algorithm = #{
        allow_recurrent_links   => true,                        % Recurrent links available
        available_af            => [{sigmoid, 2}, {tanh, 1}],    % [{function_name, function_weight}]
        available_aggrf         => [{dotprod, 1}],              % [{function_name, function_weight}]
        remove_neurons          => 0.05,                        % %_ofMaxNetwork reduction
        create_neurons          => 0.10,                        % %_ofMaxNetwork extension
        edit_random_link        => 0.15,                        % %OfPossible possible links
        change_to_random_af     => 0.05,                        % %_OfHit on each neuron
        change_to_random_aggrf  => 0.05,                        % %_OfHit on each neuron
        edit_random_bias        => 0.10                         % %_OfHit on each bias
    }.
