%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-include_lib("eevo/include/society.hrl").
-include_lib("enn/include/nnelements.hrl").
-include_lib("enn/include/layers.hrl").

-define(cortex_id(Properties), maps:get(cortex_id, Properties)).
-define(cortex_pid(Properties), maps:get(cortex_pid, Properties)).
-define(sensors(Properties), maps:get(sensors, Properties)).
-define(actuators(Properties), maps:get(actuators, Properties)).
-define(cycle(Properties), maps:get(cycle, Properties)).
-define(size(Properties), maps:get(size, Properties)).
-define(algorithm(Properties), maps:get(algorithm, Properties)).

-define(add_nscore(Properties, Score), ?add_score(Properties, Score / math:log10(10 + ?size(Properties)))).
-define(set_nscore(Properties, Score), ?set_score(Properties, Score / math:log10(10 + ?size(Properties)))).

-define(WELCOME_MESSAGE, "Welcome to neurnet application, instructions to be defined soon.....").
-define(REAL_WORLD_NAME, real_world).
-define(REAL_WORLD_ID, ?POPULATION_ID(?REAL_WORLD_NAME)).
-define(REAL_WORLD_GOVERNOR, rw_gov).
-define(REAL_WORLD_POPULATION_SPECS,
    [
        {name, ?REAL_WORLD_NAME},
        {limit, 10},
        {minimum, 0},
        {evo_alg_f, fun evolutionary_algorithm:null_evolution/2}
    ]).

-define(DEFAULT_RECURRENT_ALLOWANCE, false).
-define(DEFAULT_AVAILABLE_AF,
    [
        {direct, 1},
        {sigmoid, 1},
        {tanh, 1},
        {softplus, 1},
        {softsign, 1},
        {elu, 1},
        {selu, 1},
        {relu, 1}
    ]).
-define(DEFAULT_AVAILABLE_AGGRF,
    [
        {dotprod, 1},
        {diffprod, 1}
    ]).
-define(DEFAULT_ALGORITHM,
    #{
        allow_recurrent_links   => ?DEFAULT_RECURRENT_ALLOWANCE,    % Recurrent links available
        available_af            => ?DEFAULT_AVAILABLE_AF,           % [{function_name, function_weight}]
        available_aggrf         => ?DEFAULT_AVAILABLE_AGGRF,        % [{function_name, function_weight}]
        remove_neurons          => 0.00,                            % %_ofMaxNetwork reduction
        create_neurons          => 0.00,                            % %_ofMaxNetwork extension
        edit_random_link        => 0.00,                            % %OfPossible possible links
        change_to_random_af     => 0.00,                            % %_OfHit on each neuron
        change_to_random_aggrf  => 0.00,                            % %_OfHit on each neuron
        edit_random_bias        => 0.00                             % %_OfHit on each bias
    }).
-define(ALGORITHM_APPLY_ORDER,  % Order is important to minimise the probability of "Broken" networks
    [
        remove_neurons,
        create_neurons,
        edit_random_link,
        change_to_random_af,
        change_to_random_aggrf,
        edit_random_bias
    ]).
-define(nn_properties(Sensors, Actuators, Cortex_Id),
    #{
        cortex_id  => Cortex_Id,
        cortex_pid => undefined,
        sensors    => Sensors,
        actuators  => Actuators,
        cycle      => 0,
        size       => nn_elements:size(edb:read(Cortex_Id)),
        algorithm  => ?DEFAULT_ALGORITHM
    }).
