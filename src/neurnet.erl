%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2018 18:43
%%%-------------------------------------------------------------------
-module(neurnet).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%%% TODO -------------------------------------------------------------
% TODO: Un agente debe de estar formado por varias redes neuronales de control y la superior
% TODO: Añadir metodo de ajuste de pesos mediante estimulos positivos/negativos
% TODO: Debe de haber una population id "Real" que siempre existe, si no se especifica el pop_id, el agente se refiere ahu

-include_lib("kernel/include/logger.hrl").
-include_lib("elements.hrl").
-include_lib("neurnet.hrl").

%% API
-export([]).

-define(MAX_TIME, infinity).
-define(MAX_ATTEMPTS, 20).
-define(FITNESS_TARGET, infinity).
-define(ARCHITECTURE(Layers), enn:sequential(Layers)).
-define(NEURNET_TABLES_ATTRIBUTES_LIST,
    [
        {sensor, record_info(fields, sensor)},
        {actuator, record_info(fields, actuator)}
    ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
start_tables() ->
    Tables = ?NEURNET_TABLES_ATTRIBUTES_LIST ++
             eevo:attributes_table() ++
             enn:attributes_table(),
    nndb:create_tables(Tables),
    nndb:start(Tables).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
seed(#{sensors := Sensors, actuators := Actuators} = _Morphology, InitialLayerDensities) ->
    Cortex_Id = enn:compile(?ARCHITECTURE(
        lists:append([
                         [?input(length(Sensors))],
                         InitialLayerDensities,
                         [?output(length(Actuators))]
                     ]))),
    eevo:create_agent(nn_agent,
                      ?nn_properties(Sensors, Actuators, Cortex_Id),
                      fun mutation_function/1
    ).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
start_agent(Agent_Id) ->
    {ok, _Agent_PId} = eevo:start_agent(?REAL_WORLD_ID, Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
stop_agent(Agent_Id) ->
    ok = eevo:stop_agent(?REAL_WORLD_ID, Agent_Id).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
go(Morphology, InitialLayerDensities) ->
    go(Morphology, InitialLayerDensities, ?MAX_TIME, ?MAX_ATTEMPTS, ?FITNESS_TARGET).
go(Morphology, InitialLayerDensities, MaxTime, MaxAttempts, FitnessTarget) ->
    Seed_Id = seed(Morphology, InitialLayerDensities),
    Population_Id = eevo:create_population(
        [
            {run_time, MaxTime},
            {run_agents, MaxAttempts},
            {run_score, FitnessTarget}
        ]),
    {ok, _Gov_PId} = eevo:start_population(Population_Id), %% TODO: Make Result = eevo:start_population(Id, Seeds),
    ok = eevo:add_agent(Population_Id, Seed_Id),
    Result = receive {run_end, Population_Id, R} -> R end,
    ?LOG_NOTICE("Training results: ~p", [Result]),
    {_Score, _Best_Agent_Id} = maps:get(best_score, Result).


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
change_sensors(Agent_Id, Sensors) ->
    Agent = nndb:read(Agent_Id),
    AgentProperties = Agent#agent.properties,
    nndb:write(Agent#agent{
        properties = AgentProperties#{sensors => Sensors}
    }).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
change_actuators(Agent_Id, Actuators) ->
    Agent = nndb:read(Agent_Id),
    AgentProperties = Agent#agent.properties,
    nndb:write(Agent#agent{
        properties = AgentProperties#{actuators => Actuators}
    }).


%%%===================================================================
%%% Internal functions
%%%===================================================================

mutation_function(Properties) ->
    Cortex_Id = enn:clone(?cortex_id(Properties)),
    apply_algorithms(?ALGORITHM_APPLY_ORDER, Cortex_Id, ?algorithm(Properties)),
    Properties#{
        cortex_id := Cortex_Id,
        size      := nn_elements:size(nndb:read(Cortex_Id))
    }.

apply_algorithms([FunKey | Functions], Cortex_Id, Algorithm) ->
    Value = maps:get(FunKey, Algorithm),
    try
        algorithm:FunKey(Cortex_Id, Value, Algorithm)
    catch
        Type:Exception -> ?LOG_NOTICE("Algorithm application ~p failed: {~p, ~p}", [FunKey, Type, Exception])
    end,
    apply_algorithms(Functions, Cortex_Id, Algorithm);
apply_algorithms([], _Cortex_Id, _Algorithm) ->
    ok.



