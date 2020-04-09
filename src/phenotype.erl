%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(phenotype).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

%% API
-export([]).
-export_type([id/0, phenotype/0]).

-type id() :: {Reference :: reference(), phenotype}.
-define(PHENOTYPE_ID, {make_ref(), phenotype}).

-record(phenotype, {
    id           :: id(), 
    network      :: enn:id(),
    actuators    :: [actuator:id()],
    sensors      :: [  sensor:id()]
}).
-type phenotype() :: #phenotype{}.


-define(   CORTEX(State), maps:get(   cortex, State)).
-define(ACTUATORS(State), maps:get(actuators, State)).
-define(  SENSORS(State), maps:get(  sensors, State)).


%%%===================================================================
%%% API 
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: phenotype) -> ListOfFields :: [atom()].
fields(phenotype) -> record_info(fields, phenotype).

%%--------------------------------------------------------------------
%% @doc Returns the id of a new phenotype.
%% @end
%%--------------------------------------------------------------------
-spec new_from(Genotype :: genotype:id()) -> id().
new_from(Id) -> 
    Genotype     = edb:read(Id),
    Architecture = genotype:architecture(Genotype),
    AGroups      = genotype:actuator_groups(Genotype),
    SGroups      = genotype:sensor_groups(Genotype),
    Phenotype = #phenotype{
        id = ?PHENOTYPE_ID,
        network   = architecture:new_network(edb:read(Architecture)),
        actuators = [actuator:rand(G) || G <- edb:read(AGroups)],
        sensors   = [  sensor:rand(G) || G <- edb:read(SGroups)] 
    },
    edb:write(Phenotype),
    Phenotype#phenotype.id.


%%%===================================================================
%%% Phenotype callbacks 
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Returns the id of a new phenotype.
%% @end
%%-------------------------------------------------------------------
controller(Phenotype_Id) -> 
    Phenotype = edb:read(Phenotype_Id),
    enn:start(Phenotype#phenotype.network),
    State = #{
        cortex    => enn:cortex_pid(Phenotype#phenotype.network),
        actuators => edb:read(Phenotype#phenotype.actuators),
        sensors   => edb:read(Phenotype#phenotype.sensors)
    },
    enter_sensors(State).

%%-------------------------------------------------------------------
%% @doc Enter loops (change of state).
%% @end
%%-------------------------------------------------------------------
enter_sensors(State) -> 
    run_sensors([], ?SENSORS(State), State).

enter_predict(Signals, State) -> 
    Predictions = cortex:predict(?CORTEX(State), Signals),
    enter_actuators(Predictions, State).

enter_actuators(Predictions, State) -> 
    run_actuators([], Predictions, ?ACTUATORS(State), State).

enter_fit(Errors, State) -> 
    _BP_Errors = cortex:fit(?CORTEX(State), Errors),
    enter_sensors(State).

%%-------------------------------------------------------------------
%% @doc Run sensors.
%% @end
%%-------------------------------------------------------------------
run_sensors(Signals, [Sensor|Sx], State) ->
    case Sensor(State) of 
        {  ok, Signal, NextState} -> 
            run_sensors([Signal | Signals], Sx, NextState);
        {stop, Reason, NextState} -> 
            terminate(Reason, NextState)
    end;
run_sensors(Signals, [], State) -> 
    enter_predict(lists:reverse(Signals), State).

%%-------------------------------------------------------------------
%% @doc Run actuators.
%% @end
%%-------------------------------------------------------------------
run_actuators(Errors, [Prediction|Px], [Actuator|Ax], State) ->
    case Actuator(Prediction, State) of 
         {  ok,                NextState} -> 
            run_actuators([  0.0|Errors], Px, Ax, NextState);
         {  ok,  Error, Score, NextState} -> 
            eevo:score(self(), Score),
            run_actuators([Error|Errors], Px, Ax, NextState);
         {stop, Reason,        NextState} -> 
            terminate(Reason, NextState);
         {stop, Reason, Score, NextState} -> 
            eevo:score(self(), Score),
            terminate(Reason, NextState)
    end;
run_actuators(Errors, [], [], State) -> 
    enter_fit(lists:reverse(Errors), State).

%%-------------------------------------------------------------------
%% @doc Terminate.
%% @end
%%-------------------------------------------------------------------
terminate(Reason, _State) ->
    exit(Reason).


%%%===================================================================
%%% Internal functions
%%%===================================================================

