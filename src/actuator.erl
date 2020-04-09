%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(actuator).

%% API
-export([fields/1, load/1, run/3, mutate/1]).
-export_type([id/0, group_id/0, actuator/0, group/0]).

-define(ACTUATOR_ID(Name), {Name,       actuator}).
-define(   GROUP_ID(Name), {Name, actuator_group}).

-type id() :: {Name :: atom(), Type :: actuator}.
-record(actuator, {
    id       :: id(),
    function :: {Module :: module(), Name :: atom()}
}).
-type actuator() :: #actuator{}.

-type group_id() :: {Name :: atom(), actuator_group}.
-record(group, {
    id        :: group_id(),
    actuators :: [Actuator_id :: id()]
}).
-type group() :: #group{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: actuator | group) -> ListOfFields :: [atom()].
fields(actuator) -> record_info(fields, actuator);
fields(group)    -> record_info(fields, group).

%%--------------------------------------------------------------------
%% @doc Loads the actuators from the indicated modules. 
%% @end
%%-------------------------------------------------------------------
-spec load(Modules :: [Module :: module()]) -> ok.
load(Modules) -> 
    Sets = set_modules(Modules, sets:new()), 
    edb:write(sets:to_list(Sets)),
    ok.

%%--------------------------------------------------------------------
%% @doc Runs the specified actuator. 
%% @end
%%-------------------------------------------------------------------
-spec run(Actuator, Input, State) -> ActuatorResult when 
    Actuator :: actuator(),
    Input    :: float(),
    State    :: #{Field :: term() => Value :: term()},
    ActuatorResult :: {  ok,                NextState} |
                      {  ok,  Error, Score, NextState} |
                      {stop, Reason,        NextState} |
                      {stop, Reason, Score, NextState},
    Error     :: number(),
    Score     :: number(),
    Reason    :: term(),
    NextState :: #{Field :: term() => Value :: term()}.
run(Actuator, Input, State) -> 
    {Module, Name} = Actuator#actuator.function,
    Module:Name(Input, State).

%%--------------------------------------------------------------------
%% @doc Loads the actuators from the indicated modules. 
%% @end
%%-------------------------------------------------------------------
-spec mutate(Actuator :: actuator()) -> NewActuator_id :: id().
mutate(Actuator) -> 
    {Module, Name} = Actuator#actuator.function,
    Possible_new = Module:Name(),
    Actuator#actuator{
        function = {Module, ltools:randnth(Possible_new)}
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% --------------------------------------------------------------------
set_modules([Module | Modules], Sets) ->
    set_modules(Modules,
        add_groups(Module:groups(), Module, Sets));
set_modules([], Sets) -> 
    Sets.

add_groups([{Name, Actuators} | Groups], Module, Sets) -> 
    Group = #group{
        id        = ?GROUP_ID(Name), 
        actuators = [?ACTUATOR_ID(N) || N <-Actuators]
    },
    sets:add_element(Group, 
            add_groups(Groups, Module, 
                add_actuators(Actuators, Module, Sets)));
add_groups([], _, Sets) -> 
    Sets.

add_actuators([Name | Actuators], Module, Sets) -> 
    Actuator = #actuator{
        id       = ?ACTUATOR_ID(Name),
        function = {Module, Name}
    },
    add_actuators(Actuators, Module,
        sets:add_element(Actuator, Sets));
add_actuators([], _, Sets) -> 
    Sets.
    

%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

