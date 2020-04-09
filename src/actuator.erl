%%%-------------------------------------------------------------------
%%% @author Borja Esteban
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(actuator).

%% API
-export([load/1]).
-export_type([id/0, actuator/0, group/0]).

-type id() :: {Name :: atom(), Type :: actuator | group}.
-define(ACTUATOR_ID(Name), {Name,       actuator}).
-define(   GROUP_ID(Name), {Name, actuator_group}).

-record(actuator, {
    id       :: id(),
    function :: {Module :: module(), Name :: atom()}
}).
-type actuator() :: #actuator{}.

-record(group, {
    id        :: id(),
    actuators :: {Module :: module(), Name :: atom()}
}).
-type group() :: #group{}.



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads the actuators from the indicated modules. 
%% @end
%%-------------------------------------------------------------------
-spec load(Modules :: [Module :: module()]) -> ok.
load(Modules) -> 
    Sets = set_modules(Modules, sets:new()), 
    edb:write(sets:to_list(Sets)),
    ok.


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

