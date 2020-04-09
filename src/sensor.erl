%%%-------------------------------------------------------------------
%%% @author Borja Esteban
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor).

%% API
-export([fields/1, load/1]).
-export_type([id/0, group_id/0, sensor/0, group/0]).

-define(SENSOR_ID(Name), {Name,       sensor}).
-define(   GROUP_ID(Name), {Name, sensor_group}).

-type id() :: {Name :: atom(), Type :: sensor}.
-record(sensor, {
    id       :: id(),
    function :: {Module :: module(), Name :: atom()}
}).
-type sensor() :: #sensor{}.

-type group_id() :: {Name :: atom(), sensor_group}.
-record(group, {
    id        :: id(),
    sensors :: {Module :: module(), Name :: atom()}
}).
-type group() :: #group{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: sensor | group) -> ListOfFields :: [atom()].
fields(sensor) -> record_info(fields, sensor);
fields(group)  -> record_info(fields, group).

%%--------------------------------------------------------------------
%% @doc Loads the sensors from the indicated modules. 
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

add_groups([{Name, Sensors} | Groups], Module, Sets) -> 
    Group = #group{
        id        = ?GROUP_ID(Name), 
        sensors = [?SENSOR_ID(N) || N <-Sensors]
    },
    sets:add_element(Group, 
            add_groups(Groups, Module, 
                add_sensors(Sensors, Module, Sets)));
add_groups([], _, Sets) -> 
    Sets.

add_sensors([Name | Sensors], Module, Sets) -> 
    Sensor = #sensor{
        id       = ?SENSOR_ID(Name),
        function = {Module, Name}
    },
    add_sensors(Sensors, Module,
        sets:add_element(Sensor, Sets));
add_sensors([], _, Sets) -> 
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

