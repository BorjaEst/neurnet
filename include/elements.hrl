%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(NEW_SENSOR_ID(), {nnref:new(), sensor}).
-define(NEW_ACTUATOR_ID(), {nnref:new(), actuator}).

%%% --------------------------------------------------------------------------------------------------------------------
%TODO: Correct specs
-type sensor_id() :: {Unique_Id :: reference(), sensor}.
-record(sensor, {
    id = ?NEW_SENSOR_ID() :: sensor_id(),
    name :: atom() | string(),
    function :: {Module :: module(), Name :: atom()}
}).

%%% --------------------------------------------------------------------------------------------------------------------
%TODO: Correct specs
-type actuator_id() :: {Unique_Id :: reference(), actuator}.
-record(actuator, {
    id = ?NEW_ACTUATOR_ID() :: actuator_id(),
    name :: atom() | string(),
    function :: {Module :: module(), Name :: atom()}
}).


