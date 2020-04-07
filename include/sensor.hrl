%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc Sensor record definition to be stored in mnesia
%%% @end
%%%-------------------------------------------------------------------

-define(NEW_SENSOR_ID(), {nnref:new(), sensor}).

-type sensor_id() :: {Unique_Id :: reference(), sensor}.
-record(sensor, {
    id = ?NEW_SENSOR_ID() :: sensor_id(),
    name :: atom() | string(),
    function :: {Module :: module(), Name :: atom()}
}).
-type sensor() :: #sensor{}.

