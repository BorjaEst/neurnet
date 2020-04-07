%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc Actuator record definition to be stored in mnesia.
%%% @end
%%%-------------------------------------------------------------------

-define(NEW_ACTUATOR_ID(), {nnref:new(), actuator}).

-type actuator_id() :: {Unique_Id :: reference(), actuator}.
-record(actuator, {
    id = ?NEW_ACTUATOR_ID() :: actuator_id(),
    name :: atom() | string(),
    function :: {Module :: module(), Name :: atom()}
}).
-type actuator() :: #actuator{}.

