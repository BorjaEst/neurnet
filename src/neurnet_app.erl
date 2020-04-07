%%%-------------------------------------------------------------------
%% @doc neurnet public API
%% @end
%%%-------------------------------------------------------------------
-module(neurnet_app).
-author("borja").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
    Tables = neurnet:attributes_table(),
    edb:create_tables(Tables),
    edb:start(Tables),
    neurnet_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

