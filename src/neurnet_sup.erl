%%%-------------------------------------------------------------------
%% @doc neurnet top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(neurnet_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SUP_FLAGS, #{strategy  => one_for_one,
          intensity => 10,
          period    => 36}).

-define(SPECS_EEVO_SUP(StartArgs), #{
    id       => eevo_sup,
    start    => {eevo_sup, start_link, [StartArgs]},
    restart  => permanent,
    type     => supervisor,
    modules  => [supervisor]}).
-define(SPECS_ENN_SUP(StartArgs), #{
    id       => enn_sup,
    start    => {enn_sup, start_link, [StartArgs]},
    restart  => permanent,
    type     => supervisor,
    modules  => [supervisor]}).

-record(childSpecs, {
    eevo_sup    = ?SPECS_EEVO_SUP(   []),
    enn_sup     = ?SPECS_ENN_SUP(    [])
}).


%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(StartArgs) ->
    do_init(StartArgs, #childSpecs{}).

do_init([{eevo_config, Config} | StartArgs], ChildSpecs) ->
    do_init(StartArgs, ChildSpecs#childSpecs{
        eevo_sup = ?SPECS_EEVO_SUP(Config)
    });
do_init([{enn_config, Config} | StartArgs], ChildSpecs) ->
    do_init(StartArgs, ChildSpecs#childSpecs{
        enn_sup = ?SPECS_ENN_SUP(Config)
    });
do_init([], ChildSpecs) ->
    {ok, {?SUP_FLAGS, [
        ChildSpecs#childSpecs.eevo_sup,
        ChildSpecs#childSpecs.enn_sup
    ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
