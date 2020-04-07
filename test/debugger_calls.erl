%%%-------------------------------------------------------------------
%%% @author Borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. oct 2018 11:40
%%%-------------------------------------------------------------------
-module(debugger_calls).
-author("Borja").

%% API
-export([neurnet_SUITE/0, algorithm_SUITE/0]).

-define(LOG_DIR, "./apps/neurnet/_build/test/logs").
-define(STEP_OPTS, []).
-define(DEFAULT_OPTIONS, [{logdir, ?LOG_DIR}, {step, ?STEP_OPTS}]).

%% TESTS CALLS
neurnet_SUITE() ->
    ct:run_test([{suite, neurnet_SUITE} | ?DEFAULT_OPTIONS]).

algorithm_SUITE() ->
    ct:run_test([{suite, algorithm_SUITE} | ?DEFAULT_OPTIONS]).

