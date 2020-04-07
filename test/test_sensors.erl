%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_sensors).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("neurnet.hrl").

%% Defined agent species
-export([]).

-define(VAL_TEST_SIGNAL, 0.2).
-define(XOR_TEST_SEQ1, [-1, 1, -1, 1]).
-define(XOR_TEST_SEQ2, [-1, -1, 1, 1]).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined Sensors
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
val_sns(#{values := Values} = Properties) ->
	Signal = ?VAL_TEST_SIGNAL,
	{Signal, Properties#{
		values := [Signal | Values]
	}};
val_sns(#{} = Properties) ->
	val_sns(Properties#{
		values => []
	}).

% ....................................................................
% TODO: Define specs and comments
xor_sns1(#{xor_1 := [Signal | _]} = Properties) ->
	{Signal, Properties};
xor_sns1(#{} = Properties) ->
	Inputs = [Signal | _] = ?XOR_TEST_SEQ1,
	{Signal, Properties#{
		xor_1 => Inputs
	}}.

xor_sns2(#{xor_2 := [Signal | _]} = Properties) ->
	{Signal, Properties};
xor_sns2(#{} = Properties) ->
	Inputs = [Signal | _] = ?XOR_TEST_SEQ2,
	{Signal, Properties#{
		xor_2 => Inputs
	}}.


