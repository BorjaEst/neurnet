%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2019 16:09
%%%-------------------------------------------------------------------
-module(test_morphologies).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("neurnet.hrl").
-include_lib("elements.hrl").

%% Defined agent species
-export([]).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined Sensors
%%%===================================================================

% ......................................................................................................................
% TODO: Define specs and comments
sum_mimic() ->
	#{
		sensors =>
		[
			#sensor{name = val_GetInput1, function = {test_sensors, val_sns}},
			#sensor{name = val_GetInput2, function = {test_sensors, val_sns}}
		],
		actuators =>
		[
			#actuator{name = sum_SetOutput, function = {test_actuators, sum_act}}
		]
	}.

% ......................................................................................................................
% TODO: Define specs and comments
xor_mimic() ->
	#{
		sensors =>
		[
			#sensor{name = xor_GetInput1, function = {test_sensors, xor_sns1}},
			#sensor{name = xor_GetInput2, function = {test_sensors, xor_sns2}}
		],
		actuators =>
		[
			#actuator{name = xor_SetOutput, function = {test_actuators, xor_act}}
		]
	}.



