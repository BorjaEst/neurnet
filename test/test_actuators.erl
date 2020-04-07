%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 11:56
%%%-------------------------------------------------------------------
-module(test_actuators).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("neurnet.hrl").

%% Defined agent species
-export([]).

-define(T_CYC, 200).

-ifdef(debug_mode).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% Defined Actuators
%%%===================================================================

% ....................................................................
% TODO: Define specs and comments
sum_act(Signal, #{values := Val, eAcc := ErrAcc} = Properties) ->
    Optimal = lists:sum(Val),
    Error = ErrAcc + abs(Optimal - Signal),
    case ?cycle(Properties) < ?T_CYC of
        true -> {Optimal, Properties#{values := [], eAcc := Error}};
        false -> end_act(Error, Properties)
    end;
sum_act(Signal, #{values := _} = Properties) ->
    sum_act(Signal, Properties#{eAcc => 0.0}).

% ....................................................................
% TODO: Define specs and comments
xor_act(Signal, #{xor_1 := [I1], xor_2 := [I2], eAcc := ErrAcc} = Properties) ->
    Optimal = num_xor(I1, I2),
    Error = ErrAcc + abs(Optimal - Signal),
    case ?cycle(Properties) < ?T_CYC of
        true -> {Optimal, Properties#{xor_1 := [], xor_2 := [], eAcc := 0.0}};
        false -> end_act(Error, Properties)
    end;
xor_act(Signal, #{xor_1 := [I1 | Rest1], xor_2 := [I2 | Rest2], eAcc := ErrAcc} = Properties) ->
    Optimal = num_xor(I1, I2),
    {Optimal, Properties#{
        xor_1 := Rest1,
        xor_2 := Rest2,
        eAcc => ErrAcc + abs(Optimal - Signal)}
    };
xor_act(Signal, #{xor_1 := _, xor_2 := _} = Properties) ->
    xor_act(Signal, Properties#{eAcc => 0.0}).

num_xor(-1, -1) -> -1;
num_xor(1, -1)  -> 1;
num_xor(-1, 1)  -> 1;
num_xor(1, 1)   -> -1.

% ....................................................................
end_act(Error, Properties) ->
    Score = 1 / (math:sqrt(Error) + 0.00001),
    ?add_nscore(Properties, Score),
    io:format("*******Agent ~p score ~p ~n", [?agent_id(Properties), Score]),
    ?end_agent(Properties).