%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%% 
%%% An actuator must return one of the following values:
%%% @spec actuator_name(Signal, State) ->
%%%                     {  ok,                NextState} |
%%%                     {  ok,  Error, Score, NextState} |
%%%                     {stop, Reason,        NextState} |
%%%                     {stop, Reason, Score, NextState} |
%%% @end
%%%-------------------------------------------------------------------
-module(test_actuators).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% Defined agent species
-export([]).


%%%===================================================================
%%% Defined Actuators
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: all() -> [ActuatorCall :: atom()]
%%-------------------------------------------------------------------
all() -> [xor_score].


%%--------------------------------------------------------------------
%% @doc Scores and agent according to the xor error. Backpropagation.
%% @end
%%--------------------------------------------------------------------
xor_score() -> [logical_gate].

-spec xor_score(Signal :: float(), State :: term()) -> 
    {ok, Score :: float(), Error :: float(), NewState :: term()}.
xor_score(Signal, #{bool_in:=[I1,I2]} = State) -> 
    Error = Signal - num_xor(I1, I2),
    {ok, Error, score(Error), State#{bool_in:=[]}}.


%%====================================================================
%% Internal functions
%%====================================================================

% ....................................................................
num_xor(-1, -1) -> -1;
num_xor(+1, -1) -> +1;
num_xor(-1, +1) -> +1;
num_xor(+1, +1) -> -1.

% ....................................................................
score(Error) -> 
    2000 - 1000*abs(Error).

