%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% A sensor must return one of the following values:
%%% @spec actuator_name(State) ->
%%%                     {  ok, Signal, NextState} |
%%%                     {stop, Reason, NextState} 
%%% @end
%%%-------------------------------------------------------------------
-module(test_sensors).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% Defined agent species
-export([]).

-define(XOR_TEST_SEQ1, [-1, +1, -1, +1]).
-define(XOR_TEST_SEQ2, [-1, -1, +1, +1]).


%%%===================================================================
%%% Defined Sensors
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples with the record name and attributes
%% list. This is mainly used to prepare the tables in mnesia.
%% @end
%%-------------------------------------------------------------------
seq_1() -> [logical_gate].

-spec seq_1(State :: term()) -> Result when
    Result :: {  ok, Signal :: number(), NewState :: term()} |
              {stop, Reason :: string(), NewState :: term()}.
seq_1(#{seq_1:=[Signal|Sx]} = State) -> 
    In = [Signal|maps:get(in, State, [])],
    {ok, Signal, State#{in=>In,seq_1:=Sx}};
seq_1(#{seq_1:=[]} = State) -> 
    {stop, "end of training", State};
seq_1(#{} = State) -> 
    seq_1(State#{seq_1=>?XOR_TEST_SEQ1}).


%%--------------------------------------------------------------------
%% @doc Returns a list of tuples with the record name and attributes
%% list. This is mainly used to prepare the tables in mnesia.
%% @end
%%------------------------------------------------------------------
seq_2() -> [logical_gate].

-spec seq_2(State :: term()) -> Result when
    Result :: {  ok, Signal :: number(), NewState :: term()} |
              {stop, Reason :: string(), NewState :: term()}.
seq_2(#{seq_2:=[Signal|Sx]} = State) -> 
    In = [Signal|maps:get(in, State, [])],
    {ok, Signal, State#{in=>In,seq_2:=Sx}};
seq_2(#{seq_2:=[]} = State) -> 
    {stop, "end of training", State};
seq_2(#{} = State) -> 
    seq_2(State#{seq_2=>?XOR_TEST_SEQ2}).


%%====================================================================
%% Internal functions
%%====================================================================

