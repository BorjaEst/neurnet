%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2018 20:46
%%%-------------------------------------------------------------------
-module(nn_agent).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("neurnet.hrl").
-include_lib("elements.hrl").

-behaviour(gen_server). %TODO: Develop as gen_agent

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2]).

% hidden functions (for compatibility, do not modify)
-export([start_link/3]).
-export([handle_call/3, handle_cast/2, code_change/3]).

-record(state, {
	id :: agent_id(),
	sensors :: [function()],
	actuators :: [function()],
	properties = #{} :: #{},
	errors :: [float()],
	birth = erlang:monotonic_time(microsecond)
}).

-ifdef(debug_mode).
-define(STDLIFE_TIMEAGE, infinity).
-else.
-define(STDLIFE_TIMEAGE, 100).  % In milliseconds
-endif.

%%%===================================================================
%%% agent callbacks as gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the agent
%% Definition of NN-Parameters:
%%      - #{population_id}, Population where the agent belongs(useful to score)
%%      - #{[@ScapeModule]}, Pid of the scape "ScapeModule"
%%      - #{agent_id}, Agent Identification (useful to score)
%%      - #{cortex_id}, NN cortex identification
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(agent_init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
agent_init([Properties]) ->
	{ok, Cortex_PId} = enn:start_nn(?cortex_id(Properties)),
	erlang:monitor(process, Cortex_PId),
	agent_init2([Properties#{cortex_pid := Cortex_PId}]).

agent_init2([Properties]) ->
	?LOG_INFO({"nn_agent start: ", ?agent_id(Properties), ?cortex_id(Properties)}),
	process_flag(trap_exit, true), % Mandatory to catch supervisor exits
	timer:send_after(?STDLIFE_TIMEAGE, {'EXIT', self(), time_end}), %% Die after the standard life time
	cast_sensors(?sensors(Properties), Properties),
	{ok, #state{
		id         = ?agent_id(Properties),
		sensors    = ?sensors(Properties),
		actuators  = ?actuators(Properties),
		properties = Properties
	}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages sent to the agent
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_info({sensors_reply, ExternalInputs, NewProperties}, State) ->
	?LOG_INFO("Agent_Id ~p  sensors reply: ~p ", [State#state.id, ExternalInputs]),
	Outputs = cortex:predict(?cortex_pid(NewProperties), ExternalInputs),
	cast_actuators(State#state.actuators, NewProperties, Outputs),
	{noreply, State#state{
		properties = NewProperties
	}};

handle_info({actuators_reply, OptimalOutputs, NewProperties}, State) ->
	?LOG_INFO("Agent_Id ~p  actuators reply: ~p ", [State#state.id, OptimalOutputs]),
	Errors = cortex:fit(?cortex_pid(NewProperties), OptimalOutputs),
	cast_sensors(State#state.sensors, NewProperties#{cycle := ?cycle(NewProperties) + 1}),
	{noreply, State#state{
		errors     = Errors,
		properties = NewProperties
	}};

handle_info({'EXIT', _PId, Reason}, State) ->
	case Reason of
		normal -> {noreply, State}; %% spawn_link(nn_agent, cast_sensors/actuators, Arg) normal ending
		end_agent -> enn:stop_nn(?cortex_id(State#state.properties)), {stop, normal, State};
		time_end -> enn:stop_nn(?cortex_id(State#state.properties)), {stop, normal, State};
		_Other -> {stop, Reason, State}
	end;

handle_info({'DOWN', _DownRef, process, DownPId, _Info}, #state{properties = #{cortex_pid := DownPId}} = State) ->
%%	?LOG({"RIP Cortex: ", DownPId, Info}),
	{stop, normal, State};

handle_info(Info, State) ->
	?LOG_WARNING("Unknown handle_info Agent_Id ~p, info ~p", [State#state.id, Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The neural network is stopped before the agent shutdown
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
%%	?LOG({"nn_agent end: ", Reason, State#state.id, ?cortex_id(State#state.properties)}),
%%	Age = erlang:monotonic_time(microsecond) - State#state.birth,
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% ......................................................................................................................
cast_sensors(Sensors, Properties) ->
	spawn_link(nn_agent, cast_sensors, [self(), Sensors, Properties]).

cast_sensors(PId, Sensors, Properties) ->
	{Signals, NewProperties} = exec_sensors(Sensors, Properties, []),
	PId ! {sensors_reply, Signals, NewProperties}.

exec_sensors([#sensor{function = {M, Sensor}} | SRest], Properties, SignalsAcc) ->
	{Signal, NewProperties} = M:Sensor(Properties),
	exec_sensors(SRest, NewProperties, [Signal | SignalsAcc]);
exec_sensors([] = _Sensors, Properties, SignalsAcc) ->
	{lists:reverse(SignalsAcc), Properties}.

% ......................................................................................................................
cast_actuators(Actuators, Properties, Outputs) ->
	spawn_link(nn_agent, cast_actuators, [self(), Actuators, Properties, Outputs]).

cast_actuators(PId, Actuators, Properties, Outputs) ->
	{OptimalSignal, NewProperties} = exec_actuators(Actuators, Properties, Outputs, []),
	PId ! {actuators_reply, OptimalSignal, NewProperties}.

exec_actuators([#actuator{function = {M, Actuator}} | ARest], Properties, [Output | ORest], OptOAcc) ->
	{OptimalOutput, NewProperties} = M:Actuator(Output, Properties),
	exec_actuators(ARest, NewProperties, ORest, [OptimalOutput | OptOAcc]);
exec_actuators([] = _Actuators, Properties, [] = _Outputs, OptOAcc) ->
	{lists:reverse(OptOAcc), Properties}.


%%%===================================================================
%%% Static functions (Do not modify)
%%%===================================================================
% This is done like this to save time on implementing a specific behaviour "Agent"
start_link(Agent_Id, Population_Id, Properties) ->
	Updt_Properties = Properties#{
		population_id => Population_Id,
		agent_id      => Agent_Id
	},
	gen_server:start_link(?MODULE, [Updt_Properties], []).

init([Properties]) -> agent_init([Properties#{agent_pid => self()}]).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

