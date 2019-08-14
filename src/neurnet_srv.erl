%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2018 19:05
%%%-------------------------------------------------------------------
-module(neurnet_srv).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("neurnet.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
%%-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PRINT(Message), io:format("~n ------------------~n log ~p ~n", [Message])).

-record(state, {
	real_world_id :: population_id(),
	real_world_governor :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% % TODO: To make description
%%
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).

%%--------------------------------------------------------------------
%% @doc
%% Simple print test
%%
%% @end
%%--------------------------------------------------------------------
-spec(print(Message :: term()) ->
	ok).
print(Message) ->
	gen_server:cast(?SERVER, {print, Message}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	RealWorld_Id = eevo:create_population(?REAL_WORLD_POPULATION_SPECS),
	{ok, Governor} = eevo:start_population(RealWorld_Id),
	register(?REAL_WORLD_GOVERNOR, Governor),
	?PRINT(?WELCOME_MESSAGE),
	{ok, #state{
		real_world_id       = RealWorld_Id,
		real_world_governor = Governor
	}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
	                 {reply, Reply :: term(), NewState :: #state{}} |
	                 {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #state{}} |
	                 {noreply, NewState :: #state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	                 {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_cast({print, Message}, State) ->
	?PRINT(Message),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
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
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
