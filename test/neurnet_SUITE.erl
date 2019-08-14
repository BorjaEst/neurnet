%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(neurnet_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("neurnet.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error report: ~p", [Error])).

-define(PARALLEL_TRAININGS, 4).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
	[{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
	application:start(neurnet),
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	application:stop(neurnet),
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
	[
		{test_for_multiple_trainings, [parallel],  % TODO: Make this tests
		 [addition_static_inputs,
		  addition_random_inputs |
		  [random_dense_random_inputs || _ <- lists:seq(1, ?PARALLEL_TRAININGS - 1)]]}
	].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
	[
		go_sum,
		go_xor
%%		{group, test_for_multiple_trainings}
	].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
	[].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
	ok.

% --------------------------------------------------------------------
% TESTS --------------------------------------------------------------

% ......................................................................................................................
go_sum() ->
	[].
go_sum(_Config) ->
	{Score, Best_Agent_Id} = neurnet:go(
		_Morphology = test_morphologies:sum_mimic(),
		_InitialLayerDensities = [?dense(2, #{activation => tanh})],
		_MaxTime = infinity,        % Optional
		_MaxAttempts = infinity,    % Optional
		_FitnessTarget = 99.9       % Optional
	),
	print_results(Score, Best_Agent_Id).

% ......................................................................................................................
go_xor() ->
	[].
go_xor(_Config) ->
	{Score, Best_Agent_Id} = neurnet:go(
		_Morphology = test_morphologies:xor_mimic(),
		_InitialLayerDensities = [?dense(2, #{activation => tanh})],
		_MaxTime = infinity,        % Optional
		_MaxAttempts = infinity,    % Optional
		_FitnessTarget = 99.9       % Optional
	),
	print_results(Score, Best_Agent_Id).

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------


print_results(Score, Best_Agent_Id) ->
	Agent = nndb:read(Best_Agent_Id),
	Cortex = nndb:read(?cortex_id(Agent#agent.properties)),
	ct:log(?LOW_IMPORTANCE, "Best agent score: ~p", [Score]),
	ct:log(?LOW_IMPORTANCE, "Best agent body: ~p", [Agent]),
	ct:log(?LOW_IMPORTANCE, "Best agent cortex: ~p", [Cortex]),
	[print_layout(Layer, Neurons_Ids) || {Layer, Neurons_Ids} <- maps:to_list(Cortex#cortex.layers)].

print_layout(Layer, Neurons_Ids) ->
	ct:log(?LOW_IMPORTANCE, "Best agent layer ~p: ~p", [Layer, [nndb:read(Neuron_Id) || Neuron_Id <- Neurons_Ids]]).

