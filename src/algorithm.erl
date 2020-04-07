%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2018 20:14
%%%-------------------------------------------------------------------
-module(algorithm).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include("neurnet.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).

-ifdef(debug_mode).
-define(LOG(X), io:format("{~p,~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-define(LOG_S(X), io:format("{~p,~p,~p}: " ++ X ++ "~n", [self(), ?MODULE, ?LINE])).
-define(STDCALL_TIMEOUT, infinity).
-else.
-define(LOG(X), true).
-define(LOG_S(X), true).
-define(STDCALL_TIMEOUT, 5000).
-endif.

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Creates a set of random links between random neurons
%% - Probability: Probability each link has to be edited/activated
%% - Recurrence: When false, recurrent links are discarded
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
edit_random_link(Cortex_Id, Probability, #{allow_recurrent_links := RecAllowed}) ->
    Neurons = nn_elements:neurons(nndb:read(Cortex_Id)),
    Selected_Links = randomSelect([{From_Id, To_Id} || From_Id <- Neurons, To_Id <- Neurons], Probability),
    if
        RecAllowed -> [edit_random_link(From_Id, To_Id) || {From_Id, To_Id} <- Selected_Links];
        true -> [edit_random_link(From_Id, To_Id) || {{L1, _} = From_Id, {L2, _} = To_Id} <- Selected_Links, L1 < L2]
    end,
    ok.

edit_random_link(From_Id, To_Id) ->
    try
        mutation:create_link(From_Id, To_Id)
    catch
        error:{link_fail, _} -> mutation:edit_link(From_Id, To_Id, rand:normal(0, ?DELTA_MULTIPLIER/10.0))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Randomly changes the bias of randomly selected neurons
%% - Probability: Probability a each neuron has to change its bias
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
edit_random_bias(Cortex_Id, Probability, #{}) ->
    Neurons = nn_elements:neurons(nndb:read(Cortex_Id)),
    Selected_Neurons = randomSelect(Neurons, Probability),
    [mutation:edit_bias(Id, randomWeightValue()) || Id <- Selected_Neurons],
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Changes the activation function of randomly selected neurons
%% - Probability: Probability that each neuron has to change its af
%% - Functions_Weights: List of target functions and prob. weight
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_to_random_af(Cortex_Id, Probability, #{available_af := Functions_Weights}) ->
    Neurons = nn_elements:neurons(nndb:read(Cortex_Id)),
    Selected_Neurons = randomSelect(Neurons, Probability),
    [mutation:change_af(Id, weightSelection(Functions_Weights)) || Id <- Selected_Neurons],
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Changes the aggregation function of randomly selected neurons
%% - Probability: Probability that each neuron has to change its aggrf
%% - Functions_Weights: List of target functions and prob. weight
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
change_to_random_aggrf(Cortex_Id, Probability, #{available_aggrf := Functions_Weights}) ->
    Cortex = nndb:read(Cortex_Id),
    Neurons = nn_elements:neurons(Cortex) -- nn_elements:outputs_ids(Cortex),
    Selected_Neurons = randomSelect(Neurons, Probability),
    [mutation:change_aggrf(Id, weightSelection(Functions_Weights)) || Id <- Selected_Neurons],
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Add neurons into random layers
%% - MaxNet_Extension: Maximum % the network can increase
%% - Activation_FW: List of target af and probability weights
%% - Aggregation_FW: List of target aggrf and probability weights
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
create_neurons(Cortex_Id, MaxNet_Extension, #{available_af := Activation_FW, available_aggrf := Aggregation_FW}) ->
    Neurons = nn_elements:neurons(nndb:read(Cortex_Id)),
    ExtensionFactor = length(Neurons) * MaxNet_Extension,
    Integer_Neurons = floor(ExtensionFactor),
    Dec = ExtensionFactor - Integer_Neurons,
    case rand:uniform() of
        Prob when Prob > Dec -> N_NewNeurons = Integer_Neurons;
        Prob when Prob =< Dec -> N_NewNeurons = Integer_Neurons + 1
    end,
    [mutation:insert_neuron((rand:uniform(198) - 99) / 100,
                            weightSelection(Activation_FW),
                            weightSelection(Aggregation_FW),
                            Cortex_Id)
     || _ <- lists:seq(1, N_NewNeurons)].

%%--------------------------------------------------------------------
%% @doc
%% Remove randomly selected neurons
%% - MaxNet_Reductions: Maximum % the network can reduce
%%
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
remove_neurons(Cortex_Id, Die_Probability, #{}) ->
    Neurons = nn_elements:neurons(nndb:read(Cortex_Id), hidden),
    Selected_Neurons = randomSelect(Neurons, Die_Probability),
    [mutation:remove_neuron(Neuron_Id, Cortex_Id) || Neuron_Id <- Selected_Neurons],
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

% ....................................................................
randomElement(List) -> lists:nth(rand:uniform(length(List)), List).

% ....................................................................
randomWeightValue() -> ?DELTA_MULTIPLIER * (rand:uniform() - 0.5).

% ....................................................................
randomSelect(List, Prob) -> [X || X <- List, rand:uniform() < Prob].

% ....................................................................
randomShuffling(List) -> [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

% ....................................................................
randomFirsts(List, N) -> element(1, lists:split(N, randomShuffling(List))).

% ....................................................................
weightSelection(FunctionWeight_List) ->
    List = lists:append([lists:duplicate(W, Function) || {Function, W} <- FunctionWeight_List]),
    randomElement(List).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
this_example_test_() ->
    % {setup, Where, Setup, Cleanup, Tests | Instantiator}
    [
        {"Test example",
         {setup, local, fun no_setup/0, fun no_cleanup/1, fun test_example/1}}
    ].

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
no_setup() ->
    ok.

no_cleanup(_) ->
    ok.

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------
test_example(_) ->
    [
        ?_assertEqual(true, true)
    ].


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------



    
    



