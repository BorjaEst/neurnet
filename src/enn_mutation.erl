%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(enn_mutation).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
-export([]).
-export_type([]).

-define(HALF(N), floor(N/2.0)).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Splits a neuron into 2 sharing the inputs.
%% @end
%%-------------------------------------------------------------------
-spec divide_neuron(Network, Id) -> ok when 
    Network :: enn:id(),
    Id :: neuron:id().
divide_neuron(Network, Id) -> 
    [NN0, N1] = edb:read([Network, Id])
    N2 = neuron:clone(N1),
    edb:write([N2]),
    InLn  = network:in_links(NN0, Id),
    OutLn = network:out_links(NN0, Id),
    add_neuron(Network, neuron:id(N2)),
    Map = #{Id => neuron:id(N2)},
    move_links(Network, random_half(InLn), Map) % Move in links
    duplicate_links(Network, OutLn, Map). % Duplicates out links
    
%%-------------------------------------------------------------------
%% @doc Merges all inputs and outputs from neuron 1 into neuron 2, 
%% the bias of neuron 1 is also merged.
%% @end
%%-------------------------------------------------------------------
-spec merge_neurons(Network, Id1, Id2) -> ok when 
    Network :: enn:id(),
    Id1 :: neuron:id(),
    Id2 :: neuron:id().
merge_neurons(Network, Id1, Id2) -> 
    NN0 = edb:read(Network),
    InLn  = network:in_links(NN0, Id1),
    OutLn = network:out_links(NN0, Id1),
    merge_bias(Id1, Id2), % Merges bias of neuron 1 in neuron 2
    Map = #{Id1 => Id2},
    move_links(Network, InLn ++ OutLn, Map),  % Move in links
    delete_neuron(Network, Id1).


%%====================================================================
%% Internal functions
%%====================================================================

% Adds a neuron in the network --------------------------------------
add_neuron(Network, Id) -> 
    NN0 = edb:read(Network),
    NN1 = network:add_neuron(NN0, Id),
    edb:write(NN1).

% Deletes a neuron from the network ---------------------------------
delete_neuron(Network, Id) -> 
    NN0 = edb:read(Network),
    NN1 = network:delete_neuron(NN0, Id),
    edb:delete(Id),
    edb:write(NN1).

% Move links from N1 to N2 using a map ------------------------------
move_links(Network, Links, Map) -> 
    [NN0 | Old] = edb:read([Network | Links]),
    New = [link:clone(L, Map) || L <- Old],
    NN1 = network:add_links(NN0, [link:id(L) || L <- New]),
    NN2 = network:del_links(NN1, Links),
    edb:delete(Links),
    edb:write([NN2 | New]).

% Duplicate links of N1 into N2 using a map -------------------------
duplicate_links(Network, Links, Map) -> 
    [NN0 | Old] = edb:read([Network | Links]),
    Ln1 = [link:edit(L,link:weight(L)/2.0) || L <- Old],
    Ln2 = [link:clone(L,#{N1=>N2})         || L <- Ln1],
    NN1 = network:add_links(NN0, [link:id(L) || L <- Ln2]),
    edb:write([NN2 | Ln1 ++ Ln2]).


% Merges links of N1 into N2 using a map ----------------------------
merge_links(Network, Links, Map) -> 
    [NN0 | Old] = edb:read([Network | Links]),
    Ln1 = [link:edit(L,link:weight(L)/2.0) || L <- Old],
    Ln2 = [link:clone(L,#{N1=>N2})         || L <- Ln1],
    NN1 = network:add_links(NN0, [link:id(L) || L <- Ln2]),
    edb:write([NN2 | Ln1 ++ Ln2]).




% Adds to the bias of neuron 2 the bias of neuron 1 -----------------
merge_bias(Id1, Id2) -> 
    [N1, N2] = edb:read(Id1, Id2),
    BiasN1 = neuron:bias(N1),
    BiasN2 = neuron:bias(N2),
    NewN2 = neuron:bias(N2, BiasN1 + BiasN2),
    edb:write(NewN2).

% Returns a randomly half of elements -------------------------------
random_half(List) -> 
    sublist(ltools:suffle(List), floor(length(List)/2.0)).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

