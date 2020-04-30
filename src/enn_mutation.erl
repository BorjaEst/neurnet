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

% Adds to the bias of neuron 2 the bias of neuron 1 -----------------
merge_bias(Id1, Id2) -> 
    [N1, N2] = edb:read(Id1, Id2),
    BiasN1 = neuron:bias(N1),
    BiasN2 = neuron:bias(N2),
    NewN2 = neuron:bias(N2, BiasN1 + BiasN2),
    edb:write(NewN2).


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

