%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(architecture).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
-export([fields/1, load/1]).
-export_type([id/0, architecture/0]).

-type id() :: {Name :: atom(), architecture}.
-define(ARCHITECTURE_ID(Name), {Name, architecture}).

-record(architecture, {
    id   :: id(),
    dim  :: [LayerSize :: integer()],
    type :: model:nature()
}).
-type architecture() :: #architecture{}.

-define(DEF_DIM,  [3]).
-define(DEF_TYPE, recurrent).
-define(MAYBE(Probability), Probability > rand:uniform() andalso).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%-------------------------------------------------------------------
-spec fields(Atom :: architecture) -> ListOfFields :: [atom()].
fields(architecture) -> record_info(fields, architecture).

%%-------------------------------------------------------------------
%% @doc Loads the architectures from the indicated modules. 
%% @end
%%-------------------------------------------------------------------
-spec load(Modules :: [Module :: module()]) -> ok.
load(Modules) -> 
    Sets = set_modules(Modules, sets:new()), 
    edb:write(sets:to_list(Sets)),
    ok. 

%%-------------------------------------------------------------------
%% @doc Generates a network id from a defined architecture. 
%% @end
%%-------------------------------------------------------------------
-spec new_network(Architecture :: architecture()) -> 
    Network_id :: enn:id().
new_network(A) -> 
    Nature = A#architecture.type,
    enn:compile(
        model:Nature([layer:dense(N) || N <- A#architecture.dim])
    ).

%%-------------------------------------------------------------------
%% @doc Mutates a network returning the new mutation id. 
%% @end
%%-------------------------------------------------------------------
-spec mutate(Network_id :: enn:id()) -> 
    NewNetwork_id :: enn:id().
mutate(Network_id) -> 
    Clone_Id = enn:clone(Network_id),
    mutate_network(Clone_Id),
    Clone_Id.


%%%===================================================================
%%% Network transformations 
%%%===================================================================

% Definition of probabilities
-define(NETWORK_EXTENSION, 0.10).
-define(NETWORK_REDUCTION, 0.05).
-define(SIZE_CONSTANT,     3.00).

%%--------------------------------------------------------------------
%% @doc Applies the mutations to the network.
%% @end
%%--------------------------------------------------------------------
% TODO: Define spec
mutate_network(Network_id) -> 
    Network       = edb:read(Network_id),
    #{size:=Size} = enn:info(Network),
    Rate = math:sqrt(Size),
    ?MAYBE(?NETWORK_EXTENSION) extend_network(Network, Rate),
    ?MAYBE(?NETWORK_REDUCTION) reduce_netkork(Network, Rate),
    mutate_links(Network, Rate).

% --------------------------------------------------------------------
extend_network(Network, ExtRate) -> 
    network:neurons


    transform:extend_network(elements:id(Network), ExtRate).



% --------------------------------------------------------------------
reduce_netkork(Network, RedRate) -> 
    transform:reduce_netkork(elements:id(Network), RedRate).

mutate_links(Network, Rate) -> 
    Prob    = ?SIZE_CONSTANT/Rate,
    Neurons = ebd:read(ltools:rand(elements:neurons(Network), Prob)),
    [mutate_links(Neuron, Network) || Neuron <- Neurons].


%%%===================================================================
%%% Links transformations 
%%%===================================================================

% Definition of probabilities
-define(LINKS_EXTENSION, 0.10).
-define(LINKS_REDUCTION, 0.05).


%%--------------------------------------------------------------------
%% @doc Applies the mutations to the neuron links.
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
















%%%===================================================================
%%% Neuron transformations 
%%%===================================================================

% Definition of probabilities
-define(LINKS_EXTENSION,      0.10).
-define(LINKS_REDUCTION,       0.05).
-define(ACTIVATION_CHANGE,    0.15).
-define(AGGREGATION_CHANGE,   0.15).
-define(BIAS_SWITCH,          0.10).
-define(FANIN_CONSTANT,       0.50). 

%%--------------------------------------------------------------------
%% @doc Applies the mutations to the neuron.
%% @end
%%--------------------------------------------------------------------
% TODO: Define specs
mutate_neuron(Neuron) -> 
    Rate =  math:sqrt(length(elements:inputs_idps(Neuron))),
    ?MAYBE(   ?LINKS_EXTENSION) links_extension(Neuron, Rate),
    ?MAYBE(   ?LINKS_REDUCTION) links_reduction(Neuron, Rate),
    % ?MAYBE( ?ACTIVATION_CHANGE) activation_change(Neuron),
    % ?MAYBE(?AGGREGATION_CHANGE) aggregation_change(Neuron),
    ?MAYBE(       ?BIAS_SWITCH) switch_bias(Neuron),
    mutate_weights(Neuron).

links_extension(Neuron, ExtRate) -> 
    transform:extend_connections(elements:id(Neuron), ExtRate).

links_reduction(Neuron, RedRate) -> 
    transform:reduce_connections(elements:id(Neuron), RedRate).

switch_bias(Neuron) -> 
    Id = elements:id(Neuron),
    transform:reset_bias(Id).
    % case elements:bias(Neuron) of 
    %     disabled -> transform:disable_bias(Id);
    %     _        -> transform:reset_bias(Id)
    % end.

mutate_weights(Neuron) -> 
    Inputs      = elemens:inputs_ids(Neuron),
    Probability = ?FANIN_CONSTANT/math:sqrt(length(Inputs)),
    Selected_From = ltools:rand(Inputs, Probability),
    transform:reset_links(Selected_From, elements:id(Neuron)).


%%====================================================================
%% Internal functions
%%====================================================================

% --------------------------------------------------------------------
set_modules([Module | Modules], Sets) ->
    set_modules(Modules,
        add_architectures(Module:all(), Module, Sets));
set_modules([], Sets) -> 
    Sets.

add_architectures([Name | Architectures], Module, Sets) -> 
    Definition   = Module:Name(),
    Architecture = #architecture{
        id   = ?ARCHITECTURE_ID(Name), 
        dim  = maps:get(initial_dimensions, Definition,  ?DEF_DIM),
        type = maps:get(        model_type, Definition, ?DEF_TYPE)
    },
    add_architectures(Architectures, Module, 
        sets:add_element(Architecture, Sets));
add_architectures([], _, Sets) -> 
    Sets.





% --------------------------------------------------------------------














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

