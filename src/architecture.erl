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
mutate(_Network_id) -> 
    error(undefined). % TODO


%%%===================================================================
%%% Network transformations 
%%%===================================================================


%%%===================================================================
%%% Links transformations 
%%%===================================================================


%%%===================================================================
%%% Neuron transformations 
%%%===================================================================


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

