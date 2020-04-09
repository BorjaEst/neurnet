%%%-------------------------------------------------------------------
%%% @author Borja Esteban
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(genotype).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

%% API
-export([]).
-export_type([id/0, genotype/0]).

-type id() :: {Name :: atom(), genotype}.
-define(GENOTYPE_ID(Name), {Name, genotype}).

-record(genotype, {
    id           :: id(), 
    architecture :: architecture:id(),
    actuators    :: [actuator:group_id()],
    sensors      :: [  sensor:group_id()]
}).
-type genotype() :: #genotype{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the record fields of an element.
%% @end
%%--------------------------------------------------------------------
-spec fields(Atom :: genotype) -> ListOfFields :: [atom()].
fields(genotype) -> record_info(fields, genotype).

%%--------------------------------------------------------------------
%% @doc Loads the genotypes from the indicated modules. 
%% @end
%%-------------------------------------------------------------------
-spec load(Modules :: [Module :: module()]) -> ok.
load(Modules) -> 
    Sets = set_modules(Modules, sets:new()), 
    edb:write(sets:to_list(Sets)),
    ok. 

%%--------------------------------------------------------------------
%% @doc Builds a phenotype from a genotype. 
%% @end
%%-------------------------------------------------------------------
-spec build(GenotypeName :: atom()) -> phenotype:id().
build(GenotypeName) -> 
    phenotype:new_from(?GENOTYPE_ID(GenotypeName)).


%%====================================================================
%% Internal functions
%%====================================================================

% --------------------------------------------------------------------
set_modules([Module | Modules], Sets) -> 
    ok = architecture:load(Module:architectures()),
    ok =     actuator:load(    Module:actuators()),
    ok =       sensor:load(      Module:sensors()),
    add_genotypes(Module:genotypes(), Module, 
        set_modules(Modules, Sets));
set_modules([], Sets) -> 
    Sets.

add_genotypes([Name | Genotypes], Module, Sets) -> 
    Definition   = Module:Name(),
    Genotype = #genotype{
        id = ?GENOTYPE_ID(Name),
        architecture = map(architecture, Definition),
        actuators    = map(   actuators, Definition),
        sensors      = map(     sensors, Definition)
    },
    add_genotypes(Genotypes, Module, 
        sets:add_element(Genotype, Sets));
add_genotypes([], _, Sets) -> 
    Sets.

% --------------------------------------------------------------------
map(Field, Definition) -> 
    Names = maps:get(Field, Definition),
    map2(Field, Names).

map2(architecture,  Name) -> architecture:id((Name));
map2(   actuators, Names) -> [actuator:id(Name) || Name <- Names];
map2(     sensors, Names) -> [  sensor:id(Name) || Name <- Names].









%%%===================================================================
%%% Internal functions
%%%===================================================================









%%%===================================================================
%%% API
%%%===================================================================

mutation(Properties) ->
    Cortex_Id = enn:clone(?cortex_id(Properties)),
    apply_algorithms(?ALGORITHM_APPLY_ORDER, Cortex_Id, ?algorithm(Properties)),
    Properties#{
        cortex_id := Cortex_Id,
        size      := nn_elements:size(edb:read(Cortex_Id))
    }.




mutation_function(Properties) ->
    Cortex_Id = enn:clone(?cortex_id(Properties)),
    apply_algorithms(?ALGORITHM_APPLY_ORDER, Cortex_Id, ?algorithm(Properties)),
    Properties#{
        cortex_id := Cortex_Id,
        size      := nn_elements:size(edb:read(Cortex_Id))
    }.

apply_algorithms([FunKey | Functions], Cortex_Id, Algorithm) ->
    Value = maps:get(FunKey, Algorithm),
    try
        algorithm:FunKey(Cortex_Id, Value, Algorithm)
    catch
        Type:Exception -> ?LOG_NOTICE("Algorithm application ~p failed: {~p, ~p}", [FunKey, Type, Exception])
    end,
    apply_algorithms(Functions, Cortex_Id, Algorithm);
apply_algorithms([], _Cortex_Id, _Algorithm) ->
    ok.




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

