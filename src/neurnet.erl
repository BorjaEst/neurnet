%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% TODO: Un agente debe de estar formado por varias redes neuronales de control y la superior
%%% TODO: Añadir metodo de ajuste de pesos mediante estimulos positivos/negativos
%%% TODO: Debe de haber una population id "Real" que siempre existe, si no se especifica el pop_id, el agente se refiere ahu
%%% @end
%%%-------------------------------------------------------------------
-module(neurnet).
-author("borja").
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% API
-export([]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples with the record name and attributes
%% list. This is mainly used to prepare the tables in mnesia.
%% @end
%%--------------------------------------------------------------------
-spec attributes_table() -> 
    [{Elem :: nnelements:element(), [Attr :: atom()]}].
attributes_table() ->
    eevo:attributes_table() ++ enn:attributes_table() ++ [
        % Genotype
        % Phenotype
        % Architectures

        {      actuator, actuator:fields(      actuator)},
        {actuator_group, actuator:fields(actuator_group)},
        {        sensor,   sensor:fields(        sensor)},
        {  sensor_group,   sensor:fields(  sensor_group)}
    ].

%%--------------------------------------------------------------------
%% @doc Loads a module with the genotypes to test in neurnet. 
%% @end
%%--------------------------------------------------------------------
-spec load(GenotypeModules :: [module()]) -> ok.
load(GenotypeModules) -> 
    genotype:load(GenotypeModules).

%%--------------------------------------------------------------------
%% @doc Triggers a genotype per population to evaluate an evolutionary
%% algorithm with neural networks and returns the score and the id of
%% the best phenotype for each specified genotype.
%% @end
%%--------------------------------------------------------------------
%TODO: 


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
phenotype(Genotype) -> 
    Phenotype_Id = genotype:buid(Genotype),
    eevo:agent(#{
        function  => fun phenotype:controller/1,
        mutation  => fun phenotype:mutator/1,
        arguments => Phenotype_Id
    }).


%%%===================================================================
%%% Internal functions
%%%===================================================================

