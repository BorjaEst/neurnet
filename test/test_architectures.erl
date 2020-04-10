%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_architectures).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

%% Defined agent species
-export([]).


%%%===================================================================
%%% Defined Architectures
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: all() -> [ArchitectureName :: atom()] 
%%--------------------------------------------------------------------
all() -> [simple, complex].


%%--------------------------------------------------------------------
%% @doc Simple architecture, only 1 initial layer and sequential.
%% @end
%%--------------------------------------------------------------------
-spec simple() -> Architecture :: neurnet:architecture().
simple() ->
    _Architecture = #{
        initial_dimensions => [2],
        model_type         => sequential
    }.

%%--------------------------------------------------------------------
%% @doc Complex architecture, 2 initial layer and allows recurrent 
%% links.
%% @end
%%--------------------------------------------------------------------
-spec complex() -> Architecture :: neurnet:architecture().
complex() ->
    _Architecture = #{
        initial_dimensions => [3, 3],
        model_type         => recurrent
    }.

