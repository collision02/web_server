%%%-------------------------------------------------------------------
%% @doc uniq_int_gen public API
%% @end
%%%-------------------------------------------------------------------

-module(distance_mng_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API.

start(_Type, _Args) ->
    logger:info("Distance MNG starting!"),
    distance_mng_sup:start_link().

stop(_State) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
