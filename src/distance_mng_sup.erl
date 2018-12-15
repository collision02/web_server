%%%-------------------------------------------------------------------
%% @doc uniq_int_gen top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(distance_mng_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(any()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_h, []}
        ]}
    ]),
    {ok, {
        {one_for_one, 15, 1}, 
            #{
                id => my_web,
                start => {cowboy, start_clear, [
                    http,
                    #{port => 8080},
                    #{
                        env => #{dispatch => Dispatch},
                        middlewares => [cowboy_router, cowboy_handler]
                    }
                ]}
            }
   
	}
}.
%%====================================================================
%% Internal functions
%%====================================================================
