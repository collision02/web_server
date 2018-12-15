%% @author Sergiy Vasylchuk

-module(distance_mng).


%% ====================================================================
%% API functions
%% ====================================================================
-export([
    get_distance/1,
    start/0,
    get_the_selected_number_of_values/1,
    get_distance_in_date_range/1
]).

-define(DB, diploma).

%%-----------------------------------------------------------------------------
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> {ok, list(atom())} | {error, Reason :: term()}
%% @doc Start the server.
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, Started::list(atom())} | {error, Reason :: term()}.
start() ->
    application:ensure_all_started(distance_mng, permanent).


-spec get_distance(Id :: integer()) -> {ok, Res::float()} | {error, Reason} when
    Reason :: invalid_account_id | internal | notfound.
get_distance(Id) ->
    case db_get_distance(Id) of
        {ok, Res} ->
            {ok, Res};
        {error, notfound} ->
            {error, notfound};
        {error, Reason} ->
%%            ?ERROR("Failed to find limit for this account ~p with reason: ~tp", [Id, Reason]),
            {error, internal}
    end.

-spec forming_correct_response(Res::list()) -> term().
forming_correct_response(Res) ->
    Result = lists:map(fun({Val}) ->
                binary_to_float(Val)
                end, Res),
    jsx:encode(#{<<"distance">> => Result}).


-spec get_the_selected_number_of_values(Quantity :: integer()) -> {ok, [Res::float()]} | {error, Reason} when
    Reason :: invalid_account_id | internal | notfound.
get_the_selected_number_of_values(Quantity) ->
    case db_get_the_selected_number_of_values(Quantity) of
        {ok, Res} ->
            FinRes = forming_correct_response(Res),
            {ok, FinRes};
        {error, notfound} ->
            {error, notfound};
        {error, Reason} ->
%%            ?ERROR("Failed to find limit for this account ~p with reason: ~tp", [Quantity, Reason]),
            {error, internal}
    end.

-spec get_distance_in_date_range(Period ::map()) -> {ok, [Res::float()]} | {error, Reason} when
    Reason :: invalid_account_id | internal | notfound.
get_distance_in_date_range(#{<<"start_date">> := StartDate, <<"end_date">> := EndDate} = Period) ->
    case db_get_distance_in_date_range(StartDate, EndDate) of
        {ok, Res} ->
            {ok, Res};
        {error, notfound} ->
            {error, notfound};
        {error, Reason} ->
%%            ?ERROR("Failed to find limit for this account ~p with reason: ~tp", [ Reason]),
            {error, internal}
    end.


-spec db_get_distance(Id::integer()) -> {ok,  {LossLimit}} | {error, Reason} when
    LossLimit::binary(), Reason :: notfound | term().
db_get_distance(Id) ->
    case db_proxy:query(?DB, "SELECT distance FROM measuring_distance
                         WHERE id = $1::int8
                         AND distance != 0",
        [Id]) of
        {ok,_, [{Distance}] } ->
            {ok, Distance};
        {ok, _, [ ]} ->
            {error, notfound};
        {error, _Reason} = Res ->
            Res;
        Reason ->
            {error, Reason}
    end.

-spec db_get_the_selected_number_of_values(Quantity::integer()) -> {ok,  LossLimit} | {error, Reason} when
    LossLimit::binary(), Reason :: notfound | term().
db_get_the_selected_number_of_values(Quantity) ->
    case db_proxy:query(?DB, "SELECT distance FROM measuring_distance
                         WHERE  distance != 0
                         ORDER BY id DESC
                         LIMIT  $1::int8",
        [Quantity]) of
        {ok,_, Res } ->
            {ok, Res};
        {ok, _, [ ]} ->
            {error, notfound};
        {error, _Reason} = Res ->
            Res;
        Reason ->
            {error, Reason}
    end.

-spec db_get_distance_in_date_range(StartDate::binary(), EndDate::binary()) -> {ok,  LossLimit} | {error, Reason} when
    LossLimit::binary(), Reason :: notfound | term().
db_get_distance_in_date_range(StartDate, EndDate) ->
    case db_proxy:query(?DB, "SELECT distance FROM measuring_distance
                         WHERE  timestamp > $1:: AND  timestamp < $2::datetime
                         LIMIT  20",
        [StartDate, EndDate]) of
        {ok,_, Res } ->
            {ok, Res};
        {ok, _, [ ]} ->
            {error, notfound};
        {error, _Reason} = Res ->
            Res;
        Reason ->
            {error, Reason}
    end.