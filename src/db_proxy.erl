%% @author sergiy
%% @doc @todo Add description to db_proxy.


-module(db_proxy).




-type dbproxy_result() :: ok | list() | map() | undefined | no_return().
-type dbproxy_unwrapped_query_result() :: #{
                                            status => ok | error,
                                            columns_desc => [binary()],
                                            affected_rows => non_neg_integer() | undefined,
                                            rows => [map()] | list() | undefined
                                        }.

%% API exports
-export([
    db_error/1,
    get_connection/0,
    ok/1,
    ok/2,
    close_connection/1,
    query/2,
    query/3,
	query/4,
    tx/2
]).

-export_type([
    dbproxy_result/0,
    dbproxy_unwrapped_query_result/0
]).

%%====================================================================
%% API functions
%%====================================================================

-spec db_error(any()) -> no_return().
db_error(ErrInfo) ->
    erlang:throw(erlang:append_element({error, db_error}, ErrInfo)).


close_connection(C) ->
    epgsql:close(C).

-spec ok(Res) -> dbproxy_result() when
         Res    ::  {ok, dbproxy_unwrapped_query_result()}
                |   {error, any}
                |   dbproxy_unwrapped_query_result().
ok(Res) ->
    ok(Res, rows).

-spec ok(QueryResult :: any(), rows | row | affected_rows | ok) -> dbproxy_result().
ok({ok, Res}, Option) ->
    ok(Res, Option);
ok(#{status := error} = Res, _) ->
    error(Res);
ok(Res, _) when is_tuple(Res), element(1, Res) =:= error ->
    error(Res);
ok(#{status := ok, rows := Rows}, rows) ->
    Rows;
ok(#{status := ok, rows := [Row | _]}, row) ->
    Row;
ok(#{status := ok, rows := []}, row) ->
    undefined;
ok(#{status := ok, affected_rows := AffectedRows}, affected_rows) when AffectedRows >= 1 ->
    ok;
ok(#{status := ok}, ok) ->
    ok;
ok(Res, _) ->
    db_error(Res).

-spec get_connection() -> {ok, C:: epgsql:connection()} | {error, Reason::term()}.
get_connection() ->
	DB = appication:get_env(dbproxy, distance_mng),
	[Opts] = proplist:get_value(databases, DB),
    epgsql:connect(Opts).

-spec query(atom(), epgsql:sql_query()) ->
    {epgsql:connection(), dbproxy_unwrapped_query_result()} | dbproxy_unwrapped_query_result().
query(DB, Query) when is_atom(DB) ->
    query(DB, Query, []).

-spec query(PoolNameOrTransactionHandler, epgsql:sql_query(), epgsql:bind_param() | epgsql:typed_param()) ->
    {epgsql:connection(), dbproxy_unwrapped_query_result()} | dbproxy_unwrapped_query_result()
    when PoolNameOrTransactionHandler :: atom() | epgsql:connection().
query(DB, Query, Params) when is_atom(DB) ->
    {ok, C} = get_connection(),
    {C, Res} = query(C, DB, Query, Params),
    close_connection(C),
    Res.
query(C, DB, Query, Params) ->
%   logger:debug("[dbproxy] ~ts~n~p~n", [Query, Params]),
    Res = epgsql:equery(C, DB, Query, Params),
    {C, unwrap_query_result(Res)}.

-spec tx(atom(), fun((epgsql:connection()) -> any())) -> {error, any()} | {ok, any()}.
tx(DB, Fun) when is_atom(DB) ->
    {ok, C} = episcina:get_connection(DB),
    Res0 = epgsql:with_transaction(C, Fun, [{reraise, true}]),
    Res = case Res0 of
        {rollback, _} = Err ->
            {error, Err};
        Res0 ->
            Res0
    end,
    episcina:return_connection(DB, C),
    Res.


%%====================================================================
%% Internal functions
%%====================================================================

-spec unwrap_query_result(tuple()) -> dbproxy_unwrapped_query_result().
unwrap_query_result(_Res = {error, Error = #{}}) ->
    #{status => error, error => Error};
unwrap_query_result(_Res = {ok, ColumnsDescription, RowsValues}) when is_list(RowsValues) ->
    #{
        status => ok,
        columns_desc => ColumnsDescription,
        rows => map_rows(ColumnsDescription, RowsValues)
    };
unwrap_query_result(_Res = {ok, Count}) when is_number(Count), Count >= 0 ->
    #{status => ok, affected_rows => Count};
unwrap_query_result(_Res = {ok, Count, ColumnsDescription, RowsValues}) when is_number(Count),
    Count >= 0,
    is_list(ColumnsDescription),
    is_list(RowsValues) ->
    #{
        status => ok,
        affected_rows => Count,
        columns_desc => ColumnsDescription,
        rows => map_rows(ColumnsDescription, RowsValues)
    };
unwrap_query_result(_Res = {rollback, Why}) ->
    logger:warning("DB ERROR: ~p~n", _Res),
    #{status => error, error => rollback, rollback_reason => Why};
unwrap_query_result(_What) ->
    logger:warning("DB ERROR: ~p~n", _What),
    #{status => error, error => badarg}.

map_rows(ColumnsDescription, RowsValues) ->
    {Columns, _} = lists:foldr(fun(#{name := Name}, {Acc, N}) ->
        case Name of
            <<"?column?">> ->
                ColumnId = integer_to_binary(N),
                {[<<"anon_", ColumnId/binary>>], N + 1};
            _ ->
                {[Name | Acc], N}
        end
    end, {[], 0}, ColumnsDescription),
    Rows = lists:foldr(fun(RowT, Acc) ->
        {Row, _, _} = lists:foldl(fun(ColName, {RowAcc, RowT2, N}) ->
            {maps:put(ColName, element(N, RowT2), RowAcc), RowT2, N + 1}
        end, {#{}, RowT, 1}, Columns),
        [Row | Acc]
    end, [], RowsValues),
    Rows.
