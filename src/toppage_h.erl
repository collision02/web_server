%% @author Sergiy Vasylchuk
-module(toppage_h).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = parse_req(Method, HasBody, Req2),
	{ok, Req3, State}.

parse_req(<<"OPTIONS">>, _HasBody, Req) ->
    Req1 = cowboy_req:set_resp_header(
        <<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(
        <<"access-control-allow-origin">>, <<"*">>, Req1),
    cowboy_req:reply(200, Req2);

parse_req(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),		
		[{BinJson, true}] = PostVals,
		Json = jsx:decode(BinJson, [return_maps]),
		Action = maps:get(<<"action">>, Json),
		Res = parse_command(Action, Json),
		cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}],jsx:encode(Res), Req2);


parse_req(<<"POST">>, false, Req) ->
	io:format("parse_req POST Missing body ~n",  []),
	cowboy_req:reply(400, [], <<"Missing body.">>, Req).

parse_command(<<"get_the_selected_number_of_values">>, Json) ->
	Quantity = maps:get(<<"quantiny">>, Json),
    distance_mng:get_distance(Quantity);

parse_command(<<"get_distance_in_date_range">>, Json) ->
	StartDate = maps:get(<<"startDate">>, Json),
	EndDate = maps:get(<<"endDate">>, Json),
	distance_mng:get_distance_in_date_range(StartDate, EndDate).


