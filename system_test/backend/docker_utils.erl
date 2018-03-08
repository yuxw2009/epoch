%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(docker_utils).


%=== EXPORTS ===================================================================

%% API exports
-export([start/0]).
-export([info/0]).
-export([create_container/2]).


%=== MACROS ====================================================================

-define(DOCKER_BASE_URL, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock/">>).
-define(DOCKER_INFO_ENDPOINT, <<"info">>).
-define(DOCKER_CONTAINERS_CREATE_ENDPOINT, <<"containers/create">>).

-define(JSX_DECODE_OPTS, [{labels, attempt_atom}, return_maps]).
-define(JSX_ENCODE_OPTS, []).


%=== API FUNCTIONS =============================================================

start() ->
    case application:ensure_all_started(hackney) of
        {ok, _} -> ok;
        Error -> Error
    end.

info() -> docker_get(?DOCKER_INFO_ENDPOINT).

create_container(Name, Config) ->
    BodyObj = maps:fold(fun
        (hostname, Hostname, Body) ->
            Body#{'Hostname' => json_string(Hostname)};
        (image, Image, Body) ->
            Body#{'Image' => json_string(Image)};
        (env, Env, Body) ->
            EnvList = [json_string(K ++ "=" ++ V)
                       || {K, V} <- maps:to_list(Env)],
            Body#{'Env' => EnvList};
        (volumes, Volumes, Body) ->
            VolMap = maps:from_list([{json_string(V), #{}} || V <- Volumes]),
            Body#{'Volumes' => VolMap}
    end, #{}, Config),
    docker_post(?DOCKER_CONTAINERS_CREATE_ENDPOINT, #{name => Name}, BodyObj)   .


%=== INTERNAL FUNCTIONS ========================================================

docker_get(Path) ->
    Url = hackney_url:make_url(?DOCKER_BASE_URL, [Path], []),
    case hackney:request(get, Url, [], <<>>, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            docker_fetch_json_body(ClientRef);
        {ok, Status, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            {error, {unexpected_status, Status}}
    end.

docker_post(Path, Query, BodyObj) ->
    Url = hackney_url:make_url(?DOCKER_BASE_URL, [Path], maps:to_list(Query)),
    case encode(BodyObj) of
        {ok, BodyJson} ->
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            case hackney:request(post, Url, Headers, BodyJson, []) of
                {ok, Status, _RespHeaders, ClientRef}
                  when Status =:= 200; Status =:= 201 ->
                    docker_fetch_json_body(ClientRef);
                {ok, Status, _RespHeaders, ClientRef} ->
                    hackney:skip_body(ClientRef),
                    {error, {unexpected_status, Status}};
                Error ->
                    io:format("ERROR: ~p~n", Error),
                    Error
            end;
        Error -> Error
    end.

docker_fetch_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {ok, BodyJson} -> decode(BodyJson);
        Error -> Error
    end.

decode(JsonStr) ->
    try jsx:decode(JsonStr, ?JSX_DECODE_OPTS) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, bad_json}
    end.

encode(JsonObj) ->
    try jsx:encode(JsonObj, ?JSX_ENCODE_OPTS) of
        JsonStr -> {ok, JsonStr}
    catch
        error:badarg -> {error, bad_json}
    end.


json_string(Atom) when is_atom(Atom) -> Atom;
json_string(Bin) when is_binary(Bin) -> Bin;
json_string(Str) when is_list(Str) -> list_to_binary(Str).

