%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(docker_utils).


%=== EXPORTS ===================================================================

%% API exports
-export([start/0]).
-export([info/0]).


%=== MACROS ====================================================================

-define(DOCKER_BASE_URL, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock/">>).
-define(DOCKER_INFO_ENDPOINT, <<"info">>).

-define(JSX_DECODE_OPTS, [{labels, attempt_atom}, return_maps]).


%=== API FUNCTIONS =============================================================

start() ->
    case application:ensure_all_started(hackney) of
        {ok, _} -> ok;
        Error -> Error
    end.

info() -> docker_get(?DOCKER_INFO_ENDPOINT).


%=== INTERNAL FUNCTIONS ========================================================

docker_get(Path) ->
    Url = hackney_url:make_url(?DOCKER_BASE_URL, [Path], []),
    case hackney:request(get, Url, [], <<>>, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    try jsx:decode(Body, ?JSX_DECODE_OPTS) of
                        Json -> {ok, Json}
                    catch
                        error:badarg -> {error, bad_json}
                    end;
                Error -> Error
            end;
        {ok, Status, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            {error, {unexpected_status, Status}}
    end.