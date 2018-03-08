%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(system_test_backend_docker).



%=== EXPORTS ===================================================================

%% API exports
-export([prepare_node/2]).
-export([setup_node/3]).


%=== MACROS ====================================================================

-define(CONFIG_FILE_TEMPLATE, "epoch.yaml.mustache").
-define(EPOCH_CONFIG_FILE, "/home/epoch/epoch.yaml").


%=== GENERIC API FUNCTIONS =====================================================

prepare_node(NodeSpec, TestCtx) ->
    #{name := Hostname} = NodeSpec,
    ExtAddr = format("http://~s:3013/", [Hostname]),
    NodeState = #{
        spec => NodeSpec,
        hostname => Hostname,
        ext_addr => ExtAddr
    },
    {ok, NodeState, TestCtx}.


setup_node(NodeState, NodeStates, TestCtx) ->
    #{test_id := TestId,
      local_port := Port,
      data_dir := DataDir,
      temp_dir := TempDir} = TestCtx,
    #{spec := NodeSpec, hostname := Hostname} = NodeState,
    #{peers := PeerNames, source := {pull, Image}} = NodeSpec,
    MgrAddr = format("http://~s:~w/", [Hostname, Port]),
    ConfigFileName = format("epoch_~s.yaml", [Hostname]),
    ConfigFilePath = filename:join(TempDir, ConfigFileName),
    TemplateFile = filename:join(DataDir, ?CONFIG_FILE_TEMPLATE),
    ExpandedPeers = [maps:to_list(V)
                     || {K, V} <- maps:to_list(NodeStates),
                     lists:member(K, PeerNames)],
    Context = [{epoch_config, maps:to_list(NodeState#{peers => ExpandedPeers})}],
    ok = write_template(TemplateFile, ConfigFilePath, Context),
    ContName = format("~s_~s", [Hostname, TestId]),
    DockerConfig = #{
        hostname => ContName,
        image => Image,
        env => #{"EPOCH_CONFIG" => ?EPOCH_CONFIG_FILE},
        volumes => [?EPOCH_CONFIG_FILE]
    },
    case docker_utils:create_container(ContName, DockerConfig) of
        {ok, Res} ->
            #{'Id' := ContId} = Res,
            NodeState2 = NodeState#{
                container_name => ContName,
                container_id => ContId,
                mgr_addr => MgrAddr,
                config_path => ConfigFilePath
            },
            {ok, NodeState2, TestCtx#{local_port := Port + 1}};
        Error -> Error
    end.


%=== INTERNAL FUNCTIONS ========================================================

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

write_template(TemplateFile, OutputFile, Context) ->
    Template = bbmustache:parse_file(TemplateFile),
    Data = bbmustache:compile(Template, Context, [{key_type, atom}]),
    file:write_file(OutputFile, Data).

