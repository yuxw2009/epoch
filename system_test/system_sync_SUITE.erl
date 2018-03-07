-module(system_sync_SUITE).

% Common Test exports
-export([all/0]).

% Test cases
-export([new_node_join_old_network/1]).

%--- Common Test exports -------------------------------------------------------

all() ->
    [new_node_join_old_network].

%--- Test cases ----------------------------------------------------------------

new_node_join_old_network(_Config) -> ok.
