%% Copyright
-module(poolboy_odbc_worker_sup).
-author("Matthias Endler <matthias.endler@pantech.at>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).
-export([add_child/3]).
-export([remove_child/1]).

-define(SUPERVISOR, ?MODULE).

%% API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child(Name, SizeArgs, WorkerArgs) ->
  PoolArgs = [{name, {local, Name}}, {worker_module, poolboy_odbc_worker}|SizeArgs],
  ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
  supervisor:start_child(?SUPERVISOR, ChildSpec).

remove_child(Name) ->
  supervisor:terminate_child(?SUPERVISOR, Name),
  supervisor:delete_child(?SUPERVISOR, Name),
  ok.

%% supervisor callbacks
init([]) ->
  PoolSpecs = [],
  {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
