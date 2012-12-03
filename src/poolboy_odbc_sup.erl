%% Copyright
-module(poolboy_odbc_sup).
-author("men").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
  MgrSpec = {poolboy_odbc_mgr, {poolboy_odbc_mgr, start_link, []}, permanent, 5000, worker, [poolboy_odbc_mgr]},
  WorkerSupSpec = {poolboy_odbc_worker_sup, {poolboy_odbc_worker_sup, start_link, []}, permanent, 5000, supervisor, [poolboy_odbc_worker_sup]},
  {ok, {{one_for_one, 5, 10}, [MgrSpec, WorkerSupSpec]}}.