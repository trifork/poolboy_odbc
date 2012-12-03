%% Copyright
-module(poolboy_odbc_mgr).
-author("Matthias Endler <matthias.endler@pantech.at>").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_pool/3]).
-export([remove_pool/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

add_pool(Name, SizeArgs, PoolArgs) ->
  gen_server:call(?SERVER, {add_pool, {Name, SizeArgs, PoolArgs}}).

remove_pool(Name) ->
  gen_server:call(?SERVER, {remove_pool, Name}).

%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
-record(state, {}).

init(_Args) ->
  {ok, #state{}}.

handle_call({add_pool, {Name, SizeArgs, PoolArgs}}, _From, State) ->
  {reply, poolboy_odbc_worker_sup:add_child(Name, SizeArgs, PoolArgs), State};
handle_call({remove_pool, Name}, _From, State) ->
  {reply, poolboy_odbc_worker_sup:remove_child(Name), State};
handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.