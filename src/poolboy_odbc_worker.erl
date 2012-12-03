%% Copyright
-module(poolboy_odbc_worker).
-author("Matthias Endler <matthias.endler@pantech.at>").

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% gen_server callbacks
-record(state, {conn}).

init(Args) ->
  Dsn = proplists:get_value(dsn, Args),
  Options = proplists:get_value(options, Args, []),
  {ok, ConnRef} = odbc:connect(Dsn, Options),
  {ok, #state{conn=ConnRef}}.

handle_call({commit, CommitMode, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:commit(ConnRef, CommitMode, TimeOut), State};
handle_call({describe_table, Table, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:describe_table(ConnRef, Table, TimeOut), State};
handle_call({first, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:first(ConnRef, TimeOut), State};
handle_call({last, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:last(ConnRef, TimeOut), State};
handle_call({next, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:next(ConnRef, TimeOut), State};
handle_call({param_query, SQLQuery, Params, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:param_query(ConnRef, SQLQuery, Params, TimeOut), State};
handle_call({prev, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:prev(ConnRef, TimeOut), State};
handle_call({sql_query, SQLQuery, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:sql_query(ConnRef, SQLQuery, TimeOut), State};
handle_call({select_count, SelectQuery, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:select_count(ConnRef, SelectQuery, TimeOut), State};
handle_call({select, Position, N, TimeOut}, _From, #state{conn=ConnRef}=State) ->
  {reply, odbc:select(ConnRef, Position, N, TimeOut), State};
handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn=ConnRef}) ->
  ok = odbc:disconnect(ConnRef),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.