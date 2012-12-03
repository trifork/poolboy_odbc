%% poolboy_odbc - Pooled ODBC for Erlang - Copyright (C) 2012 Pannonia Technologies
%%
%% This library is free software; you can redistribute it and/or modify it under the terms of the
%% GNU Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the License,
%% or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
%% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%% See the GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
%% the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

-module(poolboy_odbc).
-author("Matthias Endler <matthias.endler@pantech.at>").

%% API
-export([safe_worker/2, safe_worker/3, safe_worker/4]).
-export([commit/2, commit/3]).
-export([connect/1, connect/2, connect/3]).
-export([disconnect/2]).
-export([describe_table/2, describe_table/3]).
-export([first/1, first/2]).
-export([last/1, last/2]).
-export([next/1, next/2]).
-export([param_query/3, param_query/4]).
-export([prev/1, prev/2]).
-export([start/0, start/1]).
-export([stop/0]).
-export([sql_query/2, sql_query/3]).
-export([select_count/2, select_count/3]).
-export([select/3, select/4]).

-define(TIMEOUT, 5000).

safe_worker(Pool, Fun) ->
  safe_worker(Pool, Fun, true).

safe_worker(Pool, Fun, Block) ->
  safe_worker(Pool, Fun, Block, ?TIMEOUT).

safe_worker(Pool, Fun, Block, TimeOut) ->
  Worker = connect(Pool, Block, TimeOut),
  try
    Fun(Worker)
  after
    ok = disconnect(Pool, Worker)
  end.

commit(Worker, CommitMode) ->
  commit(Worker, CommitMode, ?TIMEOUT).

commit(Worker, CommitMode, TimeOut) ->
  gen_server:call(Worker, {commit, CommitMode, TimeOut}).

connect(Pool) ->
  connect(Pool, true).

connect(Pool, Block) ->
  connect(Pool, Block, ?TIMEOUT).

connect(Pool, Block, Timeout) ->
  poolboy:checkout(Pool, Block, Timeout).

disconnect(Pool, Worker) ->
  poolboy:checkin(Pool, Worker).

describe_table(Worker, Table) ->
  describe_table(Worker, Table, ?TIMEOUT).

describe_table(Worker, Table, TimeOut) ->
  gen_server:call(Worker, {describe_table, Table, TimeOut}).

first(Worker) ->
  first(Worker, ?TIMEOUT).

first(Worker, TimeOut) ->
  gen_server:call(Worker, {first, TimeOut}).

last(Worker) ->
  last(Worker, ?TIMEOUT).

last(Worker, TimeOut) ->
  gen_server:call(Worker, {last, TimeOut}).

next(Worker) ->
  next(Worker, ?TIMEOUT).

next(Worker, TimeOut) ->
  gen_server:call(Worker, {next, TimeOut}).

param_query(Worker, SQLQuery, Params) ->
  param_query(Worker, SQLQuery, Params, ?TIMEOUT).

param_query(Worker, SQLQuery, Params, TimeOut) ->
  gen_server:call(Worker, {param_query, SQLQuery, Params, TimeOut}).

prev(Worker) ->
  prev(Worker, ?TIMEOUT).

prev(Worker, TimeOut) ->
  gen_server:call(Worker, {prev, TimeOut}).

start() ->
  start(temporary).

start(Type) ->
  application:start(?MODULE, Type).

stop() ->
  application:stop(?MODULE).

sql_query(Worker, SQLQuery) ->
  sql_query(Worker, SQLQuery, ?TIMEOUT).

sql_query(Worker, SQLQuery, TimeOut) ->
  gen_server:call(Worker, {sql_query, SQLQuery, TimeOut}).

select_count(Worker, SelectQuery) ->
  select_count(Worker, SelectQuery, ?TIMEOUT).

select_count(Worker, SelectQuery, TimeOut) ->
  gen_server:call(Worker, {select_count, SelectQuery, TimeOut}).

select(Worker, Position, N) ->
  select(Worker, Position, N, ?TIMEOUT).

select(Worker, Position, N, TimeOut) ->
  gen_server:call(Worker, {select, Position, N, TimeOut}).