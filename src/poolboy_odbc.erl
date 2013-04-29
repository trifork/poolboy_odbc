%% (The MIT License)
%%
%% Copyright (c) 2013 Matthias Endler
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the 'Software'), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(poolboy_odbc).
-author('Matthias Endler <matthias.endler@pantech.at>').

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

-type col_name() :: string().

-type col_names() :: [col_name()].

-type commit_mode() :: commit | rollback.

-type commit_reason() :: not_an_explicit_commit_connection | process_not_owner_of_odbc_connection | common_reason().

-type common_reason() :: connection_closed | term().

-type in_or_out() :: in | out | inout.

-type odbc_data_type() :: sql_integer | sql_smallint | sql_tinyint |
    {sql_decimal, precision(), scale()} |
    {sql_numeric, precision(), scale()} |
    {sql_char, size()} | {sql_wchar, size()} | {sql_varchar, size()} | {sql_wvarchar, size()} | {sql_wlongvarchar, size()} |
    {sql_float, precision()} | sql_real | sql_double | sql_bit | atom().

-type n_rows() :: non_neg_integer().

-type params() :: [{odbc_data_type(), [value()]} | {odbc_data_type(), in_or_out(), [value()]}].

-type position() :: next | {relative, integer()} | {absolute, integer()}.

-type precision() :: non_neg_integer().

-type result_reason() :: result_set_does_not_exist | process_not_owner_of_odbc_connection | common_reason().

-type result_tuple() :: {updated, n_rows()} | {selected, col_names(), rows()}.

-type row() :: {value()}.

-type rows() :: [row()].

-type scale() :: non_neg_integer().

-type scroll_reason() :: result_set_does_not_exist | driver_does_not_support_function | scrollable_cursors_disabled |
    process_not_owner_of_odbc_connection | common_reason().

-type size() :: pos_integer().

-type value() :: null | term().

-type worker_fun(T) :: fun((Worker::pid()) -> T).

-spec safe_worker(Pool::atom(), Fun::worker_fun(T)) -> {ok,T}.
safe_worker(Pool, Fun) ->
  safe_worker(Pool, Fun, true).

-spec safe_worker(Pool::atom(), Fun::worker_fun(T), Block::boolean()) -> {ok,T} | full.
safe_worker(Pool, Fun, Block) ->
  safe_worker(Pool, Fun, Block, ?TIMEOUT).

-spec safe_worker(Pool::atom(), Fun::worker_fun(T), Block::boolean(), TimeOut::timeout()) -> {ok,T} | full.
safe_worker(Pool, Fun, Block, TimeOut) ->
  case connect(Pool, Block, TimeOut) of
    full ->
      full;
    Worker ->
      try
        Fun(Worker)
      after
        ok = disconnect(Pool, Worker)
      end
  end.

-spec commit(Worker::pid(), CommitMode::commit_mode()) -> ok | {error, commit_reason()}.
commit(Worker, CommitMode) ->
  commit(Worker, CommitMode, ?TIMEOUT).

-spec commit(Worker::pid(), CommitMode::commit_mode(), TimeOut::timeout()) -> ok | {error, commit_reason()}.
commit(Worker, CommitMode, TimeOut) ->
  gen_server:call(Worker, {commit, CommitMode, TimeOut}).

-spec connect(Pool::atom()) -> pid().
connect(Pool) ->
  connect(Pool, true).

-spec connect(Pool::atom(), Block::boolean()) -> pid() | full.
connect(Pool, Block) ->
  connect(Pool, Block, ?TIMEOUT).

-spec connect(Pool::atom(), Block::boolean(), TimeOut::timeout()) -> pid() | full.
connect(Pool, Block, Timeout) ->
  poolboy:checkout(Pool, Block, Timeout).

-spec disconnect(Pool::atom(), Worker::pid()) -> ok.
disconnect(Pool, Worker) ->
  poolboy:checkin(Pool, Worker).

-spec describe_table(Worker::pid(), Table::string()) -> {ok, [{col_name(), odbc_data_type()}]} | {error, common_reason()}.
describe_table(Worker, Table) ->
  describe_table(Worker, Table, ?TIMEOUT).

-spec describe_table(Worker::pid(), Table::string(), TimeOut::timeout()) -> {ok, [{col_name(), odbc_data_type()}]} | {error, common_reason()}.
describe_table(Worker, Table, TimeOut) ->
  gen_server:call(Worker, {describe_table, Table, TimeOut}).

-spec first(Worker::pid()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
first(Worker) ->
  first(Worker, ?TIMEOUT).

-spec first(Worker::pid(), TimeOut::timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
first(Worker, TimeOut) ->
  gen_server:call(Worker, {first, TimeOut}).

-spec last(Worker::pid()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
last(Worker) ->
  last(Worker, ?TIMEOUT).

-spec last(Worker::pid(), TimeOut::timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
last(Worker, TimeOut) ->
  gen_server:call(Worker, {last, TimeOut}).

-spec next(Worker::pid()) -> {selected, col_names(), rows()} | {error, result_reason()}.
next(Worker) ->
  next(Worker, ?TIMEOUT).

-spec next(Worker::pid(), TimeOut::timeout()) -> {selected, col_names(), rows()} | {error, result_reason()}.
next(Worker, TimeOut) ->
  gen_server:call(Worker, {next, TimeOut}).

-spec param_query(Worker::pid(), SQLQuery::string(), Params::params()) ->  result_tuple() | {error, common_reason()}.
param_query(Worker, SQLQuery, Params) ->
  param_query(Worker, SQLQuery, Params, ?TIMEOUT).

-spec param_query(Worker::pid(), SQLQuery::string(), Params::params(), TimeOut::timeout()) ->  result_tuple() | {error, common_reason()}.
param_query(Worker, SQLQuery, Params, TimeOut) ->
  gen_server:call(Worker, {param_query, SQLQuery, Params, TimeOut}).

-spec prev(Worker::pid()) -> {selected, ColNames::[string()], Rows::[any()]} | {error, scroll_reason()}.
prev(Worker) ->
  prev(Worker, ?TIMEOUT).

-spec prev(Worker::pid(), TimeOut::timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
prev(Worker, TimeOut) ->
  gen_server:call(Worker, {prev, TimeOut}).

-spec start() -> ok.
start() ->
  start(temporary).

-spec start(Type::permanent | transient | temporary) -> ok.
start(Type) ->
  application:load(?MODULE),
  {ok, Apps} = application:get_key(?MODULE, applications),
  [ensure_started(App) || App <- Apps],
  application:start(?MODULE, Type).

-spec stop() -> ok.
stop() ->
  application:stop(?MODULE).

-spec sql_query(Worker::pid(), SQLQuery::string()) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
sql_query(Worker, SQLQuery) ->
  sql_query(Worker, SQLQuery, ?TIMEOUT).

-spec sql_query(Worker::pid(), SQLQuery::string(), TimeOut::timeout()) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
sql_query(Worker, SQLQuery, TimeOut) ->
  gen_server:call(Worker, {sql_query, SQLQuery, TimeOut}).

-spec select_count(Worker::pid(), SelectQuery::string()) -> {ok, n_rows()} | {error, common_reason()}.
select_count(Worker, SelectQuery) ->
  select_count(Worker, SelectQuery, ?TIMEOUT).

-spec select_count(Worker::pid(), SelectQuery::string(), TimeOut::timeout) -> {ok, n_rows()} | {error, common_reason()}.
select_count(Worker, SelectQuery, TimeOut) ->
  gen_server:call(Worker, {select_count, SelectQuery, TimeOut}).

-spec select(Worker::pid(), Position::position(), N::integer()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
select(Worker, Position, N) ->
  select(Worker, Position, N, ?TIMEOUT).

-spec select(Worker::pid(), Position::position(), N::integer(), TimeOut::timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
select(Worker, Position, N, TimeOut) ->
  gen_server:call(Worker, {select, Position, N, TimeOut}).

%% private functions

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
