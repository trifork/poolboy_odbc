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

-module(poolboy_odbc_mgr).
-author('Matthias Endler <matthias.endler@pantech.at>').

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_pool/3]).
-export([remove_pool/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-type odbc_option() :: {auto_commit, on | off} | {timeout, timeout()} | {binary_strings, on | off} | {tuple_row, on | off} |
    {scrollable_cursors, on | off} | {trace_driver, on | off}.

-type odbc_options() :: [odbc_option()].

-type pool_arg() :: {dsn, string()} | {options, odbc_options()}.

-type pool_args() :: [pool_arg()].

-type size_arg() :: {size, pos_integer()} | {max_overflow, pos_integer()}.

-type size_args() :: [size_arg()].

-spec add_pool(Name::atom(), SizeArgs::size_args(), PoolArgs::odbc_options()) -> {ok, pid()} | {error, term()}.
add_pool(Name, SizeArgs, PoolArgs) ->
  gen_server:call(?SERVER, {add_pool, {Name, SizeArgs, PoolArgs}}).

-spec remove_pool(Name::atom()) -> ok.
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