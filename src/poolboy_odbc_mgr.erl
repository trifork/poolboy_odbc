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

-spec add_pool(Name::atom(), SizeArgs::size_args(), PoolArgs::pool_args()) -> {ok, pid()} | {error, term()}.
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