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

-module(poolboy_odbc_worker_sup).
-author('Matthias Endler <matthias.endler@pantech.at>').

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
