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
