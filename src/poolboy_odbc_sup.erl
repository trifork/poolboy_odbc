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