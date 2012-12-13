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

-module(poolboy_odbc_app).
-author("Matthias Endler <matthias.endler@pantech.at>").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
  poolboy_odbc_sup:start_link().

stop(_State) ->
  ok.