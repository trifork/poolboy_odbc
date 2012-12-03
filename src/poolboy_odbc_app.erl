%% Copyright
-module(poolboy_odbc_app).
-author("Matthias Endler <matthias.endler@pantech.at>").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
  ok = ensure_started(odbc),
  poolboy_odbc_sup:start_link().

stop(_State) ->
  ok.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.