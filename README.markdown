Pooled ODBC for Erlang
======================

poolboy_odbc is a simple wrapper built on top of [poolboy](https://github.com/devinus/poolboy) and the
odbc library bundled with [Erlang/OTP](http://www.erlang.org/).

Examples:
---------

  ```erlang
  %% start poolboy_odbc application
  ok = application:start(poolboy_odbc),
  
  %% add a pool
  poolboy_odbc_mgr:add_pool('odbc/pool1', [{size, 5}, {max_overflow, 10}],
    [{dsn, "DSN=pool1;UID=user;PWD=secret"}, {options, [
      {auto_commit, off},
      {binary_strings, on}
    ]}]),
  
  %% connect to the odbc data source
  Worker = poolboy_odbc:connect('odbc/pool1'),
  
  %% simple query
  Result1 = poolboy_odbc:sql_query(Worker, "SELECT * FROM test"),
  io:format("Result 1: ~p~n", [Result1]),
  
  %% parameterized query
  Result2 = poolboy_odbc:param_query(Worker, "SELECT * FROM test WHERE name = ?", [
    {{varchar, 64}, [<<"Max Mustermann">>]}
  ]).
  io:format("Result 2: ~p~n", [Result2]),
  
  %% disconnect from the odbc data source
  poolboy_odbc:disconnect('odbc/pool1', Worker),
  
  %% remove the pool
  poolboy_odbc_mgr:remove_pool('odbc/pool1'),
  
  %% stop poolboy_odbc application
  application:stop(poolboy_odbc).
  ```