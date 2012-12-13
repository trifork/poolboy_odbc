Pooled ODBC for Erlang
======================

poolboy_odbc is a simple wrapper built on top of [poolboy](https://github.com/devinus/poolboy) and the
odbc library bundled with [Erlang/OTP](http://www.erlang.org/) to pool ODBC connections.

Usage:
------

```erl-sh
1> poolboy_odbc:start().
ok
2> poolboy_odbc_mgr:add_pool('odbc/pool1', [{size, 5}, {max_overflow, 10}],
    [{dsn, "DSN=pool1;UID=user;PWD=secret"}, {options, [
        {auto_commit, off},
        {binary_strings, on}]}]).
ok
3> Worker = poolboy_odbc:connect('odbc/pool1').
<0.31.0>
4> poolboy_odbc:sql_query(Worker, "SELECT * FROM test").
{selected, ["id", "name"],
           [{1, <<"Erika Mustermann">>}]}
```

You can now do all operations like in the odbc module, except you use the poolboy_odbc module and the Worker instead of
the connection reference.

```erl-sh
10> poolboy_odbc:disconnect('odbc/pool1', Worker).
ok
```

To reduce boilerplate code for connecting and safe disconnecting, you can use poolboy_odbc's safe_worker function.

```erl-sh
100> poolboy_odbc:safe_worker('odbc/pool1', fun(SafeWorker) ->
         {updated, 1} = poolboy_odbc:param_query(SafeWorker,
           "INSERT INTO TEST (name) VALUES (?)",
           [{{varchar, 64}, [<<"Hans Dampf">>]}])
       end)
{updated, 1}
101>
```

Authors:
--------

- Matthias Endler (matthias-endler) <matthias.endler@pantech.at>

License:
--------

poolboy_odbc is availabe under the LGPL version 3.0.
