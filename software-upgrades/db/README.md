
```
c("v1.0/db").
Db = db:new().
Db1 = db:write(name, ricardo, Db).
Db2 = db:write(pink, verizon, Db1).
db:read(name, Db2).

c("v1.1/db").
db:read(name, Db2).

c("v1.2/db_server").
db:module_info().
db_server:start().
db_server:write(red, rose).
db_server:write(pink, pig).
db_server:read(name).
db_server:read(red).
code:add_patha("./v1.2").
code:load_file("db").
code:sort_purge(db).
db_server:upgrade(dict).
db:module_info().
db_server:read(red).
db_server:write(green, tree).
db_server:read(green).
```
