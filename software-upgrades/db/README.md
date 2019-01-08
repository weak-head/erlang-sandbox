
```
c("v1.0/db").
Db = db:new().
Db1 = db:write(name, ricardo, Db).
Db2 = db:write(pink, verizon, Db1).
db:read(name, Db2).

c("v1.1/db").
db:read(name, Db2).
```