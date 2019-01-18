```

c(usr).
c(usr_db).
rr("usr.hrl").

usr:start_link().

usr:add_usr(10, 0, prepay).
usr:lookup_id(0).

usr:set_service(0, data, true).
usr:lookup_id(0).

usr:stop().
usr:lookup_id(0).

usr:start_link().
usr:lookup_id(0).

```