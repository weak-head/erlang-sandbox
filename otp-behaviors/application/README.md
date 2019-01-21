```bash

# -- bash --
> cd ebin
> erl

# -- erl shell --
rr("../include/usr.hrl").
c("../src/usr_app").
c("../src/usr").
c("../src/usr_db").
c("../src/usr_sup").

%% code:add_path("ebin").
application:start(usr).
application:start(usr).

usr:add_usr(10, 0, prepay).
usr:lookup_id(0).

application:stop(usr).

usr:lookup_id(0).
```