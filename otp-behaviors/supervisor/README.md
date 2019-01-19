```
c("generic-server/usr").
c("generic-server/usr_db").
rr("generic-server/usr.hrl").
c("supervisor/usr_sup").

usr_sup:start_link().

whereis(usr).

exit(whereis(usr), kill).

whereis(usr).

usr:lookup_id(0).

exit(whereis(usr), kill).
exit(whereis(usr), kill).
```