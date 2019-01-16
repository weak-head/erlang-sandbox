```

node_foo> erl -sname foo -setcookie secret
node_foo> c(dist).

node_buz> erl -sname buz -setcookie secret
node_buz> spawn('foo@host', dist, start_server, []).
node_buz> { server, 'foo@host' } ! {self(), 123}.
node_buz> flush().

```