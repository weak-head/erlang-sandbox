```

node_foo> erl -sname foo

node_buz> erl -sname buz

node_foo> c(distr).

node_buz> spawn('foo@host", distr, dt, [self()]).
```