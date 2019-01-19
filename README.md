# erlang-sandbox

### Records and Macros

- [record](records-and-macros/records.erl)
- [macros](records-and-macros/macros.erl)

### Sequential

- [conditional evaluation](basic-sequential/conditional_evaluation.erl)
- [handling errors](basic-sequential/handling_errors.erl)
- [quick sort, merge sort](basic-sequential/listf.erl)
- [parse, build AST and evaluate simple arithmetic expressions](basic-sequential/eval_exp.erl)

### Concurrent

- [process ring](basic-concurrent/thering.erl)
- [timer](basic-concurrent/simple_timer.erl)

### Process design patterns

- client-server
  - [channel frequencies](process-design-patterns/client_server.erl)
  - [list-based db](process-design-patterns/sandbox/db.erl)
- [finite state machine](process-design-patterns/mutex.erl)
- [event manager](process-design-patterns/event_manager.erl) ([two](process-design-patterns/io_handler.erl) [clients](process-design-patterns/log_handler.erl) and [test case](process-design-patterns/event_manager_test.erl))

### Process error handling

- [trapping process exits](process-error-handling/traping-exits/traping_exit.erl)
- [client-server with bidirectional link](process-error-handling/client_server_link.erl)
- [process supervisor](process-error-handling/extended_supervisor.erl)
- [mutex](process-error-handling/reliable_mutex.erl)

### OTP behaviors

- [generic server](otp-behaviors/generic-server)
- [supervisor](otp-behaviors/supervisor)