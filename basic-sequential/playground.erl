-module(playground).
-export([sum/1, sum/2, create/1, reverse_create/1]).

sum(N) when N =< 0 -> 0;
sum(N) -> N + sum(N-1).

sum(N,M) when N < M  -> N + sum(N+1, M);
sum(N,M) when N == M -> N;
sum(N,M) when N > M  ->
    throw({invalid_input, 'N should be less or equal to M'}).

create(N) ->
    do_create(1, N).

do_create(N,N) -> [N];
do_create(I,N) when I < N ->
    [I|do_create(I+1,N)];
do_create(I,N) when I > N ->
    throw({invalid_input}).

reverse_create(0) -> [];
reverse_create(N) when N > 0 ->
    [N|reverse_create(N-1)];
reverse_create(N) when N < 0 ->
    throw({invalid_input}).