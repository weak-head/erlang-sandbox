-module(usr_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_FileName) ->
    UsrChild = { usr,                       % Id
                 {usr, start_link, []},     % {Module, Function, Argument}
                 permanent,                 % Restart (transient, temporary, permanent)
                 2000,                      % Shutdown
                 worker,                    % Type (worker, supervisor)
                 [usr, usr_db]              % Module list
               },
    { ok,
      {
        { % Supervisor Specification
          one_for_all,  % RestartStrategy
          1,            % AllowedRestarts
          1             % MaxSeconds
        },

        % Child Specification
        [UsrChild]
      }
    }.