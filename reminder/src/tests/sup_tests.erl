-module(sup_tests).
-include_lib("eunit/include/eunit.hrl").

restart_test_() ->
    {"Test that everything restarts until a kill",
     {setup,
      fun() -> sup:start(evserv, []) end,
      fun(_) -> ok end,
      fun restart/1}}.

restart(SupPid) ->
    [?_assert(is_pid(whereis(evserv))),
     ?_assert(exit(whereis(evserv), die)),
     ?_assert(is_pid(whereis(evserv))),
     ?_assert(exit(whereis(evserv), die)),
     ?_assert(is_pid(whereis(evserv))),
     ?_assert(exit(SupPid, shutdown)),
     ?_assertEqual(undefined, whereis(evserv))].

