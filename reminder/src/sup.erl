%% A generic restarter. It can take any module, as long as it has a
%% start_link function. It will restart the process it watches
%% indefinitely, unless the supervisor itself is terminated with a
%% shutdown exit signal.

-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod,Args) ->
    spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod,Args}) ->
    process_flag(trap_exit, true),
    loop({Mod,start_link,Args}).

loop({M,F,A}) ->
    Pid = apply(M,F,A),
    receive
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
            loop({M,F,A});
        {'EXIT', _, shutdown} ->
            exit(shutdown) % will kill the child too
    end.
