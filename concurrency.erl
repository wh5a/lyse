-module(concurrency).
-compile(export_all).

dolphin(X) ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin(X);
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        {_, Y} ->
            io:format("Heh, ~p are smarter than ~p.~n", [Y,X]),
            dolphin(X)
    after 3000 -> % Optional milliseconds timeout
            timeout
    end.

test() ->
    Dolphin3 = spawn(?MODULE, dolphin, [humans]),
    Dolphin3 ! Dolphin3 ! {self(), dolphins},
    Dolphin3 ! {self(), fish},
    flush().

flush() ->
    receive
        Msg ->
            io:format("~s~n", [Msg]),
            flush()
    after 10 ->
            ok
    end.

% Creating links between processes so they die together
chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.
% Test in shell to see shell dies and restarts:
% spawn_link(concurrency, chain, [3]).
% Now convert exit signals to regular messages:
% process_flag(trap_exit, true).
% and try again:
% spawn_link(fun() -> concurrency:chain(3) end).
% flush().

% Monitor a process without dying together
% Try in shell, we create two monitors and then take one down, and finally let the monitored process die:
% {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
% Ref2 = monitor(process, Pid).
% demonitor(Ref).
% Pid ! die.
% flush().

%%%% Restarting and naming processes %%%%
% Test in shell:
% concurrency:start_critic2().
% concurrency:judge2("The Doors", "Light my Firewall").
% Kill critic and let restarter restarts it:
% whereis(critic).
% exit(whereis(critic), solar_storm).
% whereis(critic).
% unregister(critic).

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    % Associate the Pid with the atom critic
    % We created a global state. Aren't we cheating?
    register(critic, Pid),
    % Now critic appears in the list returned by registered().
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual shutdown, not a crash
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

critic2() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic2().

judge2(Band, Album) ->
    % Make a unique value to identify messages
    Ref = make_ref(),
    % Sending to the atom critic will always send to the critic2 process
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.
