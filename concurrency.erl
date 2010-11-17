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
