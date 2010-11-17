% You can call the compiler from many places:
% erlc flags file.erl when in the command line
% compile:file(FileName) when in the shell or in a module
% c() when in the shell.

% A module's metadata can be obtained through:
% moduleName:module_info().

-module(functional). % Module name must match file name.
-export([greet_and_add_two/1, use_macro/0]).
-import(io, [format/1]).

-author("Awesome guy").
-vsn(version1).

% We can pass flags to compiler like this:
-compile([debug_info, export_all]). % Override our export list

-define(sub(X,Y), X-Y). % Macro
use_macro() ->
    ?sub(23,47).

% Include a header file for record definitions
-include("records.hrl").
% Load the record information into shell before running this function:
% rr(functional).
robot() ->
    % Define a record
    X = #robot{name="Mechatron",
               type=handmade,
               details=["Moved by a small man inside"]},
    % Access its fields
    {X#robot.hobbies, X}.

% io:format/1 is the standard function used to output text.
hello() ->
    format("Hello, world!~n").
% Functions and expressions must always return something.
% As such, io:format/1 returns 'ok' to denote a normal condition.

greet_and_add_two(X) ->
    hello(),
    X + 2.

second([_,X|_]) -> X.

% Prolog style binding
same(X,X) ->
    true;
same(_,_) ->
    false.

% Guards: , means andalso   ; means orelse
right_age(X) when X >= 16, X =< 104 ->
    true;
right_age(_) ->
    false.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does  %% this is Erlang's if's 'else!'
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.

% Passing functions
one() -> 1.
two() -> 2.
add(X,Y) -> X() + Y().
pass() -> add(fun one/0, fun two/0).

% Anonymous function
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().

% Exception handling. Try in shell:
% functional:black_knight(fun() -> error(cut_arm) end).
% of and after are optional
black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch % type:exception
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    after
        format("Finally\n")
    end.

% Another way of exception handling
catcher(X,Y) ->
    case catch X/Y of
        {'EXIT', {badarith,_}} -> "uh oh";
        N -> N
    end.
