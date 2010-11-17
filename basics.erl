%%%% Reading documentation

% If you're using linux, you can access the man pages for good technical documentation.
% Erlang has a lists module: to get the documentation on lists, just type in
% erl -man lists.

% Another way to access them is through Emacs menu.

% BIFs (Built-In Functions) defined in the erlang module is automatically imported.

%%%% Erlang shell

% Type something like li and then press "tab", the shell will have completed the terms for you to lists:.
% Press tab again, and the shell will suggest you many functions to use after.
% Unfortunately, in Emacs, Tab doesn't seem to work.

% Forget a Variable using f(Variable).. If you wish to clear all variable names, do f()..

% For all the internal commands, use
% help().

%%%% Syntax. Try them in shell.

Variable_names_must_begin_with_a_capital_letter = 3.

atoms_are_literals_with_a_lower_case.

'An atom should be enclosed in single quotes if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (_), or @.'.

% true, false are Boolean atoms
true and false.
% Shortcut operators: andalso, orelse.

% Equality
5 =:= 5.
1 =/= 0.
5 =:= 5.0.
% Comparison with conversion
5 == 5.0.
5 /= 5.0.

% Ordering: < > =< >=
1 =< 1.
% number < atom < reference < fun < port < pid < tuple < list < bit string

T = {this,is,a,tuple}.
erlang:element(2, T).

[a, list, 1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].

% Strings are lists
[97, 98, 99]. % "abc"

% Pattern matching on lists
[Head|Tail] = [1,2,3,4].

% List comprehension
[X || X <- lists:seq(1,10), X rem 2 =:= 0].

% I easily forget about bit syntax. If you like, consult
% http://learnyousomeerlang.com/starting-out-for-real#bit-syntax

% Types can be converted using functions type1_to_type2
% Types can be checked using is_type

% Raising exceptions
error(badarith).
exit(permission_denied).
throw(bad_arity).

% Concurrency
spawn(fun() -> io:format("~p~n",[2 + 2]) end).
self(). % My PID
self() ! hello. % Send a message
flush(). % Flush my mailbox
