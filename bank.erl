-module(bank).
-export([add/2, withdraw/2, balance/1, new/0, test/0, bank/1]).

% bank: A single-account bank
% * * * * * * * * * * * * * * * * * * * * 
% First Erlang programming task in PARADIS
% run new() to create a new bank process and
% use the verbs to manipulate the bank.
% 
% See test/0 for an example.
%
% Author: Billy (github.com/chrysophylax)
% Date: 2017-01-29

test() ->
    Joe = new(),
    ok = add(Joe, 10),
    ok = add(Joe, 20),
    30 = balance(Joe),
    ok = withdraw(Joe, 15),
    15 = balance(Joe),
    insufficient_funds = withdraw(Joe,20),
    hooray.


new() ->
    spawn(bank, bank, [0]).

add(Pid, X) ->
    rpc(Pid, {add, X}).

withdraw(Pid, X) ->
    rpc(Pid, {withdraw, X}).

balance(Pid) ->
    rpc(Pid, balance).

rpc(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive
	{_From, Response} ->
	    Response;
	Unexpected ->
	    Unexpected
    end. 

bank(Balance) ->
    %% stateful proc loop with Balance as param
    receive
	{From, {add, Amount}} ->
	    From ! {self(), ok},
	    bank(Balance+Amount);
	{From, {withdraw, Amount}} when Amount > Balance -> 
	    From ! {self(), insufficient_funds},
	    % continue as usual
	    bank(Balance);
	{From, {withdraw, Amount}}  ->
	    From ! {self(), ok},
	    bank(Balance-Amount);
	{From, balance} ->
	    From ! {self(), Balance},
	    bank(Balance);
	terminate ->
	    ok;
	Msg ->
	    io:format("Got ~w", [Msg])

end.
