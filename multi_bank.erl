-module(multi_bank).
-export([new/0, test/0, bank/1, create/2, balance/2, withdraw/3, add/3, lend/4]).

% multi_bank: A multiple-account bank
% * * * * * * * * * * * * * * * * * * * * 
% First Erlang programming task in PARADIS
% run new() to create a new bank process and
% use the verbs to manipulate the bank.
% 
% See test/0 for an example.
%
% Author: Billy (github.com/chrysophylax)
% Date: 2017-01-29

% Notes to self: maps are kinda ??
% - syntax from draft doesn't work :'(
% -> Map#{ Key } and friends is a no go,
% until/if/when implemented, use maps: module.
% - rather verbose ok/error tuples,
% -> expressivity is better?

new() ->
    spawn(multi_bank, bank, [#{}]).

create(Pid, Account) ->
    rpc(Pid, {create, Account}).

add(Pid, Account, X) ->
    rpc(Pid, {add, Account, X}).

withdraw(Pid, Account, X) ->
    rpc(Pid, {withdraw, Account, X}).

balance(Pid, Account) ->
    rpc(Pid, {balance, Account}).

lend(Pid, A, B, N) ->
    %% alias for transfer
    %% I prefer my name but spec reqs. lend/4
    %% cf. Wiktionary:
    %% To allow to be used by someone temporarily,
    %% on condition that it or its equivalent will be returned. 
   transfer(Pid, A, B, N).

transfer(Pid, AccountA, AccountB, Amount) ->
    rpc(Pid, {transfer, AccountA, AccountB, Amount}).

rpc(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive
	{_From, Response} ->
	    Response;
	Unexpected ->
	    Unexpected
    end.   
   
  
   
   
bank(Accounts) ->
    receive
	%% create an account
	{From, {create, Name}} ->
	    Exists = maps:is_key(Name, Accounts),
	    case Exists of
		true -> 
		    From ! {self(), {error, account_already_exists}},
		    bank(Accounts);
	        false -> 
		    Accounts1 = maps:put(Name, 0, Accounts),
		    From ! {self(), {ok, account_created}},	      
		    bank(Accounts1)
	    end;
	
	%% add to an account's balance
	%% returns ok tuple or error
	{From, {add, Name, Amount}} ->
	    Account = maps:find(Name, Accounts),
	    case Account of
		{ok, Balance} -> 
		    NewBalance = Balance+Amount,
		    Accounts1 = maps:update(Name, NewBalance, Accounts),
		    From ! {self(), {ok, balance_updated}},
		    bank(Accounts1);
		error ->
		    From ! {self(), {error, account_inexistent}},
		    bank(Accounts)
	    end;
	%% returns the balance for the account
	%% or error tuple
	{From, {balance, Name}} ->
	    Account = maps:find(Name, Accounts),
	    case Account of
		{ok, Balance} ->
		    From ! {self(), {ok, Balance}},
		    bank(Accounts);
		error ->
		    From ! {self(), {error, account_inexistent}},
		    bank(Accounts)
	    end;

	%% withdraws from an account's balance
	%% returns ok tuple or error
	{From, {withdraw, Name, Amount}} ->
	    Account = maps:find(Name, Accounts),
	    case Account of
		{ok, Balance} when Balance < Amount ->
		    % we're broke
		    From ! {self(), {error, insufficient_funds}},
		    bank(Accounts);
		{ok, Balance} ->
		    NewBalance = Balance-Amount,
		    Accounts1 = maps:update(Name, NewBalance, Accounts),
		    From ! {self(), {ok, balance_updated}},
		    bank(Accounts1);
		error ->
		    From ! {self(), {error, account_inexistent}},
		    bank(Accounts)
	    end;

	%% two-step transfer of money from 
	%% one account to another
	%% - will only transfer money
	%% - if no errors ('safe').
	{From, {transfer, A, B, Amount}} ->
	    Source = maps:find(A, Accounts),
	    Target = maps:find(B, Accounts),
	    case Source of
		{ok, SourceBalance} when SourceBalance < Amount ->
		    %% not enough funds
		    From ! {self(), {error, insufficient_funds}},
		    bank(Accounts);
		{ok, SourceBalance} ->
		    %% now for the tricky bit
		    case Target of
			{ok, TargetBalance} ->
			    NewSourceBalance = SourceBalance-Amount,
			    NewTargetBalance = TargetBalance+Amount,
			    Accounts1 = maps:update(A, NewSourceBalance, Accounts),
			    Accounts2 = maps:update(B, NewTargetBalance, Accounts1),
			    From ! {self(), {ok, transfer_complete}},
			    bank(Accounts2);
			error ->
			    From ! {self(), {error, account_inexistent}},
			    bank(Accounts)
		    end
	    end;
	
	%% exit our little banking loop
	   terminate ->
	    ok
end.

test() ->
    J = new(),
    {ok,_} = create(J, billy),
    {ok,_} = create(J, joseph),
    {error, _} = withdraw(J, billy, 500),
    {ok, _} = add(J, billy, 50),
    {ok, _} = withdraw(J, billy, 25),
    {ok, 25} = balance(J, billy),
    {error, insufficient_funds} = transfer(J, billy, joseph, 50),
    {ok, transfer_complete} = lend(J, billy, joseph, 10),
    {ok, 10} = balance(J, joseph),
    %% now joseph has to transfer back the money
    {ok, transfer_complete} = transfer(J, joseph, billy, 10),
    hooray.
