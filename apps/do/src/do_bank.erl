-module(do_bank).
-behaviour(gen_server).

-include("do.hrl").

-export([create/1]).
-export([transfer/2, transfer/3]).
-export([get_account_history/1]).
-export([request_account_balance/1]).

-export([wire_request/2]).
-export([wire_response/2]).

-export([commit_transfer/2]).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
	bic :: bic(),
	waiting_transfer :: {{_,_}, do_transfer:continuation()} | undefined
}).

create(BIC) ->
	supervisor:start_child(do_sup, [BIC]).


transfer({FromBIC, _} = From, To, Amount) ->
	transfer(FromBIC, #transfer{from = From, to = To, amount = Amount}).

transfer(BIC, Transfer) ->
	gen_server:call({global, {bank, BIC}}, Transfer).

commit_transfer(BIC, Transfer) ->
	ets:insert(BIC, Transfer).

get_account_history({BIC, _} = AccountId) ->
	lists:filter(filter_account_transfers(AccountId), ets:tab2list(BIC)).

request_account_balance(AccountId) ->
	lists:foldl(
		fun 
			(#transfer{to = To, amount = A}, B) when To =:= AccountId -> B + A;
			(#transfer{from = From, amount = A}, B) when From =:= AccountId -> B - A
		end,
		0,
		get_account_history(AccountId)
	).


wire_request(ToBIC, Transfer) ->
	gen_server:cast({global, {bank, ToBIC}}, Transfer).

wire_response(ToBIC, _Transfer) ->
	case rand:uniform(100) > ?WIRE_TRANSFER_FAILURE_RATE of
		true ->
			gen_server:cast({global, {bank, ToBIC}}, transfer_ok);
		false ->
			ok
	end.



start_link(BIC) ->
	gen_server:start_link({global, {bank, BIC}}, ?MODULE, BIC, []).

init(BIC) ->
	ets:new(BIC, [named_table, duplicate_bag]),
	{ok, #state{bic = BIC}}.

handle_call(Transfer, From, #state{bic = BIC} = S) ->
	case do_transfer:transfer(BIC, Transfer) of
		ok ->
			{reply, ok, S};
		fail ->
			{reply, fail, S};
		{wait, Continuation} ->
			{noreply, S#state{waiting_transfer = {From, Continuation}}, 4000}
	end.

handle_cast(transfer_ok, #state{waiting_transfer = {From, Cont}} = S) ->
	case do_transfer:resume(Cont) of
		ok ->
			gen_server:reply(From, ok);
		fail ->
			gen_server:reply(From, fail)
	end, 
	{noreply, S#state{waiting_transfer = undefined}};
handle_cast(transfer_ok, S) ->
	{noreply, S};
handle_cast(Transfer, #state{bic = BIC} = S) ->
	do_transfer:wire_transfer(BIC, Transfer),
	{noreply, S}.

handle_info(timeout, #state{waiting_transfer = {From, Cont}} = S) ->
	do_transfer:rollback(Cont),
	gen_server:reply(From, fail),
	{noreply, S#state{waiting_transfer = undefined}}.

filter_account_transfers(A) ->
	fun 
		(#transfer{to = To}) when To =:= A -> true;
		(#transfer{from = From}) when From =:= A -> true;
		(_) -> false
	end.