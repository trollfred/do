-module(do_agent).

-export([transfer/3]).

-include("do.hrl").

-spec transfer(account_id(), account_id(), float()) -> ok.
transfer({FromBIC, _} = From, {ToBIC, _} = To, Amount) ->
	case FromBIC =:= ToBIC of
		true ->
			local_transfer(From, To, Amount);
		false ->
			wire_transfer(From, To, Amount)
	end.

local_transfer(From, To, Amount) ->
	do_bank:transfer(From, To, Amount).


wire_transfer(_From, _To, 0.0) ->
	ok;
wire_transfer(From, To, Amount) ->
	PartialAmount = lists:min([Amount + ?WIRE_TRANSFER_FEE, ?WIRE_TRANSFER_LIMIT]),
	case do_bank:transfer(From, To, PartialAmount) of
		ok ->
			wire_transfer(From, To, Amount - PartialAmount + ?WIRE_TRANSFER_FEE);
		fail ->
			wire_transfer(From, To, Amount)
	end.
