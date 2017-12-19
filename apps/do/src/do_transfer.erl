-module(do_transfer).
-include("do.hrl").
-export([wire_transfer/2]).
-export([transfer/2]).
-export([resume/1]).
-export([rollback/1]).

-type continuation() :: {[op()], [op()], transfer(), bic()}.
-export_type([continuation/0]).

wire_transfer(BIC, Transfer) ->
	apply_ops(?WIRE_RECEIVE_SCENARIO, Transfer, BIC).

transfer(BIC, #transfer{from = {BICFrom, _}, to = {BICTo, _}} = Transfer) ->
	Scenario = case BICFrom =:= BICTo of
		true -> ?LOCAL_TRANSFER_SCENARIO;
		false -> ?WIRE_SEND_SCENARIO
	end,
	apply_ops(Scenario, Transfer, BIC).

resume({Ops, Rollback, Transfer, BIC}) ->
	apply_ops(Ops, Rollback, Transfer, BIC).

rollback({_Ops, Rollback, Transfer, BIC}) ->
	rollback_ops(Rollback, Transfer, BIC).

-spec apply_op(op(), transfer(), bic()) -> transfer() | {wait, continuation()} | fail.
apply_op(commit, Transfer, BIC) ->
	do_bank:commit_transfer(BIC, Transfer),
	Transfer;
apply_op(check_limits, #transfer{amount = Amount} = Transfer, _BIC) ->
	case Amount of
		Amount when (Amount =< ?WIRE_TRANSFER_LIMIT) and (Amount > ?WIRE_TRANSFER_FEE) ->
			Transfer;
		_Amount ->
			fail
	end;
apply_op(fee, #transfer{from = From, amount = Amount} = Transfer, BIC) ->
	case transfer(BIC, #transfer{from = From, to = {BIC, BIC}, amount = ?WIRE_TRANSFER_FEE}) of
		ok ->
			Transfer#transfer{amount = Amount - ?WIRE_TRANSFER_FEE};
		_Error ->
			fail
	end;
apply_op(wire_request, #transfer{to = {ToBIC, _}} = Transfer, _BIC) ->
	case do_bank:wire_request(ToBIC, Transfer) of
		ok -> {wait, Transfer}
	end;
apply_op(wire_response, #transfer{from = {FromBIC, _}} = Transfer, _BIC) ->
	case do_bank:wire_response(FromBIC, Transfer) of
		ok -> Transfer
	end.


-spec apply_ops([op()], transfer(), bic()) -> ok | fail | {wait, continuation()}.
apply_ops(Ops, Transfer, BIC) ->
	apply_ops(Ops, [], Transfer, BIC).

apply_ops([], _Rollback, _Transfer, _BIC) ->
	ok;
apply_ops([Op | Rest], Rollback, Transfer, BIC) ->
	case apply_op(Op, Transfer, BIC) of
		fail ->
			rollback_ops(Rollback, Transfer, BIC);
		{wait, UpdatedTransfer} ->
			{wait, {Rest, [Op | Rollback], UpdatedTransfer, BIC}};
		UpdatedTransfer ->
			apply_ops(Rest, [Op | Rollback], UpdatedTransfer, BIC)
	end.




-spec rollback_op(op(), transfer(), bic()) -> transfer() | fail.
rollback_op(commit, _Transfer, _BIC) ->
	fail;
rollback_op(check_limits, Transfer, _BIC) ->
	Transfer;
rollback_op(fee, #transfer{from = From, amount = Amount} = Transfer, BIC) ->
	transfer(BIC, #transfer{from = {BIC, BIC}, to = From, amount = ?WIRE_TRANSFER_FEE}),
	Transfer#transfer{amount = Amount + ?WIRE_TRANSFER_FEE};
rollback_op(wire_request, #transfer{to = {ToBIC, _} = To, from = From} = Transfer, _BIC) ->
	case do_bank:wire_request(ToBIC, Transfer#transfer{to = From, from = To}) of
		ok -> Transfer
	end.


rollback_ops([], _Transfer, _BIC) ->
	fail;
rollback_ops([Op | Rest], Transfer, BIC) ->
	case rollback_op(Op, Transfer, BIC) of
		fail ->
			rollback_ops([Op | Rest], Transfer, BIC); %% TBD: retry counter + reporting
		RolledBackTransfer ->
			rollback_ops(Rest, RolledBackTransfer, BIC)
	end.