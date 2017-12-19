-type bic() :: atom().
-type account_id() :: {bic(), atom()}.
-type history() :: [transfer()].

-type op() :: hold 
						| check_limits 
						| fee 
						| wire_request 
						| wire_response 
						| commit.

-type transfer_scenario() :: [op()].
-define(LOCAL_TRANSFER_SCENARIO, [commit]).
-define(WIRE_SEND_SCENARIO, [check_limits, fee, wire_request, commit]).
-define(WIRE_RECEIVE_SCENARIO, [commit, wire_response]).

-record(transfer, {
	from :: account_id(),
	to :: account_id(),
	amount :: float()
}).
-type transfer() :: #transfer{}.

-define(WIRE_TRANSFER_LIMIT, 1000.0).
-define(WIRE_TRANSFER_FEE, 5.0).
-define(WIRE_TRANSFER_FAILURE_RATE, 30).