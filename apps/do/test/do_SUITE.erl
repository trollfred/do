-module(do_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([collect_debts/1]).
 
all() -> [collect_debts].
 
collect_debts(_Config) ->
	application:ensure_all_started(do),
	{ok, _} = do_bank:create(bank_a),
	{ok, _} = do_bank:create(bank_b),
	ok = do_agent:transfer({bank_a, jose}, {bank_b, maria}, 20000),
	true = 20000 == do_bank:request_account_balance({bank_b, maria}),
	true = -20105 == do_bank:request_account_balance({bank_a, jose}),
	ok = do_agent:transfer({bank_b, antonio}, {bank_b, maria}, 20000),
	true = 40000 == do_bank:request_account_balance({bank_b, maria}),
	true = -20000 == do_bank:request_account_balance({bank_b, antonio}),
	ok.
