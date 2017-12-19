do
=====

A little transfer agent model

How to test
-----------

    $ rebar3 ct

How to tinker
-------------

		$ rebar3 shell


Where data structures are defined
---------------------------------

		$ apps/do/include/do.hrl

How to create banks
-------------------
	
		$ do_bank:create(bank_name).

How to transfer money
---------------------

		$ do_bank:transfer({bank1_name, account1_name}, {bank2_name, account2_name}, 500).

How to use transfer agent
-------------------------

		$ do_agent:transfer({bank1_name, account1_name}, {bank2_name, account2_name}, 20000).

How to check your balance
-------------------------

		$ do_bank:request_account_balance({bank_name, account_name}).

What needs to be improved/next steps
-------------------------------------

	1. Instead of modeling banks and their communication as gen_servers and message passing, it would be better to implement them as ranch servers with a pool of workers
	2. Forbid negative account balance and implement holding/account locks
	3. eunit tests for do_transfer and other parts
	4. retry counter just in case
	5. right now wire transfer cancellation is not safe and needs to be improved
	6. save intermediate transfer state to storage and ability to resume after server restart


Design decision
---------------

	Initially I wanted to implement fsm-based transaction workers, but I had not enough time so I had to make it simple. Though I like the idea of having a single source of truth, which is bank transfer log.

Non-instantaneous transfers
---------------------------

	My solution was build with non-instantaneous transfers in mind, so there's a waiting state for transaction. Though right now the whole bank can only process one transaction at a time and there is only one slot for transaction on hold, it can be easily improved with use of transfer ids and a simple map.