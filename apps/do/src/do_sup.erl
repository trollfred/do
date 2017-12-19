%%%-------------------------------------------------------------------
%% @doc do top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(do_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [bank_server()]} }.

%%====================================================================
%% Internal functions
%%====================================================================

bank_server() ->
	#{id => do_bank,
    start => {do_bank, start_link, []},
    restart => permanent,
    shutdown => 5,
    type => worker,
    modules => [do_bank]
   }.
