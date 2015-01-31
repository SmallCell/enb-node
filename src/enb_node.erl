-module(enb_node).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


-export([create/1,
         delete/1,
         register/3,
         lookup/2
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(any(), non_neg_integer()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    enb_node_sup:start_link(0).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.


%%------------------------------------------------------------------------------
%% Common eNB helper functions
%%------------------------------------------------------------------------------
-spec create(integer()) -> ets:tid().
create(EnbId) ->
    ets:new(name(EnbId), [named_table, public,
                             {read_concurrency, true}]).

-spec delete(integer()) -> true.
delete(EnbId) ->
    true = ets:delete(name(EnbId)).

-spec register(integer(), atom(), pid() | ets:tid()) -> true.
register(EnbId, Name, Pid) ->
    true = ets:insert(name(EnbId), {Name, Pid}).

-spec lookup(integer(), atom()) -> term().
lookup(EnbId, Name) ->
    case ets:lookup(name(EnbId), Name) of
        [{Name, Pid}] ->
            Pid;
        [] ->
            undefined
    end.

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(EnbId) ->
    list_to_atom("enb_" ++ integer_to_list(EnbId)).
