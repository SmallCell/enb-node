-module(enb_ue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_ue_handler/1, stop_ue_handler/1]).

%% Supervisor callbacks
-export([init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================
start_ue_handler(IMSI) ->
    supervisor:start_child (?MODULE,
                            {IMSI,           % Id  = internal id
                             {enb_ue,start_link,[IMSI]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [enb_ue]         % Modules  = [Module] | dynamic
    }).

stop_ue_handler(IMSI) ->
    supervisor:terminate_child (?MODULE, IMSI),
    supervisor:delete_child (?MODULE, IMSI).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->    
    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
      ]
     }
    }.


