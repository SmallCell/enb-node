
-module(enb_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(EnbId) ->
    supervisor:start_link(?MODULE, [EnbId]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([EnbId]) ->
    enb_node:create(EnbId),
    enb_node:register(EnbId, enb_node_sup, self()),
    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Logical SRB CH supervisor
       {   enb_srb_sup,
           {enb_srb_sup,start_link, []},
           permanent,                               % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       },
       %% Logical DRB CH supervisor
       {   enb_drb_sup,
           {enb_drb_sup,start_link, []},
           permanent,                               % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       },
       %% UE Handler supervisor
       {   enb_ue_sup,
           {enb_ue_sup,start_link, []},
           permanent,                              % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       }
       %% %% RRC instance
       %% {   ue_rrc,                          % Id       = internal id
       %%     {ue_rrc,start_link,[]}, % StartFun = {M, F, A}
       %%     permanent,                               % Restart  = permanent | transient | temporary
       %%     2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
       %%     worker,                                  % Type     = worker | supervisor
       %%     [ue_rrc]                           % Modules  = [Module] | dynamic
       %% }
      ]
     }
    }.

