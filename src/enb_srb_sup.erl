-module(enb_srb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->    
      LogicalChannels = [
                         {LCID,                
                          {enb_srb,start_link,[LCID]},
                          permanent,
                          2000,                    
                          worker,                                
                          [enb_srb]
                         } || LCID <- lists:seq(0,2)],

    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      LogicalChannels      
     }
    }.
