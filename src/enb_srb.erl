-module(enb_srb).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(SIB_PORT_BASE, 8000).
-define(UE_HOST, {127,0,0,1}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         send/4
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("lte_model/include/node_logger.hrl").


-record(state, {
          socket :: gen_udp:sctp_socket(),       % Listening socket
          owner :: pid(),
          rb   :: non_neg_integer()              % Radio Bearer
         }).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

send(Pid, UeId, RB, Data) when is_pid(Pid), is_number(UeId), is_number(RB) ->
    gen_server:cast(Pid, {send, UeId, RB, Data}).

start_link(RB,Owner) ->
    Id = list_to_atom("enb_srb_" ++ integer_to_list(RB)),
    gen_server:start_link({local, Id}, ?MODULE, [RB,Owner], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([RB,Owner]) ->
    %%W/O: don't kill the shell process. process_flag(trap_exit, true),
    Opts = [{active, once}, {mode, binary}],
    Port = ?SIB_PORT_BASE + RB,
    case gen_udp:open(Port, Opts) of
        {ok, Socket} ->
            ok = gen_udp:controlling_process(Socket, self()),
            ?INFO("ENB open SRB0 on port: ~p",[Port]),
            {ok, #state{socket=Socket,
                        owner=Owner,
                        rb=RB}};
        {error, Reason} ->
            {stop, Reason}               
    end.

handle_call(_Request, _From, State) ->
    ?ERROR("enb_srb:handle_call '~p'", [_Request]),
    {reply, ok, State}.

handle_cast({send, UeId, RB, Data}, State=#state{socket=S}) ->
    UePort = ?SIB_PORT_BASE + 100 * UeId + RB,
    ?INFO(">> ENB sending on: ~p",[UePort]),
    gen_udp:send(S, ?UE_HOST, UePort, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    ?ERROR("enb_srb:handle_cast '~p'", [_Msg]),
    {noreply, State}.

%% handle_info(Info, State) ->

%%         %% Signal the network driver that we are ready to accept another connection
%%         inet:setopts(ListSock, [{active, once}]), 

handle_info({udp,_Sock,_RA,RP,Data}, State=#state{owner=Owner,socket=Socket,rb=RB}) ->
    PortBase = RP - ?SIB_PORT_BASE,    
    case {round(PortBase / 100), PortBase rem 100} of
        {UeId, RemRB} when RB == RemRB ->
            ?INFO("ENB handle msg from UE ~p SRB ~p / ~p",[UeId,RemRB,Owner]),
            Owner ! {ccch, RB, UeId, Data};
        {UeId, WrongRB} -> 
            ?ERROR("ENB ignore UE ~p mesage wrong sib port SIB: ~p/=~p", [UeId,RB,WrongRB])
    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
            
handle_info(Info, State) ->
    ?ERROR("enb_srb:handle_info - ~p", Info),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

    

