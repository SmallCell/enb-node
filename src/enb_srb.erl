-module(enb_srb).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(SIB_PORT_BASE, 8000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("lte_model/include/node_logger.hrl").


-record(state, {
          socket :: gen_udp:sctp_socket(),       % Listening socket
          rb   :: non_neg_integer()              % Radio Bearer
         }).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(RB) ->
    Id = list_to_atom("enb_srb_" ++ integer_to_list(RB)),
    gen_server:start_link({local, Id}, ?MODULE, [RB], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([RB]) ->
    %%W/O: process_flag(trap_exit, true),
    Opts = [{active, once}, {mode, binary}],
    case gen_udp:open(?SIB_PORT_BASE + RB, Opts) of
        {ok, Socket} ->
            ok = gen_udp:controlling_process(Socket, self()),
            {ok, #state{socket=Socket,
                       rb=RB}};
        {error, Reason} ->
            {stop, Reason}               
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info(Info, State) ->

%%         %% Signal the network driver that we are ready to accept another connection
%%         inet:setopts(ListSock, [{active, once}]), 

handle_info(Info={udp,_Sock,_RA,_RP,Data}, State=#state{socket=Socket,rb=RB}) ->
    {UeId, Rnti, PDU} = lte:decode_mac_pdu(Data),
    handle_srb_msg(RB, UeId, Rnti, PDU),
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

%% @doc: RB, UeId, Rnti, PDU
handle_srb_msg(0, UeId, Rnti, PDU) when UeId < 0 ->
    ?INFO("CCCH: Received RRCConnectionRequest: ~p/~p", [UeId, Rnti]),
    


