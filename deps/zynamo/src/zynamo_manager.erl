%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc Ring manager, records our current view of the ring.

%% Copyright 2010-2011 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(zynamo_manager).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    get_ring/0,
    sync_ring/2,
    save/0,
    leave/0,
    join/0,
    nodeup/1,
    nodedown/1,
    set_service/4,
    list_services/0,
    list_services/1,
    locate_service/2
]).

-type ring() :: zynamo_ring:ring().

%% @doc The ring state. The complete ring and the buckets per node.
%%      The 'past' bucket list is the state without joining nodes
%%      The 'future' bucket list is the state without leaving nodes
-record(state, {
            ring :: ring(),
            service_monitors = [] :: [{{atom(), atom()}, reference(), pid()}],
            past = [] :: [{integer(), integer(), node()}],
            future = [] :: [{integer(), integer(), node()}]
    }).


%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> 
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Fetch the current ring
-spec get_ring() -> {ok, ring()} | {error, term()}.
get_ring() ->
    gen_server:call(?MODULE, get_ring, infinity).

%% @doc Sync the ring with a ring from another node
%% @type sync_ring(node(), ring()) -> no_change | {ok, ring()}
-spec sync_ring(node(), ring()) -> {ok, ring()} | no_change | {error, term()}.
sync_ring(FromNode, Ring) ->
    gen_server:call(?MODULE, {do_sync_ring, FromNode, Ring}, infinity).


%% @doc Save the ring to disk, which will be used for a later reboot.
-spec save() -> ok.
save() ->
    gen_server:cast(?MODULE, save).

%% @doc Leave the ring
-spec leave() -> ok.
leave() ->
    gen_server:cast(?MODULE, leave).

%% @doc Join the ring
-spec join() -> ok.
join() ->
    gen_server:cast(?MODULE, join).

%% @doc Set the node's state to 'up'
-spec nodeup(node()) -> ok.
nodeup(Node) ->
    gen_server:cast(?MODULE, {nodeup, Node}).

%% @doc Set the node's state to 'down'
-spec nodedown(node()) -> ok.
nodedown(Node) ->
    gen_server:cast(?MODULE, {nodedown, Node}).

%% @doc Add a service, track its availability
-spec set_service(atom(), atom(), pid(), term()) -> ok.
set_service(Site, Service, Pid, GossipState) ->
    gen_server:cast(?MODULE, {set_service, Site, Service, Pid, GossipState}).

%% @doc Return the list of available services on the ring
-spec list_services() -> {ok, [{atom(), atom()}]} | {error, term()}.
list_services() ->
    gen_server:call(?MODULE, list_services, infinity).

%% @doc Return the list of available services for a site on the ring
-spec list_services(atom()) -> {ok, [atom()]} | {error, term()}.
list_services(Site) ->
    gen_server:call(?MODULE, {list_services, Site}, infinity).


%% @doc Return a list of (random) available service Pids, local Pid first.
-spec locate_service(atom(), atom()) -> {ok, [ node() ]} | {error, term()}.
locate_service(Site, Service) ->
    gen_server:call(?MODULE, {locate_service, Site, Service}, infinity).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init(list()) -> {ok, #state{}} | {ok, #state{}, integer()} | ignore | {stop, term()}.
init(_Args) ->
    % process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    % Try to start with the last known ring state
    Ring = case do_read() of
               {ok, R} -> zynamo_ring:resume(zynamo_ring:set_nodes_down(R));
               {error, _Reason} -> zynamo_ring:new()
           end,
    zynamo_event:up(),
    {ok, new_ring(#state{}, Ring)}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({do_sync_ring, FromNode, OtherRing}, _From, #state{ring=MyRing} = State) ->
    {Change, NewRing} = zynamo_ring:sync(OtherRing, MyRing),
    do_sync_node_state(FromNode, OtherRing, NewRing),
    case Change of
        changed ->
            save(),
            zynamo_event:changed(),
            service_events(MyRing, NewRing);
        nochange ->
            nop
    end,
    State1 = new_ring(State, NewRing),
    case zynamo_ring:is_equal(OtherRing, NewRing) of
        false -> {reply, {ok, NewRing}, State1};
        true -> {reply, ok, State1}
    end;

handle_call(get_ring, _From, #state{ring=Ring} = State) ->
    {reply, {ok, Ring}, State};

handle_call(list_services, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:list_services(Ring)}, State};

handle_call({list_services, Site}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:list_services(Site, Ring)}, State};

handle_call({locate_service, Site, Service}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:locate_service(Site, Service, Ring)}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(save, #state{ring=Ring} = State) ->
    do_save(Ring),
    {noreply, State};

%% @doc Leave the ring.
handle_cast(leave, #state{ring=Ring} = State) ->
    case zynamo_ring:is_member(node(), Ring) of
        true ->
            {_Changed, NewRing} = zynamo_ring:bye(Ring),
            {noreply, ring_changed(Ring, NewRing, State)};
        false ->
            {noreply, State}
    end;

%% @doc Take part in the ring again, overruling any earlier 'bye'
handle_cast(join, #state{ring=Ring} = State) ->
    case zynamo_ring:is_member(node(), Ring) of
        false ->
            {_Changed, NewRing} = zynamo_ring:hello(Ring),
            {noreply, ring_changed(Ring, NewRing, State)};
        true ->
            {noreply, State}
    end;

%% @doc Set the state of the node to 'up', typically done when we received
%%      gossip from the node.
handle_cast({nodeup, Node}, State) ->
    {noreply, do_nodeup(Node, State)};

%% @doc Set the state of the node to 'down'.
handle_cast({nodedown, Node}, State) ->
    {noreply, do_nodedown(Node, State)};

%% @doc Add a service to the services list of this node.
handle_cast({set_service, Site, Service, Pid, GossipState}, State) ->
    {noreply, do_set_service(Site, Service, Pid, GossipState, State)};


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle node up events, set the state of the node to 'up', 
%%      send the node our ring state.
handle_info({nodeup, Node}, State) ->
    {noreply, do_nodeup(Node, State)};

%% @doc Handle node down events, set the state of the node to 'down'
handle_info({nodedown, Node}, State) ->
    {noreply, do_nodedown(Node, State)};

%% @doc Handle a 'down' from a service
%% @todo Log the service down event
handle_info({'DOWN', MRef, process, Pid, _Reason}, #state{service_monitors=Monitors} = State) ->
    case lists:keyfind(MRef, 2, Monitors) of
        {{Site, Service}, MRef, Pid} ->
            % Delete the service, gossip the deletion
            {noreply, do_set_service(Site, Service, undefined, undefined, State)};
        {{_Site, _Service}, MRef, _OtherPid} ->
            % Monitor on a service that has been deleted, dispose the monitor and ignore
            Monitors1 = lists:keydelete(MRef, 2, Monitors),
            {noreply, State#state{service_monitors=Monitors1}};
        _ ->
            {noreply, State}
    end;
    
%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc Tell all ring members that we are going down.
terminate(_Reason, #state{ring=Ring}) ->
    [ gen_server:cast({?MODULE, Node}, {nodedown, node()}) || Node <- zynamo_ring:nodes(Ring) ],
    zynamo_event:down(),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

-spec do_save(ring()) -> ok.
do_save(Ring) ->
    {ok, Dev} = file:open(ring_file(), [write]),
    ok = file:write(Dev, <<
            "% THIS FILE IS AUTOMATICALLY GENERATED, DO NOT CHANGE WHILE RUNNING.",10,
            "%", 10,
            "% This file contains the latest ring state.",10,
            "% Delete this file to reset the ring to its defaults (when not running).",10,
            10>>),
    Data = io_lib:format("~p.~n", [Ring]),
    ok = file:write(Dev, iolist_to_binary(Data)),
    ok = file:close(Dev).


-spec do_read() -> {ok, ring()} | {error, term()}.
do_read() ->
    case catch file:consult(ring_file()) of
        {ok, Rings} ->
            {ok, hd(Rings)};
        {error, _Reason} = Error ->
            Error
    end.


do_nodeup(Node, #state{ring=Ring} = State) ->
    case zynamo_ring:get_node_state(Node, Ring) of
        down ->
            Ring1 = zynamo_ring:set_node_state(Node, up, Ring),
            zynamo_event:nodeup(Node),
            zynamo_gossip:gossip_ring(Node),
            service_events(Ring, Ring1),
            State#state{ring=Ring1};
        undefined ->
            zynamo_gossip:gossip_ring(Node),
            State;
        up ->
            State
    end.


do_nodedown(Node, #state{ring=Ring} = State) ->
    case zynamo_ring:get_node_state(Node, Ring) of
        up ->
            Ring1 = zynamo_ring:set_node_state(Node, down, Ring),
            zynamo_event:nodedown(Node),
            service_events(Ring, Ring1),
            State#state{ring=Ring1};
        undefined ->
            State;
        down ->
            State
    end.


do_sync_node_state(FromNode, OtherRing, MyRing) ->
    case zynamo_ring:get_node_state(FromNode, MyRing) of
        up -> nop;
        down -> nodeup(FromNode);
        undefined -> nop
    end,
    Me = node(),
    [ sync_node_state(N, OtherRing, MyRing) || N <- zynamo_ring:nodes(MyRing), N /= FromNode, N /= Me ].

    sync_node_state(Node, OtherRing, MyRing) ->
        S = zynamo_ring:get_node_state(Node, OtherRing),
        case zynamo_ring:get_node_state(Node, MyRing) of
            S -> 
                nop;
            up ->
                % Other node claims this node is down
                case net_adm:ping(Node) of
                    pong -> nop;
                    pang -> nodedown(Node)
                end;
            down ->
                % Other node claims this node is up
                case net_adm:ping(Node) of
                    pong -> zynamo_gossip:invite_gossip(Node);
                    pang -> nop
                end;
            undefined -> 
                nop
        end.


do_set_service(Site, Service, Pid, GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
    case zynamo_ring:get_local_service(Site, Service, Ring) of
        false -> 
            case Pid of
                undefined -> State;
                _ -> update_service(Site, Service, Pid, GossipState, State)
            end;
        {{Site,Service}, Pid, GossipState} ->
            State;
        {{Site,Service}, Pid, _OldGossipState} ->
            {ok, NewRing} = zynamo_ring:set_local_service(Site, Service, Pid, GossipState, Ring),
            State#state{ring=NewRing};
        {{Site,Service}, OtherPid, _OldGossipState} ->
            Monitors1 = case lists:keyfind(OtherPid, 3, Monitors) of
                            {{Site, Service}, OtherMRef, OtherPid} ->
                                erlang:demonitor(OtherMRef),
                                lists:keydelete(OtherPid, 3, Monitors);
                            _NotMonitored ->
                                Monitors
                        end,
            update_service(Site, Service, Pid, GossipState, State#state{service_monitors=Monitors1})
    end.

    % Service is changed, force gossip to prevent referrals to old service
    update_service(Site, Service, undefined, _GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
        Monitors1 = case lists:keyfind({Site, Service}, 1, Monitors) of
                        {{Site, Service}, OtherMRef, _OtherPid} ->
                            erlang:demonitor(OtherMRef),
                            lists:keydelete({Site, Service}, 1, Monitors);
                        false ->
                            Monitors
                    end,
        {ok, NewRing} = zynamo_ring:delete_local_service(Site, Service, Ring),
        zynamo_gossip:push_ring(),
        service_events(Ring, NewRing),
        State#state{ring=NewRing, service_monitors=Monitors1};
    update_service(Site, Service, Pid, GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
        MRef = erlang:monitor(process, Pid),
        {ok, NewRing} = zynamo_ring:set_local_service(Site, Service, Pid, GossipState, Ring),
        zynamo_gossip:push_ring(),
        service_events(Ring, NewRing),
        State#state{ring=NewRing, service_monitors=[ {{Site,Service}, MRef, Pid} | Monitors ]}.


%% @doc Ring is changed, save the ring state, notify event listeners, gossip the new ring.
ring_changed(OldRing, NewRing, State) ->
    do_save(NewRing),
    zynamo_event:changed(),
    zynamo_gossip:push_ring(),
    service_events(OldRing, NewRing),
    new_ring(State, NewRing).


%% @doc Generate service up/down events for the services on the ring
service_events(OldRing, NewRing) ->
    NewServices = zynamo_ring:list_services(NewRing),
    case zynamo_ring:list_services(OldRing) of
        NewServices -> 
            ok;
        OldServices ->
            zynamo_event:servicechanged(NewServices),
            [ zynamo_event:servicedown(Site, Service) || {Site,Service} <- OldServices -- NewServices ],
            [ zynamo_event:serviceup(Site, Service) || {Site,Service} <- NewServices -- OldServices ],
            ok
    end.

%% @doc Set the new ring, recalc the past/future hash ring
new_ring(State, NewRing) ->
    State#state{
            ring=NewRing,
            past=zynamo_ring:ranges(past, NewRing),
            future=zynamo_ring:ranges(future, NewRing)
    }.


ring_file() ->
    case application:get_env(zynamo, ring_state_dir) of
        {ok, Dir} -> Dir;
        undefined -> filename:join([code:lib_dir(zynamo, priv), "ring-"++atom_to_list(node())])
    end.

