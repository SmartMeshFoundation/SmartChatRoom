%%% File    : mod_muc_room.erl
%%% Author  : Leon <leon@smartmesh.io>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 16 Oct 2017 by Leon <leon@smartmesh.io>
%%%
%%%
%%% SmartChatRoom, Copyright Smartmesh
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_room).

-author('leon@smartmesh.io').

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3]).

-compile(export_all).

%% Nickname changed to #jid.user
route(Pid, From, _ToNick, Packet) ->
    Pid ! {route, From, From#jid.user, Packet}.

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,Creator, Nick, DefRoomOpts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(
      Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
           Creator, Nick, DefRoomOpts]).
    
start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,Creator, Nick, DefRoomOpts) ->
    gen_server:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
         RoomShaper, Creator, Nick, DefRoomOpts],[]).

init([Host, ServerHost, _Access, Room, HistorySize, _RoomShaper, Creator, _Nick, _DefRoomOpts]) ->
    process_flag(trap_exit, true),
    State = #state{
        host = Host,
        server_host = ServerHost,
        room = Room,
        jid = jlib:make_jid(Room, Host, ""),
        history = muc_room_util:lqueue_new(HistorySize)
    },
    ?INFO_MSG("Created MUC room ~s@~s by ~s",[Room, Host, jlib:jid_to_string(Creator)]),
    ?INFO_MSG("init .. state .. ~p~n",[State]),
  {ok, State}. 

handle_info({route,From,Nick,{xmlelement, "presence", _Attrs, _Els} = Packet},StateData) ->
  {M,S,SS} = now(), 
  _MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13), 
  StateData1 = process_presence(From, Nick, Packet, StateData),
  {noreply,StateData1};

handle_info({route, From, "",{xmlelement, "message", _Attrs, _Els} = Packet},StateData) ->
  lists:foreach(
  fun({_LJID, Info}) -> 
    ejabberd_router:route( 
      jlib:jid_replace_resource(StateData#state.jid,From#jid.user),
      Info#user.jid,
      Packet)
  end,?DICT:to_list(StateData#state.users)),
  StateData1 =
  muc_room_util:add_message_to_history(From,Packet,StateData),
  {noreply,StateData1};

handle_info({route, From, Nick, {xmlelement, "message", Attrs, _} = Packet}, StateData) ->
  ?INFO_MSG("info .. Nick = ~p",[Nick]),
  ?INFO_MSG("info .. StateData = ~p",[StateData]),
  Type = xml:get_attr_s("type", Attrs),
  _Lang = xml:get_attr_s("xml:lang", Attrs),
  case muc_room_util:is_user_online(From, StateData) of
    true ->
      case Type of
        "groupchat" -> 
            NewPacket=muc_room_util:add_msgTime(Packet),
            lists:foreach(
              fun({_LJID, Info}) -> 
                ejabberd_router:route( 
                  jlib:jid_replace_resource(StateData#state.jid,From#jid.user),
                  Info#user.jid,
                  NewPacket)
              end,?DICT:to_list(dict:erase(jlib:jid_tolower(From),StateData#state.users))),
              StateData1 =
              muc_room_util:add_message_to_history(From,NewPacket,StateData);
        _ ->
          ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid,From#jid.user), From, Packet),
          StateData1 = StateData
      end;
    _ ->
     StateData1 = StateData 
  end,
  {noreply, StateData1};



handle_info(Info,StateData) ->
  ?INFO_MSG("info .. ~p~n",[Info]),
  {noreply, StateData}.

handle_call(users,_From,StateData)->
  {reply,StateData#state.users,StateData};
handle_call(history,_From,StateData)->
  {reply,StateData#state.history,StateData};

handle_call(Call,_From,StateData) ->
  ?INFO_MSG("call .. ~p~n",[Call]),
  {reply,StateData,StateData}.

handle_cast(Cast,StateData) ->
  ?INFO_MSG("cast .. ~p~n",[Cast]),
  {noreply, StateData}.

%% Clear the room's route
terminate(Reason,StateData) ->
  ?INFO_MSG("Stopping MUC room ~s@~s",[StateData#state.room, StateData#state.host]),
  ?ERROR_MSG("terminate .. ~p~n",[Reason]),
  ?ERROR_MSG("stacktrace .. ~p~n",[erlang:get_stacktrace()]),
  ReasonT = "Room terminates",
  Packet = {xmlelement,"presence",[{type,unavailable}],
      [{xmlelement,"reason",[],[{xmlcdata,ReasonT}]}]},
  ?DICT:fold(
      fun(LJID,Info,_) ->
        Nick = Info#user.nick,
        ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid,Nick),
            Info#user.jid,Packet),
        muc_room_util:tab_remove_online_user(LJID,StateData)
      end,
      [],StateData#state.users),
      mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
          StateData#state.server_host),
  ok.

code_change(_OldVsn,StateData,_Extra) ->
  {ok, StateData}.
  
process_presence(From, Nick, {xmlelement, "presence", Attrs, _Els} = Packet, StateData) ->
  Type = xml:get_attr_s("type",Attrs),
  _Lang = xml:get_attr_s("xml:lang",Attrs),
  _StateDate1 = 
  case Type of
    "unavailable" -> %% User left the room
      case muc_room_util:is_user_online(From, StateData) of
        true ->
          _NewState = muc_room_util:remove_online_user(From, StateData);
        _ ->
          StateData
      end;
    "" -> %% User join the room 
      case muc_room_util:is_user_online(From, StateData) of
        false ->
          NewState = muc_room_util:add_new_user(From,Nick, Packet,StateData),
          %% User join room notification 
          ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid,From#jid.user),From,Packet),
          muc_room_util:send_history(From, StateData,xml:get_attr_s("msgTime",Attrs)),
          NewState;
        _ ->
          muc_room_util:send_history(From, StateData,xml:get_attr_s("msgTime",Attrs)),
          StateData
      end
  end.