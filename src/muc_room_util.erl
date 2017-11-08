%%% File    : muc_room_util.erl
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

-module(muc_room_util).
-author('leon@smartmesh.io').

-include("mod_muc_room.hrl").
-include("jlib.hrl").
-compile(export_all).

lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
        len = 0,
        max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if
    Len >= Max ->
        Q3 = lqueue_cut(Q2, Len - Max + 1),
        #lqueue{queue = Q3, len = Max, max = Max};
    true ->
        #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).

is_user_online(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    ?DICT:is_key(LJID, StateData#state.users).

add_online_user(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:store(LJID,
            #user{jid = JID,
                  nick = Nick},
            StateData#state.users),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users}.

tab_add_online_user(JID, StateData) ->
    {LUser, LServer, LResource} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
        muc_online_users,
        #muc_online_users{us = US, resource = LResource, room = Room, host = Host}).

tab_count_user(JID) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    case catch ets:select(
         muc_online_users,
         [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
    Res when is_list(Res) ->
        length(Res);
    _ ->
        0
    end.

get_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
    mod_muc, max_users, ?MAX_USERS_DEFAULT).

add_new_user(From, Nick, {xmlelement, _, _Attrs, _Els} = Packet, StateData) ->
    MaxUsers = get_max_users(StateData),
    NUsers = dict:fold(fun(_, _, Acc) -> Acc + 1 end, 0,StateData#state.users),
    case (NUsers < MaxUsers) of
      false ->
          % max user reached 
          EEls = {xmlelement, "error", [],
                [{xmlelement,"body",[],[{xmlcdata,"max user reached"}]}]},
          Err = jlib:make_error_reply(
                Packet,
                EEls),
          ejabberd_router:route( jlib:jid_replace_resource(StateData#state.jid, Nick),From, Err),
          StateData;
      _ ->
         NewState = add_online_user(From, Nick, StateData),
         NewState
    end.

remove_online_user(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.
tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, LResource} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
        muc_online_users,
        #muc_online_users{us = US, resource = LResource, room = Room, host = Host}).

%% Send chat history when the user enters the room
send_history(JID,StateData,[]) -> send_history(JID,StateData);
send_history(JID,StateData,MsgTime) -> 
    %% Lack of digits, need to add one second
    MsgTime1=(list_to_integer(MsgTime)*1000)+1000,
    lists:foreach(
      fun({From, Packet}) ->
          {xmlelement, "message", Attrs, _} = Packet,
          PacketMsgTime = xml:get_attr_s(<<"msgTime">>,Attrs),
          case list_to_integer(PacketMsgTime) > MsgTime1 of
            true -> ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid, From#jid.user),JID,Packet);
            false -> nothing
          end
      end, lqueue_to_list(StateData#state.history)).
  
send_history(JID, StateData) ->
    lists:foreach(
      fun({From, Packet}) ->
          ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid, From#jid.user),JID,Packet)
      end, lqueue_to_list(StateData#state.history)).

%% Chat history is stored in #state.history
add_message_to_history(From, Packet, StateData) ->
    Q1 = lqueue_in({From, Packet},
           StateData#state.history),
    StateData#state{history = Q1}.
