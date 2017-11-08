%%% File    : mod_muc_room.hrl
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

-define(MAX_USERS_DEFAULT, 200).

-define(SETS, gb_sets).
-define(DICT, dict).

-record(state, {room,
  host,
  server_host,
  jid,
  users = ?DICT:new(),
  history}).

-record(muc_online_users, {us,
       resource,
       room,
       host}).

-record(lqueue, {queue, len, max}).

-record(user, {jid,
           nick}).

-record(activity, {message_time = 0,
           presence_time = 0,
           message_shaper,
           presence_shaper,
           message,
           presence}).