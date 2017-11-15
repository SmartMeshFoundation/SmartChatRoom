%%% File    : aa_group_chat.erl
%%% Author  : Leon <leon@smartmesh.io>
%%% Purpose : Group chat support
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
-module(aa_group_chat).

-include("ejabberd.hrl").
-include("jlib.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reload_group_user/2,stop/1]).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  start_link/0,
  route_group_msg/3,
  is_group_chat/1,
  check_group_type/1
]).

-record(state, {}).

start() ->
  aa_group_chat_sup:start_child().

stop(Pid)->
  gen_server:cast(Pid,stop).

start_link() ->
  gen_server:start_link(?MODULE,[],[]).

route_group_msg(From,To,Packet)->
  {ok,Pid} = start(),
  ?DEBUG("###### route_group_msg_001 ::::> {From,To,Packet}=~p",[{From,To,Packet}]),
  gen_server:cast(Pid,{route_group_msg,From,To,Packet}),
  ?DEBUG("###### route_group_msg_002 ::::> {From,To,Packet}=~p",[{From,To,Packet}]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
  {ok,#state{}}.

handle_cast({route_group_msg,From,To,Packet}, State) ->
  try
    handle_call({route_group_msg,From,To,Packet},[],State) 
  catch 
    Type:Reason ->
      Err = erlang:get_stacktrace(),
      ?ERROR_MSG("route_group_msg_error ~n\t~p,~n\t~p,~n\t~p",[Type,Reason,Err])
  end,
  {stop, normal, State};
handle_cast(stop, State) ->
  {stop, normal, State}.

notify_group_disbanded(#jid{user=FromUser,server=_FromDomain}=From,#jid{user=GroupId,server=GDomain}=To,Packet,[TO_ID,FDomain,NewTo]) ->
    {X,E,Attr,_} = Packet,
    RAttr = lists:map(fun({K,V})->
      case K of
        "id"->          {K,TO_ID};
        "from" ->       {K,GroupId++"@"++GDomain};
        "to" ->         {K,FromUser++"@"++FDomain};
        "type" ->       {K,"normal"};
        "msgtype" ->    {K,"system"}; 
        _ ->            {K,V} 
      end
    end,Attr),
    J1 = {[{<<"groupid">>,list_to_binary(GroupId)},
           {<<"groupname">>,<<"">>},
           {<<"groupmember">>,[]},
           {<<"type">>,<<"15">>}
                 ]},
    ExitJson = jiffy:encode(J1),
    Body = [{xmlelement,"body",[],[{xmlcdata,ExitJson}]}],
    RPacket = {X,E,RAttr,Body},
    aa_hookhandler:handle_cast({group_chat_filter,From,NewTo,RPacket,false},#state{}),
    case ejabberd_router:route(To,From,RPacket) of
      ok ->
        %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
        ?DEBUG("###### route_group_type15 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]),
        {ok,ok};
      Err ->
        ?DEBUG("###### route_group_type15 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]),
        {error,Err}
    end.
%% =================================================
%% Inform users that they have left the room
%% =================================================

notify_out_group(#jid{user=FromUser,server=_FromDomain}=From,#jid{user=GroupId,server=GDomain}=To,Packet,[TO_ID,FDomain,NewTo, Groupname,Groupmember]) ->
    {X,E,Attr,_} = Packet,
    RAttr = lists:map(fun({K,V})->
      case K of
        "id"->                    {K,TO_ID};
        "from" ->                 {K,GroupId++"@"++GDomain};
        "to" ->                   {K,FromUser++"@"++FDomain};
        "type" ->                 {K,"normal"};
        "msgtype" ->              {K,"system"};
        _ ->                      {K,V} 
      end
    end,Attr),
    StructJson = {[{<<"groupid">>,list_to_binary(GroupId)},
             {<<"groupname">>,Groupname},
             {<<"groupmember">>,Groupmember},
             {<<"type">>,<<"14">>}
             ]},
    Json = jiffy:encode(StructJson),
    Body = [{xmlelement,"body",[],[{xmlcdata,Json}]}],
    RPacket = {X,E,RAttr,Body},
    aa_hookhandler:handle_cast({group_chat_filter,From,NewTo,RPacket,false},#state{}),
    case ejabberd_router:route(To,From,RPacket) of
      ok ->
        ?DEBUG("route_group_type14 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]);
        %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false});
      Err ->
        ?DEBUG("route_group_type14 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]) 
    end.

handle_call({route_group_msg,#jid{user=FromUser,server=FDomain}=From,#jid{user=GroupId,server=_GDomain}=To,Packet}, _From, State) ->
  MID= xml:get_tag_attr_s("id", Packet),
  Domain = get_group_host_domain(FDomain),
  {M,S,SS} = now(),
  TO_ID = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  NewTo = #jid{user= FromUser, server = Domain, resource = [], luser = FromUser, lserver = Domain, lresource = []},
  
  case get_user_list_by_group_id(Domain,GroupId) of 
    {not_found,_,_,_,_} ->
      notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]);
    {ok,UserList,Groupmember,Groupname,Masklist} ->
      case UserList of 
        [] ->
          notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]);
        _ ->
          RUserList = UserList,
          case lists:member(list_to_binary(FromUser),RUserList) of
            false ->
              notify_out_group(From,To,Packet,[TO_ID,FDomain,NewTo, Groupname,Groupmember]);
            true -> 
              Roster = lists:map(fun(User)-> 
                UID = binary_to_list(User),
                #jid{user=UID,server=Domain,luser=UID,lserver=Domain,resource=[],lresource=[]} 
              end,UserList),
              ?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
              ?DEBUG("group_message_title_~p src_msg_id=~p ; roster_size=~p",[GroupId,MID,length(Roster)]),
              lists:foreach(fun(Target)-> 
                route_msg(From,Target,Packet,GroupId,Groupmember,Groupname,Masklist,MID) 
              end,Roster) ,
              aa_log:save_to_log({group,From,To,Packet,{GroupId,Groupname}}, Domain)
          end
      end;
    Err ->
      ?ERROR_MSG("group_msg_error ~p",Err),
      error
  end,  
  {reply,[],State}.


handle_info({cmd,_Args},State)->
  {noreply,State}.

terminate(_Reason,_State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_group_host_domain(Domain)->
  DomainTokens = string:tokens(Domain,"."),
  case length(DomainTokens) > 2 of
    true->
      [_,D1,S2] = DomainTokens,
      D1++"."++S2;
    _->
      Domain
  end.

reload_group_user(Domain,GroupId) ->
  Response = get_user_list_by_group_id(do,Domain,GroupId),
  Group_cache_key = GroupId++"@"++Domain++"/group_cache",
  case Response of 
    {ok,[],[],[],[]} ->
      skip;
    {not_found,[],[],[],[]} ->
      aa_hookhandler:ecache_cmd(["DEL",Group_cache_key]);
    {ok,_,_,_,_} ->
      aa_hookhandler:ecache_cmd(["SET",Group_cache_key,erlang:term_to_binary(Response)]);
    _ ->
      skip
  end,
  Response.

get_user_list_by_group_id(Domain,GroupId)->
  case GroupId of 
    "cctest" ->
      {ok,[<<"cc1">>,<<"cc2">>,<<"cc3">>],[],[],[]};
    _ ->
      case ejabberd_config:get_local_option({group_cache_enable,Domain}) of 
        true ->
          get_user_list_by_group_id(cache,Domain,GroupId);
        _ ->
          get_user_list_by_group_id(do,Domain,GroupId)
      end
  end.
get_user_list_by_group_id(cache,Domain,GroupId) ->
  Group_cache_key = GroupId++"@"++Domain++"/group_cache",
  case aa_hookhandler:ecache_cmd(["GET",Group_cache_key]) of
    {ok,Bin} when erlang:is_binary(Bin) ->
      erlang:binary_to_term(Bin);
    _ ->
      reload_group_user(Domain,GroupId)
  end;
get_user_list_by_group_id(do,Domain,GroupId) when is_binary(Domain) ->
  get_user_list_by_group_id(do,binary_to_list(Domain),GroupId);
get_user_list_by_group_id(do,Domain,GroupId) when is_list(Domain) ->
  ?DEBUG("###### get_user_list_by_group_id :::> Domain=~p ; GroupId=~p",[Domain,GroupId]),
  GroupId_bin = case is_binary(GroupId) of 
    true -> 
      GroupId ; 
    _->
      list_to_binary(GroupId)
  end,

    Method = <<"getUserList">>,
  Params = {[{<<"groupId">>,GroupId_bin}]} ,
    case aa_packet_filter:call_http(Domain,Method,Params) of
      {ok,Entity} ->
      UserList = ej:get({<<"userlist">>},Entity),
    Groupmember = ej:get({<<"groupmember">>},Entity),
    Masklist    = ej:get({<<"masklist"   >>},Entity),
    Groupname   = ej:get({<<"groupname"  >>},Entity),
    {ok,UserList,Groupmember,Groupname,Masklist};
    {error,Error} ->
    ?ERROR_MSG("no group member error .. ~p",[Error]),
        {not_found,[],[],[],[]};
    Exception ->
    ?ERROR_MSG("no group member exception .. ~p",[Exception]),
    {ok,[],[],[],[]}
  end.

get_msgtype(Attr) ->
  D = dict:from_list(Attr),
    _MT = case dict:is_key("msgtype",D) of 
    true-> 
      case dict:fetch("msgtype",D) of
        "system" ->
          "system";
        _ ->
          "groupchat"
      end;
    _-> "groupchat" 
  end.
route_msg(#jid{user=FromUser}=From,#jid{user=User,server=Domain}=To,Packet,GroupId,Groupmember,Groupname,Masklist,MID) ->
  case FromUser=/=User of
    true->
      {X,E,Attr,_Body} = Packet,
      {M,S,SS} = now(),
      MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
          MT = get_msgtype(Attr),
      TO_ID = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
      Mask = case lists:member(list_to_binary(User),Masklist) of true -> "1"; false-> "0" end,
      % -------------------------------------------------
      % Hidden message special treatment
      % -------------------------------------------------
      RAttr0 = lists:map(fun({K,V})-> 
        case K of 
          "id" -> 
            if
              Mask =:= "1" ->
                {K,"hide_msg_"++TO_ID};
              true->
                {K,TO_ID}
            end;
          "to" -> {K,User++"@"++Domain};
          "msgtype" -> {K,MT};  
          "msgTime" -> skip;
          _-> {K,V} 
        end 
      end,Attr),
      RAttr1 = [{"mask",Mask},{"groupid",GroupId}|RAttr0],
      RAttr2 = lists:append([Kv||Kv<-RAttr1,Kv=/=skip],[{"msgTime",MsgTime}]),

      %% TODO Groupmember
      [JSON] = aa_log:get_text_message_from_packet(Packet), 
      StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
            J1 = ej:set({<<"groupmember">>},StructJson, Groupmember),
            J2 = ej:set({<<"groupname">>},J1, Groupname),
            RJ0 = ej:set({<<"mask">>},J2, list_to_binary(Mask)),
      J4B = jiffy:encode(RJ0),
      ?DEBUG("GROUP ::::> J4B=~p",[J4B]),
      RBody = [{xmlelement,"body",[],[{xmlcdata,J4B}]}],
      RPacket = {X,E,RAttr2,RBody},
      aa_hookhandler:handle_cast({group_chat_filter,From,To,RPacket,false},#state{}),
      case ejabberd_router:route(From, To, RPacket) of
        ok ->
          ?DEBUG("group_message_~p src_msg_id=~p ; target_msg_id=~p ; from_user=~p ; to_user=~p ; body=~s",
                        [GroupId,MID,TO_ID,FromUser,User,J4B]),
          ?DEBUG("###### route_group_msg 003 OK :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
          %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
          {ok,ok};
        Err ->
          ?DEBUG("###### route_group_msg 003 ERR=~p :::> {From,To,RPacket}=~p",[Err,{From,To,RPacket}]),
          {error,Err}
      end;
    _ ->
      {ok,skip}
  end.

is_group_chat(#jid{server=Domain}=To)->
  DomainTokens = string:tokens(Domain,"."),
  Rtn = case length(DomainTokens) > 2 of 
    true ->
      [G|_] = DomainTokens,
      (G=:="group") or (G =:= "super_group");
    _ ->
      false
  end,
  ?DEBUG("##### is_group_chat ::::>To~p ; Rtn=~p",[To,Rtn]),
  Rtn.
check_group_type(Domain) when is_binary(Domain) ->
  check_group_type(binary_to_list(Domain));

check_group_type(Domain) when is_list(Domain) ->
  DomainTokens = string:tokens(Domain,"."),
  [G|_] = DomainTokens,
  Rtn =
    if
      G =:= "group"->
        "group";
      G =:= "super_group"->
        "super_group";
      true->
        false
    end,
  ?DEBUG("##### check_group_type ::::>Rtn=~p",[Rtn]),
  Rtn.

    
