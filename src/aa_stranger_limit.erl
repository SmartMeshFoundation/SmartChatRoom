%%% File    : aa_stranger_limit.erl
%%% Author  : Leon <leon@smartmesh.io>
%%% Purpose : Message restrictions for strangers
%%% Created : 20 Nov 2017 by Leon <leon@smartmesh.io>
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
-module(aa_stranger_limit).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-include("ejabberd.hrl").
-include("jlib.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([check_deel_stranger_limit/4,set_stranger_limit/3]).
-compile(export_all).

set_stranger_limit(UserId,Limit,Domain)->
  Key = binary_to_list(UserId)++"@"++binary_to_list(Domain)++"_strange_limit_"++get_data_str(),
  aa_hookhandler:ecache_cmd(["SETEX",Key,integer_to_list(60*60*24),Limit]),
  ok.

check_deel_stranger_limit(#jid{luser = Fid,lserver = Domain} = _From,#jid{luser = Tid, lserver = TDomain} = _To,Packet,Is_save)->
  Msgtype= type_util:to_binary(xml:get_tag_attr_s("msgtype", Packet)),
  Isoffline = xml:get_tag_attr_s("isoff",Packet),
  [JSON] = aa_log:get_text_message_from_packet(Packet),  
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  IsLittleSecretary = aa_hookhandler:is_little_secretary(Domain,Fid) orelse aa_hookhandler:is_little_secretary(TDomain,Tid),

  if
    Isoffline == "1" -> true;
    Domain=/=TDomain ->
      true;
    Msgtype =:= <<"normalchat">>,Fid=/="stranger_limit",not IsLittleSecretary ->   
      FromBin = list_to_binary(Fid++"@"++Domain), 
      ToBin = list_to_binary(Tid++"@"++Domain),
      FriendLog = get_friend_log(StructJson,FromBin,ToBin,Domain),
      FriendLog1 = get_friend_log(StructJson,ToBin,FromBin,Domain),
      if
        FriendLog =/= <<"1">>;FriendLog1 =/= <<"1">> ->
          Key = Fid++"@"++Domain++"_strange_chat_"++Tid++"@"++Domain,
          Unlockey = Tid++"@"++Domain++"_strange_chat_"++Fid++"@"++Domain, 
          case aa_hookhandler:ecache_cmd(["GET",Unlockey]) of
            {ok,Bin} when is_binary(Bin)->
              if
                Is_save =:= send ->
                  case binary_to_term(Bin) of
                    {D,1}->
                      aa_hookhandler:ecache_cmd(["SETEX",Unlockey,integer_to_list(60*60*24*7),{D,1}]),
                      true;
                    {D,0}->
                      aa_hookhandler:ecache_cmd(["SETEX",Unlockey,integer_to_list(60*60*24*7),{D,1}]),
                      true;
                    _->
                      true
                  end;
                true->
                  true
              end;
            _->
              case aa_hookhandler:ecache_cmd(["GET",Key]) of
                {ok,Bin} when is_binary(Bin)->
                  case binary_to_term(Bin) of
                    {D,0}-> 
                      NowDate = date_str(),
                      if
                        D =:= NowDate ->
                          send_system_msg(Tid, TDomain,Fid,Domain,StructJson,"4",[],Is_save),
                          false;
                        true->
                          try     
                            check_self_limit(Tid, TDomain, Key,Fid,Domain,StructJson,Is_save)
                          catch
                            _:_->
                              ?ERROR_MSG("check self limit error R:~p",[erlang:get_stacktrace()]),
                              false
                          end
                      end;
                    {_D,1}->
                      true;
                    _->
                      try 
                        check_self_limit(Tid, TDomain, Key,Fid,Domain,StructJson,Is_save)
                      catch
                        _:_->
                          ?ERROR_MSG("check self limit error R:~p    ",[erlang:get_stacktrace()]),
                          false
                      end
                  end;  
                _->
                  try
                    check_self_limit(Tid,TDomain ,Key,Fid,Domain,StructJson,Is_save)
                  catch
                    _:_->
                      ?ERROR_MSG("check self limit error R:~p",[erlang:get_stacktrace()]),
                      false
                  end
              end
          end;
        true->
          true
      end;
    true->
      true
  end.
                    
check_self_limit(Tid,TDomain,LockKey,Fromid,Domain,StructJson,Is_save)->
  Key = Fromid++"@"++Domain++"_strange_limit_"++get_data_str(),
  case aa_hookhandler:ecache_cmd(["GET",Key]) of
    {ok,Limitbin} when erlang:is_binary(Limitbin) ->
      if
        Limitbin =:= <<"super">>->
          true;
        true->
          Limit = binary_to_integer(Limitbin),
          NowDate  = date_str(),
          if
             Limit > 0 ->
                  if
                    Is_save =:= send ->
                      aa_hookhandler:ecache_cmd(["SETEX",Key,integer_to_list(60*60*24),Limit-1]),
                      aa_hookhandler:ecache_cmd(["SETEX",LockKey,integer_to_list(60*60*24*7),term_to_binary({NowDate,0})]);
                    true ->
                      skip
                  end,
                  send_system_msg(Tid, TDomain, Fromid,Domain,StructJson,"1",[integer_to_list(Limit-1)],Is_save),
                  true;
             true->
                  send_system_msg(Tid, TDomain, Fromid,Domain,StructJson,"2",[],Is_save),
                  false
          end
      end;
    _->
      {M,S,SS} = now(),
      SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
      HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
      StructParam = {[ {<<"sn">>,list_to_binary(SN)}, {<<"service">>, <<"ejabberd">>}, {<<"method">>, <<"get_user_chat_number">>},{<<"params">>, {[{<<"uid">>, list_to_binary(Fromid)}]}}]}, 
      Form = "body="++ binary_to_list(jiffy:encode(StructParam)),
      case httpc:request(post,{ HTTPTarget ,[], ?HTTP_HEAD , Form },[],[] ) of
        {ok,{_,_,Body}}->
          DBody = jiffy:decode(list_to_binary(Body)),
          ?DEBUG("get_user_chat number req:~p response body :~p",[StructParam,DBody]),
          case ej:get({<<"number">>},DBody) of
              undefined -> 
                  ?ERROR_MSG("GET LIMIT FROM API NO HAVE Entity",[]),
                  false;
              <<"super">> ->
                  true;
              LimitBin ->
                  Limit = binary_to_integer(LimitBin),
                  NowDate  = date_str(),
                  if
                    Is_save =:= send ->
                      aa_hookhandler:ecache_cmd(["SETEX",Key,integer_to_list(60*60*24),Limit-1]), 
                      aa_hookhandler:ecache_cmd(["SETEX",LockKey,integer_to_list(60*60*24*7),term_to_binary({NowDate,0})]);
                    true ->
                      aa_hookhandler:ecache_cmd(["SETEX",Key,integer_to_list(60*60*24),Limit]) 
                  end,
                  send_system_msg(Tid, TDomain, Fromid,Domain,StructJson,"1",[integer_to_list(Limit-1)],Is_save),
                  true
          end;
         _->
          ?ERROR_MSG("GET LIMIT FROM API NO HAVE Entity",[]),
          false
          end
    end.
  

send_system_msg(Tid,TDomain,UId,Domain,_StructJson,Type,Args,send)->
  case aa_api_client:get_user_entry(TDomain, Tid) of
    {ok,StructJson}->
      UsernameUtf32 = ej:get({<<"name">>}, StructJson, <<"">>),
      Username = jiffy_utf8:fix(UsernameUtf32),
      Userimage = ej:get({<<"imageurl">>}, StructJson, <<"">>),
      Usergender = ej:get({<<"sex">>}, StructJson, <<"">>);
    _->
      Username = <<"">> ,
      Userimage = <<"">>,
      Usergender = <<"">>
  end,

  Lang = "zh",
  CountentKey = "system_500_"++Type,
  Countent = list_to_binary(get_itdoc_str(Lang,CountentKey,Args)),
  J1 = {[{<<"userid">>,list_to_binary(Tid)},
       {<<"username">>,Username},
       {<<"userimage">>,Userimage},
       {<<"usergender">>,Usergender},
       {<<"type">>,<<"500">>},
       {<<"mask">>,<<"0">>},
       {<<"friend_log">>,<<"0">>},
       {<<"content">>,Countent}]},
  Json = jiffy:encode(J1),
  Body = [{xmlelement,"body",[],[{xmlcdata,Json}]}],
  {M,S,SS} = now(),
  ID = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  From = "stranger_limit@"++Domain,
  To = UId++"@"++Domain,
  Attr = [{"id",ID},{"from",From},{"to",To},{"type","normal"},{"msgtype","system"}],
  Packet = {xmlelement,"message",Attr,Body},
  SFrom = #jid{user = "stranger_limit",server=Domain,resource=[],luser="stranger_limit",lserver=Domain,lresource=[]},
  STo = #jid{user = UId,server=Domain,resource=[],luser=UId,lserver=Domain,lresource=[]},
  ejabberd_router:route(SFrom,STo,Packet);


send_system_msg(_,_,_UId,_Domain,_Struct,_Type,_Args,_)->
  ok.
get_friend_log(_StructJson,FromBin,ToBin,Domain)->
  [FromStr,ToStr]  = [get_jid(binary_to_list(FromBin)),get_jid(binary_to_list(ToBin))],
  Key = "friend_log__"++FromStr++ToStr,
  case aa_hookhandler:ecache_cmd(["GET",Key]) of
    {ok,FriendLog} when erlang:is_binary(FriendLog) ->
      FriendLog;
    _->
      case call_http(Domain,<<"get_relation">>,FromBin,ToBin) of 
        {ok,Entity} ->  
          ?WARNING_MSG("-------~p~n",[Entity]),
          FriendLog = ej:get({<<"friend_log">>},Entity),
          aa_hookhandler:ecache_cmd(["PSETEX",Key,integer_to_list(1000*60*5),FriendLog]),
          FriendLog;
        _->
          <<"0">>
      end
  end.

get_itdoc_str(Lang,CountentKey,Args)->
  try
    case  ets:lookup(translations, {Lang, CountentKey}) of
        [{_, Trans}]->
        io_lib:format(Trans, Args);
      _->
        ""
    end
  catch
    _:_->
      ?ERROR_MSG("error:get_itdoc_str~p",[Lang,CountentKey,Args]),
      ""
  end.


get_data_str()->
  {Y,M,D} = date(),
  integer_to_list(Y)++integer_to_list(M)++integer_to_list(D).
  

get_jid(JIDStr) ->
  [U,DR] = string:tokens(JIDStr,"@"),
  [D|_] = string:tokens(DR,"/"),
  U++"@"++D.

call_http(Domain,Method,FromBin,ToBin)->
  {M,S,SS} = now(),
  SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
  ParamObj = {[ {<<"sn">>, list_to_binary(SN)}, {<<"service">>, <<"ejabberd">>}, {<<"method">>, Method},{<<"params">>, {[{<<"from">>, FromBin},{<<"to">>, ToBin}]}}]}, 
  ?WARNING_MSG("aa_packet_filter__call_http__paramObj=~p ; method=~p ; domain=~p ~nhttp_url=~p",[ParamObj,Method,Domain,HTTPTarget]),
  Form = "body="++ binary_to_list(jiffy:encode(ParamObj)),
  case httpc:request(post,{ HTTPTarget ,[], ?HTTP_HEAD , Form },[],[] ) of
    {ok, {_,_,Body}} ->
      DBody = jiffy:decode(list_to_binary(Body)),
      case ej:get({<<"success">>},DBody) of
        true ->
          case ej:get({<<"entity">>},DBody) of
            undefined ->
                error;
            Entity ->
              {ok,Entity}
          end;
        _ ->
          Entity = ej:get({<<"entity">>},DBody),
          ?ERROR_MSG("[aa_packet_filter__call_http__success_false] sn=~p ; entity=~p",[SN,Entity]),
          {fail,Entity}
      end;
    {error, Reason} ->
      ?ERROR_MSG("[aa_packet_filter__call_http__exception] sn=~p ; exception=~p",[SN,Reason]),
      {error,Reason}
  end.

date_str()->
  {Y,M,D} = date(),
  integer_to_list(Y)++integer_to_list(M)++integer_to_list(D).
