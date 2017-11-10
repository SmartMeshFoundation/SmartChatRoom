-module(aa_blacklist).
-include("ejabberd.hrl").
-include("jlib.hrl").

-compile([export_all]).

check(From, To)->
% false.
   #jid{user=FUser, server=FDomain} = From,
   #jid{user=TUser, server=TDomain} = To,
   case aa_super_group_chat:is_group_chat(From) or aa_super_group_chat:is_group_chat(To) of 
     true ->
       false;
     _->
       case aa_config:enable_blacklist(TDomain) of
         true ->
           get_blacklist_value(FUser, FDomain, TUser, TDomain);
         false->
          false 
       end
   end.


remove(From, To)->
  FromStr = type_util:to_list(From),
  ToStr = type_util:to_list(To),
  [FUser, FDomain] = string:tokens(FromStr, "@"),
  [TUser, TDomain] = string:tokens(ToStr, "@"),
  BlacklistKey = get_blacklist_key(FUser, FDomain, TUser, TDomain),
  aa_hookhandler:ecache_cmd(["DEL", BlacklistKey]).


get_blacklist_key(FUser, FDomain, TUser, TDomain)->
  type_util:to_list(FUser) ++ "@" 
  ++ type_util:to_list(FDomain) ++ "-" 
  ++ type_util:to_list(TUser) ++ "@"
  ++ type_util:to_list(TDomain) ++ "-blacklist".

get_blacklist_value(FUser, FDomain, TUser, TDomain)->
  BlacklistKey = get_blacklist_key(FUser, FDomain, TUser, TDomain),
  Result = aa_hookhandler:ecache_cmd(["GET", BlacklistKey]),
  ?WARNING_MSG("---Blacklistcheck ~p",[{BlacklistKey, Result}]),
  case Result of
    {ok, undefined}->
      case get_blacklist_value_from_api(TDomain, TUser, FUser, FDomain) of
        skip->
          false;
        <<"0">>->
          aa_hookhandler:ecache_cmd(["SETEX", BlacklistKey, "7200", "0"]),
          false;
        <<"1">>->
          aa_hookhandler:ecache_cmd(["SETEX", BlacklistKey, "7200", "1"]),
          true;
        _R->
          ?WARNING_MSG("get_blacklist_value_from_api error value ~p",[_R]),
          true 
      end;
    {ok, <<"0">>} ->
      false;
    {ok, <<"1">>}->
      true;
    _->
      true
  end.

get_blacklist_value_from_api(TDomain, TUser, FUser, FDomain)->
  case aa_api_client:get_blacklist_status(TDomain, TUser, FUser, FDomain) of
    {ok, StructJson}->
      Status = ej:get({<<"status">>},StructJson,<<"0">>),
      Status;
    R->
      ?ERROR_MSG("get_blacklist_value_from_api error :~p",[R]),
      skip
  end.

