%%% File    : aa_config.erl
%%% Author  : Leon <leon@smartmesh.io>
%%% Purpose : Transactions switch configuration
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
-module(aa_config).

-include("ejabberd.hrl").
-include("jlib.hrl").

-compile(export_all).

push_config(Params)->
  ?DEBUG("pushconfig ~p",[Params]),
  Hosts = ej:get( {<<"hosts">>},Params),
  _FullHosts = save_host_config(Hosts, Params, Hosts),
  ok.

save_host_config([], _Params, ResList)->
  ResList;
save_host_config([Host|LastHost], Params, ResList)->
  try
    Hobj = ej:get({type_util:to_binary(Host)},Params),
    GroupHost = ej:get({<<"group_hosts">>},Hobj),
    NewResList = lists:append(ResList, binary_to_list(GroupHost)),
    save_local_config(Host, Hobj),
    save_host_config(LastHost, Params, NewResList)
  catch
    E:R->
      ?ERROR_MSG("save_host_config fail ~p",[{E,R,erlang:get_stacktrace()}]),
      save_host_config(LastHost, Params, ResList)
  end.
    

save_local_config(Host, Hobj)->
  {obj,ConfigList} = Hobj,
  lists:foreach(fun({ConfigKey,ConfigValue})->
        case type_util:to_atom(ConfigKey) of
          http_server->
            ejabberd_config:add_local_option({type_util:to_atom(ConfigKey), type_util:to_list(Host)}, type_util:to_list(ConfigValue));
          _->
            ejabberd_config:add_local_option({type_util:to_atom(ConfigKey), type_util:to_list(Host)}, ConfigValue)
        end
      end,ConfigList).


enable_blacklist(Domain)->
  ejabberd_config:get_local_option({blacklist_enable, type_util:to_list(Domain)}) =:= true.

enable_shielding(Domain)->
  ejabberd_config:get_local_option({shielding_enable, type_util:to_list(Domain)}) =:= true.

enable_planning_system(Domain)->
  ejabberd_config:get_local_option({planning_system_enable, type_util:to_list(Domain)}) =:= true.

enable_friend(Domain, TDomain)->
  case ejabberd_config:get_local_option({friend_enable, type_util:to_list(Domain)}) of
    true->
      case Domain =:= TDomain of
        true->
          true;
        _->
          case enable_cross_domain(Domain, TDomain) of
            true->
              enable_friend_cross(Domain) and enable_friend_cross(TDomain);
            _->
              false
          end
      end;
    _->
      false
  end.

enable_auth(Domain)->
  ejabberd_config:get_local_option({auth_enable, type_util:to_list(Domain)}) =:= true.

enable_ack_from(Domain)->
  ejabberd_config:get_local_option({ack_from, type_util:to_list(Domain)}) =:= true.

enable_group_cache(Domain)->
  ejabberd_config:get_local_option({group_cache_enable, type_util:to_list(Domain)}) =:= true.

enable_cross_domain(Domain,ToDomain) when Domain == ToDomain ->
  true;
enable_cross_domain(Domain,ToDomain) -> 
  case ejabberd_config:get_local_option({cross_domain, type_util:to_list(Domain)}) of
    true->
      case ejabberd_config:get_local_option({cross_domain_list, type_util:to_list(Domain)}) of
        []->
          true;
        DomainList->
          lists:member(type_util:to_binary(ToDomain), DomainList)
      end;
    _->
      false
  end.

enable_group_cross_domain(Domain)->
  ejabberd_config:get_local_option({group_cross_domain, type_util:to_list(Domain)}) =:= true.

enable_super_group_cross_domain(Domain)->
  ejabberd_config:get_local_option({super_group_cross_domain, type_util:to_list(Domain)}) =:= true.


enable_group(Domain)->
  ejabberd_config:get_local_option({group_enable, type_util:to_list(Domain)}) =:= true.

enable_super_group(Domain)->
  ejabberd_config:get_local_option({super_group_enable, type_util:to_list(Domain)}) =:= true.

enable_friend_cross(Domain)->
  ejabberd_config:get_local_option({friend_cross_enable, type_util:to_list(Domain)}) =:= true.


