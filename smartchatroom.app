%% $Id$

{application, smartchatroom,
 [{description, "smartchatroom"},
  {vsn, "1.0"},
  {modules, [mod_muc_room,
	     muc_room_util
	    ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {mod_muc_room, []}}]}.


%% Local Variables:
%% mode: erlang
%% End:
%% vim: set filetype=erlang tabstop=4:
