%% Copyright (c) 2015, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(estp_proj_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, add/2]).

-include_lib("yolf/include/yolf.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    ?LOG_SUPERVISOR(?SERVER),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ?LOG_SUPERVISOR_INIT(?SERVER),
    Child = ?WORKER(estp_project),
    {ok, {{simple_one_for_one, 3, 30}, [Child]}}.

add(Name, Cfg) ->
    Server = estp_project:server_name(Name),
    State = [{name, Name}|Cfg],
    case whereis(Server) of
        undefined ->
            add_child(Server, State);
        Pid ->
            delete_child(Server, Pid),
            add_child(Server, State)
    end.

add_child(Server, State) ->
    lager:info(<<" == estp_proj_sup: add child:~p">>, [Server]),
    supervisor:start_child(?SERVER, [Server, State]).

delete_child(Server, Pid) ->
    lager:info(<<" == estp_proj_sup: delete child:~p">>, [Server]),
    ok = supervisor:terminate_child(?SERVER, Pid).
