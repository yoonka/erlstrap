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

-module(estp_cfg).

-export([read/0]).

-export([
         try_read/1,
         store/3,
         parse_dep/2,
         parse_var/2,
         raise/2,
         substitute/3
        ]).

-define(HOME_CFG, <<".erlstrap">>).
-define(MAIN_CFG, <<"/etc/erlstrap">>).

%%------------------------------------------------------------------------------

-spec read() -> true | {error, term()}.
read() ->
    try
        try_home() orelse try_path(?MAIN_CFG) orelse
            begin
                yio:errorn(<<"No valid configuration file found, ignoring.">>),
                {error, not_found}
            end
    catch
        throw:Term -> {error, Term}
    end.

try_home() ->
    case init:get_argument(home) of
        {ok, [[Dir]]} -> try_path(filename:join(Dir, ?HOME_CFG));
        error -> false
    end.

try_path(Path) ->
    yio:info(<<"Trying to read project definitions from ">>,
             {quote, Path}, <<": ">>),
    case try_read(Path) of
        {ok, Terms} ->
            yio:infon(<<"OK">>),
            process(Terms);
        {error, enoent} ->
            yio:infon(<<"Not found">>),
            false;
        {error, Reason} ->
            yio:infon(<<"Error: ">>, Reason),
            throw(Reason)
    end.

try_read(Path) ->
    try
        file:consult(Path)
    catch
        error:Error -> {error, Error}
    end.

process(Terms) ->
    All = parse(Terms, {[], []}),
    yio:infon(endl, <<"Read configuration:">>, endl, All),
    ProjFun = fun({{proj, _}, _}) -> true; (_) -> false end,
    {Projs, Opts} = lists:partition(ProjFun, All),
    Res = lists:map(fun(X) -> read_proj_cfg(X, Opts) end, Projs),
    [set_project(Name, Cfg) || {Name, {ok, Cfg}} <- Res],
    true.

read_proj_cfg({{proj, Name}, {_Gen, Path, _ProjOpts} = Proj}, Opts) ->
    yio:infon(endl, <<"Project ">>, {quote, Name}, <<" in ">>, {quote, Path}),
    case estp_proj_cfg:read(Proj, Opts) of
        {error, _} = Err ->
            {Name, Err};
        false ->
            {Name, false};
        Cfg ->
            yio:infon(endl, <<"Read project configuration:">>, endl, Cfg),
            {Name, {ok, Cfg}}
    end.

set_project(Name, Cfg) ->
    case estp_proj_sup:add(Name, Cfg) of
        {ok, _Pid} -> ok;
        {error, _} = Err -> proj_sup_err(Name, Err)
    end.

proj_sup_err(Name, Err) ->
    yio:errorn(<<"Error when adding project ">>, {quote, Name}, <<": ">>, Err).

%%------------------------------------------------------------------------------

parse([{project, Name, Genesis, OldPath}|T], CC) ->
    Path = substitute(OldPath, path, CC),
    parse(T, store_proj(Name, Genesis, Path, [], CC));
parse([{project, Name, Genesis, OldPath, OldProjOpts}|T], CC) ->
    Path = substitute(OldPath, path, CC),
    ProjOpts = substitute(OldProjOpts, term, CC),
    check_proj_opts(ProjOpts),
    parse(T, store_proj(Name, Genesis, Path, ProjOpts, CC));
parse([Other|T], CC) ->
    parse(T, parse_var(Other, CC));
parse([], {_Ctx, Cfg}) ->
    lists:reverse(Cfg).

store_proj(Name, OldGenesis, Path, ProjOpts, CC) ->
    Genesis = parse_gen(OldGenesis, CC),
    case filelib:is_dir(Path) of
        false -> raise(bad_path, Path);
        true -> store({proj, Name}, {Genesis, Path, ProjOpts}, CC)
    end.

parse_gen(git, _CC) ->
    git;
parse_gen(Other, CC) ->
    parse_dep(Other, CC).

parse_dep({Name}, {_Ctx, Cfg}) ->
    case proplists:get_value({comp, Name}, Cfg) of
        undefined -> raise(comp_not_found, Name);
        Val -> Val
    end;
parse_dep({Name, git, OldRepo}, CC) ->
    {Name, git, substitute(OldRepo, str, CC), []};
parse_dep({Name, git, OldRepo, Opts}, CC) ->
    Repo = substitute(OldRepo, str, CC),
    GitOpts = parse_git_opts(substitute(Opts, term, CC), CC),
    {Name, git, Repo, GitOpts};
parse_dep({Name, git, OldRepo, Opts, OldLocation}, CC) ->
    {Name, git, Repo, GitOpts} = parse_dep({Name, git, OldRepo, Opts}, CC),
    Location = substitute(OldLocation, path, CC),
    {Name, git, Repo, store_git({location, Location}, GitOpts)};
parse_dep(Other, _CC) ->
    raise(bad_cfg, Other).

parse_git_opts(L, CC) when is_list(L), not is_integer(hd(L)) ->
    parse_git_opts(L, CC, []);
parse_git_opts(Other, CC) ->
    [parse_git(substitute(Other, str, CC), CC)].

parse_git_opts([OldOpt|T], CC, Acc) ->
    GitOpt = substitute(OldOpt, str, CC),
    parse_git_opts(T, CC, store_git(parse_git(GitOpt, CC), Acc));
parse_git_opts([], _CC, Acc) ->
    lists:reverse(Acc).

parse_git(Bin, _CC) when is_binary(Bin) ->
    {b, Bin};
parse_git(List, _CC) when is_list(List), is_integer(hd(List)) ->
    {b, list_to_binary(List)};
parse_git({b, BranchTag}, CC) ->
    {b, substitute(BranchTag, str, CC)}.

store_git({Key, _Val} = Elem, Acc) ->
    case proplists:get_value(Key, Acc) of
        undefined -> [Elem|Acc];
        _ -> raise_git(Key)
    end.

raise_git(location) -> raise(exists, {git_opt, <<"location">>});
raise_git(b)        -> raise(exists, {git_opt, <<"branch/tag">>}).

store(Key, Val, {Ctx, Cfg}) ->
    case proplists:get_value(Key, Cfg) of
        undefined -> {Ctx, [{Key, Val}|Cfg]};
        _ -> raise(exists, Key)
    end.

check_proj_opts([merge_vars|T]) -> check_proj_opts(T);
check_proj_opts([Bad|_]) -> raise(bad_opt, Bad);
check_proj_opts([]) -> ok;
check_proj_opts(Bad) -> raise(bad_opt, Bad).

%%------------------------------------------------------------------------------

%% Substitute variable values when they are used and Type is known, not now
parse_var({var, Name, Str}, CC) ->
    store({var, Name}, substitute(Str, lazy, CC), CC);
parse_var({var, Name, env, Env}, CC) ->
    store({var, Name}, substitute({env, Env}, lazy, CC), CC);
parse_var({var, Name, env, Env, Default}, CC) ->
    store({var, Name}, substitute({env, Env, Default}, lazy, CC), CC);
parse_var({component, Name, git, OldRepo}, CC) ->
    Repo = substitute(OldRepo, str, CC),
    store({comp, Name}, {Name, git, Repo}, CC);
parse_var({component, Name, git, OldRepo, OldVsn}, CC) ->
    Repo = substitute(OldRepo, str, CC),
    Vsn = substitute(OldVsn, term, CC),
    store({comp, Name}, {Name, git, Repo, Vsn}, CC);
parse_var(Bad, _CC) ->
    raise(bad_cfg, Bad).

%%------------------------------------------------------------------------------

raise(Id, Val) ->
    err(Id, Val),
    throw(Id).

err(no_env, Env) ->
    yio:errorn(<<"Error: Environment variable ">>, {quote, Env},
               <<" not known, aborting.">>);
err(exists, {var, Name}) ->
    yio:errorn(<<"Error: Variable ">>, {quote, Name},
               <<" already defined, aborting.">>);
err(exists, {proj, Name}) ->
    yio:errorn(<<"Error: Project ">>, {quote, Name},
               <<" already defined, aborting.">>);
err(exists, {comp, Name}) ->
    yio:errorn(<<"Error: Component ">>, {quote, Name},
               <<" already defined, aborting.">>);
err(exists, {dep, Name}) ->
    yio:errorn(<<"Error: Component ">>, {quote, Name},
               <<" already present in the match section, aborting.">>);
err(exists, {match, Name}) ->
    yio:errorn(<<"Error: Match section with name ">>, {quote, Name},
               <<" already defined, aborting.">>);
err(exists, {git_opt, Opt}) ->
    yio:errorn(<<"Error: Git option for ">>, Opt,
               <<" already defined, aborting.">>);
err(var_not_found, Name) ->
    yio:errorn(<<"Error: Variable ">>, {quote, Name},
               <<" not defined, aborting.">>);
err(comp_not_found, Name) ->
    yio:errorn(<<"Error: Component ">>, {quote, Name},
               <<" not defined, aborting.">>);
err(bad_path, Path) ->
    yio:errorn(<<"Error: Path ">>, {quote, Path},
               <<" is not a valid directory, aborting.">>);
err(bad_opt, Option) ->
    yio:errorn(<<"Error: Incorrect project option ">>, {quote, Option},
               <<", aborting.">>);
err(bad_ctx, git_repo) ->
    yio:errorn(<<"Error: Resolving git branch is only possible in the ">>,
               <<"project configuration file, aborting.">>);
err(bad_cfg, Bad) ->
    yio:errorn(<<"Error: Can not parse configuration entry:">>, endl, Bad).

%%------------------------------------------------------------------------------

substitute(E, _Type, _CC) when is_binary(E); is_number(E); is_atom(E) ->
    E;
substitute({clamp, List}, Type, CC) when is_list(List) ->
    Res = lists:map(fun(X) -> substitute(X, Type, CC) end, List),
    if Type =:= str -> list_to_binary(Res);
       Type =:= path -> filename:join(Res);
       Type =:= term -> lists:append(Res);
       Type =:= lazy -> {clamp, Res}
    end;
substitute(C, Type, CC) when is_tuple(C), element(1, C) =:= clamp ->
    substitute({clamp, tl(tuple_to_list(C))}, Type, CC);
substitute({Name}, Type, {_Ctx, Cfg} = CC) ->
    case proplists:get_value({var, Name}, Cfg) of
        undefined -> raise(var_not_found, Name);
        Val -> substitute(Val, Type, CC)
    end;
substitute({env, OldEnv}, Type, CC) ->
    Env = substitute(OldEnv, str, CC),
    case os:getenv(Env) of
        false -> raise(no_env, Env);
        Val -> substitute(Val, Type, CC)
    end;
substitute({env, OldEnv, Default}, Type, CC) ->
    Env = substitute(OldEnv, str, CC),
    Res = case os:getenv(Env) of false -> Default; Val -> Val end,
    substitute(Res, Type, CC);
substitute(List, Type, _CC)
  when is_list(List), is_integer(hd(List)), Type =/= term ->
    list_to_binary(List);
substitute(List, term, _CC) ->
    List;
substitute(Tuple, _Type, _CC) when is_tuple(Tuple) ->
    raise(bad_cfg, Tuple).

%%------------------------------------------------------------------------------
