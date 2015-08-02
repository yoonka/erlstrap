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

-module(estp_proj_cfg).

-export([read/2]).

-define(DEF_CFG_NAME, <<"erlstrap.conf">>).

%%------------------------------------------------------------------------------

read({_Genesis, Path, ProjOpts}, Opts) ->
    case IsFile = not filelib:is_dir(Path) of
        false -> File = filename:join(Path, ?DEF_CFG_NAME);
        true -> File = Path
    end,
    yio:info(<<"Trying to read configuration from ">>, {quote, File}, <<": ">>),
    case {IsFile, estp_cfg:try_read(File)} of
        {_, {ok, Terms}} ->
            yio:infon(<<"OK">>),
            process(Terms, ProjOpts, Opts);
        {true, {error, enoent}} ->
            yio:infon(<<"Not found">>),
            {error, not_found};
        {false, {error, enoent}} ->
            yio:infon(<<"Not found">>),
            false;
        {_, {error, Reason}} ->
            yio:infon(<<"Error: ">>, Reason),
            {error, Reason}
    end.

process(Terms, _ProjOpts, _Opts) ->
    try
        parse(Terms, {[{git_repo, dummy}], []}) %%TODO add Git repo to context
    catch
        throw:Term -> {error, Term}
    end.

parse([{as, Name, for, Match, use, Deps}|T], CC)
  when is_tuple(Match), is_list(Deps) ->
    Val = {parse_match(Match, CC), parse_deps(Deps, CC, [])},
    parse(T, estp_cfg:store({match, Name}, Val, CC));
parse([Other|T], CC) ->
    case parse_proj(Other, CC) of
        false -> parse(T, estp_cfg:parse_var(Other, CC));
        NewOpts -> parse(T, NewOpts)
    end;
parse([], {_Ctx, Cfg}) ->
    lists:reverse(Cfg).

%%------------------------------------------------------------------------------

parse_match({Op, List}, CC)
  when Op =:= all; Op =:= any ->
    lists:map(fun(X) -> parse_match(X, CC) end, List);
parse_match(M, CC)
  when is_tuple(M), element(1, M) =:= all;
       is_tuple(M), element(1, M) =:= any ->
    parse_match({element(1, M), tl(tuple_to_list(M))}, CC);
parse_match({A, eq, B}, CC) ->
    subst_cmd({eq, A, B}, CC);
parse_match({A, begins_with, B}, CC) ->
    subst_cmd({begins, A, B}, CC);
parse_match({A, ends_with, B}, CC) ->
    subst_cmd({ends, A, B}, CC);
parse_match({A, contains, B}, CC) ->
    subst_cmd({contains, A, B}, CC);
parse_match({re, OldSubject, OldRE}, CC) ->
    Subject = estp_cfg:substitute(OldSubject, str, CC),
    {re, Subject, get_re(OldRE, CC)};
parse_match({re, OldSubject, OldRE, OldOptions}, CC) ->
    Subject = estp_cfg:substitute(OldSubject, str,  CC),
    Options = estp_cfg:substitute(OldOptions, term, CC),
    {re, Subject, get_re(OldRE, CC), Options}.

get_re({OldRE, OldOptions}, CC) ->
    RE = estp_cfg:substitute(OldRE, str, CC),
    Options = estp_cfg:substitute(OldOptions, term, CC),
    re:compile(RE, Options);
get_re(OldRE, CC) ->
    RE = estp_cfg:substitute(OldRE, str, CC),
    re:compile(RE).

subst_cmd({Cmd, A, B}, CC) ->
    {Cmd, substitute(A, CC), substitute(B, CC)}.

substitute({git, branch}, {Ctx, _Cfg}) ->
    case proplists:get_value(git_repo, Ctx) of
        undefined -> estp_cfg:raise(bad_ctx, git_repo);
        _GitRepo -> ok %%TODO get branch
    end;
substitute(Other, CC) ->
    estp_cfg:substitute(Other, str, CC).

%%------------------------------------------------------------------------------

parse_deps([Dep|T], CC, Acc) ->
    parse_deps(T, CC, store(estp_cfg:parse_dep(Dep, CC), Acc));
parse_deps([], _CC, Acc) ->
    lists:reverse(Acc).

store(Term, List) ->
    Name = element(1, Term),
    case lists:keymember(Name, 1, List) of
        false -> [Term|List];
        true -> estp_cfg:raise(exists, {dep, Name})
    end.

parse_proj({var, Name, git, branch}, CC) ->
    Branch = substitute({git, branch}, CC),
    estp_cfg:store({var, Name}, Branch, CC);
parse_proj(_Other, _CC) ->
    false.
