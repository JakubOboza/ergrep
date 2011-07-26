-module(ergrep).
-export([start/2]).

start(Pattern, Dir) ->
  register(ergrep, self()),
  spawn(fun() -> mapF(Pattern, Dir ++ "/*") end),
  reduceF(0, []).

mapF(Pattern, Root) ->
    ergrep ! {spawn, self()},
    Items = filelib:wildcard(Root),
    lists:map(fun(I) -> process_item(filelib:is_dir(I), I, Pattern) end, Items),
    ergrep ! {die, self()}.


process_item(true, Item, Pattern) -> process_dir(Item, Pattern);
process_item(false, Item, Pattern) -> process_file(Item, Pattern).

process_file(Name, Pattern) ->
    {ok, Content} = file:read_file(Name),
    {ok, RegExp} = re:compile(Pattern),
    Splited = re:split(Content, "\n"),
    Result = lists:map(fun(Line) -> process_line(re:run(Line, RegExp), Line, Name) end, Splited).

process_line(nomatch, _, _) -> true;
process_line(_, Line, File) ->
  ergrep ! {emit, self(), {File, Line}},
  io:format("~s:~s~n", [File, binary_to_list(Line)]).


process_dir(Name, Pattern) ->
%   io:format("Dir ~p~n", [Name]), % debug
    spawn(fun() -> mapF(Pattern, Name ++ "/*") end).


reduceF(ProcCount, Results) ->
%   io:format("~p~n",[ProcCount]), % debug
    receive
        {emit, Ref, Value } -> reduceF(ProcCount, [Value | Results]);
        {spawn, Ref }       -> reduceF(ProcCount + 1, Results);
        {die, Ref }         -> Count = ProcCount - 1,
          if
            Count =:= 0 -> Results;
            true  -> reduceF(Count, Results)
          end
        after
        1000 -> true % wait for a sec
    end.
