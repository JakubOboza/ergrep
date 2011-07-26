-module(ergrep).
-export([start/2]).

start(Pattern, Dir) ->
  register(ergrep, self()),
  spawn(fun() -> mapF(Pattern, Dir ++ "/*") end),
  reduceF(0).

mapF(Pattern, Root) ->
    ergrep ! {spawn, self()},
    Items = filelib:wildcard(Root),
    lists:map(fun(I) -> process_item(filelib:is_dir(I), I, Pattern) end, Items),
    ergrep ! {die, self()}.


process_item(true, Item, Pattern) -> process_dir(Item, Pattern);
process_item(false, Item, Pattern) -> process_file(Item, Pattern).

process_file(Name, Pattern) ->
    io:format("File ~p~n", [Name]).

process_dir(Name, Pattern) ->
%   io:format("Dir ~p~n", [Name]), % debug
    spawn(fun() -> mapF(Pattern, Name ++ "/*") end).


reduceF(ProcCount) ->
%   io:format("~p~n",[ProcCount]), % debug
    receive
        {spawn, Ref } -> reduceF(ProcCount + 1);
        {die, Ref } -> Count = ProcCount - 1,
          if
            Count =:= 0 -> true;
            true  -> reduceF(ProcCount - 1)
          end
        after
        1000 -> true % wait for a sec
    end.
