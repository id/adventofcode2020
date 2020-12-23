-module(day15).

-export([solve/2]).

solve(Input, Turns) ->
  State = lists:foldl(
            fun({N, Turn}, Map) ->
                maps:put(N, {Turn, undefined}, Map)
            end, #{},
            lists:zip(Input, lists:seq(1, erlang:length(Input)))),
  N = lists:last(Input),
  Turn = erlang:length(Input) + 1,
  solve_part1(N, State, Turn, Turns).

solve_part1(N, State, Turn, Turn) ->
  {Next, _} = next_spoken(N, State, Turn),
  Next;
solve_part1(N, State0, Turn, Turns) when Turn rem 100000 =:= 0 ->
  io:format(user, "~p\n", [Turn]),
  {Next, State} = next_spoken(N, State0, Turn),
  solve_part1(Next, State, Turn+1, Turns);
solve_part1(N, State0, Turn, Turns) ->
  {Next, State} = next_spoken(N, State0, Turn),
  solve_part1(Next, State, Turn+1, Turns).

next_spoken(N, State, Turn) ->
  NewN = case maps:get(N, State, undefined) of
           undefined -> 0;
           {_, undefined} -> 0;
           {PrevTurn, PrevPrevTurn} -> PrevTurn-PrevPrevTurn
         end,
  {NewN, maps:update_with(NewN, fun({PrevTurn, _}) -> {Turn, PrevTurn} end, {Turn, undefined}, State)}.

