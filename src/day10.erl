-module(day10).

-export([part1/0, part2/0]).

part1() ->
  Ratings = get_ratings("../input/day10.txt"),
  solve_part1(Ratings, 0, 0, 0).

solve_part1([], _, Ones, Threes) ->
  Ones * Threes;
solve_part1([R | Ratings], PrevRating, Ones, Threes) when R - PrevRating =:= 1 ->
  solve_part1(Ratings, R, Ones+1, Threes);
solve_part1([R | Ratings], PrevRating, Ones, Threes) when R - PrevRating =:= 3 ->
  solve_part1(Ratings, R, Ones, Threes+1);
solve_part1([R | Ratings], _PrevRating, Ones, Threes) ->
  solve_part1(Ratings, R, Ones, Threes).

get_ratings(File) ->
  {ok, Bin} = file:read_file(File),
  Lines0 = binary:split(Bin, [<<"\n">>], [global]),
  Lines = lists:filter(fun (<<>>) -> false; (_) -> true end, Lines0),
  Ratings0 = lists:map(fun erlang:binary_to_integer/1, Lines),
  Ratings = [lists:max(Ratings0) + 3 | Ratings0],
  lists:sort(Ratings).
