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

part2() ->
  Ratings = get_ratings("../input/day10_test1.txt"),
  G = digraph:new(),
  digraph:add_vertex(G, 0),
  lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, Ratings),
  add_edges(0, Ratings, G),
  dfs(0, lists:last(Ratings), sets:new(), 1).

dfs(U, V, Visited0, N) ->
  case sets:is_element(U, Visited0) of
    true -> N;
    false ->
      Visited = sets:add_element(U, Visited0),
      case U =:= V of
        true -> N+1;
        false ->


add_edges(_, [], _G) -> ok;
add_edges(V, [R | Ratings], G) ->
  digraph:add_edge(G, R, V),
  io:format(user, "~p -> ~p\n", [R, V]),
  add_edges2(V, Ratings, G),
  add_edges(R, Ratings, G).

add_edges2(_, [], _G) -> ok;
add_edges2(V, [R | Ratings], G) when R-V =< 3 -> 
  digraph:add_edge(G, R, V),
  io:format(user, "~p -> ~p\n", [R, V]),
  add_edges2(V, Ratings, G);
add_edges2(_V, _Ratings, _G) ->
  ok.

%% part2() ->
%%   Ratings = get_ratings("../input/day10_test1.txt"),
%%   solve_part2_2([0], Ratings),
%%   collect_results(erlang:length(Ratings)-1, 0).

%% collect_results(0, N) ->
%%   N;
%% collect_results(Length, N) ->
%%   receive
%%     {solved, X} ->
%%       io:format(user, "solved: ~p, left ~p~n", [X, Length]),
%%       collect_results(Length-1, N+X)
%%   end.

%% solve_part2_2(_Left, [_]) ->
%%   ok;
%% solve_part2_2(Left, [R | Right]) ->
%%   Self = self(),
%%   proc_lib:spawn(fun() -> Self ! {solved, solve_part2(Left, Right, 0)} end),
%%   solve_part2_2([R | Left], Right).

%% solve_part2(_Left, [], N) ->
%%   N;
%% solve_part2(Left, [R | Right], N) ->
%%   case is_valid(Left, [R | Right]) of
%%     true ->
%%       solve_part2(Left, Right, N+1);
%%     false -> 
%%       N
%%   end.

%% part2() ->
%%   Ratings = get_ratings("../input/day10_test1.txt"),
%%   solve_part2(0, Ratings, 0, 1).

%% solve_part2(_, [], _, N) -> N;
%% solve_part2(R1, [R2 | Ratings], Count, N) -> 

%% solve_part2(_Left, [], N) ->
%%   N;
%% solve_part2(Left, [R | Right], N0) ->
%%   N = case is_valid(Left, Right) of
%%         true ->
%%           solve_part2(Left, Right, N0+1);
%%         false -> 
%%           N0
%%       end,
%%   N,
%%   solve_part2([R | Left], Right, N).

%% is_valid([], []) -> true;
%% is_valid([_], []) -> true;
%% is_valid([], [_]) -> true;
%% is_valid(_, []) -> true;
%% is_valid([R1], [R2]) when R2 - R1 =< 3 -> true;
%% is_valid([R1] = Left, [R2 | Right]) when R2 - R1 =< 3 -> is_valid([R2 | Left], Right);
%% is_valid([R1 | _] = Left, [R2 | Right]) when R2 - R1 =< 3 -> is_valid([R2 | Left], Right);
%% is_valid(_, _) -> false.
  

get_ratings(File) ->
  {ok, Bin} = file:read_file(File),
  Lines0 = binary:split(Bin, [<<"\n">>], [global]),
  Lines = lists:filter(fun (<<>>) -> false; (_) -> true end, Lines0),
  Ratings0 = lists:map(fun erlang:binary_to_integer/1, Lines),
  Ratings = [lists:max(Ratings0) + 3 | Ratings0],
  lists:sort(Ratings).
