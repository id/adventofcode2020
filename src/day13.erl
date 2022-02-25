-module(day13).

-export([part1/0, part2/0]).

part1() ->
  {ok, Bin} = file:read_file("../input/day13.txt"),
  [Ts0, IDs0 | _] = binary:split(Bin, [<<"\n">>], [global]),
  Ts = erlang:binary_to_integer(Ts0),
  IDs1 = binary:split(IDs0, [<<",">>], [global]),
  IDs2 = lists:filter(fun(<<"x">>) -> false; (_) -> true end, IDs1),
  IDs = lists:map(fun binary_to_integer/1, IDs2),
  IdToDiff = lists:map(fun(Id) -> {Id, abs(1 - Ts/Id)} end, IDs),
  IdToDiff.

part2() ->
  {ok, Bin} = file:read_file("../input/day13_test1.txt"),
  [_, IDs0 | _] = binary:split(Bin, [<<"\n">>], [global]),
  IDs1 = binary:split(IDs0, [<<",">>], [global]),
  IDs = map_ids(IDs1, 0, []),
  IDs.
  %solve_part2(IDs, IDs, 0).

%% solve_part2([_], Ts) ->
%%   Ts.
%% solve_part2([{Ts1, Id1}, {Ts2, Id2} | Rest] = IDs, AllIDs, Ts) ->
%%   case (Ts+Ts2) div Id2 =:= 0 of
%%     false ->
%%       solve_part2(IDs, AllIDs, Ts+Id1);
%%     true ->
%%       case validate(AllIDs, 0, Ts) of
%%         true ->
%%           solve_part2([{Ts2, Id2} | Rest], AllIDs, Ts);
%%         false ->
%%           solve_part2(IDs, AllIDs, Ts+Id1)
%%       end
%%   end.

%% validate([], _) ->
%%   true;
%% validate([{Ts, Id} | IDs], T) ->


map_ids([], _, Acc) ->
  lists:reverse(Acc);
map_ids([<<"x">> | IDs], Ts, Acc) ->
  map_ids(IDs, Ts+1, Acc);
map_ids([BinId | IDs], Ts, Acc) ->
  Id = binary_to_integer(BinId),
  map_ids(IDs, Ts+1, [{Id, Ts, Id-Ts} | Acc]).
