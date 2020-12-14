-module(day12).

-export([part1/0, part2/0]).

part1() ->
  {ok, Bin} = file:read_file("../input/day12.txt"),
  Lines0 = binary:split(Bin, [<<"\n">>], [global]),
  Lines = lists:filter(fun (<<>>) -> false; (_) -> true end, Lines0),
  parse_instructions({1,0}, {0,0}, Lines).

parse_instructions(_Direction, {X, Y} = Ship, []) ->
  abs(X) + abs(Y);
parse_instructions(Direction, Ship, [<<>> | Lines]) ->
  parse_instructions(Direction, Ship, Lines);
parse_instructions({DX, DY} = Direction, {X, Y}, [<<"F", Arg/binary>> | Lines]) ->
  N = binary_to_integer(Arg),
  parse_instructions(Direction, {X + DX*N, Y+DY*N}, Lines);
parse_instructions(Direction, {X, Y}, [<<"N", Arg/binary>> | Lines]) ->
n  parse_instructions(Direction, {X, Y + binary_to_integer(Arg)}, Lines);
parse_instructions(Direction, {X, Y}, [<<"S", Arg/binary>> | Lines]) ->
  parse_instructions(Direction, {X, Y - binary_to_integer(Arg)}, Lines);
parse_instructions(Direction, {X, Y}, [<<"E", Arg/binary>> | Lines]) ->
  parse_instructions(Direction, {X + binary_to_integer(Arg), Y}, Lines);
parse_instructions(Direction, {X, Y}, [<<"W", Arg/binary>> | Lines]) ->
  parse_instructions(Direction, {X - binary_to_integer(Arg), Y}, Lines);
parse_instructions(Direction0, Ship, [<<"R", Arg/binary>> | Lines]) ->
  Direction = rotate(Direction0, r, binary_to_integer(Arg) div 90),
  parse_instructions(Direction, Ship, Lines);
parse_instructions(Direction0, Ship, [<<"L", Arg/binary>> | Lines]) ->
  Direction = rotate(Direction0, l, binary_to_integer(Arg) div 90),
  parse_instructions(Direction, Ship, Lines).

part2() ->
  {ok, Bin} = file:read_file("../input/day12.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  parse_instructions2({0, 0}, {10, 1}, Lines).

parse_instructions2({X, Y} = Ship, Waypoint, []) ->
  abs(X) + abs(Y);
parse_instructions2(Ship, Waypoint, [<<>> | Lines]) ->
  parse_instructions2(Ship, Waypoint, Lines);
parse_instructions2(Ship0, Waypoint, [<<"F", Arg/binary>> | Lines]) ->
  Ship = move(Ship0, Waypoint, binary_to_integer(Arg)),
  parse_instructions2(Ship, Waypoint, Lines);
parse_instructions2(Ship, {X, Y}, [<<"N", Arg/binary>> | Lines]) ->
  parse_instructions2(Ship, {X, Y + binary_to_integer(Arg)}, Lines);
parse_instructions2(Ship, {X, Y}, [<<"S", Arg/binary>> | Lines]) ->
  parse_instructions2(Ship, {X, Y - binary_to_integer(Arg)}, Lines);
parse_instructions2(Ship, {X, Y}, [<<"E", Arg/binary>> | Lines]) ->
  parse_instructions2(Ship, {X + binary_to_integer(Arg), Y}, Lines);
parse_instructions2(Ship, {X, Y}, [<<"W", Arg/binary>> | Lines]) ->
  parse_instructions2(Ship, {X - binary_to_integer(Arg), Y}, Lines);
parse_instructions2(Ship, Waypoint0, [<<"R", Arg/binary>> | Lines]) ->
  Waypoint = rotate(Waypoint0, r, binary_to_integer(Arg) div 90),
  parse_instructions2(Ship, Waypoint, Lines);
parse_instructions2(Ship, Waypoint0, [<<"L", Arg/binary>> | Lines]) ->
  Waypoint = rotate(Waypoint0, l, binary_to_integer(Arg) div 90),
  parse_instructions2(Ship, Waypoint, Lines).

move({ShipX, ShipY}, {X, Y}, Arg) ->
  {ShipX + X*Arg, ShipY + Y*Arg}.

rotate(Waypoint, _, 0) -> 
  Waypoint;
rotate({X, Y}, r, N) -> 
  rotate({Y, -X}, r, N-1);
rotate({X, Y}, l, N) -> 
  rotate({-Y, X}, l, N-1).
