%%% Extremely slow and bloated implementation using digraph

-module(day17).

-export([part1/1, part2/1]).

-define(dbg(Fmt), io:format(user, Fmt ++ "\n", [])).
-define(dbg(Fmt, Args), io:format(user, Fmt ++ "\n", Args)).
-define(dbg_var(Var), io:format(user, "~p\n", [Var])).

part1(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  Size = byte_size(hd(Lines)) + 6*2,
  G = digraph:new(),
  [add_plane(G, Size, Z) || Z <- lists:seq(-6, 6)],
  [connect_planes(Z, Z+1, Size, G) || Z <- lists:seq(-6, 5)],
  load_state(Lines, 0, G),
  NewG = cycle(6, G),
  length(active_vertices(NewG)).

active_vertices(G) ->
  Vertices = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
  lists:filter(fun({_, active}) -> true; (_) -> false end, Vertices).

load_state([], _, _) ->
  ok;
load_state([Line | Lines], Y, G) ->
  process_line(Line, 0, Y, G),
  load_state(Lines, Y+1, G).

process_line(<<>>, _, _, _) ->
  ok;
process_line(<<".", Line/binary>>, X, Y, G) ->
  digraph:add_vertex(G, {0,6+X,6+Y}, inactive),
  process_line(Line, X+1, Y, G);
process_line(<<"#", Line/binary>>, X, Y, G) ->
  digraph:add_vertex(G, {0,6+X,6+Y}, active),
  process_line(Line, X+1, Y, G).

cycle(0, G) ->
  G;
cycle(Cycles, G) ->
  ?dbg("cycle #~p", [6-Cycles]),
  NewG = digraph_utils:subgraph(G, digraph:vertices(G)),
  do_cycle(digraph:vertices(G), G, NewG),
  cycle(Cycles-1, NewG).

do_cycle([], _G, _NewG) ->
  ok;
do_cycle([V | Vertices], G, NewG) ->
  {V, Label} = digraph:vertex(G, V),
  Neighbours = lists:usort(digraph:out_neighbours(G, V)),
  {Active, Inactive} = check_state(Neighbours, G, 0, 0),
  case {Label, Active, Inactive} of
    {active, 2, _} -> ok;
    {active, 3, _} -> ok;
    {active, _, _} ->
      digraph:add_vertex(NewG, V, inactive);
    {inactive, 3, _} -> 
      digraph:add_vertex(NewG, V, active);
    {_, _, _} -> ok
  end,
  do_cycle(Vertices, G, NewG).

check_state([], _G, Active, Inactive) ->
  {Active, Inactive};
check_state([V | Vertices], G, Active, Inactive) ->
  {V, Label} = digraph:vertex(G, V),
  case Label of
    active -> check_state(Vertices, G, Active+1, Inactive);
    inactive -> check_state(Vertices, G, Active, Inactive+1)
  end.

connect_planes(Z1, Z2, Size, G) ->
  PlaneXY = [{X, Y} || X <- lists:seq(0, Size-1), Y <- lists:seq(0, Size-1)],
  do_connect_planes(Z1, Z2, PlaneXY, G).

do_connect_planes(_Z1, _Z2, [], _G) ->
  ok;
do_connect_planes(Z1, Z2, [{X,Y} | XY], G) ->
  [digraph:add_edge(G, {Z1, X, Y}, {Z2, X+DX, Y+DY}) ||
    DX <- lists:seq(-1,1), DY <- lists:seq(-1,1)],
  [digraph:add_edge(G, {Z2, X+DX, Y+DY}, {Z1, X, Y}) ||
    DX <- lists:seq(-1,1), DY <- lists:seq(-1,1)],
  do_connect_planes(Z1, Z2, XY, G).

add_plane(G, Size, Z) ->
  PlaneXY = [{X, Y} || X <- lists:seq(0, Size-1), Y <- lists:seq(0, Size-1)],
  add_vertices(PlaneXY, Z, G),
  add_edges(PlaneXY, Z, G).

add_vertices([], _Z, _G) ->
  ok;
add_vertices([{X,Y} | XY], Z, G) ->
  digraph:add_vertex(G, {Z,X,Y}, inactive),
  add_vertices(XY, Z, G).

add_edges([], _Z, _G) ->
  ok;
add_edges([{X,Y} | XY], Z, G) ->
  [digraph:add_edge(G, {Z, X, Y}, {Z, X+DX, Y+DY}) ||
    DX <- lists:seq(-1,1), DY <- lists:seq(-1,1), DX /= 0 orelse DY /= 0],
  add_edges(XY, Z, G).

part2(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  Size = byte_size(hd(Lines)) + 6*2,
  G = digraph:new(),
  [add_cube(G, Size, W) || W <- lists:seq(-6, 6)],
  [connect_cubes(W, W+1, Size, G) || W <- lists:seq(-6, 5)],
  load_state2(Lines, 0, G),
  NewG = cycle(6, G),
  length(active_vertices(NewG)).

load_state2([], _, _) ->
  ok;
load_state2([Line | Lines], Y, G) ->
  process_line2(Line, 0, Y, G),
  load_state2(Lines, Y+1, G).

process_line2(<<>>, _, _, _) ->
  ok;
process_line2(<<".", Line/binary>>, X, Y, G) ->
  digraph:add_vertex(G, {6+X, 6+Y, 6, 0}, inactive),
  process_line2(Line, X+1, Y, G);
process_line2(<<"#", Line/binary>>, X, Y, G) ->
  digraph:add_vertex(G, {6+X, 6+Y, 6, 0}, active),
  process_line2(Line, X+1, Y, G).

add_cube(G, Size, W) ->
  Vertices = [{X, Y, Z, W} || 
               X <- lists:seq(0, Size-1), 
               Y <- lists:seq(0, Size-1), 
               Z <- lists:seq(0, Size-1)],
  lists:foreach(fun(V) -> digraph:add_vertex(G, V, inactive) end, Vertices),
  lists:foreach(
    fun({X, Y, Z, _}) ->
        [digraph:add_edge(G, {X, Y, Z, W}, {X+DX, Y+DY, Z+DZ, W}) ||
          DX <- lists:seq(-1,1), DY <- lists:seq(-1,1), DZ <- lists:seq(-1,1), 
          DX /= 0 orelse DY /= 0 orelse DZ /= 0]
    end, Vertices).

connect_cubes(W1, W2, Size, G) ->
  [connect_cubes(W1, W2, X, Y, Z, G) || X <- lists:seq(0, Size-1), 
                                        Y <- lists:seq(0, Size-1), 
                                        Z <- lists:seq(0, Size-1)].

connect_cubes(W1, W2, X, Y, Z, G) ->
  [digraph:add_edge(G, {X, Y, Z, W1}, {X+DX, Y+DY, Z+DZ, W2}) ||
    DX <- lists:seq(-1,1), DY <- lists:seq(-1,1), DZ <- lists:seq(-1,1)],
  [digraph:add_edge(G, {X+DX, Y+DY, Z+DZ, W2}, {X, Y, Z, W1}) ||
    DX <- lists:seq(-1,1), DY <- lists:seq(-1,1), DZ <- lists:seq(-1,1)].
