-module(day07).

-export([part1/0]).

-define(pattern, <<"^(.+) bags contain (.+)\.$">>).
-define(bag_pattern, <<"^\.\s(.+)\sbags*$">>).
-define(the_bag, <<"shiny gold">>).

part1() ->
  {ok, Bin} = file:read_file("../input/day07.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  G = digraph:new(),
  process_lines(G, Lines),
  L = digraph_utils:reachable_neighbours([?the_bag], G),
  erlang:length(L).

process_lines(_G, []) ->
  ok;
process_lines(G, [Line | Lines]) ->
  {match, [Color, Content]} = re:run(Line, ?pattern, [{capture, all_but_first, binary}]),
  InnerBags = binary:split(Content, <<", ">>, [global]),
  digraph:add_vertex(G, Color),
  process_inner_bags(G, Color, InnerBags),
  process_lines(G, Lines).

process_inner_bags(_G, _Color, []) ->
  ok;
process_inner_bags(G, Color, [<<"no other bags">> | Bags]) ->
  process_inner_bags(G, Color, Bags);
process_inner_bags(G, Color, [Bag | Bags]) ->
  {match, [V]} = re:run(Bag, ?bag_pattern, [{capture, all_but_first, binary}]),
  digraph:add_vertex(G, V),
  digraph:add_edge(G, V, Color),
  process_inner_bags(G, Color, Bags).
