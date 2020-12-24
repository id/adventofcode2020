-module(day16).

-export([part1/1, part2/1]).

-define(rule_pattern, "^.+:\s([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$").
-define(rule_pattern2, "^(.+):\s([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$").

part1(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  {Tickets, Ranges} = process_header(Lines, []),
  process_lines(Tickets, Ranges, 0).

process_header([<<>> | Rest], Ranges) ->
  {_, Tickets} = lists:split(4, Rest),
  {Tickets, Ranges};
process_header([Line | Rest], Ranges0) ->
  {match, [Res]} = re:run(Line, ?rule_pattern, [global, {capture, all_but_first, binary}]),
  [Lo1, Hi1, Lo2, Hi2] = lists:map(fun erlang:binary_to_integer/1, Res),
  Ranges1 = merge_ranges(Lo1, Hi1, Ranges0, Ranges0),
  Ranges = merge_ranges(Lo2, Hi2, Ranges1, Ranges1),
  process_header(Rest, Ranges).

merge_ranges(Lo, Hi, [], NewRanges) ->
  [{Lo, Hi} | NewRanges];
merge_ranges(Lo, Hi, [{Lo1, Hi1} | _], NewRanges) when Lo >= Lo1, Hi =< Hi1 ->
  NewRanges;
merge_ranges(Lo, Hi, [{Lo1, Hi1} | _], NewRanges) when Lo1 >= Lo, Hi1 =< Hi ->
  [{Lo, Hi} | lists:delete({Lo1, Hi1}, NewRanges)];
merge_ranges(Lo, Hi, [{_, _} | Ranges], NewRanges) ->
  merge_ranges(Lo, Hi, Ranges, NewRanges).

process_lines([], _, Acc) ->
  Acc;
process_lines([Ticket | Tickets], Ranges, Sum0) ->
  L = binary:split(Ticket, <<",">>, [global]),
  Numbers = lists:filtermap(fun(<<>>) -> false; (Bin) -> {true, binary_to_integer(Bin)} end, L),
  Sum = lists:foldl(fun(N, Acc) -> add_if_invalid(N, Ranges, Acc) end, Sum0, Numbers),
  process_lines(Tickets, Ranges, Sum).

add_if_invalid(N, Ranges, Acc) ->
  case lists:any(fun({Lo, Hi}) -> N >= Lo andalso N =< Hi end, Ranges) of
    true -> Acc;
    false -> Acc+N
  end.

part2(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  {Tickets, MergedRanges, Fields} = process_header2(Lines, [], []),
  ValidTickets = filter_invalid(Tickets, MergedRanges, []),
  find_fields_order(lists:seq(1, length(Fields)), ValidTickets, Fields, #{}).

find_fields_order(_Positions, _Tickets, [], Options) ->
  Options;
find_fields_order(Positions, Tickets, [{Field, R1, R2} | Fields], Options0) ->
  FieldOptions = find_field_pos(Positions, Tickets, R1, R2, []),
  Options = maps:put(Field, FieldOptions, Options0),
  find_fields_order(Positions, Tickets, Fields, Options).

find_field_pos([], _Tickets, {_Lo1, _Hi1}, {_Lo2, _Hi2}, FieldOptions) ->
  FieldOptions;
find_field_pos([Pos | Positions], Tickets, {Lo1, Hi1}, {Lo2, Hi2}, FieldOptions) ->
  case valid_field(Pos, Tickets, {Lo1, Hi1}, {Lo2, Hi2}) of
    true -> find_field_pos(Positions, Tickets, {Lo1, Hi1}, {Lo2, Hi2}, [Pos | FieldOptions]);
    false -> find_field_pos(Positions, Tickets, {Lo1, Hi1}, {Lo2, Hi2}, FieldOptions)
  end.

valid_field(_Pos, [], {_Lo1, _Hi1}, {_Lo2, _Hi2}) ->
  true;
valid_field(Pos, [Ticket | Tickets], {Lo1, Hi1}, {Lo2, Hi2}) ->
  N = lists:nth(Pos, Ticket),
  case (N >= Lo1 andalso N =< Hi1) orelse (N >= Lo2 andalso N =< Hi2) of
    true -> valid_field(Pos, Tickets, {Lo1, Hi1}, {Lo2, Hi2});
    false -> false
  end.

process_header2([<<>> | Rest], MergedRanges, Ranges) ->
  {_, Tickets} = lists:split(4, Rest),
  {Tickets, MergedRanges, lists:reverse(Ranges)};
process_header2([Line | Rest], MergedRanges0, Ranges) ->
  {match, [[Field | Res]]} = re:run(Line, ?rule_pattern2, [global, {capture, all_but_first, binary}]),
  [Lo1, Hi1, Lo2, Hi2] = lists:map(fun erlang:binary_to_integer/1, Res),
  MergedRanges1 = merge_ranges(Lo1, Hi1, MergedRanges0, MergedRanges0),
  MergedRanges = merge_ranges(Lo2, Hi2, MergedRanges1, MergedRanges1),
  process_header2(Rest, MergedRanges, [{Field, {Lo1, Hi1}, {Lo2, Hi2}} | Ranges]).

filter_invalid([], _, Acc) ->
  Acc;
filter_invalid([<<>> | Tickets], Ranges, Acc) ->
  filter_invalid(Tickets, Ranges, Acc);
filter_invalid([Ticket | Tickets], Ranges, Acc) ->
  L = binary:split(Ticket, <<",">>, [global]),
  Numbers = lists:filtermap(fun(<<>>) -> false; (Bin) -> {true, binary_to_integer(Bin)} end, L),
  case lists:any(fun(N) -> is_invalid(N, Ranges) end, Numbers) of
    true -> filter_invalid(Tickets, Ranges, Acc);
    false -> filter_invalid(Tickets, Ranges, [Numbers | Acc])
  end.

is_invalid(N, Ranges) ->
  not lists:any(fun({Lo, Hi}) -> N >= Lo andalso N =< Hi end, Ranges).
