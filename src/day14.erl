-module(day14).

-export([part1/0, part2/0]).

-define(mem, mem).
-define(mask_size, 36).

part1() ->
  {ok, Bin} = file:read_file("../input/day14.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  ets:new(mem, [named_table]),
  process_lines(Lines, 0, 0).

process_lines([], _, _) ->
  Mem = ets:tab2list(?mem),
  lists:foldl(fun({_, V}, Acc) -> V+Acc end, 0, Mem);
process_lines([<<>> | Lines], AndMask, OrMask) ->
  process_lines(Lines, AndMask, OrMask);
process_lines([<<"mask = ", Mask/binary>> | Lines], _, _) ->
  OrMask = binary_to_integer(binary:replace(Mask, <<"X">>, <<"0">>, [global]), 2),
  AndMask = binary_to_integer(binary:replace(Mask, <<"X">>, <<"1">>, [global]), 2),
  process_lines(Lines, AndMask, OrMask);
process_lines([<<"mem[", _/binary>> = Line | Lines], AndMask, OrMask) ->
  {Addr, V} = parse_line(Line),
  ets:insert(?mem, {Addr, V band AndMask bor OrMask}),
  process_lines(Lines, AndMask, OrMask).

part2() ->
  {ok, Bin} = file:read_file("../input/day14.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  ets:new(mem, [named_table]),
  process_lines2(Lines, 0, 0).
  
process_lines2([], _, _) ->
  Mem = ets:tab2list(?mem),
  lists:foldl(fun({_, V}, Acc) -> V+Acc end, 0, Mem);
process_lines2([<<>> | Lines], Mask, OrMask) ->
  process_lines2(Lines, Mask, OrMask);
process_lines2([<<"mask = ", Mask/binary>> | Lines], _, _) ->
  OrMask = binary_to_integer(binary:replace(Mask, <<"X">>, <<"0">>, [global]), 2),
  process_lines2(Lines, Mask, OrMask);
process_lines2([<<"mem[", _/binary>> = Line | Lines], Mask, OrMask) ->
  {Addr0, V} = parse_line(Line),
  Addr = Addr0 bor OrMask,
  apply_mask(Addr, V, Mask, ?mask_size-1),
  process_lines2(Lines, Mask, OrMask).

apply_mask(_Addr, _V, <<>>, _) ->
  ok;
apply_mask(Addr, V, <<"X", Mask/binary>>, Pos) ->
  N = 1 bsl Pos,
  ets:insert(?mem, {Addr, V}),
  ets:insert(?mem, {Addr bxor N, V}),
  apply_mask(Addr, V, Mask, Pos-1),
  apply_mask(Addr bxor N, V, Mask, Pos-1);
apply_mask(Addr, V, <<_:1/binary, Mask/binary>>, Pos) ->
  apply_mask(Addr, V, Mask, Pos-1).

parse_line(<<"mem[", Addr:1/binary, "] = ", V/binary>>) ->
  {binary_to_integer(Addr), binary_to_integer(V)};
parse_line(<<"mem[", Addr:2/binary, "] = ", V/binary>>) ->
  {binary_to_integer(Addr), binary_to_integer(V)};
parse_line(<<"mem[", Addr:3/binary, "] = ", V/binary>>) ->
  {binary_to_integer(Addr), binary_to_integer(V)};
parse_line(<<"mem[", Addr:4/binary, "] = ", V/binary>>) ->
  {binary_to_integer(Addr), binary_to_integer(V)};
parse_line(<<"mem[", Addr:5/binary, "] = ", V/binary>>) ->
  {binary_to_integer(Addr), binary_to_integer(V)}.

