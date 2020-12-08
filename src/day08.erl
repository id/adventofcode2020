-module(day08).

-export([part1/0, part2/0]).

-define(ram, ram).
-define(ram2, ram2).

part1() ->
  {ok, Bin} = file:read_file("../input/day08.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  case ets:info(?ram) of
    undefined -> ok;
    _ -> ets:delete(?ram)
  end,
  ets:new(?ram, [named_table]),
  parse_program(0, Lines),
  run(0, 0).

run(Pointer0, Acc0) ->
  case ets:lookup(?ram, Pointer0) of
    [{_, _, _, 1}] ->
      Acc0;
    [{Pos, Op, Arg, 0}] ->
      ets:insert(?ram, {Pos, Op, Arg, 1}),
      {Pointer, Acc} = do(Pos, Op, Arg, Acc0),
      run(Pointer, Acc)
  end.

part2() ->
  {ok, Bin} = file:read_file("../input/day08.txt"),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  case ets:info(?ram) of
    undefined -> ok;
    _ -> ets:delete(?ram)
  end,
  case ets:info(?ram2) of
    undefined -> ok;
    _ -> ets:delete(?ram2)
  end,
  ets:new(?ram, [named_table]),
  ets:new(?ram2, [named_table]),
  parse_program(0, Lines),
  ets:insert(?ram2, ets:tab2list(?ram)),
  run2(0, 0, 0).

run2(FlipPos0, Pointer0, Acc0) ->
  case ets:lookup(?ram2, Pointer0) of
    [] -> 
      io:format(user, "---> ~p~n", [Acc0]),
      erlang:exit(0);
    [{Pos, Op, Arg, Counter}] ->
      case Counter of
        0 -> ok;
        1 -> 
          ets:insert(?ram2, ets:tab2list(?ram)),
          FlipPos = try_flip_next(ets:lookup(?ram2, FlipPos0+1)),
          run2(FlipPos, 0, 0)
      end,
      ets:insert(?ram2, {Pos, Op, Arg, Counter+1}),
      {Pointer, Acc} = do(Pos, Op, Arg, Acc0),
      run2(FlipPos0, Pointer, Acc)
  end.

try_flip_next([{Pos, acc, _, _}]) ->
  try_flip_next(ets:lookup(?ram2, Pos+1));
try_flip_next([{Pos, jmp, Arg, _}]) ->
  ets:insert(?ram2, {Pos, nop, Arg, 0}),
  Pos;
try_flip_next([{Pos, nop, Arg, _}]) ->
  ets:insert(?ram2, {Pos, jmp, Arg, 0}),
  Pos.

do(Pos, nop, _, Acc) ->   {Pos+1, Acc};
do(Pos, acc, Arg, Acc) -> {Pos+1, Acc+Arg};
do(Pos, jmp, Arg, Acc) -> {Pos+Arg, Acc}.


parse_program(_, []) ->
  ok;
parse_program(_Counter, [<<>> | _Lines]) ->
  ok;
parse_program(Counter, [Line | Lines]) ->
  {Op, Arg} = parse_instruction(Line),
  ets:insert(?ram, {Counter, Op, Arg, 0}),
  parse_program(Counter+1, Lines).

parse_instruction(<<"nop ", Arg/binary>>) ->
  {nop, binary_to_integer(Arg)};
parse_instruction(<<"acc ", Arg/binary>>) ->
  {acc, binary_to_integer(Arg)};
parse_instruction(<<"jmp ", Arg/binary>>) ->
  {jmp, binary_to_integer(Arg)}.
