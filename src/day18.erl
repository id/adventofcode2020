-module(day18).

-export([part1/1, part2/1]).
-export([eval_expr/3]).
-export([eval2_expr/3]).

-define(dbg(Fmt), io:format(user, Fmt ++ "\n", [])).
-define(dbg(Fmt, Args), io:format(user, Fmt ++ "\n", Args)).
-define(dbg_var(Var), io:format(user, "~p\n", [Var])).

-define(l_paren, 1).
-define(r_paren, 2).
-define(add, 4).
-define(mul, 8).

part1(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  eval(Lines, 0).

eval([], Sum) ->
  Sum;
eval([<<>> | Lines], Sum) ->
  eval(Lines, Sum);
eval([Line | Lines], Sum) ->
  Res = eval_expr(Line, [], []),
  eval(Lines, Sum+Res).

eval_expr(<<>>, _, [V]) ->
  V;
eval_expr(<<>>, [Op], [V1, V2]) ->
  do_op(V1, Op, V2);
eval_expr(<<" ", Rest/binary>>, Ops, Values) ->
  eval_expr(Rest, Ops, Values);
eval_expr(<<"(", Rest0/binary>>, [Op], [V1]) ->
  {Rest, V2} = eval_expr(Rest0, [], []),
  eval_expr(Rest, [], [do_op(V1, Op, V2)]);
eval_expr(<<"(", Rest0/binary>>, Ops, Values) ->
  {Rest, V} = eval_expr(Rest0, [], []),
  eval_expr(Rest, Ops, [V | Values]);
eval_expr(<<")", Rest/binary>>, [Op], [V1, V2]) ->
  {Rest, do_op(V1, Op, V2)};
eval_expr(<<")", Rest/binary>>, [], [V]) ->
  {Rest, V};
eval_expr(<<"+", Rest/binary>>, Ops, Values) ->
  eval_expr(Rest, [add | Ops], Values);
eval_expr(<<"*", Rest/binary>>, Ops, Values) ->
  eval_expr(Rest, [mul | Ops], Values);
eval_expr(<<V1:1/binary, Rest/binary>>, [Op | Ops], [V2 | Values]) ->
  eval_expr(Rest, Ops, [do_op(binary_to_integer(V1), Op, V2) | Values]);
eval_expr(<<V1:1/binary, Rest/binary>>, Ops, Values) ->
  eval_expr(Rest, Ops, [binary_to_integer(V1) | Values]).

do_op(V1, add, V2) -> V1 + V2;
do_op(V1, mul, V2) -> V1 * V2.

part2(Input) ->
  {ok, Bin} = file:read_file(Input),
  Lines = binary:split(Bin, [<<"\n">>], [global]),
  eval2(Lines, 0).

eval2([], Sum) ->
  Sum;
eval2([<<>> | Lines], Sum) ->
  eval2(Lines, Sum);
eval2([Line | Lines], Sum) ->
  Res = eval2_expr(Line, [], []),
  eval2(Lines, Sum+Res).

calc(_, [], Values) ->
  {[], Values};
calc(CurrentOp, [Op | Ops], [V1, V2 | Values]) when CurrentOp < Op ->
  V = do_op(V1, Op, V2),
  calc(CurrentOp, Ops, [V | Values]);
calc(_, Ops, Values) ->
  {Ops, Values}.

eval2_expr(<<>>, [], [V]) ->
  V;
eval2_expr(<<"+", Rest/binary>>, [], Values) ->
  eval2_expr(Rest, [add], Values);
eval2_expr(<<"*", Rest/binary>>, [], Values) ->
  eval2_expr(Rest, [mul], Values);
eval2_expr(<<"+", Rest/binary>>, Ops, Values) ->
  eval2_expr(Rest, [add | Ops], Values);
eval2_expr(<<"*", Rest/binary>>, [mul | _] = Ops, Values0) ->
  eval2_expr(Rest, [mul | Ops], Values);
eval2_expr(<<"*", Rest/binary>>, Ops0, Values0) ->
  {Ops, Values} = calc(Ops0, Values0),
  eval2_expr(Rest, [mul | Ops], Values);


eval2_expr(Rest, [mul | Ops], [V1, V2 | Values]) ->
  eval2_expr(Rest, Ops, [V1 * V2 | Values]);
eval2_expr(<<" ", Rest/binary>>, Ops, Values) ->
  eval2_expr(Rest, Ops, Values);
eval2_expr(<<"(", Rest0/binary>>, [Op], [V1]) ->
  {Rest, V2} = eval2_expr(Rest0, [], []),
  eval2_expr(Rest, [], [do_op(V1, Op, V2)]);
eval2_expr(<<"(", Rest0/binary>>, Ops, Values) ->
  {Rest, V} = eval2_expr(Rest0, [], []),
  eval2_expr(Rest, Ops, [V | Values]);
eval2_expr(<<")", Rest/binary>>, [Op], [V1, V2]) ->
  {Rest, do_op(V1, Op, V2)};
eval2_expr(<<")", Rest/binary>>, [], [V]) ->
  {Rest, V};
eval2_expr(<<V1:1/binary, Rest/binary>>, [add | Ops], [V2 | Values]) ->
  eval2_expr(Rest, Ops, [binary_to_integer(V1) + V2 | Values]);
eval2_expr(<<V1:1/binary, Rest/binary>>, Ops, Values) ->
  eval2_expr(Rest, Ops, [binary_to_integer(V1) | Values]).
