-module(day3).

-define(ORIGIN, {0, 0}).

-export([part1/1, part2/1]).

%%====================================================================
%% Main functions
%%====================================================================

part1(Input) ->
    Wires = read_wires(Input),
    ParsedWires = lists:map(fun(Wire) -> [parse(Path) || Path <- Wire] end, Wires),
    [Wire1, Wire2] = [load_wire(W) || W <- ParsedWires],
    Intersections = calculate_intersections(Wire1, Wire2),
    lists:min([distance_to_origin(Point) || Point <- Intersections]).

part2(Input) ->
    Wires = read_wires(Input),
    ParsedWires = lists:map(fun(Wire) -> [parse(Path) || Path <- Wire] end, Wires),
    FullWires = [load_wire(W) || W <- ParsedWires],
    minimum_steps(FullWires).

%%====================================================================
%% Internal functions
%%====================================================================

minimum_steps([Wire1, Wire2] = Wires) ->
    Intersections = calculate_intersections(Wire1, Wire2),
    [DWire1, DWire2] = [add_steps(W) || W <- Wires],
    Steps = [steps_for(Point, DWire1, DWire2) || Point <- Intersections],
    lists:min([X + Y || {X, Y} <- Steps]).

add_steps(Wire) ->
    lists:zip(lists:reverse(Wire), lists:seq(0, length(Wire) - 1)).

steps_for(Point, DecoratedWire1, DecoratedWire2) ->
    Step1 = hd([Steps || {Point1, Steps} <- DecoratedWire1, Point1 == Point]),
    Step2 = hd([Steps || {Point1, Steps} <- DecoratedWire2, Point1 == Point]),
    {Step1, Step2}.

calculate_intersections(Wire1, Wire2) ->
    [Set1, Set2] = [sets:from_list(W) || W <- [Wire1, Wire2]],
    sets:to_list(sets:intersection(Set1, Set2)) -- [{0, 0}]. %% remove trivial solution

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

distance_to_origin(Point) ->
    manhattan_distance(?ORIGIN, Point).

load_wire(Paths) ->
    lists:foldl(fun(Path, Acc) ->
        lists:flatten([points(Path, hd(Acc)) -- [hd(Acc)] | Acc]) 
    end, [?ORIGIN], Paths).

points({Op, A}, Point) ->
    Ones = lists:duplicate(A, 1),
    lists:foldl(fun(Step, Acc) -> [Op(Step, hd(Acc))] ++ Acc end, [Point], Ones).

parse(<<"R", B/binary>>) -> {fun(A, {X, Y}) -> {X + A, Y} end, binary_to_integer(B)};
parse(<<"L", B/binary>>) -> {fun(A, {X, Y}) -> {X - A, Y} end, binary_to_integer(B)};
parse(<<"U", B/binary>>) -> {fun(A, {X, Y}) -> {X, Y + A} end, binary_to_integer(B)};
parse(<<"D", B/binary>>) -> {fun(A, {X, Y}) -> {X, Y - A} end, binary_to_integer(B)}.

read_wires(Bin) ->
    [string:lexemes(Wire, ",") || Wire <- string:lexemes(Bin, [$\n])].

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

load_example(Example) ->
    ParsedWires = lists:map(fun(Wire) -> [parse(Path) || Path <- Wire] end, Example),
    [load_wire(W) || W <- ParsedWires].

example1() ->
    [
        [<<"R75">>,<<"D30">>,<<"R83">>,<<"U83">>,<<"L12">>,<<"D49">>,<<"R71">>,<<"U7">>,<<"L72">>],
        [<<"U62">>,<<"R66">>,<<"U55">>,<<"R34">>,<<"D71">>,<<"R55">>,<<"D58">>,<<"R83">>]
    ].

minimum_steps_test() ->
    Ex1 = load_example(example1()),
    ?assertEqual(minimum_steps(Ex1), 610).

-endif.
