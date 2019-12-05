-module(day1).

-export([part1/1, part2/1]).

%%====================================================================
%% Main functions
%%====================================================================

part1(Input) ->
    Masses = read_masses(Input),
    Fuels = lists:map(fun required_fuel/1, Masses),
    lists:sum(Fuels).

part2(Input) ->
    Masses = read_masses(Input),
    Fuels = lists:map(fun required_fuel_total/1, Masses),
    lists:sum(Fuels).

%%====================================================================
%% Internal functions
%%====================================================================

required_fuel(Mass) ->
    (Mass div 3) - 2.

required_fuel_total(Mass) ->
    required_fuel_total(Mass, 0).

required_fuel_total(Mass, Total) when Mass =< 0 -> 
    Total;
required_fuel_total(Mass, Total) ->
   FuelMass = max(required_fuel(Mass), 0),
   required_fuel_total(FuelMass, FuelMass + Total).

read_masses(Input) ->
    Lines = binary:split(Input, [<<"\n">>], [global, trim_all]),
    lists:map(fun(Line) -> list_to_integer(binary_to_list(Line)) end, Lines).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

required_fuel_test() ->
    ?assertEqual(required_fuel(12), 2),
    ?assertEqual(required_fuel(14), 2),
    ?assertEqual(required_fuel(1969), 654),
    ?assertEqual(required_fuel(100756), 33583).

required_fuel_total_test() ->
    ?assertEqual(required_fuel_total(14), 2),
    SomeFuel = 654 + 216 + 70 + 21 + 5,
    ?assertEqual(required_fuel_total(1969), SomeFuel),
    LotOfFuel = 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2,
    ?assertEqual(required_fuel_total(100756), LotOfFuel).

-endif.
