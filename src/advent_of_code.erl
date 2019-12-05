-module(advent_of_code).

-export([run/2]).

-type day() :: 1..30.
-type part() :: 1 | 2.

%%====================================================================
%% API functions
%%====================================================================

-spec run(day(), part()) -> number().
run(Day, Part) ->
    Mod = module(Day), Fun = part(Part), Input = read_input(Mod),
    try 
        io:format("~n~n***** Advent of Code - 2019 edition *****~n~n", []),
        io:format("***  ~p (~p) answer: ~p  ***~n~n", [Mod, Fun, Mod:Fun(Input)]),
        io:format("****************************************", [])
    catch
        _:_:_ ->
            io:format("Not implemented yet!~n", []),
            io:format("******************************************~n~n", [])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

read_input(Day) ->
    Path = "src/" ++ atom_to_list(Day) ++ "/input.txt",
    {ok, Bin} = file:read_file(Path),
    Bin.

part(1) -> part1;
part(2) -> part2.

module(Number) ->
    Days = [{N, list_to_atom("day" ++ integer_to_list(N))} 
            || N <- lists:seq(0, 30)],
    proplists:get_value(Number, Days).
