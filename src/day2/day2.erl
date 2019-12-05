-module(day2).

-export([part1/1, part2/1]).

%%====================================================================
%% Main functions
%%====================================================================

part1(Input) ->
    Program = read_program(Input),
    ProgramArray = array:from_list(Program),
    ProgramAlarm1202 = replace_params(ProgramArray, [{1, 12}, {2, 2}]),
    run(ProgramAlarm1202).

part2(Input) ->
    Program = read_program(Input),
    ProgramArray = array:from_list(Program),
    [{1, Param1}, {2, Param2}] = params_for_output(19690720, ProgramArray),
    100 * Param1 + Param2.

%%====================================================================
%% Internal functions
%%====================================================================

params_for_output(Expected, Program) ->
    PossibleParams = [[{1, X}, {2, Y}] || X <- lists:seq(0, 99), Y <- lists:seq(0, 99)],
    RightParams = fun(P) -> run(replace_params(Program, P)) == Expected end,
    {value, Answer} = 
        lists:search(fun(Params) -> RightParams(Params) end, PossibleParams),
    Answer.

replace_params(Array, [{1, P1}, {2, P2}]) ->
    array:set(1, P1, array:set(2, P2, Array)).

run(ProgramArray) ->
    Memory = run(build_programs(ProgramArray), ProgramArray),
    array:get(0, Memory). %% the program's output is the value at addr=0

run([], CodeArray) -> CodeArray;
run([SubProgram | Rest], CodeArray) ->
    case run_code(SubProgram, CodeArray) of
        {halt, Array} ->
            Array;
        {continue, NewArray} ->
            run(Rest, NewArray)
    end.

run_code(#{opcode := OpCode, idx1 := Idx1, idx2 := Idx2, res_idx := ResultIdx}, Array) ->
    case operation(array:get(OpCode, Array)) of
        halt ->
            {halt, Array};
        Op ->
            Args = [array:get(I, Array) 
                || I <- [array:get(Idx1, Array), array:get(Idx2, Array)]],
            {continue, array:set(array:get(ResultIdx, Array), Op(Args), Array)}
    end.

operation(1) -> fun(Args) -> apply(erlang, '+', Args) end;
operation(2) -> fun(Args) -> apply(erlang, '*', Args) end;
operation(99) -> halt;
operation(_) -> error_bad_op.

build_programs(ProgramArray) ->
    List = array:to_list(ProgramArray),
    AmountOfPrograms = length(List) div 4,
    lists:foldl(fun(ProgramNumber, Programs) ->
        Op = 4 * ProgramNumber,
        Idx1 = 4 * ProgramNumber + 1,
        Idx2 = 4 * ProgramNumber + 2,
        ResultIdx = 4 * ProgramNumber + 3,
        Program = program(Op, Idx1, Idx2, ResultIdx),
        lists:append(Programs, [Program])
        end, [], lists:seq(0, AmountOfPrograms - 1)).

program(Op, Idx1, Idx2, ResIdx) ->
    #{ opcode => Op
     , idx1 => Idx1
     , idx2 => Idx2
     , res_idx => ResIdx
     }.

read_program(Input) ->
    ContentsWithoutNewLines = binary:split(Input, <<"\n">>, [global, trim_all]),
    Codes = binary:split(hd(ContentsWithoutNewLines), <<",">>, [global, trim_all]),
    lists:map(fun binary_to_integer/1, Codes).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

subprograms_test() ->
    Code1 = [1, 3, 4, 5],
    ?assertEqual(build_programs(Code1), [program(0, 1, 2, 3)]),

    Code2 = [1, 3, 4, 5, 2, 4, 6, 7],
    ?assertEqual(build_programs(Code2), [program(0, 1, 2, 3), program(4, 5, 6, 7)]).

run_code_test() ->
    Code1 = [1, 0, 0, 0, 99],
    ?assertEqual(run(Code1), [2, 0, 0, 0, 99]),
    
    Code2 = [2, 3, 0, 3, 99],
    ?assertEqual(run(Code2), [2, 3, 0, 6, 99]),
    
    Code3 = [2,4,4,5,99,0],
    ?assertEqual(run(Code3), [2, 4, 4, 5, 99, 9801]),

    Code4 = [1, 1, 1, 4, 99, 5, 6, 0, 99],
    ?assertEqual(run(Code4), [30, 1, 1, 4, 2, 5, 6, 0, 99]).

replace_params_test() ->
    Code = [1, 2, 3],
    ?assertEqual(replace_params(Code, [{0, 2}, {1, 3}]), [2, 3, 3]).

-endif.
