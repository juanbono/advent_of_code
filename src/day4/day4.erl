-module(day4).

-export([part1/1, part2/1]).

%%====================================================================
%% Main functions
%%====================================================================

part1(Input) ->
    {Start, End} = read_range(Input),
    AllPasswords = [integer_to_list(N) || N <- lists:seq(Start, End)],
    Passwords = lists:filter(fun is_valid_password1/1, AllPasswords),
    length(Passwords).

part2(Input) ->
    {Start, End} = read_range(Input),
    AllPasswords = [integer_to_list(N) || N <- lists:seq(Start, End)],
    Passwords = lists:filter(fun is_valid_password2/1, AllPasswords),
    length(Passwords).

%%====================================================================
%% Internal functions
%%====================================================================

is_valid_password1(Pass) ->
    is_non_decreasing(Pass) andalso has_double(Pass).

is_valid_password2(Pass) ->
    is_non_decreasing(Pass) andalso has_double_strict(Pass).

is_non_decreasing(Pass) -> lists:sort(Pass) == Pass.

has_double([]) -> false;
has_double([A, A]) -> true;
has_double([A, A | _Rest]) -> true;
has_double([_H | T]) -> false orelse has_double(T).

has_double_strict(Pass) ->
    EncodedPass = encode(Pass),
    proplists:is_defined(2, EncodedPass).

%% run-length encoding
encode(List) ->
    Counts = lists:foldr(
        fun (Prev, [{Count, Prev} | Rest]) ->
                [{Count + 1, Prev} | Rest];
            (Current, Acc) ->
                [{1, Current} | Acc]
        end, [], List),
    lists:flatten(Counts).

read_range(Input) ->
    FirstLine = hd(binary:split(Input, <<"\n">>)),
    [Start, End] = [binary_to_integer(N) || N <- string:lexemes(FirstLine, [$-])],
    {Start, End}.

%%====================================================================
%% Tests
%%====================================================================
