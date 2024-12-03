:- use_module(library(dcg/basics)).

all_instr([]) --> eos.
all_instr([X-Y|Xs]) --> mult(X, Y), all_instr(Xs).
all_instr(Xs) --> [_], all_instr(Xs).

enabled_instr([]) --> eos.
enabled_instr([X-Y|Xs]) --> mult(X, Y), enabled_instr(Xs).
enabled_instr(Xs) --> "don't()", disabled_instr(Xs).
enabled_instr(Xs) --> [_], enabled_instr(Xs).

disabled_instr([]) --> eos.
disabled_instr(Xs) --> "do()", enabled_instr(Xs).
disabled_instr(Xs) --> [_], disabled_instr(Xs).

mult(X, Y) --> "mul(", number(X), ",", number(Y), ")".

process(X-Y, V0, V) :- V is V0 + X * Y.

solve(P1, P2, Codes) :-
    phrase(all_instr(Xs), Codes),
    foldl(process, Xs, 0, P1),
    phrase(enabled_instr(Ys), Codes),
    foldl(process, Ys, 0, P2).

solve :-
    read_file_to_codes("../../input/2024/03.txt", Codes, []),
    solve(P1, P2, Codes),
    format("Part 1: ~w~nPart 2: ~w~n", [P1, P2]),
    halt.
