:- use_module(library(dcg/basics)).

parse_file(Path, Xs) :- read_file_to_codes(Path, Codes, []), phrase(lines(Xs), Codes).

lines([]) --> eos.
lines([X|Xs]) --> line(X), lines(Xs).

line([]) --> "\n".
line([X|Xs]) --> number(X), whites, line(Xs).

sub(A, B, X) :- X is A - B.
sub_rev(A, B, X) :- X is B - A.
ops(Op) :- member(Op, [sub, sub_rev]).

ordered(Dam, _, [_]) :- Dam >= 0.
ordered(Dam, Op, [A,B|Tl]) :- call(Op, A, B, X), between(1, 3, X), ordered(Dam, Op, [B|Tl]).
ordered(Dam, Op, [A,_|Tl]) :- Dam1 is Dam - 1, ordered(Dam1, Op, [A|Tl]).

ordered(Dam, Xs) :- ops(Op), ordered(Dam, Op, Xs).
ordered(Dam, [_|Xs]) :- Dam1 is Dam - 1, ops(Op), ordered(Dam1, Op, Xs).

solve(P1, P2) :-
    parse_file("../../input/2024/02.txt", Reports),
    include(ordered(0), Reports, Safe0), length(Safe0, P1),
    include(ordered(1), Reports, Safe1), length(Safe1, P2).

solve :-
    solve(P1, P2),
    format("Part 1: ~w~nPart 2: ~w~n", [P1, P2]),
    halt.
