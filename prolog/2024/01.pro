:- use_module(library(dcg/basics)).

parse_file(Path, Xs, Ys) :-
    read_file_to_codes(Path, Codes, []),
    phrase(lines(Xs, Ys), Codes).

lines([], []) --> eos.
lines([X|Xs], [Y|Ys]) --> line(X, Y), lines(Xs, Ys).

line(X, Y) --> number(X), whites, number(Y), "\n".

total_diff([], [], 0).
total_diff([X|Xs], [Y|Ys], Total) :-
    total_diff(Xs, Ys, SubTotal),
    Total is abs(X - Y) + SubTotal.

inner_product([], 0).
inner_product([K-[A, B]|T], Res) :-
    inner_product(T, Res0),
    Res is K * A * B + Res0.
inner_product([_|T], Res) :- inner_product(T, Res). % ignore values not present in both lists

solve(P1, P2) :-
    parse_file("../../input/2024/01.txt", Xs, Ys),
    sort(0, @=<, Xs, Xss), sort(0, @=<, Ys, Yss),
    total_diff(Xss, Yss, P1),

    clumped(Xss, Keys), clumped(Yss, Bag), % count occurrences of each number in both lists
    append(Keys, Bag, Occ0),
    sort(1, @=<, Occ0, Occ),
    group_pairs_by_key(Occ, Joined), % merge shared occurrences in both lists
    inner_product(Joined, P2).

solve :-
    solve(P1, P2),
    format("Part 1: ~w~nPart 2: ~w~n", [P1, P2]),
    halt.
