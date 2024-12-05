:- use_module(library(clpfd)).

read_file_lines(FileName, Lines) :-
    open(FileName, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).
read_lines(Stream, []) :- at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, L),
    string_chars(L, Line),
    read_lines(Stream, Lines).

is_xmas([['X','M','A','S']|_]).
is_xmas([['S','A','M','X']|_]).
is_xmas([
    ['S',_,_,_],
    ['A',_,_,_],
    ['M',_,_,_],
    ['X',_,_,_]
]).
is_xmas([
    ['X',_,_,_],
    ['M',_,_,_],
    ['A',_,_,_],
    ['S',_,_,_]
]).
is_xmas([
    ['X',_,_,_],
    [_,'M',_,_],
    [_,_,'A',_],
    [_,_,_,'S']
]).
is_xmas([
    ['S',_,_,_],
    [_,'A',_,_],
    [_,_,'M',_],
    [_,_,_,'X']
]).
is_xmas([
    [_,_,_,'X'],
    [_,_,'M',_],
    [_,'A',_,_],
    ['S',_,_,_]
]).
is_xmas([
    [_,_,_,'S'],
    [_,_,'A',_],
    [_,'M',_,_],
    ['X',_,_,_]
]).

is_x_mas([
    ['M',_,'S'],
    [_,'A',_],
    ['M',_,'S']
]).
is_x_mas([
    ['S',_,'S'],
    [_,'A',_],
    ['M',_,'M']
]).
is_x_mas([
    ['S',_,'M'],
    [_,'A',_],
    ['S',_,'M']
]).
is_x_mas([
    ['M',_,'M'],
    [_,'A',_],
    ['S',_,'S']
]).

slice(Index, Length, List, Slice) :-
    length(Prefix, Index),
    append([Prefix, Slice, _], List),
    length(Slice, Length).

window([L0|Lines], Size, W) :-
    length(L0, Len0),
    Max0 is Len0 - Size,
    between(0, Max0, X0),
    length([L0|Lines], Len1),
    Max1 is Len1 - Size,
    between(0, Max1, X1),
    slice(X1, Size, [L0|Lines], Rows),
    maplist(slice(X0, Size), Rows, W).

combine_lists(Postfix, Prefix, R) :- append(Prefix, Postfix, R).

pad_matrix(Lines, Lines1) :-
    maplist(combine_lists(['.','.','.']), Lines, Lines0),
    Lines0 = [L0|_],
    length(L0, LineLen),
    length(Padding, LineLen),
    maplist(=('.'), Padding),
    append(Lines0, [Padding, Padding, Padding], Lines1).

count_xmas(Goal, W, C, C1) :-
    findall(_, call(Goal, W), M),
    length(M, C0),
    C1 is C + C0.

solve(P1, P2, Lines) :-
    pad_matrix(Lines, Lines1),
    findall(W, window(Lines1, 4, W), Windows1),
    foldl(count_xmas(is_xmas), Windows1, 0, P1),
    findall(W, window(Lines1, 3, W), Windows2),
    foldl(count_xmas(is_x_mas), Windows2, 0, P2).

solve :-
    read_file_lines("../../input/2024/04.txt", Lines),
    solve(P1, P2, Lines),
    format("Part 1: ~w~nPart 2: ~w~n", [P1, P2]),
    halt.
