% Puzzle 9

main :-
    get_lines(Lines),
%    part1(Lines).
    part1_fast(Lines),
    part2(Lines).

% This version is *SLOW*. Probably could use a cut or something :)
part1(Lines):-
    find_nonmember(Lines, X),
    write(X), nl.

part1_fast(Lines) :-
    find_nonmember_fast(Lines).

part2(Lines) :-
    contiguous(Lines, 3199139634, Res),
    write(Res), nl.

get_lines(Lines) :-
    open('input', read, Stream),
    read_file(Stream, Lines),
    close(Stream).

% ! -- "cut":
%   1) always succeeds
%   2) commits to any choices made since the parent was unified with the left-hand side of the rule

% fail:
%  1) immediately fail when encountered as a goal, thus invoking backtracking
nonmember(Arg, [Arg|_]) :- !, fail.
nonmember(Arg, [_|Tail]) :- !, nonmember(Arg, Tail).
nonmember(_, []).

find_nonmember(L, X) :-
    take(25, L, M),
    nth0(25, L, X),  % 0-indexed
    get_sums(M, Sums),
    nonmember(X, Sums).
find_nonmember([_|T], X) :-
    find_nonmember(T, X).

find_nonmember_fast([H|T]) :-
    take(25, [H|T], M),
    nth0(25, [H|T], X),  % 0-indexed
    get_sums(M, Sums),
    (
        member(X, Sums), find_nonmember_fast(T);
        write(X), nl
    ).

contiguous(L, Target, Res) :-
    get_sublist_with_sum(L, Target, M),
    min_member(X, M),
    max_member(Y, M),
    Res is X + Y.

drop_last([], []).
drop_last([_|[]], []).
drop_last([H|T], [H|Res]) :-
    drop_last(T, Res).

% Tries successively smaller prefixes of L
find_sublist_sum([], _, _) :- !, fail.
find_sublist_sum(L, Target, L) :- sum(L, Target).
find_sublist_sum(L, Target, N) :-
    drop_last(L, M),
    !,
    find_sublist_sum(M, Target, N).

get_sublist_with_sum(L, Target, M) :- find_sublist_sum(L, Target, M).
get_sublist_with_sum([_|T], Target, M) :- get_sublist_with_sum(T, Target, M).

sum([], 0).
sum([H|T], Res) :-
    sum(T, N),
    Res is H + N.

take(_, [], []).
take(0, _, []).
take(N, [H|T], [H|R]) :-
    Nm1 is N - 1,
    take(Nm1, T, R).

get_sums_from_first([_], []).
get_sums_from_first([N1,N2|T], [Res|Rest]) :-
    Res is N1 + N2,
    get_sums_from_first([N1|T], Rest).

get_sums([_], []).
get_sums([H|T], All) :-
    get_sums_from_first([H|T], L),
    get_sums(T, Rest),
    append(L, Rest, All).

read_file(Stream, []) :-
    at_end_of_stream(Stream).

read_file(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, H),
    read_file(Stream, T).