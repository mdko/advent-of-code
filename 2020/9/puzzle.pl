% Puzzle 9

main :-
    get_lines(Lines),
    part1(Lines),
    part2(Lines).

part1(Lines):-
    check(Lines).

part2(Lines) :-
    contiguous(Lines, 3199139634, Res),
    write(Res), nl.

get_lines(Lines) :-
    open('input', read, Stream),
    read_file(Stream, Lines),
    close(Stream).

check([H|T]) :-
    take(25, [H|T], M),
    nth0(25, [H|T], X),  % 0-indexed
    get_sums(M, Sums),
    (
      member(X, Sums), check(T);
      write(X), nl  % I'd like to actually return this value
    ).

contiguous(L, Target, Res) :-
    get_sublist_with_sum(L, Target, M),
    min_member(M, X),
    max_member(M, Y),
    Res is X + Y.

drop_last([], []).
drop_last([_|[]], []).
drop_last([H|T], [H|Res]) :-
    drop_last(T, Res).

% This works
find_sublist_sum([], _, []).
find_sublist_sum(L, Target, L) :- sum(L, Target).
find_sublist_sum(L, Target, N) :-
    drop_last(L, M),
    find_sublist_sum(M, Target, N).

is_empty(List) :- not(member(_, List)).

% TODO figure out how to do this
get_sublist_with_sum([], _, []).
get_sublist_with_sum([H|T], Target, M) :-
    (
      find_sublist_sum([H|T], Target, M), not(is_empty(M));
      get_sublist_with_sum(T, Target, M)
    ).

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