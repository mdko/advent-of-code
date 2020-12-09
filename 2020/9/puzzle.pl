% Puzzle 9

main :-
    get_lines(Lines),
    part1(Lines),
    part2(Lines).

part1(Lines) :-
    check(Lines).

part2(Lines) :-
    check(Lines).

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
        write(X), nl
    ).

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