main :-
    stream_to_lazy_list(current_input, InCodes),
    lazy_list_materialize(InCodes),
    string_codes(InStr, InCodes),
    split_string(InStr, "\n", "\n", GamesRaw),
    maplist(string_chars, GamesRaw, GamesChr),
    maplist(simulate_game, GamesChr, GameScores),
    sum_list(GameScores, TotalScore),
    write(TotalScore),nl,
    maplist(simulate_game2, GamesChr, GameScores2),
    sum_list(GameScores2, TotalScore2),
    write(TotalScore2),nl.

simulate_game(GameChr, Score) :-
    %write("Simulating "),write(GameRaw),nl,
    choice_score(GameChr, ChoiceScore),
    outcome_score(GameChr, OutcomeScore),
    Score is ChoiceScore + OutcomeScore.

simulate_game2(GameChr, Score) :-
    find_choice(GameChr, Choice),
    [L, _, _] = GameChr,
    simulate_game([L, _, Choice], Score).

choice_score([_, _, 'X'], 1).
choice_score([_, _, 'Y'], 2).
choice_score([_, _, 'Z'], 3).

find_choice([X, _, 'X'], R) :- find_choice(X, 0, R).
find_choice([X, _, 'Y'], R) :- find_choice(X, 3, R).
find_choice([X, _, 'Z'], R) :- find_choice(X, 6, R).
find_choice(X, N, R) :- outcome_score([X, _, R], N).

outcome_score(['A', _, 'X'], 3).
outcome_score(['A', _, 'Y'], 6).
outcome_score(['A', _, 'Z'], 0).
outcome_score(['B', _, 'X'], 0).
outcome_score(['B', _, 'Y'], 3).
outcome_score(['B', _, 'Z'], 6).
outcome_score(['C', _, 'X'], 6).
outcome_score(['C', _, 'Y'], 0).
outcome_score(['C', _, 'Z'], 3).
