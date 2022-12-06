:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(games(Games), current_input),
    forall(member(Mode, [direct, victory]),
           (maplist(simulate_game(Mode), Games, GameScores),
            sum_list(GameScores, TotalScore),
            writeln(TotalScore))).

games(Strats) --> seqof(game, [], eos, Strats).
game(strat(A, B)) --> [A, 0' , B], eol.

simulate_game(direct, Game, Score) :-
    choice_score(Game, ChoiceScore),
    outcome_score(Game, OutcomeScore),
    Score #= ChoiceScore + OutcomeScore.

simulate_game(victory, Strat, Score) :-
    find_game(Strat, Game),
    simulate_game(direct, Game, Score).

choice_score(strat(_, 0'X), 1).
choice_score(strat(_, 0'Y), 2).
choice_score(strat(_, 0'Z), 3).

find_game(strat(Lhs, 0'X), Strat) :- find_game(Lhs, 0, Strat).
find_game(strat(Lhs, 0'Y), Strat) :- find_game(Lhs, 3, Strat).
find_game(strat(Lhs, 0'Z), Strat) :- find_game(Lhs, 6, Strat).
find_game(Lhs, Score, Strat) :- Strat = strat(Lhs, _), outcome_score(Strat, Score).

outcome_score(strat(0'A, 0'X), 3).
outcome_score(strat(0'A, 0'Y), 6).
outcome_score(strat(0'A, 0'Z), 0).
outcome_score(strat(0'B, 0'X), 0).
outcome_score(strat(0'B, 0'Y), 3).
outcome_score(strat(0'B, 0'Z), 6).
outcome_score(strat(0'C, 0'X), 6).
outcome_score(strat(0'C, 0'Y), 0).
outcome_score(strat(0'C, 0'Z), 3).
