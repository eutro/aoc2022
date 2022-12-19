:- module(driver_common, [setup_input/0]).

setup_input :- (getenv('AOC_INPUT', Input) -> see(Input) ; true).
