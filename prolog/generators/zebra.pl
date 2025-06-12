:- module(zebra, [
    generate_zebra/3,
    verify_zebra/2
]).

:- use_module('../dsl_macros').

% House structure: h(Number, Color, Nationality, Drink, Cigarette, Pet)
generate_zebra(Spec, Solution, Metadata) :-
    length(Houses, 5),
    maplist(init_house, Houses),
    apply_zebra_constraints(Houses),
    label_zebra(Houses),
    extract_zebra_solution(Houses, Solution),
    analyze_zebra_solution(Houses, Metadata).

init_house(h(N, C, Nt, D, Cg, P)) :-
    N in 1..5,
    C in 1..5,
    Nt in 1..5,
    D in 1..5,
    Cg in 1..5,
    P in 1..5.

apply_zebra_constraints(Houses) :-
    % Each attribute must be different within its category
    maplist(attr_different(Houses), [2,3,4,5,6]),
    
    % The Englishman lives in the red house
    member(h(_, 1, 2, _, _, _), Houses),
    
    % The Spaniard owns the dog
    member(h(_, _, 3, _, _, 1), Houses),
    
    % Coffee is drunk in the green house
    member(h(_, 2, _, 1, _, _), Houses),
    
    % The Ukrainian drinks tea
    member(h(_, _, 4, 2, _, _), Houses),
    
    % The green house is immediately to the right of the ivory house
    findall(H, (member(H, Houses), H = h(_, 2, _, _, _, _)), [GreenHouse]),
    findall(H, (member(H, Houses), H = h(_, 3, _, _, _, _)), [IvoryHouse]),
    right_of(GreenHouse, IvoryHouse),
    
    % The Old Gold smoker owns snails
    member(h(_, _, _, _, 1, 2), Houses),
    
    % Kools are smoked in the yellow house
    member(h(_, 4, _, _, 2, _), Houses),
    
    % Milk is drunk in the middle house
    member(h(3, _, _, 3, _, _), Houses),
    
    % The Norwegian lives in the first house
    member(h(1, _, 1, _, _, _), Houses),
    
    % The man who smokes Chesterfields lives in the house next to the man with the fox
    next_to_house(Houses, Chesterfield, Fox),
    member(h(_, _, _, _, 3, _), Chesterfield),
    member(h(_, _, _, _, _, 3), Fox),
    
    % Kools are smoked in the house next to the house where the horse is kept
    next_to_house(Houses, Kools, Horse),
    member(h(_, _, _, _, 2, _), Kools),
    member(h(_, _, _, _, _, 4), Horse),
    
    % The Lucky Strike smoker drinks orange juice
    member(h(_, _, _, 4, 4, _), Houses),
    
    % The Japanese smokes Parliaments
    member(h(_, _, 5, _, 5, _), Houses),
    
    % The Norwegian lives next to the blue house
    next_to_house(Houses, Norwegian, BlueHouse),
    member(h(_, _, 1, _, _, _), Norwegian),
    member(h(_, 5, _, _, _, _), BlueHouse).

attr_different(Houses, AttrIndex) :-
    maplist(attr_at_index(AttrIndex), Houses, Attrs),
    all_different(Attrs).

attr_at_index(1, h(_, _, _, _, _, _), _) :- !.
attr_at_index(2, h(_, C, _, _, _, _), C).
attr_at_index(3, h(_, _, N, _, _, _), N).
attr_at_index(4, h(_, _, _, D, _, _), D).
attr_at_index(5, h(_, _, _, _, C, _), C).
attr_at_index(6, h(_, _, _, _, _, P), P).

next_to_house(Houses, H1, H2) :-
    nth1(I1, Houses, H1),
    nth1(I2, Houses, H2),
    next_to(I1, I2).

label_zebra(Houses) :-
    term_variables(Houses, Vars),
    labeling([], Vars).

extract_zebra_solution(Houses, Solution) :-
    maplist(house_to_dict, Houses, Solution).

house_to_dict(h(N, C, Nt, D, Cg, P), Dict) :-
    Dict = {
        number: N,
        color: C,
        nationality: Nt,
        drink: D,
        cigarette: Cg,
        pet: P
    }.

analyze_zebra_solution(Houses, Metadata) :-
    findall(Step, (backtrack_step(Houses, Step)), Steps),
    analyze_steps(Steps, Metadata).

verify_zebra(Grid, Solution) :-
    extract_zebra_solution(Grid, Solution),
    check_zebra_constraints(Grid).

check_zebra_constraints(Grid) :-
    apply_zebra_constraints(Grid). 