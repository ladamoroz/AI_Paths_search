:- use_module(library(lists)).
:- dynamic parent/2.
:- dynamic opened/5.
:- dynamic closed/5.
:- dynamic at_home/1.

start(0,0).
xmax(9).
ymax(9).

in_field(X,Y):- xmax(Xmax), ymax(Ymax), Xbound is Xmax-1, Ybound is Ymax-1, between(0, Xbound, X), between(0,Ybound,Y).

neighbours(X0,Y0, X1,Y1):-
        (
         X1 is X0+1, Y1 is Y0;
         X1 is X0, Y1 is Y0+1;
         X1 is X0+1, Y1 is Y0+1;
         X1 is X0-1, Y1 is Y0;
         X1 is X0-1, Y1 is Y0-1;
         X1 is X0-1, Y1 is Y0+1;
         X1 is X0+1, Y1 is Y0-1;
         X1 is X0, Y1 is Y0-1
        ),
        in_field(X1,Y1).

covid_zone(X,Y):-
        covid(Xc,Yc),
        (
         neighbours(X,Y,Xc,Yc);
         covid(X,Y)
        ).

generate_covid():-
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        start(Xs,Ys),
        \+start(X,Y),
        \+(neighbours(X,Y,Xs,Ys)),
        \+(covid(X,Y)),
        assert(covid(X,Y)).

generate_covids():-
        retractall(covid(_,_)),
        repeat, generate_covid() ->!,
        repeat, generate_covid() ->!.


generate_doctor() :-
        retractall(doctor(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(covid_zone(X,Y)),
        assert(doctor(X,Y)).


generate_mask() :-
        retractall(mask(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(doctor(X,Y)),
        \+(covid_zone(X,Y)),
        assert(mask(X,Y)).

generate_home():-
        retractall(home(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(covid_zone(X,Y)),
        assert(home(X,Y)).

generate_map():-
    generate_covids(),
    repeat, generate_doctor()->!,
    repeat, generate_mask()->!,
    repeat, generate_home()->!,
    print_map().

equals(X,Y,X0,Y0) :-
    X is X0,
    Y is Y0.

print_map():-
        covid(X,Y) -> write('Covid: '),print([X, Y]), write('\n'),
        covid(X0,Y0),\+equals(X,Y,X0,Y0) -> write('Covid: '),print([X0, Y0]), write('\n'),
        doctor(X1, Y1) -> write('Doctor: '),print([X1,Y1]), write('\n'),
        mask(X2,Y2) -> write('Mask: '),print([X2,Y2]), write('\n'),
        home(X3,Y3) -> write('Home: '),print([X3,Y3]), write('\n').

check_immunity(X0, Y0) :-
        ((doctor(X0,Y0);
        mask(X0, Y0);
        immunity(1)),
        retractall(immunity(_)),
        assert(immunity(1)));
        retractall(immunity(_)),
        assert(immunity(0)).
        
move(X0,Y0,X,Y):-
    neighbours(X0,Y0,X,Y),
    (
        \+(covid_zone(X,Y));
        (
        covid_zone(X,Y),
        immunity(1))
     ).

backtracking_search([X0, Y0], [X0, Y0], _,[[X0,Y0]], Distance):-
        home(X0,Y0),
        min_distance(Mindist),
        Mindist > Distance,
        retractall(min_distance(_)),
        assert(min_distance(Distance)),
        retractall(immunity(_)),
        assert(immunity(0)).

backtracking_search([X0, Y0], [Xh, Yh], Visited, [[X0, Y0]|Path], Distance) :-
        move(X0,Y0,X,Y),
        check_immunity(X0,Y0),
        \+(member([X,Y], Visited)),
        min_distance(Mindist),
        Dist is Distance +1,
        Dist < Mindist,
        backtracking_search([X, Y], [Xh,Yh], [[X, Y]|Visited], Path, Dist).

backtracking() :-
        generate_map(),
        home(Xh,Yh),
        xmax(Xmax),
        ymax(Ymax),
        retractall(min_distance(_)),
        assert(min_distance(Xmax*Ymax)),
        retractall(immunity(_)),
        assert(immunity(0)),
        backtracking_path(Road,Xh,Yh),
        min_distance(D),
        write('Road: '), print(Road), write('\nDistance: '), print(D).

backtracking_path(Sorted,Xh,Yh) :-
        start(X0,Y0),
        setof(Len-Path, (backtracking_search([X0,Y0],[Xh,Yh], [], Path, 0), length(Path, Len)), All),
        member(_-Sorted, All),!.





g(X,Y,G) :-
    ((clause(parent([Xp,Yp],[X,Y]), true)), clause(opened(Xp, Yp, _, _, _), true)) ->
        opened(Xp, Yp, Gp, _, _), G is Gp+1;
    ((clause(parent([Xp,Yp],[X,Y]), true)), clause(closed(Xp, Yp, _, _, _), true)) ->
        closed(Xp,Yp,Gp,_,_), G is Gp+1;
    (start(Xs,Ys),
    G is max(abs(X -Xs),abs(Y-Ys))).
    
h(X,Y,H) :-
    home(Xh, Yh),
    H is max(abs(X - Xh), abs(Y-Yh)).

a_star() :-
    generate_map(),
    start(Xs,Ys),
    home(Xh, Yh),
    g(Xs, Ys, Gs),
    h(Xs, Ys, Hs),
    Fs is Gs + Hs,
    retractall(opened(_, _, _, _, _)),
    retractall(closed(_, _, _, _, _)),
    retractall(parent(_, _)),
    assert(at_home(0)),
    assert(opened(Xs, Ys, Gs, Hs, Fs)),
    retractall(immunity(_)),
    assert(immunity(0)),
    a_search(),
    at_home(Home),
    Home == 1 -> a_star_shorted(Road,Xh,Yh),
    length(Road,Len),
    write('Road: '), write(Road), write('\nDistance: '), write(Len).

a_star_shorted(Sorted, Xh,Yh):-
    start(Xs,Ys),
    setof(Len-Path, (path([Xs,Ys],[Xh,Yh],Path), length(Path, Len)), All),
    member(_-Sorted, All),!.


path([Xs, Ys], [Xs, Ys], [[Xs, Ys]]):-!.
path([Xs, Ys], [Xh, Yh], [[Xs,Ys]|Path]):-
    parent([Xs, Ys], [X1, Y1]),
    path([X1, Y1], [Xh, Yh], Path).

a_search() :-
    \+(opened(_,_,_,_,_)),
    write('No path').

a_search() :-
    home(Xh, Yh),
    (clause(closed(Xh,Yh,_,_,_),true) ->
        assert(at_home(1)),
        retractall(immunity(_)),
        assert(immunity(0))).

a_search() :-
    (home(Xh,Yh), \+(closed(Xh,Yh,_,_,_)), opened(_,_,_,_,_) )->
    current([Xc,Yc]),
    opened(Xc,Yc,Gc,Hc,Fc),
    assert(closed(Xc,Yc,Gc,Hc,Fc)),
    retractall(opened(Xc,Yc,_,_,_)),
    a_neighbours(Xc,Yc,Gc,Hc,Fc),
    a_search().

a_neighbours(X,Y,G,H,F) :-
    ((check_immunity(X,Y), move(X,Y,Xn,Yn), available(Xn,Yn), \+(opened(Xn,Yn,_,_,_)))->
     assert(parent([X, Y], [Xn, Yn])),
     g(Xn, Yn, Gn),
     h(Xn, Yn, Hn),
     Fn is Gn + Hn,
     assert(opened(Xn, Yn, Gn, Hn, Fn)),
     a_neighbours(X,Y,G,H,F)
    );
    
    ((check_immunity(X,Y), move(X, Y, Xn, Yn), available(Xn, Yn), opened(Xn, Yn, Gn, Hn,_),G1 is G+1, G1 < Gn) ->
     F1 is G1+Hn,
     assert(opened(Xn, Yn, G1, Hn, F1)),
     retract(opened(Xn, Yn, Gn, _, _)),
     retract(parent([_, _], [Xn, Yn])),
     assert(parent([X, Y], [Xn, Yn])),
     a_neighbours(X, Y, G, H, F)
    );
    
    true.

available(X,Y) :-
    \+(covid_zone(X, Y)),
    \+(closed(X,Y,_,_,_)).

current([X,Y]) :-
    findall(opened(X0,Y0,_,_,F0), opened(X0,Y0,_,_,F0), [A|Tail]),
    current(A, Tail, [X,Y]).

current(opened(X0,Y0,_,_,_),[],[X,Y]) :-
    X is X0,
    Y is Y0.

current(opened(X0,Y0,_,_,F0), [opened(X1,Y1,_,_,F1)|Tail], [X,Y]) :-
    (F0 < F1) -> (current(opened(X0,Y0,_,_,F0),Tail, [X,Y]));
    current(opened(X1,Y1,_,_,F1),Tail, [X,Y]).

main() :-
    statistics(runtime, [Start|_]),
    backtracking(),
    %a_star(),
    statistics(runtime,[Stop|_]),
    ExecTime is Stop-Start,
    write('\nExecution time: '),write(ExecTime),write('ms').
