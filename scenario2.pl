%module for list manipulations
:- use_module(library(lists)).


:- dynamic parent/2.            %dynamic predicate parent with 2 parameters to find parent of cell in A* algorithm
:- dynamic opened/6.            %dynamic predicate opened with 5 parameters to manipulate cells which need to visit in A* algorithm
:- dynamic closed/6.            %dynamic predicate closed with 5 parametes to manipulate with visited cells in A* algorithm
:- dynamic at_home/1.           %dynamic predicate at_home with 1 parameter to determine whether actor at home in A* algorithm
:- dynamic covid_near/2.         %dynamic predicate with 2 parameters to determine whether covid is near or not
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Generating the map
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

start(0,0).             %started cell of actor
xmax(9).                %maximum number of cells horizontally
ymax(9).                %maximum number of cells vertically

/*Comment 5 rows below and uncomment generate_map() in a_star() or bactracking() to test on random map*/

covid(8,0).
covid(8,1).
doctor(0,2).
mask(7,5).
home(6,6).

%predicate to check whether cell in the borders of the map
in_field(X,Y):- xmax(Xmax), ymax(Ymax), Xbound is Xmax-1, Ybound is Ymax-1, between(0, Xbound, X), between(0,Ybound,Y).


%predicate to determine neighbours of the cell
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


%predicate to determine cells infected by covid
covid_zone(X,Y):-
        covid(Xc,Yc),
        (
         neighbours(X,Y,Xc,Yc);
         covid(X,Y)
        ).


%predicate to randomly choose covid cell
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


%predicate to place 2 covids on the map
generate_covids():-
        retractall(covid(_,_)),
        repeat, generate_covid() ->!,
        repeat, generate_covid() ->!.


%predicate to randomly choose doctor cell
generate_doctor() :-
        retractall(doctor(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(covid_zone(X,Y)),
        assert(doctor(X,Y)).

%predicate to randomly choose mask cell
generate_mask() :-
        retractall(mask(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(doctor(X,Y)),
        \+(covid_zone(X,Y)),
        assert(mask(X,Y)).


%predicate to randomly choose home cell
generate_home():-
        retractall(home(_,_)),
        xmax(Xmax),
        ymax(Ymax),
        X is random(Xmax),
        Y is random(Ymax),
        \+(covid_zone(X,Y)),
        assert(home(X,Y)).


%predicate to generate places of covids, doctor, mask, home and print it
generate_map():-
    generate_covids(),
    repeat, generate_doctor()->!,
    repeat, generate_mask()->!,
    repeat, generate_home()->!,
    print_map().


%predicate to print places of covids, doctor, mask and home
print_map():-
        covid(X,Y) -> write('Covid: '),print([X, Y]), write('\n'),
        covid(X0,Y0),\+equals(X,Y,X0,Y0) -> write('Covid: '),print([X0, Y0]), write('\n'),
        doctor(X1, Y1) -> write('Doctor: '),print([X1,Y1]), write('\n'),
        mask(X2,Y2) -> write('Mask: '),print([X2,Y2]), write('\n'),
        home(X3,Y3) -> write('Home: '),print([X3,Y3]), write('\n').


%predicate to determine whether coordinates of 2 cells are equal
equals(X,Y,X0,Y0) :-
    X is X0,
    Y is Y0.
    


%predicate determine whether covid is near
covid_is_near():-
    covid(Xc,Yc),
    neighbours(Xc,Yc,X,Y),
    neighbours(X,Y,Xcn,Ycn),
    assert(covid_near(Xcn,Ycn)).


%predicate to generate possible move of actor
move(X0,Y0,X,Y,Im):-
    neighbours(X0,Y0,X,Y),
    (
     (\+(covid_zone(X,Y)), covid_near(X,Y));
     (covid_zone(X,Y),Im ==1);
    \+(covid_zone(X,Y))
    
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Backtracking search
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%base case of backtracking when actor reach home
backtracking_search([X0, Y0], [X0, Y0], _,[[X0,Y0]], Distance,_):-
        home(X0,Y0),
        min_distance(Mindist),
        Mindist > Distance,
        retractall(min_distance(_)),
        assert(min_distance(Distance)).

%backtracking predicate to find possible paths to home cell
backtracking_search([X0, Y0], [Xh, Yh], Visited, [[X0, Y0]|Path], Distance,Im) :-
        ((doctor(X0,Y0); mask(X0,Y0); Im == 1)-> I is 1; I is 0),
        move(X0,Y0,X,Y,I),
        \+(member([X,Y], Visited)),
        min_distance(Mindist),
        Dist is Distance +1,
        Dist < Mindist,
        backtracking_search([X, Y], [Xh,Yh], [[X, Y]|Visited], Path, Dist,I).


%predicate for backtracking search of shortest path to home on generated map
backtracking() :-
        %uncomment generate_map() if you want to randomly generate map
        %generate_map(),
        home(Xh,Yh),
        xmax(Xmax),
        ymax(Ymax),
        retractall(min_distance(_)),
        assert(min_distance(Xmax*Ymax)),
        backtracking_path(Road,Xh,Yh),
        min_distance(D),
        write('Road: '), print(Road), write('\nDistance: '), print(D).

%predicate to choose shortest path among all paths to home
backtracking_path(Sorted,Xh,Yh) :-
        start(X0,Y0),
        setof(Len-Path, (backtracking_search([X0,Y0],[Xh,Yh], [], Path, 0,0), length(Path, Len)), All),
        member(_-Sorted, All),!.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A* search
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%predicate to find cost of movement to (X,Y) cell
g(X,Y,G) :-
    (parent([Xp,Yp],[X,Y]),
     opened(Xp, Yp, Gp, _, _,_),
     G is Gp+1);

    (parent([Xp,Yp],[X,Y]),
     closed(Xp,Yp,Gp,_,_,_),
     G is Gp+1);

    (start(Xs,Ys),
    G is max(abs(X -Xs),abs(Y-Ys))).


%heuristics for cost to move from (X,Y) cell to home cell
h(X,Y,H) :-
    home(Xh, Yh),
    H is max(abs(X - Xh), abs(Y-Yh)).

%terminating condition for A* algorithm when no path found
a_search() :-
    \+(opened(_,_,_,_,_,_)),
    write('No road').

%terminating condition for A* algorithm when path found
a_search() :-
    home(Xh, Yh),
    closed(Xh,Yh,_,_,_,_),
    assert(at_home(1)).

%predicate to find path in from start to home with use of A* algorithm
a_search() :-
    home(Xh,Yh),
    \+(closed(Xh,Yh,_,_,_,_)),
    opened(_,_,_,_,_,_),
    children([Xc,Yc]),
    opened(Xc,Yc,Gc,Hc,Fc,I),
    assert(closed(Xc,Yc,Gc,Hc,Fc,I)),
    retractall(opened(Xc,Yc,_,_,_,_)),
    a_neighbours(Xc,Yc,Gc,Hc,Fc,I),
    a_search().

%predicate to calculate cost of movement from cell to its neighbour cell
a_neighbours(X,Y,G,H,F,Im) :-
    ((doctor(X,Y); mask(X,Y); Im == 1)-> I is 1; I is 0),
    move(X,Y,Xn,Yn,I),
    \+(closed(Xn,Yn,_,_,_,_)),

    ((
     \+(opened(Xn,Yn,_,_,_,_)),
     assert(parent([X, Y], [Xn, Yn])),
     g(Xn, Yn, Gn),
     h(Xn, Yn, Hn),
     Fn is Gn + Hn,
     assert(opened(Xn, Yn, Gn, Hn, Fn, I)),
     a_neighbours(X, Y, G, H, F, I)
    );
    
    (
     opened(Xn, Yn, Gn, Hn,_,_),
     G1 is G+1,
     G1 < Gn,
     F1 is G1+Hn,
     assert(opened(Xn, Yn, G1, Hn, F1,I)),
     retract(opened(Xn, Yn, Gn, _, _,_)),
     retract(parent([_, _], [Xn, Yn])),
     assert(parent([X, Y], [Xn, Yn])),
     a_neighbours(X, Y, G, H, F, I)
    ));
    
    true.

%predicate to find all children
children([X,Y]) :-
    findall(opened(X0,Y0,_,_,F0,_), opened(X0,Y0,_,_,F0,_), [Head|Tail]),
    min_f_child(Head, Tail, [X,Y]).


%terminating condition for finding child with min F
min_f_child(opened(X0,Y0,_,_,_,_),[],[X,Y]) :-
    X is X0,
    Y is Y0.

%finding child with min F among children
min_f_child(opened(X0,Y0,_,_,F0,_), [opened(X1,Y1,_,_,F1,_)|Tail], [X,Y]) :-
    (F0 < F1) -> (min_f_child(opened(X0,Y0,_,_,F0,_),Tail, [X,Y]));
    min_f_child(opened(X1,Y1,_,_,F1,_),Tail, [X,Y]).
    

%predicate to find shortest path among all paths to home
a_star_shorted(Sorted, Xh,Yh):-
    start(Xs,Ys),
    setof(Len-Path, (path([Xs,Ys],[Xh,Yh],Path), length(Path, Len)), All),
    member(_-Sorted, All),!.


%predicate to reconstruct path from start cell to home cell
path([Xs, Ys], [Xs, Ys], [[Xs, Ys]]):-!.
path([Xs, Ys], [Xh, Yh], [[Xs,Ys]|Path]):-
    parent([Xs, Ys], [X1, Y1]),
    path([X1, Y1], [Xh, Yh], Path).

%predicate to find shortest path with use of A* algorithm on the generated map
a_star() :-
    %uncomment generate_map() if you want to randomly generate map
    %generate_map(),
    start(Xs,Ys),
    home(Xh, Yh),
    g(Xs, Ys, Gs),
    h(Xs, Ys, Hs),
    Fs is Gs + Hs,
    retractall(opened(_, _, _, _, _,_)),
    retractall(closed(_, _, _, _, _,_)),
    retractall(parent(_, _)),
    assert(at_home(0)),
    (doctor(Xs,Ys); mask(Xs,Ys) -> I is 1; I is 0),
    assert(opened(Xs, Ys, Gs, Hs, Fs, I)),
    a_search(),
    at_home(Home),
    Home == 1 -> a_star_shorted(Road,Xh,Yh),
    length(Road,Len),
    D is Len-1,
    write('Road: '), write(Road), write('\nDistance: '), write(D).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Run algorithms and determine the execution time of each
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

main() :-
    statistics(runtime, [Start|_]),
    %Uncomment necessary algorithm
    %backtracking(),
    a_star(),
    statistics(runtime,[Stop|_]),
    ExecTime is Stop-Start,
    write('\nExecution time: '),write(ExecTime),write('ms').
