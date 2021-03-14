# Introduction to AI. Assignment 1.

## Description of content
There are 3 files: scenario1.pl, scenario2.pl and report.pdf. 

scenario1.pl contains A* and backtracking algorithms for situation when agent can percieve covid standing next to infected cells. 

scenario2.pl contains A* and backtracking algorithms for situation when agent can percieve covid standing 1 cell away from infected cells. 

report.pdf contains impossible maps, description of used algorithms, and statistical comparison of algorithms in different scenarios.

## Code testing
To choose algorithm which you want to test, comment backtracking() or a_star() in main predicate.

If you want to test some given map then comment generate_map() in backtracking() or a_star() and insert the map in form:\
covid(4,8).\
covid(2,6).\
doctor(4,1).\
mask(8,6).\
home(1,2).

