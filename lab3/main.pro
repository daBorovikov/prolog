implement main
    open core, stdio, file, string

domains
    unit = africa; asia; north_america; south_america; oceania; europe; australia.

class facts - lab3Db
    country : (integer Id, string Country_Name, unit Part, integer Population) nondeterm.
    capital : (integer Id, string Capital_Name, integer Capital_Population) nondeterm.

class predicates
    capitals_of_continent : (string Capital_Name, unit Continent_Name) nondeterm anyflow.
    population_greater_than : (string Country_Name, integer Country_Population) nondeterm anyflow.
    most_populous_capital_of_continent : (unit Continent_Name, string Capital_Name) nondeterm anyflow.
    pop_greater_than : (integer Country_Population, integer* Country) nondeterm anyflow.
    len : (integer*, integer, integer [out]) nondeterm.
    sum_list : (integer*, unsigned, unsigned [out]) nondeterm.
    country_of_continent : (unit Part, integer* Country) nondeterm anyflow.

clauses
    len([_ | List], Inc, Count) :-
        len(List, Inc + 1, Count).
    len([], Count, Count).

    sum_list([Elem | List], Sum, All) :-
        New_sum = Sum + Elem,
        sum_list(List, New_sum, All).
    sum_list([], All, All).

    capitals_of_continent(X, Y) :-
        country(Id, _, Y, _),
        capital(Id, X, _).

    population_greater_than(X, Y) :-
        country(_, X, _, Z),
        Z > Y.

    most_populous_capital_of_continent(X, Y) :-
        capitals_of_continent(P, X),
        capital(Id, P, N),
        not((capitals_of_continent(P1, X) and capital(Id1, P1, N1) and N1 > N)),
        Y = P.

    pop_greater_than(X, Y) :-
        Y =
            [ Id ||
                country(Id, _, _, Z),
                Z > X
            ].

    country_of_continent(Part, Y) :-
        Y = [ X || country(Id, _, Part, X) ].

class predicates
    countries_of_population_greater_than : (integer PopulationLimit, string* Country) nondeterm anyflow.
clauses
    countries_of_population_greater_than(PopulationLimit, Countries) :-
        Countries =
            [ Country_Name ||
                country(_, Country_Name, _, Population),
                Population > PopulationLimit
            ].

class predicates
    avg_population_by_continent : (unit Part, integer AveragePopulation [out]) nondeterm.
clauses
    avg_population_by_continent(Part, AveragePopulation) :-
        country_of_continent(Part, Y),
        sum_list(Y, 0, Total),
        len(Y, 0, Count),
        AveragePopulation = math::round(Total / Count).

class predicates
    min_population_by_continent : (unit Part, integer MinPopulation, string Country) nondeterm anyflow.
    min_list : (integer*, integer, integer [out]) nondeterm.

clauses
    min_list([Elem | List], CurrentMin, Min) :-
        Elem < CurrentMin,
        min_list(List, Elem, Min).

    min_list([Elem | List], CurrentMin, Min) :-
        Elem >= CurrentMin,
        min_list(List, CurrentMin, Min).

    min_list([], Min, Min).
    min_population_by_continent(Part, MinPopulation, Country) :-
        Populations = [ Population || country(_, _, Part, Population) ],
        min_list(Populations, 2147483647, MinPopulation),
        country(_, Country, Part, MinPopulation).

class predicates
    max_population_by_continent : (unit Part, integer MaxPopulation, string Country) nondeterm anyflow.
    max_list : (integer*, integer, integer [out]) nondeterm.

clauses
    max_list([Elem | List], CurrentMax, Max) :-
        Elem > CurrentMax,
        max_list(List, Elem, Max).

    max_list([Elem | List], CurrentMax, Max) :-
        Elem <= CurrentMax,
        max_list(List, CurrentMax, Max).

    max_list([], Max, Max).

    max_population_by_continent(Part, MaxPopulation, Country) :-
        Populations = [ Population || country(_, _, Part, Population) ],
        max_list(Populations, 0, MaxPopulation),
        country(_, Country, Part, MaxPopulation).

clauses
    run() :-
        consult("../lab3.txt", lab3Db),
        fail.

    run() :-
        capitals_of_continent(X, asia),
        write(X),
        nl,
        fail.

    run() :-
        write("Countries with population greater than 100,000,000:\n"),
        population_greater_than(X, 100000000),
        write(X),
        nl,
        fail.

    run() :-
        write("Most populous capital in Oceania: "),
        most_populous_capital_of_continent(oceania, Y),
        write(Y),
        nl,
        fail.

    run() :-
        write("Countries with population greater than 100,000,000: "),
        pop_greater_than(100000000, X),
        len(X, 0, Count),
        write(Count),
        nl,
        fail.

    run() :-
        write("Population of asia: "),
        country_of_continent(asia, Y),
        sum_list(Y, 0, Count),
        write(Count),
        nl,
        fail.

    run() :-
        write("Countries with population greater than 50,000,000:\n"),
        countries_of_population_greater_than(50000000, Countries),
        write(Countries),
        nl,
        fail.

    run() :-
        write("\nAverage population in Europe: "),
        avg_population_by_continent(europe, AvgPopulation),
        write(AvgPopulation),
        nl,
        fail.

    run() :-
        min_population_by_continent(europe, MinPop, Country),
        write("Minimum population in country in europe:", MinPop, " ", Country),
        nl,
        fail.

    run() :-
        max_population_by_continent(europe, MaxPop, Country),
        write("Max population in country in europe:", MaxPop, " ", Country),
        nl,
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
