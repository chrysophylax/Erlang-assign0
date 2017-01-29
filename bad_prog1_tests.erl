-module(bad_prog1_tests).
-export([run/0]).
-import(bad_prog1, [factorial/1, double/1, area/1, temperature_convert/1, perimeter/1]).

% bad_prog1_tests: Unit testing for bad_prog1
% * * * * * * * * * * * * * * * * * * * *  
% See run/0 for an example.
%
% Author: Billy (github.com/chrysophylax)
% Date: 2017-01-29

run() ->
    test_perimeter(),
    test_temperatureF(),
    test_area(),
    test_double(),
    test_factorial(),
    hooray.

test_perimeter() ->
    %%What can we write here programmatically?
    %%I mean, _ is a valid pattern for perimeter/1.
    %%However we just print to out instead of erroring.
    %%I guess we have to read.

    Triangle = {triangle, 4, 10, 12},
    perimeter(Triangle).



test_temperatureF() ->
    {c, -5.0}  = temperature_convert({f, 23}),
    {f, 23.0} = temperature_convert({c, -5}).


test_area() ->
    %% squares and rects, ints and floats
    25 = area({square,5}),
    35.0 = area({rectangle,5.0,7.0}),
    %% malformed arg
    {'EXIT', _} = (catch area({triangle, 20})).

test_factorial() ->
    %% check special cases
    1 = factorial(0),
    1 = factorial(1),
    2 = factorial(2),
    %% check for malformed arg
    {'EXIT', _} = (catch factorial(sqwonch)).

test_double() ->
    %% test for good and bad args
    25.0 = double(12.5),
    30 = double(15),
    {'EXIT', _} = (catch double(sqwonch)).

