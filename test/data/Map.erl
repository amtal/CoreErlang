-module('Map').

-export([f/0]).

f() ->
    #{a := A, b := B} = #{a => 1, b => 2},
    {A, B}.
