-module('Hello').

-export([id/1, add/2, sub/2]).

id(X) -> X.

add(X, Y) -> X + Y.

sub(X, Y) -> X - Y.
