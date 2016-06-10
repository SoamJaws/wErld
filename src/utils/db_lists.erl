-module(db_lists).
-export([droplast/1]).

droplast([_|[]]) -> [];
droplast([H|T]) -> [H|droplast(T)].
