-ifndef(__GEN_SERVER_UTILS_HRL).
-define(__GEN_SERVER_UTILS_HRL, true).

-type id(Type) :: {Type, atom()}.
-type address(Type) :: {id(Type), pid()}.

-define(ADDRESS(Type), {{Type, Id}, Pid}).
-define(RECIPENT, ?ADDRESS(?MODULE)).

-endif.
