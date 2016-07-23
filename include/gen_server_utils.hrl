-ifndef(__GEN_SERVER_UTILS_HRL).
-define(__GEN_SERVER_UTILS_HRL, true).

-type id(Type) :: {Type, atom()}.
-type address(Type) :: {id(Type), pid()}.
-type gen_address() :: address(atom()).

-define(ADDRESS(Type), {{Type, Id}, Pid}).
-define(RECIPENT, ?ADDRESS(?MODULE)).

-define(SPAWN(Id, Fun), spawn(fun() ->
                                put(id, Id),
                                put(module, ?MODULE_STRING),
                                Fun()
                              end)
       ).

-endif.
