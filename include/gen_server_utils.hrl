-ifndef(__GEN_SERVER_UTILS_HRL).
-define(__GEN_SERVER_UTILS_HRL, true).

-type id(Type) :: {Type, atom()}.
-type address(Type) :: {id(Type), pid()}.
-type gen_address() :: address(atom()).

-define(ADDRESS, {{_Type, _Id}, Pid}).
-define(ADDRESS(Type), {{Type, Id}, Pid}).
-define(ADDRESS_NO_ID(Type), {{Type, _Id}, Pid}).
-define(ADDRESS_NO_ID_PID(Type), {{Type, _Id}, _Pid}).
-define(RECIPENT, ?ADDRESS(?MODULE)).
-define(RECIPENT_NO_ID, ?ADDRESS_NO_ID(?MODULE)).

-define(SPAWN(Id, Fun), spawn(fun() ->
                                put(id, Id),
                                put(module, ?MODULE_STRING),
                                Fun()
                              end)
       ).

-endif.
