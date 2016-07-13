-type id(Type) :: {Type, atom()}.
-type address(Type) :: {id(Type), pid()}.

-define(ADDRESS(Type), {{Type, Id}, Pid}).
-define(RECIPENT, ?ADDRESS(?MODULE)).
