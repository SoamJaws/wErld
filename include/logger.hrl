-ifndef(__LOGGER_HRL).
-define(__LOGGER_HRL, true).

-define(LOG_INFO(Content), logger:log_info(Content, get(module), get(id))).
-define(LOG_WARNING(Content), logger:log_warning(Content, get(module), get(id))).
-define(LOG_ERROR(Content), logger:log_error(Content, get(module), get(id))).
-define(LOG_SEND(Content, To), logger:log_send(Content, get(module), get(id), {{get(module), get(id)}, self()}, To)).
-define(LOG_RECEIVE(Content), logger:log_receive(Content, get(module), get(id))).
-define(LOG_RECEIVE(Content, From), logger:log_receive(Content, get(module), get(id), From, {{get(module), get(id)}, self()})).

-endif.
