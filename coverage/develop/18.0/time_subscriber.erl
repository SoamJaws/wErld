-module(time_subscriber).
-include("time.hrl").

-callback ?NEW_TIME( Recipent :: gen_address()
                   , Time :: time()
                   ) -> ok.

-callback ?NEW_TIME( Recipent :: gen_address()
                   , Time :: time()
                   , BlockCaller :: boolean()
                   ) -> ok.
