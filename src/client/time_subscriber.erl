-module(time_subscriber).
-include("time.hrl").

-callback ?NEW_TIME( Recipent :: gen_address() | {global, atom()}
                   , Time :: time()
                   ) -> ok.

-callback ?NEW_TIME( Recipent :: gen_address() | {global, atom()}
                   , Time :: time()
                   , BlockCaller :: boolean()
                   ) -> ok.
