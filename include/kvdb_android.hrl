-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).
-define(LOG_INFO(Log, Args), io:format("(~p) [kvdb] "++Log++"\n", [self() | Args])).
