-module(sketch_ffi).

-export([save_current_cache/1, get_current_cache/0, stacktrace/0]).

create_cache() ->
  ets:new(cache_manager, [set, public, named_table]).

save_current_cache(Cache) ->
  Exists = ets:whereis(cache_manager),
  case Exists of undefined -> create_cache(); _ -> ok end,
  Pid = self(),
  ets:insert(cache_manager, {Pid, Cache}).

get_current_cache() ->
  Pid = self(),
  case ets:lookup(cache_manager, Pid) of
    [{_, Cache}] -> {ok, Cache};
    _ -> {error, nil}
  end.

stacktrace() ->
  try throw(42) catch _:_:Stk ->
    lists:sublist(Stk, 2, 4)
  end.
