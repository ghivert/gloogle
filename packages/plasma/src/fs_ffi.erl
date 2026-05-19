-module(fs_ffi).

-export([home/0]).

home() ->
  case init:get_argument(home) of
    {ok, Content} -> {ok, unicode:characters_to_binary(Content)};
    error -> {error, nil}
  end.
