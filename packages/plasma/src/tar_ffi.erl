-module (tar_ffi).

-export([extract/2, extract_binary/2, remove/1]).

extract_binary(Binary, Destination) ->
  case erl_tar:extract({binary, Binary}, [{cwd, Destination}, compressed]) of
    {error, _} -> {error, nil};
    ok -> {ok, nil}
  end.

extract(From, Destination) ->
  case erl_tar:extract(From, [{cwd, Destination}, compressed]) of
    {error, _} -> {error, nil};
    ok -> {ok, nil}
  end.

% Suppress the tarball.
remove(From) ->
  Cmd = binary_to_list(<<"cd ", From/binary, " && rm -rf ", From/binary>>),
  os:cmd(Cmd).
