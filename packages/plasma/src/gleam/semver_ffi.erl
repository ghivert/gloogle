-module(semver_ffi).

-export([is_match/2]).

is_match(Version, Requirement) ->
  case verl:is_match(Version, Requirement) of
    {error, _} -> {error, nil};
    Bool -> {ok, Bool}
  end.
