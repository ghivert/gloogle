-module(tom_ffi).
-export([
    infinity_to_dynamic/1,
    infinity_from_dynamic/1,
    nan_to_dynamic/1,
    nan_from_dynamic/1
]).

nan_to_dynamic(positive) -> 
    positive_nan;
nan_to_dynamic(negative) ->
    negative_nan.

nan_from_dynamic(positive_nan) ->
    {ok, positive};
nan_from_dynamic(negative_nan) ->
    {ok, negative};
nan_from_dynamic(_Other) ->
    % Value here is a placeholder
    {error, positive}.

infinity_to_dynamic(positive) -> 
    positive_infinity;
infinity_to_dynamic(negative) ->
    negative_infinity.

infinity_from_dynamic(positive_infinity) ->
    {ok, positive};
infinity_from_dynamic(negative_infinity) ->
    {ok, negative};
infinity_from_dynamic(_Other) ->
    % Value here is a placeholder
    {error, positive}.

