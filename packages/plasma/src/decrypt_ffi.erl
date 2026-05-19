-module(decrypt_ffi).
-export([decode_json/2, new_decoder/1]).

%% Define the `decode_json` function in `decrypt`, which will be wrapped in a
%% `Decoder(a)`. `decode_json :: fn (Dynamic) -> #(a, List(DecodeError))`.
decode_json(Decoder, Data) ->
  {decoder, Fun} = Decoder,
  Dynamic = parse(Data),
  Fun(Dynamic).

%% Get `Decoder` type back, because it's opaque. Opaque types are not exported,
%% but `decode.success` returns a `Decoder(a)`. Since it's an instance of the
%% object, it's possible to get back the class directly by accessing
%% `constructor`.
new_decoder(Fun) -> {decoder, Fun}.

%% Converts a data from a string to a JSON structure. If the string is not a
%% valid JSON, or if the data is not a string, returns the data as-is, as it
%% can be a valid data structure according to the decoder.
parse(Data) ->
  try json:decode(Data)
  catch
    throw:_ -> Data;
    exit:_ -> Data;
    error:_:_ -> Data
  end.
