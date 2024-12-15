-module(palabre_ffi).

-export([configure/1, format/2, format_iso8601/0, log/3, uuid/0, is_json/0, is_color/0]).

-include_lib("palabre/include/palabre_Options.hrl").

configure(#options{level = Level, json = Json, color = Clr}) ->
  persistent_term:put(json, Json),
  Color = read_color(Clr),
  persistent_term:put(color, Color),
  logger:update_primary_config(#{
    level => Level,
    filter_default => log,
    filters =>
      [{domain, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}},
       {domain, {fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}],
    metadata => #{}}),
  logger:update_handler_config(default,
    #{formatter => {palabre_ffi, #{color => Color, json => Json}}}),
  nil.

read_color(Color) ->
  case Color of
    {some, Clr} -> Clr;
    _ ->
      not (read_variable("NO_COLOUR", false)
        orelse read_variable("NO_COLOR", false))
  end.

read_variable(Name, Default) ->
  case os:getenv(Name) of
    false -> Default;
    "" -> false;
    "false" -> false;
    _ -> true
  end.

log(Level, Msg, Text) ->
  logger:log(Level, [{palabre, Msg, Text}]).

format(#{level := Level, msg := Msg, meta := _Meta}, #{json := Json, color := Color}) ->
  case Json of
    false -> [format_level(Level, #{color => Color}), format_msg(Msg, #{json => false}), $\n];
    true -> [thoas:encode(maps:put("level", Level, format_msg(Msg, #{json => true}))), $\n]
  end.

format_level(Level, #{color := Color}) ->
  case Level of
    emergency when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31memergency\x1b[0m";
    alert     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31malert\x1b[0m";
    critical  when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31mcritical\x1b[0m";
    error     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31merror\x1b[0m";
    warning   when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;33mwarning\x1b[0m";
    notice    when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;32mnotice\x1b[0m";
    info      when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;34minfo\x1b[0m";
    debug     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;36mdebug\x1b[0m";
    emergency -> "level=emergency";
    alert     -> "level=alert";
    critical  -> "level=critical";
    error     -> "level=error";
    warning   -> "level=warning";
    notice    -> "level=notice";
    info      -> "level=info";
    debug     -> "level=debug"
  end.

format_msg(Report0, #{json := Json}) ->
  case Report0 of
    {string, Msg} -> json_wrap([$\s, Msg]);
    {report, [{palabre, Fields, Text}]} ->
      case Json of
        false -> [$\s, palabre@internals@converter:format_fields(Fields), $\s, Text];
        true -> palabre@internals@converter:format_json(Fields, Text)
      end;
    {report, Report1} when is_map(Report1) -> json_wrap(format_kv(maps:to_list(Report1)));
    {report, Report1} when is_list(Report1) -> json_wrap(format_kv(Report1));
    _ -> json_wrap([$\s, gleam@string:inspect(Report0)])
  end.

json_wrap(Content) ->
  case is_json() of
    false -> Content;
    true ->
      case Content of
        [$\s, Content1] -> #{message => Content1};
        _ -> #{message => Content}
      end
    end.

format_kv(Pairs) ->
  case Pairs of
    [] -> [];
    [{K, V} | Rest] when is_atom(K) ->
      [$\s, erlang:atom_to_binary(K), $=, gleam@string:inspect(V) | format_kv(Rest)];
    [{K, V} | Rest] ->
      [$\s, gleam@string:inspect(K), $=, gleam@string:inspect(V) | format_kv(Rest)];
    Other ->
      gleam@string:inspect(Other)
  end.

format_iso8601() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(
      io_lib:format(
        "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
        [Year, Month, Day, Hour, Min, Sec])).

uuid() ->
  Uuid0 = uuid:get_v4(),
  Uuid1 = uuid:uuid_to_string(Uuid0),
  unicode:characters_to_binary(Uuid1).

is_json() ->
  persistent_term:get(json, false).

is_color() ->
  persistent_term:get(color, false).
