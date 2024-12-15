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
    emergency when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31memrg\x1b[0m";
    alert     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31malrt\x1b[0m";
    critical  when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31mcrit\x1b[0m";
    error     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31meror\x1b[0m";
    warning   when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;33mwarn\x1b[0m";
    notice    when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;32mntce\x1b[0m";
    info      when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;34minfo\x1b[0m";
    debug     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;36mdebg\x1b[0m";
    emergency -> "level=emrg";
    alert     -> "level=alrt";
    critical  -> "level=crit";
    error     -> "level=eror";
    warning   -> "level=warn";
    notice    -> "level=ntce";
    info      -> "level=info";
    debug     -> "level=debg"
  end.

format_msg(Report0, #{json := Json}) ->
  case Report0 of
    {string, Msg} ->
      case is_json() of
        true -> maps:put("id", uuid(), maps:put("when", format_iso8601(), json_wrap([$\s, Msg])));
        false ->
          Defaults = [{<<"when"/utf8>>, [format_iso8601()]}, {<<"id"/utf8>>, [uuid()]}],
          Converted = palabre@internals@converter:format_fields(Defaults),
          case is_color() of
            false -> json_wrap([$\s, Converted, $\s, Msg]);
            true -> json_wrap([$\s, Converted, $\s, "\x1b[1m", Msg, "\x1b[0m"])
          end
      end;
    {report, [{palabre, Fields, Text}]} ->
      Fields1 = [{<<"when"/utf8>>, [format_iso8601()]}, {<<"id"/utf8>>, [uuid()]} | Fields],
      case Json of
        false -> [$\s, palabre@internals@converter:format_fields(Fields1), $\s, Text];
        true -> palabre@internals@converter:format_json(Fields1, Text)
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
  not (persistent_term:get(json, false))
    andalso persistent_term:get(color, false).
