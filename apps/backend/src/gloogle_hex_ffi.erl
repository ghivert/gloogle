-module(gloogle_hex_ffi).
-export([extract_tar/4, remove_tar/1, is_match/2, get_home/0, set_level/1]).

package_interface_path(ContentDest, BaseName) ->
  BuildFolder = <<"/build/dev/docs/">>,
  Package = <<"/package-interface.json">>,
  Path = <<ContentDest/binary, BuildFolder/binary, BaseName/binary, Package/binary>>,
  unicode:characters_to_binary(Path).

save_file(ContentDest, HttpBody) ->
  PackageInterfacePath = <<ContentDest/binary, "/package-interface.json">>,
  file:write_file(PackageInterfacePath, HttpBody),
  {unicode:characters_to_binary(PackageInterfacePath), <<"">>}.

% Get the tarball package from Hex, build the package, extract the
% package-interface.json and the gleam.toml.
extract_tar(Binary, BaseName, Version, Slug) ->
  PackagePath = <<"/tmp/", Slug/binary>>,
  ContentDest = <<PackagePath/binary, "/contents">>,
  Content = <<PackagePath/binary, "/contents.tar.gz">>,
  case erl_tar:extract({binary, Binary}, [{cwd, PackagePath}]) of
    {error, _} -> {error, nil};
    _ ->
      Url = <<"https://hexdocs.pm/", BaseName/binary, "/", Version/binary, "/package-interface.json">>,
      erl_tar:extract(Content, [{cwd, ContentDest}, compressed]),
      {PackageInterface, Result} =
        case httpc:request(Url) of
          {ok, {_, _, HttpBody}} -> save_file(ContentDest, HttpBody);
          {ok, {_, HttpBody}} -> save_file(ContentDest, HttpBody);
          {error, _} ->
            BuildCmd = <<"cd ", ContentDest/binary, " && gleam docs build">>,
            Res = os:cmd(binary_to_list(BuildCmd)),
            {package_interface_path(ContentDest, BaseName), Res}
        end,
      GleamToml = unicode:characters_to_binary(<<ContentDest/binary, "/gleam.toml">>),
      {ok, {PackageInterface, GleamToml, unicode:characters_to_binary(Result)}}
  end.

% Suppress the tarball.
remove_tar(Slug) ->
  PackagePath = <<"/tmp/", Slug/binary>>,
  Cmd = <<"cd ", PackagePath/binary, " && rm -rf ", PackagePath/binary>>,
  os:cmd(binary_to_list(Cmd)).

is_match(Version, Requirement) ->
  case verl:is_match(Version, Requirement) of
    {error, _} -> {error, nil};
    Bool -> {ok, Bool}
  end.

get_home() ->
  case init:get_argument(home) of
    {ok, Content} -> {ok, unicode:characters_to_binary(Content)};
    error -> {error, nil}
  end.

set_level(Level) ->
  logger:set_primary_config(level, Level).
