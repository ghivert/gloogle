-module(gling_hex_ffi).
-export([extract_tar/3, remove_tar/1, is_match/2, get_home/0]).

package_interface_path(ContentDest, BaseName) ->
  BuildFolder = <<"/build/dev/docs/">>,
  Package = <<"/package-interface.json">>,
  Path = <<ContentDest/binary, BuildFolder/binary, BaseName/binary, Package/binary>>,
  unicode:characters_to_binary(Path).

% Get the tarball package from Hex, build the package, extract the
% package-interface.json and the gleam.toml.
extract_tar(Binary, BaseName, Slug) ->
  PackagePath = <<"/tmp/", Slug/binary>>,
  ContentDest = <<PackagePath/binary, "/contents">>,
  Content = <<PackagePath/binary, "/contents.tar.gz">>,
  erl_tar:extract({binary, Binary}, [{cwd, PackagePath}]),
  erl_tar:extract(Content, [{cwd, ContentDest}, compressed]),
  BuildCmd = <<"cd ", ContentDest/binary, " && gleam docs build">>,
  os:cmd(binary_to_list(BuildCmd)),
  PackageInterface = package_interface_path(ContentDest, BaseName),
  GleamToml = unicode:characters_to_binary(<<ContentDest/binary, "/gleam.toml">>),
  {PackageInterface, GleamToml}.

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
