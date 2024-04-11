{ pkgs, ... }:

{
  packages = with pkgs; [ rebar3 inotify-tools ];

  languages.elixir.enable = true;
  languages.gleam.enable = true;
  languages.erlang.enable = true;
}
