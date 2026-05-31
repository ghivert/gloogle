import data/msg
import gleam/result
import grille_pain
import grille_pain/options
import lustre/lazy
import modem
import venus/router
import venus/view/body/search_result as sr

pub fn components() {
  use _ <- result.try(lazy.setup())
  use _ <- result.try(sr.setup())
  Ok(Nil)
}

pub fn grille_pain() {
  options.default()
  |> options.timeout(5000)
  |> grille_pain.setup()
}

pub fn initial_route() {
  modem.initial_uri()
  |> result.map(router.parse_uri)
  |> result.unwrap(router.Home)
}

pub fn modem() {
  use uri <- modem.init
  router.parse_uri(uri)
  |> msg.BrowserChangedRoute
}
