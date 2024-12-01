import data/msg
import frontend/router
import frontend/view/body/search_result as sr
import gleam/result
import grille_pain
import grille_pain/options
import lustre
import lustre/lazy
import modem
import sketch
import sketch/magic

pub fn sketch() {
  use cache <- result.try(sketch.cache(strategy: sketch.Ephemeral))
  use _ <- result.try(magic.setup(cache))
  Ok(Nil)
}

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

pub fn start_application(init, update, view) {
  lustre.application(init, update, view)
  |> lustre.start("#app", Nil)
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
