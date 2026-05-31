import api/github
import gleam/bool
import gleam/dict.{type Dict}
import gleam/function_
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/result_
import jupiter/context.{type Context}
import jupiter/context/environment
import jupiter/loss.{type Loss}
import jupiter/postgres/queries
import palabres

const module = "tasks/popularity"

pub fn compute_popularity(ctx: Context) -> Loss(Nil) {
  case ctx.env {
    environment.Development -> Ok(Nil)
    environment.Production -> {
      palabres.info("Syncing popularity")
      |> palabres.at(module:, function: "compute_popularity")
      |> palabres.log
      use _ <- function_.tap(do_compute_popularity(ctx, offset: 0))
      palabres.info("Syncing popularity finished!")
      |> palabres.at(module:, function: "compute_popularity")
      |> palabres.log
    }
  }
}

fn do_compute_popularity(ctx: Context, offset offset: Int) {
  let address = queries.select_package_repository_address(ctx.db, offset)
  use repos <- result.try(address)
  use <- bool.guard(when: list.is_empty(repos), return: Ok(Nil))
  repos
  |> list.map(update_repo_popularity(ctx, _))
  |> result.all
  |> result.try(fn(_) { do_compute_popularity(ctx, offset: offset + 100) })
}

fn update_repo_popularity(ctx: Context, repo: Option(#(String, String))) {
  repo
  |> loss.from_option("No repository found")
  |> result.try(do_update_repo_popularity(ctx, _))
  |> result.lazy_or(fn() { Ok(Nil) })
}

fn do_update_repo_popularity(ctx: Context, repo: #(String, String)) {
  let #(popularity, repo) = repo
  palabres.debug("Syncing repository")
  |> palabres.string("repository", repo)
  |> palabres.string("popularity", popularity)
  |> palabres.at(module:, function: "update_repo_popularity")
  |> palabres.log
  use github <- result.try(github.get_stargazer_count(ctx.github_token, repo))
  let data = popularity_data(github:)
  queries.insert_analytics(ctx.db, popularity, "package", data)
  |> result.try(fn(_) { update_package_popularity(ctx, repo, data) })
  |> result_.tap(fn(_) {
    palabres.debug("Synced repository")
    |> palabres.string("repository", repo)
    |> palabres.string("popularity", popularity)
    |> palabres.at(module:, function: "updated_repo_popularity")
    |> palabres.log
  })
}

fn popularity_data(github github: Int) {
  dict.from_list([#("github", github)])
}

fn update_package_popularity(
  ctx: Context,
  repo: String,
  popularity: Dict(String, Int),
) -> Loss(Nil) {
  popularity
  |> queries.update_package_popularity(ctx.db, repo, _)
  |> result.replace(Nil)
}
