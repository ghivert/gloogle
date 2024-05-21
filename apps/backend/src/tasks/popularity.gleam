import api/github
import backend/config.{type Context}
import backend/postgres/queries
import gleam/bool
import gleam/dict
import gleam/function
import gleam/list
import gleam/option
import gleam/result
import wisp

pub fn compute_popularity(ctx: Context) {
  case ctx.env {
    config.Development -> Ok(Nil)
    config.Production -> {
      wisp.log_info("Syncing popularity")
      do_compute_popularity(ctx, offset: 0)
      |> function.tap(fn(_) { wisp.log_info("Syncing popularity finished!") })
    }
  }
}

fn do_compute_popularity(ctx: Context, offset offset: Int) {
  let db = ctx.db
  use repos <- result.try(queries.select_package_repository_address(db, offset))
  use <- bool.guard(when: list.is_empty(repos), return: Ok(Nil))
  list.map(repos, fn(repo) {
    repo
    |> option.map(update_repo_popularity(ctx, _))
    |> option.unwrap(Ok(Nil))
    |> result.try_recover(fn(_) { Ok(Nil) })
  })
  |> result.all()
  |> result.try(fn(_) { do_compute_popularity(ctx, offset: offset + 100) })
}

fn update_repo_popularity(ctx: Context, repo: String) {
  wisp.log_debug("Syncing " <> repo)
  use count <- result.try(github.get_stargazer_count(ctx.github_token, repo))
  dict.from_list([#("github", count)])
  |> queries.update_package_popularity(ctx.db, repo, _)
  |> result.replace(Nil)
  |> function.tap(fn(_) { wisp.log_debug("Synced " <> repo) })
}
