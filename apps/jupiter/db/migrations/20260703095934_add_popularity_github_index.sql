-- migrate:up
-- Backs the `ORDER BY popularity -> 'github' DESC` used by
-- `select_package_by_popularity` and `select_more_popular_packages`. The index
-- expression must match the ORDER BY expression exactly (jsonb `->`). Partial on
-- `IS NOT NULL` to mirror the filter in `select_package_by_popularity`, keeping
-- the index compact.
create index package_popularity_github
  on package ((popularity -> 'github'))
  where (popularity -> 'github') is not null;

-- migrate:down
drop index package_popularity_github;
