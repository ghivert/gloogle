import backend/postgres
import backend/config.{type Config}

pub fn sync(cnf: Config) {
  let ctx = postgres.connect(cnf)
}
