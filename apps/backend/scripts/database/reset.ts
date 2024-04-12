import * as helpers from '../helpers.ts'

const id = await helpers.getDatabaseID()
if (id) {
  await helpers.dropDatabase()
  await helpers.createDatabases()
  console.info("🎉  Database successfully resetted!")
} else {
  console.info("🤔  It looks like you don't have a local setup, creating it instead of resetting…")
  await helpers.createDatabases()
  await helpers.writeDotEnv()
}
