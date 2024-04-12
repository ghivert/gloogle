import * as helpers from '../helpers.ts'

const id = await helpers.getDatabaseID()
if (!id) {
  await helpers.createDatabases()
  await helpers.writeDotEnv()
  console.info("🎉  Database successfully initiated!")
} else {
  console.info("🤔  It looks like you already have a local setup, are you sure you want to relaunch setup? Then, run yarn db:reset.")
}
