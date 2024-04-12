import * as helpers from '../helpers.ts'

const id = await helpers.getDatabaseID()
if (id) {
  await helpers.dropDatabase()
} else {
  console.info("🤔  It looks like you don't have a local setup, exiting…")
}
