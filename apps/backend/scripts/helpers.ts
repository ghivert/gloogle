import * as dotenv from 'https://deno.land/std@0.222.1/dotenv/mod.ts'
import * as path from 'https://deno.land/std@0.222.1/path/mod.ts'
import * as thread from './thread.ts'

export const NAME = 'gling-postgres'

export const createDatabases = async () => {
  const runPg = ['run', '--name', NAME, '-e', 'POSTGRES_PASSWORD=gling', '-p', '5432:5432', '-d', 'postgres']
  await new Deno.Command('docker', { args: runPg }).output()
  await thread.sleep()
  console.info('🎉  Postgres container successfully created!')
  const id = await getDatabaseID()
  if (id) {
    await new Deno.Command('docker', { args: ['exec', id, 'createdb', '-U', 'postgres', 'gling_development'] }).output()
    await new Deno.Command('docker', { args: ['exec', id, 'createdb', '-U', 'postgres', 'gling_test'] }).output()
    await thread.sleep()
  }
  console.info('🥳  Database gling_development successfully created!')
  console.info('🥳  Database gling_test successfully created!')
}

export const writeDotEnv = async () => {
  const dirname = path.fromFileUrl(import.meta.url)
  const envPath = path.resolve(dirname, '../../.env')
  const envPathTest = path.resolve(dirname, '../../.env.test')
  const envs = [[envPath, 'gling_development'], [envPathTest, 'gling_test']]
  await Promise.all(envs.map(async ([envPath, dbName]) => {
    const content = await dotenv.load({ envPath })
    const dbUrl = ['postgres://postgres:gling@localhost:5432', dbName].join('/')
    const newContent: Record<string, string> = { ...content, DATABASE_URL: dbUrl }
    await Deno.writeTextFile(envPath, dotenv.stringify(newContent))
  }))
  console.info('✏️  .env and .env.test correctly initialized!')
}

export const getDatabaseID = async () => {
  const { stdout } = await new Deno.Command('docker', { args: ['container', 'list'] }).output()
  const parts = new TextDecoder().decode(stdout).split('\n')
  const line = parts.find((line) => line.includes(NAME))
  if (!line) return null
  return line.split(' ')[0]
}

export const dropDatabase = async () => {
  const id = await getDatabaseID()
  if (!id) return
  await new Deno.Command('docker', { args: ['container', 'stop', id] }).output()
  await thread.sleep()
  await new Deno.Command('docker', { args: ['container', 'rm', id] }).output()
  await thread.sleep()
  console.info('🎉  Database successfully dropped!')
}
