function selectToml(fs, resolve) {
  if (fs.existsSync(resolve('.mise.toml'))) return resolve('.mise.toml')
  if (fs.existsSync(resolve('.mise.local.toml')))
    return resolve('.mise.local.toml')
}

function parse(content) {
  try {
    return JSON.parse(content)
  } catch (error) {
    console.log(error)
    return null
  }
}

function readFile(file) {
  try {
    return fs.readFileSync(file, 'utf-8')
  } catch (error) {
    return null
  }
}

function readEnvFile(fs, path, tomlDir, envFileName) {
  envFileName = parse(envFileName)
  if (!envFileName) return {}
  const envFilePath = path.resolve(tomlDir, envFileName)
  if (!envFilePath) return {}
  const content = readFile(envFilePath, 'utf-8')
  if (!content) return {}
  return content.split('\n').reduce((acc, val) => {
    if (!val) return acc
    const [key, value] = val
      .split('=')
      .map(v => v.trim())
      .map(v => parse(v) ?? v)
    return { ...acc, [key]: value }
  }, {})
}

function addVenvInPath(path, tomlDir, venvPath, scriptEnv) {
  if (!venvPath) return
  const venvRootPath = path.resolve(tomlDir, venvPath)
  const venvBinPath = path.resolve(tomlDir, venvPath, 'bin')
  scriptEnv.PATH = `${venvBinPath}:${scriptEnv.PATH}`
  scriptEnv.VIRTUAL_ENV = venvRootPath
  if (scriptEnv.PYTHON_HOME) delete scriptEnv.PYTHON_HOME
}

module.exports = {
  name: `yarn-mise`,
  factory: require => {
    const path = require('path')
    const fs = require('fs')
    return {
      hooks: {
        /** @param project Project */
        setupScriptEnvironment(project, scriptEnv) {
          const workspaceCwd = path.dirname(scriptEnv.npm_package_json)
          const workspace = project.tryWorkspaceByCwd(workspaceCwd)
          if (!workspace) return
          const paths = workspace.relativeCwd.split('/').reduce((acc, val) => {
            const prev = acc.at(-1)
            const value = prev ? [prev, val].join('/') : val
            return [...acc, value]
          }, [])
          for (const part of paths) {
            const tomlDir = path.resolve(project.cwd, part)
            const filePath = selectToml(fs, c => path.resolve(tomlDir, c))
            if (!filePath) continue
            const content = fs.readFileSync(filePath, 'utf-8')
            content.split('\n').reduce((isEnv, val) => {
              if (val === '') return isEnv
              if (val.startsWith('#')) return isEnv
              if (val === '[env]') return true
              if (val.startsWith('[')) return false
              if (val.startsWith('_.file')) {
                const envs = readEnvFile(
                  fs,
                  path,
                  tomlDir,
                  val.split('=').map(v => v.trim())[1]
                )
                Object.values(envs).forEach(([key, value]) => {
                  scriptEnv[key] = value
                })
              }
              if (val.includes('python.venv'))
                addVenvInPath(
                  path,
                  tomlDir,
                  parse(val.split('=')[1]?.trim()),
                  scriptEnv
                )
              if (isEnv) {
                const match = val.match(/^([_a-zA-Z0-9]+)\s?=\s?(.*)/)
                if (!match) return true
                const [_, variable, value] = match
                const parsed = parse(value)
                if (!parsed) return true
                scriptEnv[variable.toUpperCase()] = parsed
                return true
              }
              return isEnv
            }, false)
          }
        },
      },
    }
  },
}
