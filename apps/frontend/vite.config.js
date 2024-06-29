import { sentryVitePlugin } from '@sentry/vite-plugin'
import 'dotenv/config'
import { defineConfig } from 'vite'
import gleam from 'vite-gleam'

export default defineConfig(({ mode }) => ({
  plugins: [
    gleam(),
    sentryVitePlugin({
      disable: mode === 'development',
      org: process.env.SENTRY_ORG,
      project: process.env.SENTRY_PROJECT,
      authToken: process.env.SENTRY_AUTH_TOKEN,
    }),
  ],
  build: {
    sourcemap: true,
  },
  rollupOptions: {
    output: {
      interop: 'auto',
    },
  },
}))
