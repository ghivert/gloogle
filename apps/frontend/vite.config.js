import { sentryVitePlugin } from '@sentry/vite-plugin'
import 'dotenv/config'
import gleam from 'vite-gleam'

export default {
  plugins: [
    gleam(),
    sentryVitePlugin({
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
}
