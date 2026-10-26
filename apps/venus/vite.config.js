import { sentryVitePlugin } from '@sentry/vite-plugin'
import { defineConfig } from 'vite'
import { gleamImportPlugin } from '@chouquette/gleam-vite-loader'

export default defineConfig(({ mode }) => ({
  plugins: [
    gleamImportPlugin(),
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
