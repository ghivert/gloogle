import gleam from 'vite-gleam'

export default {
  plugins: [gleam()],
  build: {
    minify: false,
  },
  rollupOptions: {
    output: {
      interop: 'auto',
    },
  },
}
