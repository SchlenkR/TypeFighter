import { defineConfig } from 'vite';
import { copyFileSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  root: './src/visu',
  server: {
    port: 3000,
  },
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'src/visu/index.html'),
      },
    },
  },
  publicDir: resolve(__dirname, 'src/visu/public'),
  plugins: [
    {
      name: 'copy-gif-worker',
      buildStart() {
        // Copy gif.js worker to public directory
        const workerSrc = resolve(
          __dirname,
          'node_modules/gif.js/dist/gif.worker.js'
        );
        const workerDest = resolve(__dirname, 'src/visu/public/gif.worker.js');
        try {
          copyFileSync(workerSrc, workerDest);
        } catch (e) {
          console.warn('Could not copy gif.worker.js:', e);
        }
      },
    },
    {
      name: 'full-reload-on-change',
      handleHotUpdate({ server }) {
        server.ws.send({
          type: 'full-reload',
          path: '*',
        });
        return [];
      },
    },
  ],
});
