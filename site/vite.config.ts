import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  base: process.env.SITE_BASE ?? "/",
  plugins: [react()],
  worker: { format: "es" },
  server: { port: 3100 },
});
