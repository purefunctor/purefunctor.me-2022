const path = require("path");

const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");
const { sassPlugin } = require("esbuild-sass-plugin");

const production = process.env.NODE_ENV === "production";
const watch = process.env.WATCH === "true";

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    minify: production,
    outdir: "dist",
    watch: watch,
    plugins: [
      PurescriptPlugin({
        output: production ? path.resolve(__dirname, "dce-output") : undefined,
      }),
      sassPlugin(),
    ],
    loader: {
      ".ico": "file",
      ".png": "file",
    },
  })
  .catch((_e) => process.exit(1));
