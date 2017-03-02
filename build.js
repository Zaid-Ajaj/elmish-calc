const path = require('path');
const fs = require('fs-extra');
const fable = require('fable-compiler');

const BUILD_DIR = "build"
const PROJ_FILE = "./src/app.fsx"

const fableconfig = {
  "babelPlugins": [ "transform-runtime" ],
  "projFile": PROJ_FILE,
  "sourceMaps": true,
  "outDir": "build"
};

const targets = {
  clean() {
    return fable.promisify(fs.remove, path.join(BUILD_DIR))
  },
  build() {
    return this.clean()
      .then(_ => fable.compile(fableconfig))
  }
}

// As with FAKE scripts, run a default target if no one is specified
targets[process.argv[2] || "build"]().catch(err => {
  console.log(err);
  process.exit(-1);
});
