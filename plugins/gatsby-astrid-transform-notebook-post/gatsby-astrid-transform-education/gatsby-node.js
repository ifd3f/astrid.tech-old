"use strict"

require("source-map-support").install()
require("ts-node").register({
  compilerOptions: {
    module: "commonjs",
    target: "es2017",
  },
})

module.exports = require("./gatsby-node.ts")
