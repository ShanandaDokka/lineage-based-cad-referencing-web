module.exports = {
  "entry": {
    "elodin-fastopt": ["/Users/shana/Desktop/delete/Research/lineage-based-cad-referencing-web/js/target/scala-3.2.1/scalajs-bundler/main/elodin-fastopt-entrypoint.js"]
  },
  "output": {
    "path": "/Users/shana/Desktop/delete/Research/lineage-based-cad-referencing-web/js/target/scala-3.2.1/scalajs-bundler/main",
    "filename": "[name]-library.js",
    "library": "ScalaJSBundlerLibrary",
    "libraryTarget": "var"
  },
  "mode": "development",
  "devServer": {
    "port": 8080
  },
  "devtool": "source-map",
  "module": {
    "rules": [{
      "test": new RegExp("\\.js$"),
      "enforce": "pre",
      "use": ["source-map-loader"]
    }]
  }
}