let path = require('path');
let glob = require('glob-all');
let ExtractTextPlugin = require('extract-text-webpack-plugin');
let PurgecssPlugin = require('purgecss-webpack-plugin');
let OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');

// https://github.com/FullHuman/purgecss#extractor
class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-z0-9-:\/]+/g) || [];
  }
}

module.exports = {
  entry: './index.js',
  output: {
    path: path.resolve(__dirname, '../dist'),
    filename: 'select.css'
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            { loader: 'css-loader', options: { importLoaders: 1 } },
						// Use for Tailwind processing
            'postcss-loader'
          ]
        })
      }
    ]
  },
  plugins: [
		// Bundle CSS to separate file, not JS
    new ExtractTextPlugin('select.css'),

		// Remove class names not used in any source files
		new	PurgecssPlugin({
			// Locations of any files to scan
			paths: glob.sync([
				path.join(__dirname, "../src/**/*.purs")
			]),
			extractors: [
				{
					extractor: TailwindExtractor,
					extensions: [ "purs" ]
				}
			]
		}),

		// Minify the CSS after processing
		new OptimizeCssAssetsPlugin()

	]
};
