const path              = require('path');
const webpack           = require('webpack');
const merge             = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

console.log('Starting webpack process...');

// Determine build env by npm command options
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';
const ENV =
  { 'port': process.env.PORT || 8080
  , 'host': process.env.HOST || 'localhost'
  , 'title': process.env.TITLE || 'elm-transition-example'
  };
const isDev = TARGET_ENV === 'development';

// Common webpack config
const commonConfig =
  { output:
    { path: path.resolve(__dirname, 'docs/')
    , filename: '[name].js'
    // , publicPath: '/elm-chat-todo/'
    }
  , entry:
    { 'index':
      [ path.join(__dirname, 'src/index.js')
      ]
    }
  , resolve:
    { extensions: ['.js', '.elm']
    , modules: [
      'node_modules'
      ]
    }
  , module:
    { rules:
      [ { test: /\.(eot|ttf|woff|woff2|svg)$/
        , use:
          [ { loader: 'file-loader'
            , options:
              { outputPath: 'static/'
              }
            }
          ]
        }
      , { test: /\.(jpg|jpeg|png)$/
        , use: 'url-loader'
        }
      ]
    }
  , plugins:
    [ new HtmlWebpackPlugin
      ( { chunks: ['index']
        , template: 'src/index.html'
        , inject: 'body'
        , filename: 'index.html'
        , data: ENV
        , hash: true
        , minify:
          { collapseInlineTagWhitespace: true
          , collapseWhitespace: true
          , html5: true
          , removeComments: true
          }
        }
      )

    // Inject variables to JS file.
    , new webpack.DefinePlugin
      ( { 'process.env':
          Object.keys(ENV).reduce((o, k) =>
            merge(o, {
              [k]: JSON.stringify(ENV[k]),
            }), {}
          )
        }
      )
    ]
  };

// Settings for `npm start`
if (TARGET_ENV === 'development') {
  console.log('Serving locally...');

  module.exports = merge(commonConfig,
    { devServer:
      { contentBase: 'src'
      , inline: true
      , port: ENV.port
      , host: ENV.host
      }
    , module:
      { rules:
        [ { test: /\.elm$/
          , exclude: [/elm-stuff/, /node_modules/]
          , use:
            [ { loader: 'elm-hot-loader'
              ,
              }
            , { loader: 'elm-webpack-loader'
              , options:
                { verbose: true
                , warn: true
                // , debug: true
                }
              }
            ]
          }
        , { test: /\.(css|scss)$/
          , use:
            [ 'style-loader'
            , { 'loader': 'css-loader'
              }
            , { 'loader': 'sass-loader'
              , 'options':
                { 'includePaths':
                  [ path.resolve(__dirname, 'src/scss/')
                  ]
                }
              }
            , 'postcss-loader'
            ]
          }
        ]
      }
    }
  );
}

// Settings for `npm run build`.
if (TARGET_ENV === 'production') {
  console.log('Building for prod...');

  module.exports = merge(commonConfig,
    { module:
      { rules:
        [ { test: /\.elm$/
          , exclude: [/elm-stuff/, /node_modules/]
          , use:
            [ { loader: 'elm-webpack-loader'
              , options:
                { verbose: true
                , warn: true
                // , debug: true
                }
              }
            ]
          }
        , { test: /\.(css|scss)$/
          , use: ExtractTextPlugin.extract
            ( { fallback: 'style-loader'
              , use:
                [ { 'loader': 'css-loader'
                  }
                , { 'loader': 'sass-loader'
                  , 'options':
                    { 'includePaths':
                      [ path.resolve(__dirname, 'src/scss/')
                      ]
                    }
                  }
                , 'postcss-loader'
                ]
              }
            )
          }
        ]
      }
    , plugins:
      [ new CopyWebpackPlugin
        ( [
          ]
        )
      // Extract CSS into a separate file
      , new ExtractTextPlugin
        ( { filename: 'static/[name].css'
          , allChunks: true
          }
        )
      // Minify & mangle JS/CSS
      , new webpack.optimize.UglifyJsPlugin
        ( { minimize: true
          , compressor:
            { warnings: false
            }
          // , mangle: true
          }
        )
      ]
    }
  );
}
