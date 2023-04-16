const path = require('path');
const HtmlWebPackPlugin = require('html-webpack-plugin');

module.exports = {
    mode: 'development',
    entry: {
        app: './src/app.ts',
        'editor.worker': 'monaco-editor/esm/vs/editor/editor.worker.js',
        'json.worker': 'monaco-editor/esm/vs/language/json/json.worker',
    },
    resolve: {
        extensions: ['.ts', '.js'],
        alias: {
            'typedjson': path.resolve(__dirname, '../typed-json-js-export/target/scala-3.2.2/typed-json-js-export-fastopt.js'),
        },
    },
    output: {
        globalObject: 'self',
        filename: '[name].bundle.js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
        rules: [
            {
                test: /\.ts?$/,
                use: 'ts-loader',
                exclude: /node_modules/
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.ttf$/,
                use: ['file-loader']
            }
        ]
    },
    plugins: [
        new HtmlWebPackPlugin({
            title: 'Typed Json Monaco Editor Sample',
            template: './src/index.html',
            filename: './index.html',
            inject: false,
        })
    ],
};
