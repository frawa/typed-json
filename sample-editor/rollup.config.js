import { nodeResolve } from '@rollup/plugin-node-resolve';
import typescript from '@rollup/plugin-typescript';

export default {
    input: 'src/app.ts',
    output: {
        // dir: 'dist',
        file: 'public/bundle.js',
        // format: 'cjs',
        format: 'iife',
        sourcemap: true
    },
    plugins: [
        nodeResolve(),
        typescript()
    ]
};