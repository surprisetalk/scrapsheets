```nu
mkdir dist
ln src/index.html dist/index.html
ln src/style.css dist/style.css
ln automerge.js dist/automerge.js
ln automerge.wasm dist/automerge.wasm
watch src { cp -f src/* dist/* ; try { deno run -A npm:elm make src/Main.elm --debug --output=dist/index.js } }
deno run -A npm:serve dist -s -C -S -n
```
