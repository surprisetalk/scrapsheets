```nu
mkdir dist
ln src/index.html dist/index.html
watch src { try { deno run -A npm:elm make src/Main.elm --debug --output=dist/index.js } }
deno run -A npm:serve dist -s
```
