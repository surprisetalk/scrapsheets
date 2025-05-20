```nu
mkdir dist
ln src/index.html dist/index.html
watch src { try { elm make src/Main.elm --debug --output=dist/index.js } }
http-server dist
```
