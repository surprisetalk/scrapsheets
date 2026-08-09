```nu
deno task build
deno task dev
deno task test
```

```nu
# watch mode
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }
```

- [Twinkling lights and nested loops: distributed
  problem solving and spreadsheet development](https://www.lri.fr/~mbl/Stanford/CS477/papers/Nardi-Twinkling-IJMMS.pdf)

<!--
templates/pages/portals <- queries. pages can't reference anything, agents can reference pages but not queries, and queries can reference pages and agents (but not queries).
--->
