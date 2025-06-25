// TODO: pglite

import { add } from "./main.ts";
import { assertEquals } from "@std/assert";

/*
TODO:
g /shop/sheet
g /shop/tool
p /signup
p /password
p /login
p /shop/sheet/id
p /shop/tool/id
g /ledger
g /library
p /library/:id
for [template, page, portal, agent, query]:
  p /library
  w /library/:id
*/

Deno.test(function addTest() {
  assertEquals(add(2, 3), 5);
});
