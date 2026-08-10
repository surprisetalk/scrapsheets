Deno.test("elm-test", async () => {
  const dir = new URL(".", import.meta.url).pathname;
  // elm-test runs under node, not `deno run npm:`: its workers talk over a unix
  // socket that deno's node:net emulation refuses to connect to.
  const cmd = new Deno.Command("npx", {
    args: ["--yes", "elm-test@0.19.2-0"],
    stdout: "piped",
    stderr: "piped",
    cwd: dir,
  });
  const { code, stdout, stderr } = await cmd.output();
  const out = new TextDecoder().decode(stdout);
  const err = new TextDecoder().decode(stderr);
  if (code !== 0) throw new Error(`elm-test failed:\n${out}\n${err}`);
});
