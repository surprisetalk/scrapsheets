Deno.test("elm-test", async () => {
  const dir = new URL(".", import.meta.url).pathname;
  const cmd = new Deno.Command("npx", { args: ["elm-test"], stdout: "piped", stderr: "piped", cwd: dir });
  const { code, stdout, stderr } = await cmd.output();
  const out = new TextDecoder().decode(stdout);
  const err = new TextDecoder().decode(stderr);
  if (code !== 0) throw new Error(`elm-test failed:\n${out}\n${err}`);
});
