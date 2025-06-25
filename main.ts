// TODO: hono

app.post("/signup", async c => {
  const usr = {
    name: (await form(c)).name,
    email: (await form(c)).email,
    password: null,
  };
  const [{ email = undefined, token = undefined } = {}] = await sql`
    with usr_ as (insert into usr ${sql(usr)} on conflict do nothing returning *)
    select uid, email, email_token(now(), email) as token from usr_
  `;
  if (email && token) await sendVerificationEmail(email, token);
  return c.redirect("/");
});

app.post("/password", async c => {
  const { email, token, password } = await form(c);
  const [usr] = await sql`
    update usr
    set password = crypt(${password}, gen_salt('bf', 8)), email_verified_at = coalesce(email_verified_at, now())
    where true
      and to_timestamp(split_part(${token},':',1)::bigint) > now() - interval '2 days'
      and ${email} = email
      and ${token} = email_token(to_timestamp(split_part(${token},':',1)::bigint), email)
    returning uid
  `;
  if (usr) await setSignedCookie(c, "uid", usr.uid, cookieSecret);
  return ok(c);
});

app.post("/login", async c => {
  const { email, password } = await form(c);
  const [usr] = await sql`
    select *, password = crypt(${password}, password) AS is_password_correct
    from usr where email = ${email}
  `;
  if (!usr || !usr.is_password_correct)
    throw new HTTPException(401, { message: "Wrong credentials." });
  await setSignedCookie(c, "uid", usr.uid, cookieSecret);
  return c.redirect("/u");
});

app.get("/shop/sheet", async c => {
  // TODO: Query params.
  const rows = await sql`select * from sheet where price >= 0 limit 100`;
  return c.json(rows, 200);
});

app.get("/shop/tool", async c => {
  // TODO:
  return c.json(rows, 200);
});

// TODO: https://hono.dev/docs/helpers/websocket
app.get(
  "/library/:id/sync",
  upgradeWebSocket(c => {
    // TODO: https://docs.yjs.dev/api/document-updates
  }),
);

app.use(async (c, next) => {
  const uid = await getSignedCookie(c, cookieSecret, "uid");
  if (!uid) throw new HTTPException(401, { message: "Not authorized." });
  c.set("uid", uid);
  await next();
});

app.post("/shop/sheet/:id", async c => {
  await sql`
    insert into sheet_usr (sheet_id, usr_id, role, price, subscription) 
    select sheet_id, ${c.get("uid")}, 'readonly', price, subscription
    from sheet 
    where sheet_id = ${c.req.param("id")}
  `;
  return c.json(null, 204);
});

app.post("/shop/tool/:id", async c => {
  // TODO::
  return c.json(null, 204);
});

app.get("/ledger", async c => {
  // TODO:
  return c.json(rows, 200);
});

app.get("/library", async c => {
  const rows =
    await sql`select * from sheet s left join sheet_usr su using (usr_id) where su.id = ${c.get("uid")}`;
  return c.json(rows, 200);
});

app.post("/library", async c => {
  const [{ sheet_id, usr_id }] = await sql`
    with s as (insert into sheet (created_by) values (${c.get("uid")}) returning *)
    insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from s returning *
  `;
  return c.json(sheet_id, 201);
});

app.patch("/library/:id", async c => {
  await sql`update sheet set ${todo} where sheet_id = ${c.req.param("id")}`;
  return c.json(null, 204);
});

app.post("/portal/connect/:type", async c => {
  // TODO: oauth
});

app.all("/portal/proxy/:id", async c => {
  // TODO: oauth proxy
});

app.all("/mcp/sheet/:id", async c => {
  // TODO: mcp server
});
