import { HTTPException } from "jsr:@hono/hono/http-exception";
import { Hono } from "jsr:@hono/hono";
import { logger } from "jsr:@hono/hono/logger";
import { cors } from "jsr:@hono/hono/cors";
import { jwt, sign } from "jsr:@hono/hono/jwt";
import type { JwtVariables } from "jsr:@hono/hono/jwt";
import sg from "npm:@sendgrid/mail";
import pg from "https://deno.land/x/postgresjs@v3.4.5/mod.js";
import { upgradeWebSocket } from "jsr:@hono/hono/deno";
import { Repo } from "npm:@automerge/automerge-repo";
import { NodeWSServerAdapter } from "npm:@automerge/automerge-repo-network-websocket";

export type Col = { type: "string" };
export type Row = Record<string, any>;
export type Sheet =
  | { type: "template"; template: Sheet }
  | { type: "page"; cols: Col[]; Row: Row[] }
  | { type: "portal" }
  | { type: "agent"; agent: unknown }
  | { type: "query"; db: string; lang: "prql" | "sql"; code: string };
export interface LibraryItem {
  sheet_id: string;
  name: string;
  tags: string[];
}

const JWT_SECRET = Deno.env.get("JWT_SECRET") ?? Math.random().toString();
const TOKEN_SECRET = Deno.env.get("TOKEN_SECRET") ?? Math.random().toString();

export const createJwt = async (usr_id: string) =>
  await sign(
    {
      sub: usr_id,
      exp: Math.floor(Date.now() / 1000) + 60 * 60 * 24 * 7,
    },
    JWT_SECRET,
  );

const createToken = async (
  email: string,
  ts: Date = new Date(),
): Promise<string> => {
  const epoch = Math.floor(ts.getTime() / 1000);
  const hash = await crypto.subtle.digest(
    "SHA-256",
    new TextEncoder().encode(`${epoch}:${email}:${TOKEN_SECRET}`),
  );
  return `${epoch}:${Array.from(new Uint8Array(hash))
    .map(b => b.toString(16).padStart(2, "0"))
    .join("")}`;
};

const sendVerificationEmail = async (email: string) => {
  if (!Deno.env.get(`SENDGRID_API_KEY`)) return;
  const token = await createToken(email);
  await sg
    .send({
      to: email,
      from: "hello@scrap.land",
      subject: "Verify your email",
      text:
        `` +
        `Welcome to scrap.land` +
        `\n\n` +
        `Please verify your email: ` +
        `https://scrap.land/password` +
        `?email=${encodeURIComponent(email)}` +
        `&token=${encodeURIComponent(token)}`,
    })
    .catch(err => {
      console.log(`/password?email=${email}&token=${token}`);
      console.error(
        `Could not send verification email to ${email}:`,
        err?.response?.body || err,
      );
    });
};

export const sql = pg({
  host: "127.0.0.1",
  port: 5434,
  database: "postgres",
  username: "postgres",
  password: "",
  ssl: false,
  prepare: false,
  fetch_types: true,
  onnotice: msg => msg.severity !== "DEBUG" && console.log(msg),
});

const cselect = async ({
  select,
  from,
  where,
  order,
  limit = "50",
  offset = "0",
}: {
  select: any;
  from: any;
  where: any[];
  order?: any;
  limit: string;
  offset: string;
}) => {
  const where_ = where.filter(x => x).length
    ? sql`where true ${where.filter(x => x).map((x: any) => sql`and ${x}`)}`
    : sql``;
  const data =
    await sql`${select} ${from} ${where_} ${order ?? sql``} limit ${limit} offset ${offset}`;
  const [{ count }] = await sql`select count(*) ${from} ${where_}`;
  return { data, count };
};

// TODO: Add rate-limiting middleware everywhere.
export const app = new Hono<{
  Variables: JwtVariables & { usr_id: string };
}>();

app.use("*", logger());

// TODO: Add a storage adapter.
// TODO: Connect the network adapetr to the websocket.
// TODO: https://github.com/automerge/automerge-repo-sync-server/blob/main/src/server.js
// TODO: Configure the port.
export const automerge = new Repo({
  network: [],
  /** @ts-ignore @type {(import("@automerge/automerge-repo").PeerId)}  */
  peerId: `server-${Deno.hostname()}`,
  sharePolicy: () => Promise.resolve(false),
});

app.get(
  "/library/sync",
  upgradeWebSocket(c => {
    const { auth } = c.req.query(); // TODO: Verify Bearer token on auth.
    return {
      onOpen: undefined,
      onMessage: undefined,
      onClose: undefined,
      onError: undefined,
    };
  }),
);

app.use("*", cors());

app.use(async (c, next) => {
  await next();
  if (c.res.headers.get("Content-Type")?.startsWith("application/json")) {
    const obj = await c.res.json();
    c.res = new Response(JSON.stringify(obj, null, 2), c.res);
  }
});

app.notFound(() => {
  throw new HTTPException(404, { message: "Not found." });
});

app.onError((err, c) => {
  // if (err?.code === "23505") return c.json({ error: "Already exists" }, 409);
  if (err instanceof HTTPException) return err.getResponse();
  if (err) console.error(err);
  return c.json({ error: "Sorry, something went wrong." }, 500);
});

app.post("/signup", async c => {
  const { email } = c.req.query();
  await sendVerificationEmail(email);
  return c.json(null, 200);
});

app.post("/signup/:token{.+}", async c => {
  const token = c.req.param("token");
  const { email, password } = await c.req.json();
  if (!token || !email || !password) return c.json(null, 400);
  const [ts, _hash] = token.split(":");
  const epoch = parseInt(ts);
  if (Date.now() / 1000 - epoch > 86400) return c.json(null, 401);
  if (token !== (await createToken(email, new Date(epoch * 1000))))
    return c.json(null, 401);
  await sql`
    insert into usr ${sql({ email, password: sql`crypt(${password}, gen_salt('bf', 8))` as unknown as string })} 
    on conflict do update set password = excluded.password
  `;
  return c.json(null, 200);
});

app.post("/login", async c => {
  const { email, password } = await c.req.json();
  const [usr] =
    await sql`select usr_id from usr where email = ${email} and crypt(${password}, password)`;
  if (!usr) return c.json(null, 401);
  return c.json(
    { data: { usr_id: usr.usr_id, jwt: await createJwt(usr.usr_id) } },
    200,
  );
});

app.get("/shop/sheet", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/shop/tool", async c => {
  // TODO:
  return c.json(null, 500);
});

app.use("*", jwt({ secret: JWT_SECRET }));

app.use("*", async (c, next) => {
  c.set("usr_id", c.get("jwtPayload")?.sub);
  await next();
});

app.post("/shop/sheet/:id", async c => {
  // TODO: Share sheet as a portal.
  return c.json(null, 500);
});

app.post("/shop/tool/:id", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/ledger", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/db", async c => {
  // TODO:
  return c.json(null, 500);
});

app.put("/db/:id", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/library", async c => {
  const { limit, offset, ...qs } = c.req.query();
  const res = await cselect({
    select: sql`select s.*`,
    from: sql`
      from sheet s 
      left join sheet_usr su using (sheet_id) 
    `,
    where: [
      sql`su.usr_id = ${c.get("usr_id")}`,
      qs.sheet_id && sql`sheet_id = ${qs.sheet_id}`,
    ],
    order: undefined,
    limit,
    offset,
  });
  return c.json(res);
});

app.post("/library", async c => {
  const hand = automerge.create({});
  const [{ sheet_id }] = await sql`
    with s as (insert into sheet ${sql({ sheet_id: hand.documentId, created_by: c.get("usr_id") })} returning *)
    insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from s returning sheet_id
  `;
  return c.json({ data: sheet_id }, 201);
});

app.patch("/library/:id", async c => {
  await sql`
    update sheet 
    set ${sql(await c.req.json(), "name", "tags")} 
    where true
    and created_by = ${c.get("usr_id")}
    and sheet_id = ${c.req.param("id")}
  `;
  return c.json(null, 200);
});

app.post("/query", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/agent/:id", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/portal/:id", async c => {
  // TODO:
  return c.json(null, 500);
});

app.post("/portal/connect/:type", async c => {
  // TODO: oauth
  return c.json(null, 500);
});

app.all("/mcp/sheet/:id", async c => {
  // TODO: mcp server
  return c.json(null, 500);
});

// Deno.serve(app.fetch).finished.then(async () => {
//   await sql.end();
//   for (const room of Object.values(rooms)) await room.destroy();
// });

export default app;
