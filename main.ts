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
import type { AnyDocumentId, DocHandle } from "npm:@automerge/automerge-repo";
import { NodeWSServerAdapter } from "npm:@automerge/automerge-repo-network-websocket";
import ala from "npm:alasql";

const JWT_SECRET = Deno.env.get("JWT_SECRET") ?? Math.random().toString();
const TOKEN_SECRET = Deno.env.get("TOKEN_SECRET") ?? Math.random().toString();

ala.options.modifier = "RECORDSET";

export type Template = "TODO";
export type Recordset = { columns: Col[]; data: Row[] };
export type Col = { columnid: string; type?: "string" | "int" }; // { name: string; type: "string" | "int" };
export type Row = Record<string, any>;
export type Page = { cols: Col[]; rows: Row[] };
export type Net =
  | null
  | { type: "http"; cron: string; url: string }
  | { type: "websocket"; url: string };
export type Query = {
  db_id: string | null; // TODO: Get rid of this and reference tables directly.
  lang: "prql" | "sql";
  code: string;
};
export type Sheet =
  | { type: "template"; doc: Template }
  | { type: "page"; doc: Page }
  | { type: "portal"; doc: null }
  | { type: "net"; doc: Net }
  | { type: "query"; doc: Query };
export interface LibraryItem {
  sheet_id: string;
  type: Sheet["type"];
  doc_id: string;
  name: string;
  tags: string[];
}

// TODO: template,page,net,query are now doc:123 which sync literally, and portal/db are different?

const querify = async ({ db_id, lang, code }: Query): Promise<Page> => {
  if (lang === "sql" && db_id === null) {
    const sheet_ids: string[] = [];
    const code_ = code.replace(
      /@[^ ]+/g,
      ref => (sheet_ids.push(ref.slice(1)), "?"),
    );
    const pages: Record<string, Page> = {};
    for (const sheet_id of sheet_ids)
      pages[sheet_id] = pages[sheet_id] ?? (await pagify(sheet_id));
    const { columns: cols, data: rows }: Recordset = await ala(
      code_,
      sheet_ids.map(id => pages[id].rows),
    );
    return { cols, rows };
  } else if (lang === "sql") {
    throw new Error("TODO");
  } else if (lang === "prql" && db_id === null) {
    throw new Error("TODO");
  } else if (lang === "prql") {
    throw new Error("TODO");
  } else {
    throw new Error("TODO");
  }
};

const pagify = async (sheet_id: string): Promise<Page> => {
  const [type, doc_id] = sheet_id.split(":");
  const hand = await automerge.find(doc_id as AnyDocumentId);
  switch (type) {
    case "template":
      throw new HTTPException(500, { message: "TODO" });
    case "page":
      return hand.doc() as Page;
    case "query":
      return await querify(hand.doc() as Query);
    case "portal":
      throw new HTTPException(500, { message: "Bad sheet recursion." });
    default:
      throw new HTTPException(500, { message: "Unknown sheet type." });
  }
};

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

// TODO: Add a storage adapter. Persist somewhere -- doesn't really matter where.
// TODO: Connect the network adapetr to the websocket.
// TODO: https://github.com/automerge/automerge-repo-sync-server/blob/main/src/server.js
// TODO: Configure the port.
export const automerge = new Repo({
  network: [],
  // @ts-ignore @type
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
  const data = await sql`select * from sheet s where price >= 0 limit 25`;
  return c.json({ data }, 200);
});

app.post("/net/:id", async c => {
  await sql`insert into net ${sql({ sheet_id: `net:${c.req.param("id")}`, body: await c.req.text() })}`;
  return c.json(null, 200);
});

app.use("*", jwt({ secret: JWT_SECRET }));

app.use("*", async (c, next) => {
  c.set("usr_id", c.get("jwtPayload")?.sub);
  await next();
});

app.post("/shop/sheet/buy/:id", async c => {
  const sheet_id = c.req.param("id");
  if (!sheet_id.startsWith("portal:"))
    throw new HTTPException(400, { message: "Cannot purchase this sheet." });
  await sql`insert into sheet_usr ${sql({ sheet_id, usr_id: c.get("usr_id") })}`;
  return c.json(null, 201);
});

app.post("/shop/sheet/sell/:id", async c => {
  const sheet_id_ = c.req.param("id");
  const [type, doc_id] = sheet_id_.split(":");
  if (type === "portal")
    throw new HTTPException(400, { message: "Cannot sell this sheet." });
  const [{ sheet_id }] = await sql`
    with s as (insert into sheet ${sql({ doc_id: sql`md5(${doc_id})` as unknown as string, type: "portal", created_by: c.get("usr_id"), data: { sheet_id: sheet_id_ } })} returning *)
    insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from s returning sheet_id
  `;
  return c.json({ data: sheet_id }, 201);
});

app.post("/db", async c => {
  const [{ db_id }] =
    await sql`insert into db ${sql({ ...(await c.req.json()), usr_id: c.get("usr_id") })} returning *`;
  return c.json({ data: db_id }, 201);
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
  const { type, doc } = await c.req.json();
  // TODO: Validate to ensure it matches Sheet.
  if (!["template", "page", "agent", "query", "net"].includes(type))
    return c.json({ error: "Invalid initial sheet." }, 400);
  const hand = automerge.create(doc);
  const [{ sheet_id }] = await sql`
    with s as (insert into sheet ${sql({ doc_id: hand.documentId, type, created_by: c.get("usr_id") })} returning *)
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
  return c.json(
    { data: { type: "page", doc: await querify(await c.req.json()) } },
    200,
  );
});

app.get("/net/:id", async c => {
  const rows = await sql`
    select j.created_at, j.body
    from sheet s
    inner join net j using (sheet_id)
    inner join sheet_usr su on (su.sheet_id,su.usr_id) = (s.sheet_id,${c.get("usr_id")})
    where s.sheet_id = 'net:'||${c.req.param("id")}
  `;
  return c.json(
    {
      data: {
        type: "page",
        doc: {
          cols: [
            { name: "created_at", type: "string" },
            { name: "body", type: "text" },
          ],
          rows,
        },
      },
    },
    200,
  );
});

app.get("/portal/:id", async c => {
  const [sheet] = await sql`
    select s_.sheet_id, s_.type, s_.doc_id, su.sheet_id is not null as is_allowed
    from sheet s
    inner join sheet s_ on s_.sheet_id = s.data->>'sheet_id'
    left join sheet_usr su on (su.sheet_id,su.usr_id) = (s.sheet_id,${c.get("usr_id")})
    where s.sheet_id = 'portal:'||${c.req.param("id")}
  `;
  if (!sheet) throw new HTTPException(404, { message: "Not found." });
  if (!sheet.is_allowed)
    throw new HTTPException(403, { message: "Forbidden." });
  switch (sheet.type) {
    case "portal":
      throw new HTTPException(500, { message: "Bad sheet recursion." });
    case "template": {
      const hand = await automerge.find(sheet.doc_id);
      return c.json({ data: { type: "template", doc: hand.doc() } }, 200);
    }
    default:
      return c.json(
        { data: { type: "page", doc: await pagify(sheet.sheet_id) } },
        200,
      );
  }
});

app.post("/portal/connect/:type", async c => {
  // TODO: oauth
  return c.json(null, 500);
});

app.all("/mcp/sheet/:id", async c => {
  // TODO: mcp server
  return c.json(null, 500);
});

export default app;
