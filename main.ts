import { HTTPException } from "jsr:@hono/hono/http-exception";
import { Context, Hono } from "jsr:@hono/hono";
import { logger } from "jsr:@hono/hono/logger";
import { cors } from "jsr:@hono/hono/cors";
import { jwt, sign, verify } from "jsr:@hono/hono/jwt";
import type { JwtVariables } from "jsr:@hono/hono/jwt";
import sg from "npm:@sendgrid/mail";
import pg from "https://deno.land/x/postgresjs@v3.4.5/mod.js";
import { upgradeWebSocket } from "jsr:@hono/hono/deno";
import { Repo } from "npm:@automerge/automerge-repo";
import * as AM from "npm:@automerge/automerge-repo";
import type { AnyDocumentId } from "npm:@automerge/automerge-repo";
// import { NodeWSServerAdapter } from "npm:@automerge/automerge-repo-network-websocket";
import { NodeFSStorageAdapter } from "npm:@automerge/automerge-repo-storage-nodefs";
import ala from "npm:alasql";
import * as path from "https://deno.land/std@0.188.0/path/mod.ts";

const JWT_SECRET = Deno.env.get("JWT_SECRET") ?? Math.random().toString();
const TOKEN_SECRET = Deno.env.get("TOKEN_SECRET") ?? Math.random().toString();

ala.options.modifier = "RECORDSET";

export type Tag<T extends string, X extends Row[]> = {
  type: T;
  data: X;
};

export type Type =
  // TODO: Consider using JSONSchema?
  | "type"
  | "text"
  | "create"
  | "usd"
  | "int"
  | ["array", Type]
  | ["tuple", Type[]]
  | { [k: string]: Type };
export type Row<T = unknown> = Record<string | number, T>;
export type Col = { name: string; type: Type; key: string | number };
export type Table = [Row<Col>, ...Row[]];
export type Args = { key: string; value: unknown }[];
export type Template =
  | Tag<"template", [Template]>
  | Tag<"table", [Row<Col>]>
  | Tag<"query", [Query]>
  | Tag<"net-hook", []>
  | Tag<"net-http", [NetHttp]>
  | Tag<"net-socket", [NetSocket]>
  | Tag<`codex-${string}`, []>;
export type Query = { lang: "sql" | "prql"; code: string; args: Args };
export type NetHttp = { url: string; interval: number };
export type NetSocket = { url: string };
export type Sheet =
  | Tag<"template", [Template]>
  | Tag<"table", Table>
  | Tag<"net-hook", []>
  | Tag<"net-http", [NetHttp]>
  | Tag<"net-socket", [NetSocket]>
  | Tag<"query", [Query]>
  | Tag<"portal", Args>
  | Tag<"codex", []>;

export type Page = {
  data: Table;
  count: number;
  offset: number;
};

export const arrayify = <T>(arr: Array<T>): Record<number, T> => {
  const obj: Record<number, T> = {};
  for (const i in arr) obj[i] = arr[i];
  return obj;
};

const sheet = async (
  c: Context,
  sheet_id: string,
  { limit, offset, ...qs }: Record<string, string>,
): Promise<Page> => {
  const [type, doc_id] = sheet_id.split(":");
  const hand = await automerge
    .find<{ data: Sheet["data"] }>(doc_id as AnyDocumentId)
    .catch(() => ({
      doc: () => {
        throw new HTTPException(404, { message: "Sheet not found." });
      },
    }));
  switch (type) {
    case "table": {
      const doc = hand.doc().data as Table;
      return { data: doc, count: doc.length - 1, offset: 0 };
    }
    case "net-hook":
    case "net-http":
    case "net-socket":
      return await cselect({
        cols: null,
        select: sql`select n.created_at, n.body`,
        from: sql`from sheet_usr su inner join net n using (sheet_id)`,
        where: [
          sql`(su.sheet_id,su.usr_id) = (${sheet_id},${c.get("usr_id")})`,
        ],
        order: undefined,
        limit,
        offset,
      });
    case "query":
      return await querify(c, hand.doc().data[0] as Query, {
        limit,
        offset,
        ...qs,
      });
    case "template":
      throw new HTTPException(500, { message: "Bad template." });
    case "portal":
      throw new HTTPException(500, { message: "Bad sheet recursion." });
    default:
      throw new HTTPException(500, { message: "Unknown sheet type." });
  }
};

const querify = async (
  c: Context,
  { lang, code, args: _args = [] }: Query,
  _reqQuery: Record<string, string>,
): Promise<Page> => {
  if (lang === "sql") {
    // TODO: Use psql/etc if all sheets are from the same codex.
    const sheet_ids: string[] = [];
    const code_ = code.replace(
      /@[^ ]+/g,
      (ref) => (sheet_ids.push(ref.slice(1)), "?"),
    );
    const docs: Record<string, Record<string, unknown>[]> = {};
    for (const sheet_id of sheet_ids) {
      if (docs[sheet_id]) continue;
      const [cols, ...rows] = (await sheet(c, sheet_id, {})).data;
      docs[sheet_id] = rows.map((row) =>
        Object.fromEntries(
          Object.values(cols).map((col) => [col.name, row[col.key]]),
        )
      );
    }
    const {
      columns: cols,
      data: rows,
    }: {
      columns: { columnid: string }[];
      data: Record<string, unknown>[];
    } =
      // TODO: Consider just using postgresjs and passing in tables as ${sql([...])}
      await ala(
        code_,
        sheet_ids.map((id) => docs[id]),
      );
    return {
      data: [
        Object.fromEntries(
          cols.map((col, i) => [
            i,
            {
              name: col.columnid,
              type: "text",
              key: col.columnid,
            },
          ]),
        ),
        ...rows,
      ],
      count: -1, // TODO:
      offset: -1, // TODO:
    };
  } else {
    throw new HTTPException(500, { message: "TODO" });
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
  return `${epoch}:${
    Array.from(new Uint8Array(hash))
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("")
  }`;
};

const sendVerificationEmail = async (email: string) => {
  if (!Deno.env.get(`SENDGRID_API_KEY`)) return;
  const token = await createToken(email);
  await sg
    .send({
      to: email,
      from: "hello@scrap.land",
      subject: "Verify your email",
      text: `` +
        `Welcome to scrap.land` +
        `\n\n` +
        `Please verify your email: ` +
        `https://scrap.land/password` +
        `?email=${encodeURIComponent(email)}` +
        `&token=${encodeURIComponent(token)}`,
    })
    .catch((err) => {
      console.log(`/password?email=${email}&token=${token}`);
      console.error(
        `Could not send verification email to ${email}:`,
        err?.response?.body || err,
      );
    });
};

export const sql = pg(
  Deno.env.get("DATABASE_URL") ??
    "postgresql://postgres@127.0.0.1:5434/postgres",
  {
    fetch_types: true,
    onnotice: (msg) => msg.severity !== "DEBUG" && console.log(msg),
  },
);

const cselect = async ({
  cols,
  select,
  from,
  where,
  order,
  limit = "50",
  offset = "0",
}: {
  cols: Col[] | null;
  select: any;
  from: any;
  where: any[];
  order?: any;
  limit: string;
  offset: string;
}): Promise<Page> => {
  const where_ = where.filter((x) => x).length
    ? sql`where true ${where.filter((x) => x).map((x: any) => sql`and ${x}`)}`
    : sql``;
  const rows = await sql`${select} ${from} ${where_} ${order ?? sql``} limit ${limit} offset ${offset}`;
  const cols_: Row<Col> = arrayify(
    rows.columns.map((col, i) => ({
      name: col.name,
      type: "text", // TODO: col.type
      key: col.name,
    })),
  );
  const [{ count }] = await sql`select count(*) ${from} ${where_}`;
  return {
    data: [(cols as unknown as Row<Col>) ?? cols_, ...rows],
    count,
    offset: parseInt(offset),
  };
};

const page = (c: Context) => ({ data, offset, count }: Page) => {
  c.header("Content-Range", `${offset}-${offset + data.length - 1}/${count}`);
  return c.json({ data }, 200);
};

// TODO: Add rate-limiting middleware everywhere.
export const app = new Hono<{
  Variables: JwtVariables & { usr_id: string };
}>();

app.use("*", logger());

class HonoWebSocketAdapter extends AM.NetworkAdapter implements AM.NetworkAdapterInterface {
  private connections = new Map<AM.PeerId, any>();
  private _isReady = true;
  isReady(): boolean {
    return this._isReady;
  }
  whenReady(): Promise<void> {
    return Promise.resolve();
  }
  connect(peerId: AM.PeerId, peerMetadata?: AM.PeerMetadata): void {
    // Connection already established via WebSocket
    console.log(`HonoAdapter.connect() called for peer: ${peerId}`);
    this.emit("peer-candidate", { peerId, peerMetadata: peerMetadata || {} });
  }
  disconnect(): void {
    for (const [peerId, ws] of this.connections) {
      try {
        ws.close();
      } catch (error) {
        console.error(`Error closing connection for ${peerId}:`, error);
      }
    }
    this.connections.clear();
    // Don't emit peer-disconnected for server itself on general disconnect
  }
  send(message: AM.Message): void {
    console.log(`HonoAdapter sending message to ${message.targetId}:`, {
      type: message.type,
      senderId: message.senderId,
      targetId: message.targetId,
      connectionExists: this.connections.has(message.targetId),
    });
    const connection = this.connections.get(message.targetId);
    if (connection && connection.readyState === WebSocket.OPEN) {
      // Send the message data as binary if it has a data property, otherwise JSON
      if ("data" in message && message.data instanceof Uint8Array) {
        console.log(
          `Sending binary data (${message.data.length} bytes) to ${message.targetId}`,
        );
        connection.send(message.data);
      } else {
        console.log(`Sending JSON message to ${message.targetId}`);
        connection.send(JSON.stringify(message));
      }
      console.log(`Message sent successfully to ${message.targetId}`);
    } else {
      console.warn(`No open connection for peer ${message.targetId}`);
      console.log(
        `Available connections:`,
        Array.from(this.connections.keys()),
      );
    }
  }
  addConnection(peerId: AM.PeerId, ws: any): void {
    console.log(`Adding connection for peer: ${peerId}`);
    this.connections.set(peerId, ws);
    console.log(`Total connections: ${this.connections.size}`);

    // Emit peer-candidate event to let automerge know about the new peer
    console.log(`Emitting peer-candidate for: ${peerId}`);
    this.emit("peer-candidate", {
      peerId,
      peerMetadata: {},
    });
  }
  removeConnection(peerId: AM.PeerId): void {
    console.log(`Removing connection for peer: ${peerId}`);
    this.connections.delete(peerId);
    console.log(`Total connections: ${this.connections.size}`);
    console.log(`Emitting peer-disconnected for: ${peerId}`);
    this.emit("peer-disconnected", { peerId });
  }
  handleMessage(peerId: AM.PeerId, data: Uint8Array): void {
    console.log(`Received binary message from ${peerId}:`, {
      length: data.length,
      type: data.constructor.name,
    });
    try {
      // Automerge expects raw binary data, not parsed JSON
      // The message format is handled internally by automerge
      console.log(`Emitting raw binary message for automerge processing`);
      this.emit("message", {
        data,
        senderId: peerId,
      } as any);
    } catch (error) {
      console.error(
        `Failed to handle automerge message from ${peerId}:`,
        error,
      );
    }
  }
}

// TODO: Replace this with AM.NodeWebSocketAdapter. Try to grab hono's websocket server and pass it in. Use automerge.networkSubsystem.add...?
const honoAdapter = new HonoWebSocketAdapter();

export const automerge = new Repo({
  network: [honoAdapter],
  storage: new NodeFSStorageAdapter(`${path.dirname(path.fromFileUrl(Deno.mainModule))}/data/automerge`),
  peerId: `server-${Deno.hostname()}` as AM.PeerId,
  sharePolicy: () => Promise.resolve(true), // TODO:
});

app.get(
  "/library/sync",
  upgradeWebSocket(async (c) => {
    const { auth } = c.req.query();
    let usr_id: string | null = null;
    if (auth) {
      try {
        const token = auth.startsWith("Bearer ") ? auth.slice(7) : auth;
        const payload = await verify(token, JWT_SECRET);
        usr_id = payload.sub as string;
      } catch (error) {
        console.error("JWT verification failed:", error);
      }
    }

    const peerId = (
      usr_id ? `user-${usr_id}` : `anonymous-${Math.random().toString(36).substr(2, 9)}`
    ) as AM.PeerId;

    return {
      onOpen(event, ws) {
        console.log(`WebSocket connected: ${peerId}`);
        console.log(`Auth status: ${usr_id ? "authenticated" : "anonymous"}`);
        honoAdapter.addConnection(peerId, ws);
      },
      onMessage(event, ws) {
        console.log(`WebSocket message received from ${peerId}`, {
          dataType: typeof event.data,
          dataLength: (() => {
            if (typeof event.data === "string") return event.data.length;
            if (event.data instanceof ArrayBuffer) return event.data.byteLength;
            if (event.data instanceof Uint8Array) return event.data.length;
            return 0;
          })(),
          constructor: event.data?.constructor?.name,
        });
        let messageData: Uint8Array;
        if (typeof event.data === "string") {
          // Convert string to Uint8Array
          messageData = new TextEncoder().encode(event.data);
          console.log(`Converted string to Uint8Array for ${peerId}`);
        } else if (event.data instanceof ArrayBuffer) {
          // Convert ArrayBuffer to Uint8Array
          messageData = new Uint8Array(event.data);
          console.log(`Converted ArrayBuffer to Uint8Array for ${peerId}`);
        } else if (event.data instanceof Uint8Array) {
          // Already Uint8Array, use directly
          messageData = event.data;
          console.log(`Using Uint8Array directly for ${peerId}`);
        } else {
          console.warn(
            `Unsupported data type from ${peerId}:`,
            typeof event.data,
            event.data?.constructor?.name,
          );
          return;
        }

        honoAdapter.handleMessage(peerId, messageData);
      },
      onClose(event, ws) {
        console.log(`WebSocket disconnected: ${peerId}`);
        honoAdapter.removeConnection(peerId);
      },
      onError(event, ws) {
        console.error(`WebSocket error for ${peerId}:`, event);
        honoAdapter.removeConnection(peerId);
      },
    };
  }),
);

// TODO:
app.get(
  "/portal/time/sync",
  upgradeWebSocket((c) => {
    const { auth } = c.req.query(); // TODO: Verify Bearer token on auth.
    let interval: any;
    return {
      onOpen: (_event, ws) => {
        interval = setInterval(
          () =>
            ws.send(
              JSON.stringify({
                type: "table",
                data: [
                  [{ key: 0, name: "time", type: "int" }],
                  { 0: new Date().getTime() },
                ],
              }),
            ),
          10,
        );
      },
      onMessage: undefined,
      onClose: () => clearInterval(interval),
      onError: undefined,
    };
  }),
);

app.get(
  "/portal/stonks/sync",
  upgradeWebSocket((c) => {
    const { auth } = c.req.query(); // TODO: Verify Bearer token on auth.
    const stonks: Record<string, number> = {
      AAPL: 645.32,
      MSFT: 412.78,
      GOOGL: 823.45,
      AMZN: 567.91,
      NVDA: 789.23,
      META: 345.67,
      TSLA: 892.14,
      BRKB: 234.56,
      JPM: 478.9,
      V: 656.23,
      JNJ: 321.45,
      WMT: 754.89,
      PG: 423.67,
      UNH: 587.12,
      HD: 698.34,
      DIS: 276.45,
      MA: 812.56,
      PYPL: 389.78,
      BAC: 523.91,
      NFLX: 734.23,
      ADBE: 456.78,
      CRM: 621.34,
      PFE: 298.56,
      ABT: 865.23,
      CSCO: 342.67,
      CVX: 778.9,
      PEP: 512.34,
    };
    let interval: any;
    return {
      onOpen: (_event, ws) => {
        interval = setInterval(() => {
          for (const i in stonks) stonks[i] += 0.5 - Math.random();
          ws.send(
            JSON.stringify({
              type: "table",
              data: [
                [
                  { key: 1, name: "price", type: "usd" },
                  { key: 0, name: "ticker", type: "text" },
                ],
                ...Object.entries(stonks),
              ],
            }),
          );
        }, 100);
      },
      onMessage: undefined,
      onClose: () => clearInterval(interval),
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

app.post("/signup", async (c) => {
  const { email } = c.req.query();
  await sendVerificationEmail(email);
  return c.json(null, 200);
});

app.post("/signup/:token{.+}", async (c) => {
  const token = c.req.param("token");
  const { email, password } = await c.req.json();
  if (!token || !email || !password) return c.json(null, 400);
  const [ts, _hash] = token.split(":");
  const epoch = parseInt(ts);
  if (Date.now() / 1000 - epoch > 86400) return c.json(null, 401);
  if (token !== (await createToken(email, new Date(epoch * 1000))))
    return c.json(null, 401);
  await sql`
    insert into usr ${
    sql({
      email,
      password: sql`crypt(${password}, gen_salt('bf', 8))` as unknown as string,
    })
  } 
    on conflict do update set password = excluded.password
  `;
  return c.json(null, 200);
});

app.post("/login", async (c) => {
  const { email, password } = await c.req.json();
  const [usr] = await sql`select usr_id from usr where email = ${email} and crypt(${password}, password)`;
  if (!usr) return c.json(null, 401);
  return c.json(
    { data: { usr_id: usr.usr_id, jwt: await createJwt(usr.usr_id) } },
    200,
  );
});

app.get("/shop", async (c) => {
  const { limit, offset, ...qs } = c.req.query();
  return page(c)(
    await cselect({
      cols: [
        { name: "name", type: "text", key: "name" },
        { name: "price", type: "usd", key: "sell_price" },
        { name: "", type: "create", key: "row_0" },
      ],
      select: sql`select created_at, sell_id, sell_type, sell_price, name, row_0`,
      from: sql`from sheet s`,
      where: [
        sql`sell_price >= 0`,
        sql`sell_type is not null`,
        qs.name && sql`name ilike ${qs.name + "%"}`,
        qs.sell_type && sql`sell_type = ${qs.sell_type}`,
        qs.sell_price &&
        sql`sell_price between ${qs.sell_type.split("-")[0]}::numeric and ${qs.sell_type.split("-")[1]}::numeric`,
      ],
      order: sql`order by name`,
      limit,
      offset,
    }),
  );
});

app.post("/net/:id", async (c) => {
  await sql`insert into net ${
    sql({
      sheet_id: c.req.param("id"),
      body: await c.req.text(),
    })
  }`;
  return c.json(null, 200);
});

app.use("*", jwt({ secret: JWT_SECRET }));

app.use("*", async (c, next) => {
  c.set("usr_id", c.get("jwtPayload")?.sub);
  await next();
});

app.post("/buy/:id", async (c) => {
  const sheet_id = await sql.begin(async (sql) => {
    const [sheet] = await sql`select * from sheet where sell_id = ${
      c.req.param(
        "id",
      )
    } and sell_price >= 0`;
    if (!sheet) throw new HTTPException(404, { message: "Not found." });
    if (!sheet.sell_type)
      throw new HTTPException(400, { message: "Not for sale." });
    const row_0 = sheet.type === "template" ? sheet.row_0.data : {};
    const doc_id = sheet.sell_type.startsWith("codex-")
      ? Math.random().toString().slice(2)
      : automerge.create({ data: row_0 }).documentId;
    const [{ sheet_id }] = await sql`
      with sell as (
        select * from sheet where sell_id = ${
      c.req.param(
        "id",
      )
    } and sell_price >= 0
      ), buy as (
        insert into sheet (created_by, type, doc_id, name, buy_id, buy_price, row_0) 
        select ${
      c.get(
        "usr_id",
      )
    }, sell_type, ${doc_id}, name, sell_id, sell_price, ${row_0}
        from sell
        returning sheet_id, created_by
      ), buy_usr as (
        insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from buy
      )
      select sheet_id from buy
    `;
    if (!sheet_id) throw new HTTPException(500, { message: "Not purchased." });
    // TODO: Charge usr on stripe, etc.
    return sheet_id;
  });
  return c.json({ data: sheet_id }, 201);
});

app.post("/sell/:id", async (c) => {
  const { price } = await c.req.json();
  if (price === undefined)
    throw new HTTPException(400, { message: "Price required." });
  await sql`
    update sheet set sell_price = ${price} 
    where true
      and sheet_id = ${c.req.param("id")} 
      and created_by = ${c.get("usr_id")}
  `;
  return c.json(null, 200);
});

app.get("/library", async (c) => {
  const { limit, offset, ...qs } = c.req.query();
  return page(c)(
    await cselect({
      cols: null,
      select: sql`select s.created_at, s.type, s.doc_id, s.name, s.tags, s.sell_price`,
      from: sql`
        from sheet_usr su 
        inner join sheet s using (sheet_id) 
      `,
      where: [
        sql`su.usr_id = ${c.get("usr_id")}`,
        qs.sheet_id && sql`sheet_id = ${qs.sheet_id}`,
        qs.doc_id && sql`doc_id = ${qs.doc_id}`,
        qs.type && sql`type = ${qs.type}`,
      ],
      order: undefined,
      limit,
      offset,
    }),
  );
});

// TODO: This is not correct.
app.put("/library/:id", async (c) => {
  const [sheet] = await sql`
    update sheet 
    set ${
    sql(
      { name: null, tags: [], ...(await c.req.json()) },
      "name",
      "tags",
    )
  } 
    where true
    and sheet_id = ${c.req.param("id")}
    and created_by = ${c.get("usr_id")}
    returning sheet_id
  `;
  if (!sheet) {
    const [type, doc_id] = c.req.param("id").split(":");
    const sheet = {
      name: "",
      tags: [],
      ...(await c.req.json()),
      type,
      doc_id,
      row_0: await automerge
        .find<{ data: Table }>(doc_id as AnyDocumentId)
        .then((hand) => hand.doc().data[0] ?? {}),
      created_by: c.get("usr_id"),
    };
    await sql`
      with s as (insert into sheet ${
      sql(
        sheet,
        "type",
        "doc_id",
        "name",
        "tags",
        "created_by",
        "row_0",
      )
    } on conflict (sheet_id) do nothing returning *)
      insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from s
    `;
  }
  return c.json(null, 200);
});

// TODO: This currently takes sheet_id, but we want it to take doc_id.
app.get("/net/:id", async (c) => {
  return page(c)(await sheet(c, c.req.param("id"), c.req.query()));
});

app.post("/query", async (c) => {
  return page(c)(await querify(c, await c.req.json(), c.req.query()));
});

app.get("/codex/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const [type, doc_id] = sheet_id.split(":");
  switch (type) {
    case "codex-db": {
      const [db] = await sql`select dsn from db where sheet_id = ${sheet_id}`;
      if (!db) {
        throw new HTTPException(400, {
          message: `No DSN found.`,
        });
      }
      // TODO: Be really careful about arbitrary DB access! Don't let them access our DB haha
      const sql_ = pg(db.dsn, {
        onnotice: (msg) => msg.severity !== "DEBUG" && console.log(msg),
      });
      const rows = await sql_`
        select 
          table_name as name,
          '[[{"name":"name","type":"text","key":"column_name"},{"name":"type","type":"text","key":"data_type"},{"name":"key","type":"int","key":"ordinal_position"}]]'::jsonb || jsonb_agg(t)::jsonb as columns
        from information_schema.tables t
        inner join information_schema.columns c using (table_catalog,table_schema,table_name)
        where table_schema = 'public'
        group by table_name, table_type
      `;
      const cols = rows.columns.map((col, i) => ({
        name: col.name,
        type: "text",
        key: col.name,
      })); // TODO:
      await sql_.end();
      return c.json({ data: [cols, ...rows] }, 200);
    }
    case "codex-scrapsheets": {
      return c.json(
        {
          data: [
            [
              { name: "name", type: "text", key: "name" },
              { name: "columns", type: "table", key: "columns" },
            ],
            {
              name: "shop",
              columns: [
                [
                  { name: "name", type: "text", key: 0 },
                  { name: "type", type: "text", key: 1 },
                  { name: "key", type: "int", key: 2 },
                ],
                ["created_at", "text", 0],
                ["sell_id", "text", 1],
                ["sell_type", "text", 2],
                ["sell_price", "text", 3],
                ["name", "text", 4],
              ],
            },
            {
              name: "library",
              columns: [
                [
                  { name: "name", type: "text", key: 0 },
                  { name: "type", type: "text", key: 1 },
                  { name: "key", type: "int", key: 2 },
                ],
                ["s.created_at", "text", 0],
                ["s.type", "text", 1],
                ["s.doc_id", "text", 2],
                ["s.name", "text", 3],
                ["s.tags", "text", 4],
                ["s.sell_price", "text", 5],
              ],
            },
          ],
        },
        200,
      );
    }
    default:
      throw new HTTPException(400, {
        message: `Unrecognized codex type: ${type}`,
      });
  }
});

app.post("/codex-db/:id", async (c) => {
  const sheet_id = `codex-db:${c.req.param("id")}`;
  await sql`
    insert into db (sheet_id, dsn) 
    select ${sheet_id}, ${await c.req.json()}
    where exists (select true from sheet_usr su where (su.sheet_id,su.usr_id) = (${sheet_id},${
    c.get(
      "usr_id",
    )
  }))
    on conflict (sheet_id) do update set dsn = excluded.dsn
  `;
  return c.json(null, 200);
});

app.get("/codex/:id/connect", async (c) => {
  // TODO:
  return c.json(null, 500);
});

app.get("/codex/:id/callback", async (c) => {
  // TODO:
  return c.json(null, 500);
});

app.get("/portal/:id", async (c) => {
  const [sheet_] = await sql`
    select s_.*
    from sheet_usr su
    inner join sheet s using (sheet_id)
    inner join sheet s_ on s_.sell_id = s.buy_id
    where true
      and su.usr_id = ${c.get("usr_id")} 
      and su.sheet_id = ${"portal:" + c.req.param("id")}
  `;
  if (!sheet_) throw new HTTPException(404, { message: "Not found." });
  return page(c)(await sheet(c, sheet_.sheet_id, c.req.query()));
});

app.get("/stats/:id", async (c) => {
  // TODO: Get column/table stats about any sheet as a table.
  return c.json(null, 500);
});

app.all("/mcp/:id", async (c) => {
  // TODO: mcp server
  return c.json(null, 500);
});

export default app;
