import { HTTPException } from "@hono/hono/http-exception";
import { Context, Hono } from "@hono/hono";
import { logger } from "@hono/hono/logger";
import { cors } from "@hono/hono/cors";
import { jwt, sign, verify } from "@hono/hono/jwt";
import type { JwtVariables } from "@hono/hono/jwt";
import pg from "postgresjs";
import { upgradeWebSocket } from "@hono/hono/deno";
import { Repo } from "@automerge/automerge-repo";
import * as Automerge from "@automerge/automerge";
import * as AM from "@automerge/automerge-repo";
import type { AnyDocumentId } from "@automerge/automerge-repo";
import { NodeWSServerAdapter } from "@automerge/automerge-repo-network-websocket";
import { NodeFSStorageAdapter } from "@automerge/automerge-repo-storage-nodefs";
import ala from "alasql";
import * as prql from "prql-js";
import * as path from "@std/path";
import examplesSql from "./examples.sql" with { type: "text" };
import { DATASETS } from "./src/examples.mjs";
import { checkRefPath, checkResultColumns, formatQueryError, nearest, register, scanRefs } from "./src/sql.mjs";
import Stripe from "stripe";

// --- secrets & crypto

const JWT_SECRET = Deno.env.get("JWT_SECRET") ?? Math.random().toString();
const JWT_ALG = "HS256";
const TOKEN_SECRET = Deno.env.get("TOKEN_SECRET") ?? Math.random().toString();
const DSN_KEY = Deno.env.get("DSN_ENCRYPTION_KEY") ?? Math.random().toString();

const b64 = (buf: Uint8Array): string => {
  let s = "";
  for (const byte of buf) s += String.fromCharCode(byte);
  return btoa(s);
};
const unb64 = (s: string): Uint8Array<ArrayBuffer> => {
  const bin = atob(s);
  const buf = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) buf[i] = bin.charCodeAt(i);
  return buf;
};

const dsnAesKey = crypto.subtle
  .digest("SHA-256", new TextEncoder().encode(DSN_KEY))
  .then((bits) => crypto.subtle.importKey("raw", bits, "AES-GCM", false, ["encrypt", "decrypt"]));

const encryptDsn = async (plain: string): Promise<string> => {
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const cipher = new Uint8Array(
    await crypto.subtle.encrypt({ name: "AES-GCM", iv }, await dsnAesKey, new TextEncoder().encode(plain)),
  );
  const buf = new Uint8Array(iv.length + cipher.length);
  buf.set(iv);
  buf.set(cipher, iv.length);
  return b64(buf);
};

const decryptDsn = async (stored: string): Promise<string> => {
  try {
    const buf = unb64(stored);
    const plain = await crypto.subtle.decrypt(
      { name: "AES-GCM", iv: buf.slice(0, 12) },
      await dsnAesKey,
      buf.slice(12),
    );
    return new TextDecoder().decode(plain);
  } catch {
    throw new HTTPException(500, {
      message: "Could not decrypt DSN. Was DSN_ENCRYPTION_KEY changed? Re-save the connection string.",
    });
  }
};

const PBKDF2_ITERS = 210_000;
const hashPassword = async (password: string): Promise<string> => {
  const salt = crypto.getRandomValues(new Uint8Array(16));
  const key = await crypto.subtle.importKey("raw", new TextEncoder().encode(password), "PBKDF2", false, ["deriveBits"]);
  const bits = new Uint8Array(
    await crypto.subtle.deriveBits({ name: "PBKDF2", hash: "SHA-256", salt, iterations: PBKDF2_ITERS }, key, 256),
  );
  return `pbkdf2$${PBKDF2_ITERS}$${b64(salt)}$${b64(bits)}`;
};
const verifyPassword = async (password: string, stored: string | null): Promise<boolean> => {
  if (!stored) return false;
  const [scheme, iters, salt, want] = stored.split("$");
  if (scheme !== "pbkdf2") return false;
  const key = await crypto.subtle.importKey("raw", new TextEncoder().encode(password), "PBKDF2", false, ["deriveBits"]);
  const bits = new Uint8Array(
    await crypto.subtle.deriveBits(
      { name: "PBKDF2", hash: "SHA-256", salt: unb64(salt), iterations: parseInt(iters) },
      key,
      256,
    ),
  );
  return b64(bits) === want;
};
if (!Deno.env.get("JWT_SECRET"))
  console.warn("WARNING: JWT_SECRET not set, using random value. Tokens will break on restart.");
if (!Deno.env.get("TOKEN_SECRET"))
  console.warn("WARNING: TOKEN_SECRET not set, using random value. Tokens will break on restart.");
if (!Deno.env.get("DSN_ENCRYPTION_KEY"))
  console.warn("WARNING: DSN_ENCRYPTION_KEY not set, using random value. Encrypted DSNs will break on restart.");

// Simple in-memory rate limiter (token bucket algorithm)
const rateLimitBuckets = new Map<string, { tokens: number; lastRefill: number }>();
const RATE_LIMIT_MAX_TOKENS = 1000; // Max burst per IP (debounced queries fire per keystroke)
const RATE_LIMIT_REFILL_RATE = 100; // Tokens per second
const RATE_LIMIT_WINDOW_MS = 60_000; // Cleanup old entries after 1 minute of inactivity

const rateLimit = (identifier: string): boolean => {
  const now = Date.now();
  let bucket = rateLimitBuckets.get(identifier);

  if (!bucket) {
    bucket = { tokens: RATE_LIMIT_MAX_TOKENS, lastRefill: now };
    rateLimitBuckets.set(identifier, bucket);
  }

  // Refill tokens based on time elapsed
  const elapsed = (now - bucket.lastRefill) / 1000;
  bucket.tokens = Math.min(RATE_LIMIT_MAX_TOKENS, bucket.tokens + elapsed * RATE_LIMIT_REFILL_RATE);
  bucket.lastRefill = now;

  if (bucket.tokens < 1)
    return false; // Rate limited

  bucket.tokens -= 1;
  return true;
};

// Cleanup stale rate limit entries periodically
setInterval(() => {
  const now = Date.now();
  for (const [key, bucket] of rateLimitBuckets) {
    if (now - bucket.lastRefill > RATE_LIMIT_WINDOW_MS)
      rateLimitBuckets.delete(key);
  }
}, RATE_LIMIT_WINDOW_MS);

ala.options.modifier = "RECORDSET";
register(ala);

// The page resolves @sheet refs the same way (src/index.html), so both engines
// run byte-identical SQL. Rows travel in params[0] rather than a module-level
// cache: the server runs concurrent requests, and params are per-call.
ala.from.SHEET = (
  id: unknown,
  _opts: unknown,
  cb: unknown,
  idx: unknown,
  query: unknown,
) => {
  const loaded = (query as { params?: Record<string, Row[]>[] })?.params?.[0] ?? {};
  const rows = loaded[id as string];
  if (!rows) {
    const hit = nearest(String(id), Object.keys(loaded));
    throw new HTTPException(400, {
      message: [
        `I could not load the sheet "@${id}".`,
        ``,
        hit
          ? `  Did you mean: @${hit}`
          : `  Loaded:       ${Object.keys(loaded).join(", ") || "(no @sheet is referenced)"}`,
        `  Source:       the @sheet refs in this query`,
        `  Fix:          ${
          hit ? `write @${hit} instead` : "reference the sheet as @type:doc_id so it loads before the query runs"
        }`,
      ].join("\n"),
    });
  }
  return typeof cb === "function" ? cb(rows, idx, query) : rows;
};

export type Tag<T extends string, X extends Row[]> = {
  type: T;
  data: X;
};

// --- types

export type Type =
  // TODO: Consider using JSONSchema?
  | "type"
  | "text"
  | "create"
  | "usd"
  | "int"
  | "num"
  | "bool"
  | "float"
  | "percentage"
  | "json"
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
export type NetHttp = { url: string; interval: number; headers?: string };
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

// --- sheet & query core

// Direct access, or derived access through a purchase (the buyer's sheet
// points back at the seller's via buy_id = sell_id).
const assertSheetAccess = async (c: Context, sheet_id: string): Promise<void> => {
  const [access] = await sql`
    select true from sheet_usr su
    where su.usr_id = ${c.get("usr_id")}
      and (
        su.sheet_id = ${sheet_id}
        or exists (
          select 1 from sheet b
          inner join sheet s on s.sell_id = b.buy_id
          where b.sheet_id = su.sheet_id and s.sheet_id = ${sheet_id}
        )
      )
    union all
    select true from sheet where sheet_id = ${sheet_id} and public
  `;
  if (!access) {
    throw new HTTPException(403, {
      message: `Expected read access to ${sheet_id} for usr ${
        c.get("usr_id")
      }, received none. Source: sheet_usr membership, a purchase of the listing, or sheet.public. Ask an owner to share it with you, or have them make it public.`,
    });
  }
};

// Owner-only actions (sharing, publishing) check this rather than created_by, so
// ownership can be transferred later without touching every call site.
const assertSheetOwner = async (c: Context, sheet_id: string): Promise<void> => {
  const [owner] = await sql`
    select true from sheet_usr su
    inner join sheet s using (sheet_id)
    where su.sheet_id = ${sheet_id}
      and su.usr_id = ${c.get("usr_id")}
      and (su.role = 'owner' or s.created_by = ${c.get("usr_id")})
  `;
  if (!owner) {
    throw new HTTPException(403, {
      message: `Expected usr ${
        c.get("usr_id")
      } to own ${sheet_id}, received a non-owner. Source: sheet_usr.role and sheet.created_by. Only an owner can change sharing; ask one to make the change or grant you the owner role.`,
    });
  }
};

const sheet = async (
  c: Context,
  sheet_id: string,
  { limit, offset, ...qs }: Record<string, string>,
  // The @sheet refs already being resolved above this call, so a query sheet
  // that reaches itself is reported as a cycle instead of blowing the stack.
  path_: string[] = [],
): Promise<Page> => {
  const [type, doc_id] = sheet_id.split(":");
  await assertSheetAccess(c, sheet_id);
  const hand = await automerge
    .find<{ data: Sheet["data"] }>(doc_id as AnyDocumentId)
    .catch(() => ({
      doc: () => {
        throw new HTTPException(404, {
          message:
            `Expected an automerge document for sheet ${sheet_id}, received none. Source: doc_id ${doc_id}. The sheet row exists but its document is missing or unreadable; re-create the sheet, or claim it again with PUT /library/${sheet_id}.`,
        });
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
        // Appended after body, so existing `select body from @net-hook:x` queries
        // and existing column positions are untouched.
        select: sql`select n.created_at, n.body, n.method, n.req_headers, n.query_params`,
        from: sql`from sheet_usr su inner join net n using (sheet_id)`,
        where: [
          sql`(su.sheet_id,su.usr_id) = (${sheet_id},${c.get("usr_id")})`,
        ],
        // Without an order the heap decides, so paging a log repeats and skips rows.
        order: sql`order by n.created_at desc, n.net_id desc`,
        limit,
        offset,
      });
    case "query":
      return await querify(c, hand.doc().data[0] as Query, {
        limit,
        offset,
        ...qs,
      }, path_);
    case "template":
      throw new HTTPException(400, {
        message:
          `Expected a readable sheet, received the template ${sheet_id}. A template is a listing, not a sheet with rows. Buy it with POST /buy/:sell_id and read the copy you get back.`,
      });
    case "portal":
      throw new HTTPException(400, {
        message:
          `Expected a readable sheet, received the portal ${sheet_id}. A portal has no stored rows. Read it live over GET /portal/${doc_id}/sync, or through GET /portal/:id if you bought it.`,
      });
    default:
      throw new HTTPException(400, {
        message: `Expected sheet type table, net-hook, net-http, net-socket, or query, received ${
          JSON.stringify(type)
        } from sheet id ${sheet_id}. Fix the type prefix on the id.`,
      });
  }
};

const executeSql = async (
  c: Context,
  sqlCode: string,
  path_: string[] = [],
): Promise<Page> => {
  // scanRefs is shared with the page, so @type:doc_id resolves identically here.
  const { sql: code_, ids: sheet_ids } = scanRefs(sqlCode);

  // Load referenced sheets, remembering source column types by name.
  const docs: Record<string, Record<string, unknown>[]> = {};
  const nameToType: Record<string, Type> = {};
  for (const sheet_id of sheet_ids) {
    if (docs[sheet_id]) continue;
    try {
      checkRefPath(path_, sheet_id);
    } catch (err) {
      throw new HTTPException(400, { message: err instanceof Error ? err.message : String(err) });
    }
    const [cols, ...rows] = (await sheet(c, sheet_id, {}, [...path_, sheet_id]).catch(async (err) => {
      // A mistyped @ref reads as "no access". Name the sheet the author meant.
      const mine: { sheet_id: string }[] = await sql`
        select sheet_id from sheet_usr where usr_id = ${c.get("usr_id")}
      `;
      const hit = nearest(sheet_id, mine.map((r) => r.sheet_id));
      if (!hit) throw err;
      throw new HTTPException(400, {
        message: [
          `I could not load the sheet "@${sheet_id}".`,
          ``,
          `  Did you mean: @${hit}`,
          `  Source:       the @sheet refs in this query`,
          `  Fix:          write @${hit} instead`,
        ].join("\n"),
      });
    })).data;
    for (const col of Object.values(cols)) nameToType[col.name] = col.type;
    docs[sheet_id] = rows.map((row) =>
      Object.fromEntries(
        Object.values(cols).map((col) => [col.name, row[col.key]]),
      )
    );
  }

  let result: { columns: { columnid: string }[]; data: Record<string, unknown>[] };
  try {
    result = await ala(code_, [docs]);
  } catch (err) {
    // An AlaSQL parse error used to surface as a generic 500. Say where it is.
    if (err instanceof HTTPException) throw err;
    throw new HTTPException(400, { message: formatQueryError(err, sqlCode) });
  }
  const { columns: cols, data: rows } = result;
  try {
    checkResultColumns(cols, rows, Object.keys(nameToType), sqlCode);
  } catch (err) {
    throw new HTTPException(400, { message: err instanceof Error ? err.message : String(err) });
  }

  return {
    data: [
      Object.fromEntries(
        cols.map((col, i) => [
          i,
          {
            name: col.columnid,
            type: nameToType[col.columnid] ?? "text",
            key: col.columnid,
          },
        ]),
      ),
      ...rows,
    ],
    count: rows.length,
    offset: 0,
  };
};

const querify = async (
  c: Context,
  { lang, code, args: _args = [] }: Query,
  _reqQuery: Record<string, string>,
  path_: string[] = [],
): Promise<Page> => {
  if (lang === "sql")
    return await executeSql(c, code, path_);
  else if (lang === "prql") {
    // Compile PRQL to SQL, then execute
    try {
      const sqlResult = prql.compile(code);
      if (!sqlResult)
        throw new HTTPException(400, { message: "PRQL compilation returned empty result" });
      return await executeSql(c, sqlResult, path_);
    } catch (err) {
      if (err instanceof HTTPException) throw err;
      const message = err instanceof Error ? err.message : "PRQL compilation failed";
      throw new HTTPException(400, { message: `PRQL error: ${message}` });
    }
  } else {
    throw new HTTPException(400, { message: `Unsupported query language: ${lang}` });
  }
};

export const createJwt = async (usr_id: string) =>
  await sign(
    {
      sub: usr_id,
      exp: Math.floor(Date.now() / 1000) + 60 * 60 * 24 * 7,
    },
    JWT_SECRET,
    JWT_ALG,
  );

export const createToken = async (
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
  const key = Deno.env.get(`RESEND_API_KEY`);
  if (!key) return;
  const token = await createToken(email);
  const res = await fetch("https://api.resend.com/emails", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${key}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      to: email,
      from: "hello@sheets.scrap.land",
      subject: "Verify your email",
      text: `` +
        `Welcome to scrap.land` +
        `\n\n` +
        `Please verify your email: ` +
        `https://sheets.scrap.land/password` +
        `?email=${encodeURIComponent(email)}` +
        `&token=${encodeURIComponent(token)}`,
    }),
  });
  if (!res.ok) {
    console.log(`/password?email=${email}&token=${token}`);
    console.error(
      `Could not send verification email to ${email}:`,
      res.status,
      await res.text(),
    );
  }
};

// --- database

export const sql = pg(
  Deno.env.get("DATABASE_URL") ??
    "postgresql://postgres@127.0.0.1:5434/postgres",
  {
    // The default (test) gateway is a single PGlite session, so concurrent
    // queries must serialize onto one connection.
    max: Deno.env.get("DATABASE_URL") ? 10 : 1,
    fetch_types: true,
    onnotice: (msg: { severity?: string }) => msg.severity !== "DEBUG" && console.log(msg),
  },
);
type Sql = typeof sql;

// deno-lint-ignore no-explicit-any
type SqlFragment = any;

const pgType = (oid: number): Type =>
  [1700, 790].includes(oid)
    ? "usd" // numeric, money
    : [23, 20, 21, 700, 701].includes(oid)
    ? "int" // int2, int4, int8, float4, float8
    : [114, 3802].includes(oid)
    ? "json" // json, jsonb
    : "text";

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
  select: SqlFragment;
  from: SqlFragment;
  where: SqlFragment[];
  order?: SqlFragment;
  limit: string;
  offset: string;
}): Promise<Page> => {
  const where_ = where.filter((x) => x).length
    ? sql`where true ${where.filter((x) => x).map((x: SqlFragment) => sql`and ${x}`)}`
    : sql``;
  const rows = await sql`${select} ${from} ${where_} ${order ?? sql``} limit ${limit} offset ${offset}`;
  const cols_: Row<Col> = arrayify(
    rows.columns.map((col: { name: string; type: number }) => ({
      name: col.name,
      type: pgType(col.type),
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

// --- app & middleware

export const app = new Hono<{
  Variables: JwtVariables & { usr_id: string };
}>();

app.use("*", logger());

// Rate limiting middleware
app.use("*", async (c, next) => {
  // Use IP address as identifier, fall back to a default for local dev
  const ip = c.req.header("x-forwarded-for")?.split(",")[0]?.trim() ||
    c.req.header("x-real-ip") ||
    "127.0.0.1";

  if (!rateLimit(ip))
    throw new HTTPException(429, { message: "Too many requests. Please slow down." });

  await next();
});

// --- seeding

// Idempotent: examples.sql and every dataset upsert on doc_id.
export const seed = async () => {
  // schema/db.sql is schema only, so pg-schema-diff can diff it. This sentinel
  // user owns every seeded sheet and must exist before examples.sql runs.
  await sql`insert into usr (name, email) values ('Scrapsheets', '') on conflict (email) do nothing`;
  await sql.unsafe(examplesSql);
  for (
    const { doc_id, name, tags, doc } of [
      ...DATASETS,
      { doc_id: "webhook-inbox", name: "webhook inbox", tags: ["example", "net"], doc: { type: "net-hook", data: [] } },
    ]
  ) {
    await sql`
      insert into sheet (sell_price, created_by, type, name, tags, doc_id, row_0)
      values (0, (select usr_id from usr where email = ''), 'template', ${name}, ${tags},
              ${"dataset-" + doc_id}, ${sql.json({ name, ...doc })})
      on conflict (doc_id) do update set name = excluded.name, tags = excluded.tags, row_0 = excluded.row_0
    `;
  }
};

let seeded: Promise<unknown> | undefined;
app.use("*", async (_c, next) => {
  await (seeded ??= seed().catch((err) => {
    seeded = undefined;
    throw new Error(`seed() failed applying examples.sql/DATASETS: ${err instanceof Error ? err.message : err}`);
  }));
  await next();
});

// --- automerge sync

// The official NodeWSServerAdapter drives node-ws-shaped sockets; these shims
// make Hono/Deno WebSockets fit that shape.
type WsEventHandler = (...args: unknown[]) => void;

class WsSocketShim {
  isAlive = true;
  private handlers: Record<string, WsEventHandler[]> = {};
  // deno-lint-ignore no-explicit-any
  constructor(private ws: any) {}
  on(event: string, handler: WsEventHandler): void {
    (this.handlers[event] ??= []).push(handler);
  }
  emit(event: string, ...args: unknown[]): void {
    for (const handler of this.handlers[event] ?? []) handler(...args);
  }
  send(data: ArrayBuffer | Uint8Array): void {
    this.ws.send(data);
  }
  close(): void {
    this.ws.close();
  }
  terminate(): void {
    this.ws.close();
  }
  ping(): void {
    // Deno reaps dead sockets itself; answer the adapter's keepalive locally.
    this.emit("pong");
  }
  get readyState(): number {
    return this.ws.readyState;
  }
}

const wss = {
  clients: new Set<WsSocketShim>(),
  handlers: {} as Record<string, WsEventHandler[]>,
  on(event: string, handler: WsEventHandler): void {
    (this.handlers[event] ??= []).push(handler);
  },
  emit(event: string, ...args: unknown[]): void {
    const handlers = this.handlers[event] ?? [];
    if (event === "connection" && !handlers.length)
      throw new Error("A WebSocket connected before the automerge adapter subscribed to the server.");
    for (const handler of handlers) handler(...args);
  },
};

// deno-lint-ignore no-explicit-any
const wsAdapter = new NodeWSServerAdapter(wss as any);

// senderId (client-chosen peer ID from the join message) -> authenticated usr + owning socket
const peerUserMap = new Map<AM.PeerId, { auth: WsAuth; shim: WsSocketShim }>();

// Per-document access for sync. sharePolicy only governs proactive announcements
// in automerge-repo; explicit requests are enforced in the /library/sync message
// path below, so both call this.
export type Role = "owner" | "editor" | "viewer";

const syncAccessCache = new Map<string, { role: Role | null; expires: number }>();

// PUT /library/:id must fetch a client-created doc over sync BEFORE its sheet
// row exists; grantSync bridges that window (and clears any cached denial).
const invalidateSync = (documentId: string): void => {
  for (const key of syncAccessCache.keys()) if (key.endsWith(`/${documentId}`)) syncAccessCache.delete(key);
};

const pendingSync = new Map<string, number>();
const grantSync = (usr_id: string, documentId: string): void => {
  pendingSync.set(`${usr_id}/${documentId}`, Date.now() + 30_000);
  invalidateSync(documentId);
};

// The role a peer holds on a document, or null for no access. Membership,
// purchase-derived access and public sheets all resolve here, so sync and HTTP
// stop disagreeing about who may read what.
const syncRole = async (auth: WsAuth, documentId: string): Promise<Role | null> => {
  const { usr_id, share } = auth;
  if (usr_id && (pendingSync.get(`${usr_id}/${documentId}`) ?? 0) > Date.now()) return "owner";
  const key = `${usr_id ?? "-"}:${share ?? "-"}/${documentId}`;
  const hit = syncAccessCache.get(key);
  if (hit && hit.expires > Date.now()) return hit.role;
  if (syncAccessCache.size > 10_000) syncAccessCache.clear();
  const [access] = await sql`
    select coalesce(
      (select su.role from sheet_usr su inner join sheet s using (sheet_id)
        where s.doc_id = ${documentId} and su.usr_id = ${usr_id}),
      -- A purchased sheet reads the seller's original, but never edits it.
      (select 'viewer' from sheet_usr su
         inner join sheet b on b.sheet_id = su.sheet_id
         inner join sheet s on s.sell_id = b.buy_id
        where su.usr_id = ${usr_id} and s.doc_id = ${documentId}),
      (select 'viewer' from sheet s where s.doc_id = ${documentId} and s.public),
      (select 'viewer' from sheet s where s.doc_id = ${documentId} and s.sheet_id = ${share})
    ) as role
  `;
  const role = (access?.role ?? null) as Role | null;
  syncAccessCache.set(key, { role, expires: Date.now() + 30_000 });
  return role;
};

// True when a sync frame would actually mutate the document. Read-only sync
// traffic (heads, "have you got this?") carries no changes and must keep
// flowing, or a viewer could never load the sheet at all.
const carriesChanges = (msg: { type?: string; data?: unknown }): boolean => {
  if (msg.type !== "sync") return false;
  const bytes = msg.data instanceof Uint8Array ? msg.data : undefined;
  if (!bytes) return false;
  try {
    return Automerge.decodeSyncMessage(bytes).changes.length > 0;
  } catch {
    // An unreadable payload from a viewer is not something to forward on faith.
    return true;
  }
};

const peerCanSync = async (peerId: AM.PeerId, documentId?: AM.DocumentId): Promise<boolean> => {
  const auth = peerUserMap.get(peerId)?.auth;
  return documentId && auth ? (await syncRole(auth, documentId)) !== null : false;
};

const SERVER_PEER_ID = `server-${Deno.hostname()}` as AM.PeerId;

export const automerge = new Repo({
  network: [wsAdapter],
  storage: new NodeFSStorageAdapter(`${path.dirname(path.fromFileUrl(Deno.mainModule))}/data/automerge`),
  peerId: SERVER_PEER_ID,
  // access gates the synchronizer's outbound sharing too, not just announcements.
  shareConfig: { announce: peerCanSync, access: peerCanSync },
});

app.get(
  "/library/sync",
  upgradeWebSocket(async (c) => {
    const auth = await verifyWsAuth(c.req.query().auth);
    let shim: WsSocketShim | undefined;
    let joined: AM.PeerId | undefined;
    let queue = Promise.resolve();
    const cleanup = () => {
      if (!shim) return;
      wss.clients.delete(shim);
      if (joined && peerUserMap.get(joined)?.shim === shim) peerUserMap.delete(joined);
      shim.emit("close");
      shim = undefined;
    };
    return {
      onOpen(_event, ws) {
        if (ws.raw) ws.raw.binaryType = "arraybuffer";
        shim = new WsSocketShim(ws);
        wss.clients.add(shim);
        wss.emit("connection", shim);
      },
      onMessage(event) {
        if (!shim) throw new Error("Sync frame arrived with no open socket (already closed or never opened).");
        const raw = event.data;
        const data = typeof raw === "string"
          ? new TextEncoder().encode(raw)
          : raw instanceof Uint8Array
          ? raw
          : raw instanceof ArrayBuffer
          ? new Uint8Array(raw)
          : null;
        if (!data) throw new Error(`Unsupported WebSocket frame: ${raw?.constructor?.name}`);
        // Messages queue so the async access check cannot reorder the sync protocol.
        queue = queue.then(async () => {
          if (!shim) return;
          let msg: { type?: string; senderId?: string; documentId?: string } | undefined;
          try {
            msg = AM.cbor.decode(data) as typeof msg;
          } catch {
            // Not CBOR; the adapter closes invalid connections itself.
            shim.emit("message", data);
            return;
          }
          if (!joined && msg?.type === "join" && msg.senderId) {
            joined = msg.senderId as AM.PeerId;
            peerUserMap.set(joined, { auth, shim });
          }
          if (msg?.documentId) {
            const role = await syncRole(auth, msg.documentId);
            if (!shim) return;
            if (role === null) {
              shim.send(AM.cbor.encode({
                type: "doc-unavailable",
                senderId: SERVER_PEER_ID,
                targetId: msg.senderId,
                documentId: msg.documentId,
              }));
              return;
            }
            // shareConfig gates reads. A viewer may still *send*, so the write is
            // refused here, by looking at whether the payload carries changes.
            if (role === "viewer" && carriesChanges(msg)) {
              shim.send(AM.cbor.encode({
                type: "error",
                senderId: SERVER_PEER_ID,
                targetId: msg.senderId,
                documentId: msg.documentId,
                message:
                  `You have viewer access to this sheet, so your edit was not saved. Ask an owner for editor access.`,
              }));
              return;
            }
          }
          if (shim) shim.emit("message", data);
        }).catch((err) => {
          // A dropped sync message wedges the stateful protocol; close so the
          // client reconnects with fresh state instead of waiting forever.
          console.error(`/library/sync (usr ${auth.usr_id}):`, err);
          shim?.close();
        });
      },
      onClose: cleanup,
      onError: cleanup,
    };
  }),
);

// Helper to verify WebSocket auth tokens
// A share link is a JWT claiming one sheet at viewer level, so the read path is
// the ordinary sync socket rather than a second, parallel way in.
export type WsAuth = { usr_id: string | null; share: string | null };

const verifyWsAuth = async (auth: string | undefined): Promise<WsAuth> => {
  if (!auth) return { usr_id: null, share: null };
  try {
    const token = auth.startsWith("Bearer ") ? auth.slice(7) : auth;
    const payload = await verify(token, JWT_SECRET, JWT_ALG);
    return { usr_id: (payload.sub as string) ?? null, share: (payload.share as string) ?? null };
  } catch {
    return { usr_id: null, share: null };
  }
};

// --- live portals

// deno-lint-ignore no-explicit-any
const portal = (name: string, ms: number, init: () => any, tick: (s: any) => { cols: any[]; rows: any[] }) =>
  app.get(
    `/portal/${name}/sync`,
    upgradeWebSocket(async (c) => {
      const { auth } = c.req.query();
      await verifyWsAuth(auth);
      const state = init();
      let interval: ReturnType<typeof setInterval> | undefined;
      return {
        onOpen: (_event, ws) => {
          interval = setInterval(() => {
            const { cols, rows } = tick(state);
            ws.send(JSON.stringify({ type: "table", data: [cols, ...rows] }));
          }, ms);
        },
        onClose: () => clearInterval(interval),
      };
    }),
  );

portal("time", 10, () => null, () => ({
  cols: [{ key: 0, name: "time", type: "int" }],
  rows: [{ 0: new Date().getTime() }],
}));

portal("stonks", 100, () => ({
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
}), (stonks) => {
  for (const i in stonks) stonks[i] += 0.5 - Math.random();
  return {
    cols: [{ key: 1, name: "price", type: "usd" }, { key: 0, name: "ticker", type: "text" }],
    rows: Object.entries(stonks) as unknown as Row[],
  };
});

portal("dice", 500, () => {
  const dice: Record<string, number> = { d4: 4, d6: 6, d8: 8, d10: 10, d12: 12, d20: 20, d100: 100, coin: 2 };
  const state: Record<string, { roll: number; total: number; rolls: number }> = {};
  for (const d in dice) state[d] = { roll: 0, total: 0, rolls: 0 };
  return { dice, state };
}, ({ dice, state }) => {
  for (const d in dice) {
    state[d].roll = Math.ceil(Math.random() * dice[d]);
    state[d].total += state[d].roll;
    state[d].rolls++;
  }
  return {
    cols: [
      { key: 0, name: "die", type: "text" },
      { key: 1, name: "roll", type: "int" },
      { key: 2, name: "total", type: "int" },
      { key: 3, name: "rolls", type: "int" },
      { key: 4, name: "average", type: "percentage" },
    ],
    rows: (Object.entries(state) as [string, { roll: number; total: number; rolls: number }][])
      .sort((a, b) => b[1].roll - a[1].roll)
      .map(([name, s]) => ({ 0: name, 1: s.roll, 2: s.total, 3: s.rolls, 4: s.total / s.rolls / dice[name] })),
  };
});

portal("orbit", 100, () => ({
  planets: [
    ["Mercury", 40, 4],
    ["Venus", 55, 7],
    ["Earth", 75, 12],
    ["Mars", 95, 20],
    ["Jupiter", 130, 50],
    ["Saturn", 170, 80],
    ["Uranus", 210, 140],
    ["Neptune", 260, 250],
  ] as [string, number, number][],
  seasons: ["spring", "summer", "autumn", "winter"],
}), ({ planets, seasons }) => {
  const now = Date.now();
  return {
    cols: [
      { key: 0, name: "planet", type: "text" },
      { key: 1, name: "distance", type: "int" },
      { key: 2, name: "x", type: "int" },
      { key: 3, name: "y", type: "int" },
      { key: 4, name: "year", type: "percentage" },
      { key: 5, name: "season", type: "text" },
    ],
    rows: planets.map(([name, dist, period]: [string, number, number]) => {
      const angle = (now / (period * 1000)) * 2 * Math.PI;
      const pct = (angle % (2 * Math.PI)) / (2 * Math.PI);
      return {
        0: name,
        1: dist,
        2: Math.round(dist * Math.cos(angle)),
        3: Math.round(dist * Math.sin(angle)),
        4: pct,
        5: seasons[Math.floor(pct * 4) % 4],
      };
    }),
  };
});

portal("cafe", 500, () => ({
  names: ["Ada", "Grace", "Alan", "Linus", "Matz", "Guido", "Bjarne", "Haskell", "Elm", "Rust"],
  drinks: [
    ["espresso", 3.5],
    ["latte", 5.0],
    ["cappuccino", 4.5],
    ["cortado", 4.0],
    ["cold brew", 4.5],
    ["matcha", 5.5],
    ["chai", 4.0],
    ["americano", 3.0],
  ] as [string, number][],
  orders: [] as { customer: string; drink: string; price: number; wait: number; status: string; _t: number }[],
  tick: 0,
}), (s) => {
  s.tick++;
  for (const o of s.orders) o.wait = s.tick - o._t;
  if (Math.random() < 0.3) {
    const b = s.orders.find((o: { status: string }) => o.status === "ordered");
    if (b) b.status = "brewing";
  }
  if (Math.random() < 0.3) {
    const r = s.orders.find((o: { status: string }) => o.status === "brewing");
    if (r) r.status = "ready";
  }
  for (let i = s.orders.length - 1; i >= 0; i--)
    if (s.orders[i].status === "ready" && s.orders[i].wait > 6) s.orders.splice(i, 1);
  if (Math.random() < 0.4 && s.orders.length < 8) {
    const [drink, price] = s.drinks[Math.floor(Math.random() * s.drinks.length)];
    s.orders.push({
      customer: s.names[Math.floor(Math.random() * s.names.length)],
      drink,
      price,
      wait: 0,
      status: "ordered",
      _t: s.tick,
    });
  }
  const rank: Record<string, number> = { ordered: 0, brewing: 1, ready: 2 };
  const sorted = [...s.orders].sort((a, b) => (rank[a.status] ?? 3) - (rank[b.status] ?? 3));
  return {
    cols: [
      { key: 0, name: "customer", type: "text" },
      { key: 1, name: "drink", type: "text" },
      { key: 2, name: "price", type: "usd" },
      { key: 3, name: "wait", type: "int" },
      { key: 4, name: "status", type: "text" },
    ],
    rows: sorted.map((o) => ({ 0: o.customer, 1: o.drink, 2: o.price, 3: o.wait, 4: o.status })),
  };
});

portal(
  "forest",
  1000,
  () =>
    ["oak", "pine", "maple", "birch", "willow", "cedar", "elm", "ash", "cherry", "palm", "bamboo", "cactus"]
      .map((name) => ({ name, age: Math.floor(Math.random() * 100), health: 0.5 + Math.random() * 0.5 })),
  (trees) => {
    for (const t of trees) {
      t.age++;
      t.health = Math.max(0.05, Math.min(1.0, t.health + (Math.random() - 0.45) * 0.1));
      if (Math.random() < 0.01) {
        t.age = 0;
        t.health = 0.3;
      }
    }
    return {
      cols: [
        { key: 0, name: "tree", type: "text" },
        { key: 1, name: "age", type: "int" },
        { key: 2, name: "height", type: "text" },
        { key: 3, name: "health", type: "percentage" },
        { key: 4, name: "status", type: "text" },
      ],
      rows: trees.map((t: { name: string; age: number; health: number }) => ({
        0: t.name,
        1: t.age,
        2: ".".repeat(Math.min(20, Math.floor(t.age / 10))),
        3: t.health,
        4: t.age < 10 ? "seed" : t.age < 50 ? "sapling" : t.age < 200 ? "mature" : "ancient",
      })),
    };
  },
);

portal("words", 200, () => {
  const targets = ["cat", "dog", "hi", "elm", "yes", "no", "go", "ok"];
  return {
    targets,
    target: targets[Math.floor(Math.random() * targets.length)],
    targetAge: 0,
    monkeys: ["Alice", "Bob", "Carol", "Dave", "Eve", "Frank"].map((name) => ({
      name,
      attempt: "",
      match: 0,
      attempts: 0,
      best: 0,
    })),
  };
}, (s) => {
  s.targetAge++;
  if (s.targetAge > 150) {
    s.target = s.targets[Math.floor(Math.random() * s.targets.length)];
    s.targetAge = 0;
  }
  let solved = false;
  for (const m of s.monkeys) {
    m.attempt = Array.from({ length: s.target.length }, () => String.fromCharCode(97 + Math.floor(Math.random() * 26)))
      .join("");
    let hits = 0;
    for (let i = 0; i < s.target.length; i++) if (m.attempt[i] === s.target[i]) hits++;
    m.match = hits / s.target.length;
    m.attempts++;
    if (m.match > m.best) m.best = m.match;
    if (m.match === 1) solved = true;
  }
  if (solved) {
    s.target = s.targets[Math.floor(Math.random() * s.targets.length)];
    s.targetAge = 0;
    for (const m of s.monkeys) m.best = 0;
  }
  return {
    cols: [
      { key: 0, name: "monkey", type: "text" },
      { key: 1, name: "target", type: "text" },
      { key: 2, name: "attempt", type: "text" },
      { key: 3, name: "match", type: "percentage" },
      { key: 4, name: "attempts", type: "int" },
      { key: 5, name: "best", type: "percentage" },
    ],
    rows: s.monkeys.map((m: { name: string; attempt: string; match: number; attempts: number; best: number }) => ({
      0: m.name,
      1: s.target,
      2: m.attempt,
      3: m.match,
      4: m.attempts,
      5: m.best,
    })),
  };
});

// --- public routes

app.use("*", cors());

app.notFound((c) => {
  throw new HTTPException(404, {
    message: `Expected a known route, received ${c.req.method} ${
      new URL(c.req.url).pathname
    }. Check the path and the method.`,
  });
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
    insert into usr ${sql({ email, password: await hashPassword(password) })}
    on conflict (email) do update set password = excluded.password
  `;
  return c.json(null, 200);
});

app.post("/login", async (c) => {
  const { email, password } = await c.req.json();
  const [usr] = await sql`select usr_id, password from usr where email = ${email}`;
  if (!usr || !(await verifyPassword(password, usr.password))) return c.json(null, 401);
  return c.json(
    { data: { usr_id: usr.usr_id, jwt: await createJwt(usr.usr_id) } },
    200,
  );
});

const stripeCall = async <T>(what: string, run: () => Promise<T>): Promise<T> => {
  try {
    return await run();
  } catch (err) {
    if (err instanceof HTTPException) throw err;
    throw new HTTPException(502, {
      message: `Expected Stripe to ${what}, received ${
        err instanceof Error ? err.message : err
      }. Check STRIPE_SECRET_KEY and that api.stripe.com is reachable.`,
    });
  }
};

const fulfillPurchase = async (
  tx: Sql,
  {
    usr_id,
    sell_id,
    amount,
    stripe_session_id,
    stripe_payment_intent_id,
  }: {
    usr_id: string;
    sell_id: string;
    amount: number;
    stripe_session_id: string | null;
    stripe_payment_intent_id: string | null;
  },
): Promise<string> => {
  // A paid Checkout Session is a contract: deliver even if the seller unsold
  // after the buyer paid. $0 buys still require a live listing.
  const live = stripe_session_id ? sql`` : sql`and sell_price >= 0`;
  const [sheet] = await tx`select * from sheet where sell_id = ${sell_id} ${live}`;
  if (!sheet) {
    throw new HTTPException(404, {
      message: `Expected a shop listing with sell_id ${sell_id}, received none. The listing may have been taken down.`,
    });
  }
  if (stripe_session_id) {
    await tx`
      insert into payment (buyer_id, seller_id, sell_id, amount, stripe_session_id, stripe_payment_intent_id)
      values (
        ${usr_id}, ${sheet.created_by}, ${sell_id}, ${amount}, ${stripe_session_id}, ${stripe_payment_intent_id}
      )
      on conflict (stripe_session_id) do nothing
    `;
    const [pay] = await tx`select sheet_id from payment where stripe_session_id = ${stripe_session_id} for update`;
    if (pay?.sheet_id) return pay.sheet_id;
  }
  if (!sheet.sell_type) {
    throw new HTTPException(400, {
      message: `Expected sell_type on listing ${sell_id}, received null. Only templates and live sheets can be sold.`,
    });
  }
  const row_0 = sheet.type === "template" ? sheet.row_0.data : [];
  const doc_id = sheet.sell_type.startsWith("codex-")
    ? Math.random().toString().slice(2)
    : automerge.create({ type: sheet.sell_type, data: row_0 }).documentId;
  const [bought] = await tx`
    with buy as (
      insert into sheet (created_by, type, doc_id, name, buy_id, buy_price, row_0)
      select ${usr_id}, sell_type, ${doc_id}, name, sell_id, ${amount}, ${row_0}
      from sheet where sell_id = ${sell_id} ${live}
      returning sheet_id, created_by
    ), buy_usr as (
      insert into sheet_usr (sheet_id, usr_id, role) select sheet_id, created_by, 'owner' from buy
    )
    select sheet_id from buy
  `;
  if (!bought?.sheet_id) {
    throw new HTTPException(403, {
      message:
        `Expected listing ${sell_id} to be purchasable by usr ${usr_id}, received no row. Source: the insert selects from sheet where sell_id matches and the listing is live. The listing was taken down, or you already own it, or you are its seller.`,
    });
  }
  const { sheet_id } = bought;
  if (stripe_session_id) {
    await tx`
      update payment set sheet_id = ${sheet_id}, stripe_payment_intent_id = ${stripe_payment_intent_id}
      where stripe_session_id = ${stripe_session_id} and sheet_id is null
    `;
  } else {
    await tx`
      insert into payment (buyer_id, seller_id, sell_id, sheet_id, amount, stripe_session_id, stripe_payment_intent_id)
      values (${usr_id}, ${sheet.created_by}, ${sell_id}, ${sheet_id}, ${amount}, null, null)
    `;
  }
  return sheet_id;
};

app.post("/stripe", async (c) => {
  const secret = Deno.env.get("STRIPE_WEBHOOK_SECRET");
  if (!secret) {
    throw new HTTPException(500, {
      message:
        "Expected STRIPE_WEBHOOK_SECRET (a Stripe webhook signing secret starting with whsec_), received nothing. Set STRIPE_WEBHOOK_SECRET in the environment.",
    });
  }
  const sig = c.req.header("stripe-signature");
  if (!sig) {
    throw new HTTPException(400, {
      message:
        "Expected stripe-signature header, received none. Stripe signs every webhook; configure the endpoint and STRIPE_WEBHOOK_SECRET.",
    });
  }
  const raw = await c.req.text();
  let event: Stripe.Event;
  try {
    event = await Stripe.webhooks.constructEventAsync(raw, sig, secret);
  } catch (err) {
    throw new HTTPException(400, {
      message:
        `Expected a stripe-signature header matching STRIPE_WEBHOOK_SECRET, received a payload that failed verification: ${
          err instanceof Error ? err.message : err
        }. Check STRIPE_WEBHOOK_SECRET and that the raw body is not parsed before verification.`,
    });
  }
  if (event.type !== "checkout.session.completed") return c.json(null, 200);
  const session = event.data.object;
  if (session.payment_status !== "paid") return c.json(null, 200);
  if (typeof session.id !== "string" || !session.id) {
    throw new HTTPException(400, {
      message: `Expected checkout.session id, received ${
        JSON.stringify(session.id)
      }. Create the session through POST /buy/:id.`,
    });
  }
  const usr_id = session.metadata?.usr_id;
  const sell_id = session.metadata?.sell_id;
  if (!usr_id || !sell_id) {
    throw new HTTPException(400, {
      message: `Expected checkout.session metadata.usr_id and metadata.sell_id, received ${
        JSON.stringify(session.metadata)
      }. Create the session through POST /buy/:id.`,
    });
  }
  const [buyer] = await sql`select usr_id from usr where usr_id::text = ${usr_id}`;
  if (!buyer) {
    throw new HTTPException(400, {
      message: `Expected checkout.session metadata.usr_id to be a usr, received ${
        JSON.stringify(usr_id)
      }. Create the session through POST /buy/:id.`,
    });
  }
  const amount_total = session.amount_total;
  if (typeof amount_total !== "number" || !Number.isInteger(amount_total) || amount_total < 1) {
    throw new HTTPException(400, {
      message: `Expected checkout.session amount_total in cents, received ${
        JSON.stringify(amount_total)
      }. The session must be a paid Checkout Session.`,
    });
  }
  const payment_intent = session.payment_intent;
  const sheet_id = await sql.begin((tx: Sql) =>
    fulfillPurchase(tx, {
      usr_id: String(buyer.usr_id),
      sell_id,
      amount: amount_total / 100,
      stripe_session_id: session.id,
      stripe_payment_intent_id: typeof payment_intent === "string" ? payment_intent : payment_intent?.id ?? null,
    })
  );
  invalidateSync(sheet_id.split(":")[1]);
  return c.json({ data: sheet_id }, 200);
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
        sql`sell_price between ${qs.sell_price.split("-")[0]}::numeric and ${qs.sell_price.split("-")[1]}::numeric`,
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
      method: c.req.method,
      req_headers: JSON.stringify(Object.fromEntries(c.req.raw.headers)),
      query_params: JSON.stringify(c.req.query()),
    })
  }`;
  return c.json(null, 200);
});

// Reject loopback, private, link-local (incl. cloud metadata 169.254.169.254), CGNAT, and IPv6 ULA/link-local ranges.
const ipBlocked = (ipRaw: string): boolean => {
  const mapped = ipRaw.match(/^::ffff:(\d+\.\d+\.\d+\.\d+)$/i);
  const ip = mapped ? mapped[1] : ipRaw;
  if (ip.includes(".")) {
    const [a, b] = ip.split(".").map(Number);
    return a === 0 || a === 127 || a === 10 || a === 169 && b === 254 ||
      a === 172 && b >= 16 && b <= 31 || a === 192 && b === 168 || a === 100 && b >= 64 && b <= 127;
  }
  const v6 = ip.toLowerCase();
  return v6 === "::" || v6 === "::1" || /^f[cd]/.test(v6) || /^fe[89ab]/.test(v6);
};

// SSRF guard: block internal hosts by literal IP and by resolved DNS; follow redirects manually re-validating each hop.
const safeFetch = async (start: string, headers: Record<string, string> = {}): Promise<Response> => {
  let url = start;
  for (let hop = 0; hop < 5; hop++) {
    const u = new URL(url);
    if (!["http:", "https:"].includes(u.protocol))
      throw new HTTPException(400, { message: "Only HTTP(S) URLs allowed." });
    const host = u.hostname.replace(/^\[|\]$/g, "");
    if (host === "localhost" || host.endsWith(".local"))
      throw new HTTPException(400, { message: "Internal URLs not allowed." });
    const isLiteral = /^[0-9.]+$/.test(host) || host.includes(":");
    const ips = isLiteral ? [host] : (await Promise.all(
      (["A", "AAAA"] as const).map((t) => Deno.resolveDns(host, t).catch(() => [] as string[])),
    )).flat();
    if (ips.some(ipBlocked)) throw new HTTPException(400, { message: "Internal URLs not allowed." });
    const res = await fetch(url, {
      redirect: "manual",
      signal: AbortSignal.timeout(30_000),
      headers: {
        "User-Agent": "Scrapsheets/1.0",
        "Accept": "application/json, application/xml, text/xml, application/atom+xml, */*",
        // Caller headers may carry credentials: never replay them to a host an
        // origin redirected to.
        ...(u.origin === new URL(start).origin ? headers : {}),
      },
    });
    const location = res.status >= 300 && res.status < 400 ? res.headers.get("location") : null;
    if (!location) return res;
    url = new URL(location, url).href;
  }
  throw new HTTPException(400, { message: "Too many redirects." });
};

// --- net-http polling

export const parseNetHeaders = (raw = ""): Record<string, string> =>
  Object.fromEntries(
    raw.split("\n").filter((line) => line.trim()).map((line) => {
      const m = line.match(/^([^:\s]+):\s*(.+)$/);
      if (!m) throw new Error(`Header line is not "Name: value": ${JSON.stringify(line)}`);
      return [m[1], m[2].trim()];
    }),
  );

const netDue = new Map<string, number>();

// Read at most `cap` bytes, then abandon the rest of the body.
const readCapped = async (res: Response, cap: number): Promise<string> => {
  const reader = res.body?.getReader();
  if (!reader) return "";
  const chunks: Uint8Array[] = [];
  let size = 0;
  while (size < cap) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    size += value.byteLength;
  }
  await reader.cancel().catch(() => {});
  const buf = new Uint8Array(Math.min(size, cap));
  let offset = 0;
  for (const chunk of chunks) {
    if (offset >= buf.length) break;
    buf.set(chunk.subarray(0, Math.min(chunk.byteLength, buf.length - offset)), offset);
    offset += chunk.byteLength;
  }
  return new TextDecoder().decode(buf);
};

export const pollNetOnce = async (fetcher = safeFetch, now = Date.now()): Promise<void> => {
  const sheets = await sql`select sheet_id, doc_id from sheet where type = 'net-http'`;
  for (const { sheet_id, doc_id } of sheets) {
    if ((netDue.get(sheet_id) ?? 0) > now) continue;
    netDue.set(sheet_id, now + 3600_000);
    try {
      const config = (await automerge.find<{ data: [NetHttp] }>(doc_id)).doc()?.data?.[0];
      if (!config) throw new Error("The document has no config in data[0].");
      netDue.set(sheet_id, now + Math.max(60, Number(config.interval) || 3600) * 1000);
      if (!config.url) continue;
      const res = await fetcher(config.url, parseNetHeaders(config.headers));
      const text = await readCapped(res, 65536);
      // Errors become log rows too: the user who typed the URL must see them.
      const body = res.ok ? text : JSON.stringify({ error: `HTTP ${res.status}`, body: text.slice(0, 1000) });
      await sql`insert into net (sheet_id, method, body) values (${sheet_id}, 'GET', ${body})`;
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      console.error(`net-http poll ${sheet_id}:`, message);
      await sql`
        insert into net (sheet_id, method, body) values (${sheet_id}, 'GET', ${JSON.stringify({ error: message })})
      `.catch((dbErr: unknown) => console.error(`net-http poll ${sheet_id}: could not record the error:`, dbErr));
    }
  }
};

setInterval(() => pollNetOnce().catch((err) => console.error("net-http poll:", err)), 15_000);

// CORS proxy for external data sources (unauthenticated, rate-limited)
app.get("/proxy", async (c) => {
  const url = c.req.query("url");
  if (!url) return c.json({ error: "Missing url parameter" }, 400);

  // Rate limit by IP
  const ip = c.req.header("x-forwarded-for")?.split(",")[0] || "unknown";
  if (!rateLimit(`proxy:${ip}`))
    return c.json({ error: "Rate limit exceeded" }, 429);

  try {
    const res = await safeFetch(url);
    const contentType = res.headers.get("content-type") || "application/octet-stream";
    const body = await res.text();
    return c.text(body, res.status as 200, {
      "Content-Type": contentType,
      "X-Proxy-Status": String(res.status),
    });
  } catch (err) {
    if (err instanceof HTTPException) return c.json({ error: err.message }, err.status);
    return c.json({ error: `Fetch failed: ${err instanceof Error ? err.message : "Unknown error"}` }, 502);
  }
});

// --- authenticated routes

app.use("*", jwt({ secret: JWT_SECRET, alg: JWT_ALG }));

app.use("*", async (c, next) => {
  c.set("usr_id", c.get("jwtPayload")?.sub);
  await next();
});

app.post("/buy/:id", async (c) => {
  const sell_id = c.req.param("id");
  const usr_id = c.get("usr_id");
  const [sheet] = await sql`select * from sheet where sell_id = ${sell_id} and sell_price >= 0`;
  if (!sheet) {
    throw new HTTPException(404, {
      message: `Expected a shop listing with sell_id ${sell_id}, received none. The listing may have been taken down.`,
    });
  }
  if (!sheet.sell_type) {
    throw new HTTPException(400, {
      message: `Expected sell_type on listing ${sell_id}, received null. Only templates and live sheets can be sold.`,
    });
  }
  const dollars = Number(sheet.sell_price);
  if (!Number.isFinite(dollars)) {
    throw new HTTPException(400, {
      message: `Expected sell_price to be a number of dollars, received ${
        JSON.stringify(sheet.sell_price)
      } from sheet ${sheet.sheet_id}. Re-list the sheet with a numeric price.`,
    });
  }
  if (dollars === 0) {
    const sheet_id = await sql.begin((tx: Sql) =>
      fulfillPurchase(tx, {
        usr_id,
        sell_id,
        amount: 0,
        stripe_session_id: null,
        stripe_payment_intent_id: null,
      })
    );
    invalidateSync(sheet_id.split(":")[1]);
    return c.json({ data: sheet_id }, 201);
  }
  const cents = Math.round(dollars * 100);
  if (cents < 1) {
    throw new HTTPException(400, {
      message:
        `Expected sell_price of at least $0.01, received ${sheet.sell_price} on ${sheet.sheet_id}. Re-list at a whole-cent price or 0.`,
    });
  }
  const key = Deno.env.get("STRIPE_SECRET_KEY");
  if (!key) {
    throw new HTTPException(500, {
      message:
        "Expected STRIPE_SECRET_KEY (a Stripe secret key starting with sk_), received nothing. Set STRIPE_SECRET_KEY in the environment.",
    });
  }
  const stripe = new Stripe(key);
  const [usr] = await sql`select email, stripe_customer_id from usr where usr_id = ${usr_id}`;
  if (!usr) {
    throw new HTTPException(401, {
      message: `Expected a usr row for usr_id ${usr_id}, received none. Sign in again.`,
    });
  }
  let { stripe_customer_id } = usr;
  if (!stripe_customer_id) {
    const customer = await stripeCall("create a customer", () =>
      stripe.customers.create({
        email: usr.email || undefined,
        metadata: { usr_id: String(usr_id) },
      }));
    await sql`
      update usr set stripe_customer_id = ${customer.id}
      where usr_id = ${usr_id} and stripe_customer_id is null
    `;
    const [again] = await sql`select stripe_customer_id from usr where usr_id = ${usr_id}`;
    stripe_customer_id = again.stripe_customer_id;
    if (!stripe_customer_id) {
      throw new HTTPException(500, {
        message:
          `Expected usr.stripe_customer_id after creating a Stripe customer, received null for usr_id ${usr_id}. Retry the purchase.`,
      });
    }
  }
  const session = await stripeCall("create a Checkout Session", () =>
    stripe.checkout.sessions.create({
      mode: "payment",
      payment_method_types: ["card"],
      customer: stripe_customer_id,
      line_items: [{
        quantity: 1,
        price_data: {
          currency: "usd",
          unit_amount: cents,
          product_data: { name: sheet.name || "Scrapsheet" },
        },
      }],
      metadata: { usr_id: String(usr_id), sell_id },
      success_url: "https://sheets.scrap.land/",
      cancel_url: "https://sheets.scrap.land/",
    }));
  if (!session.url) {
    throw new HTTPException(502, {
      message: `Expected Stripe Checkout Session url, received none for session ${session.id}. Retry the purchase.`,
    });
  }
  return c.json({ data: { checkout_url: session.url } }, 200);
});

app.post("/sell/:id", async (c) => {
  const body = await c.req.json();
  const { price } = body;
  if (price === undefined) {
    throw new HTTPException(400, {
      message: `Expected a "price" field in the body, received ${
        JSON.stringify(body)
      }. Post {"price": 0} to list it for free.`,
    });
  }
  const updated = await sql`
    update sheet set sell_price = ${price}
    where true
      and sheet_id = ${c.req.param("id")}
      and created_by = ${c.get("usr_id")}
      and buy_price is null
    returning sheet_id
  `;
  if (!updated.length)
    throw new HTTPException(400, { message: "Cannot sell this sheet. Purchased sheets cannot be resold." });
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

app.put("/library/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const usr_id = c.get("usr_id");
  const body = await c.req.json();

  // Check if the user already has access to this sheet
  const [existingAccess] = await sql`
    select s.sheet_id, s.created_by
    from sheet_usr su
    inner join sheet s using (sheet_id)
    where su.sheet_id = ${sheet_id} and su.usr_id = ${usr_id}
  `;

  if (existingAccess) {
    // User has access - update name/tags if they own it
    // Compare as strings to handle bigint/string type variations from DB
    if (String(existingAccess.created_by) === String(usr_id)) {
      await sql`
        update sheet
        set ${sql({ name: body.name ?? "", tags: body.tags ?? [] }, "name", "tags")}
        where sheet_id = ${sheet_id}
      `;
    }
    return c.json(null, 200);
  }

  // User doesn't have access - check if sheet exists at all
  const [existingSheet] = await sql`select sheet_id from sheet where sheet_id = ${sheet_id}`;

  if (existingSheet) {
    throw new HTTPException(403, {
      message:
        `Expected write access to ${sheet_id} for usr ${usr_id}, received none. Source: sheet_usr membership. The sheet is already claimed by someone else; ask an owner to share it with you.`,
    });
  }

  // Sheet doesn't exist - create it (user is claiming a new automerge doc)
  const [type, doc_id] = sheet_id.split(":");
  if (!type || !doc_id) {
    throw new HTTPException(400, {
      message: `Expected a sheet id shaped type:doc_id, received ${JSON.stringify(sheet_id)}. Use e.g. table:abc123.`,
    });
  }

  // Verify the automerge document exists. The doc may live only on the caller's
  // client, so grant sync access for the fetch window.
  grantSync(usr_id, doc_id);
  const row_0 = await automerge
    .find<{ data: Table }>(doc_id as AnyDocumentId)
    .then((hand) => hand.doc()?.data?.[0] ?? {})
    .catch(() => {
      throw new HTTPException(404, {
        message:
          `Expected automerge document ${doc_id} to be reachable, received none. Source: the sync server, which reads it from your client during the claim. Keep the tab open and retry, so the document can be pushed.`,
      });
    });

  const sheet = {
    name: body.name ?? "",
    tags: body.tags ?? [],
    type,
    doc_id,
    row_0,
    created_by: usr_id,
  };

  await sql`
    with s as (
      insert into sheet ${sql(sheet, "type", "doc_id", "name", "tags", "created_by", "row_0")}
      returning sheet_id, created_by
    )
    insert into sheet_usr (sheet_id, usr_id, role) select sheet_id, created_by, 'owner' from s
  `;
  invalidateSync(doc_id);

  return c.json(null, 201);
});

// --- sharing

const ROLES: Role[] = ["owner", "editor", "viewer"];

app.get("/library/:id/share", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetAccess(c, sheet_id);
  const rows = await sql`
    select u.email, su.role, su.created_at
    from sheet_usr su inner join usr u using (usr_id)
    where su.sheet_id = ${sheet_id}
    order by su.role, u.email
  `;
  const [sheet] = await sql`select public from sheet where sheet_id = ${sheet_id}`;
  if (!sheet) {
    throw new HTTPException(404, {
      message:
        `Expected a sheet row for ${sheet_id}, received none. Source: the sheet table. Claim it first with PUT /library/${sheet_id}.`,
    });
  }
  return c.json({ data: { members: rows, public: sheet.public } });
});

app.post("/library/:id/share", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { email, role } = await c.req.json();
  if (typeof email !== "string" || !email.includes("@")) {
    throw new HTTPException(400, {
      message: `Expected an email address to share with, received ${JSON.stringify(email)}.`,
    });
  }
  if (!ROLES.includes(role)) {
    throw new HTTPException(400, {
      message: `Expected role to be one of ${ROLES.join(", ")}, received ${JSON.stringify(role)}.`,
    });
  }

  const [target] = await sql`select usr_id from usr where email = ${email}`;
  if (!target) {
    throw new HTTPException(404, {
      message: `No account for ${email}. They need to sign up before the sheet can be shared with them.`,
    });
  }

  await sql`
    insert into sheet_usr (sheet_id, usr_id, role) values (${sheet_id}, ${target.usr_id}, ${role})
    on conflict (sheet_id, usr_id) do update set role = excluded.role
  `;
  invalidateSync(sheet_id.split(":")[1]);
  return c.json(null, 201);
});

app.delete("/library/:id/share", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { email } = await c.req.json();
  const [removed] = await sql`
    delete from sheet_usr
    where sheet_id = ${sheet_id}
      and usr_id = (select usr_id from usr where email = ${email})
      and role <> 'owner'
    returning usr_id
  `;
  if (!removed) {
    throw new HTTPException(404, {
      message: `${email} is not a non-owner member of this sheet, so there was nothing to remove.`,
    });
  }
  invalidateSync(sheet_id.split(":")[1]);
  return c.json(null, 200);
});

// Public toggles the anonymous read path; both go through syncRole.
app.post("/library/:id/public", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { public: isPublic } = await c.req.json();
  if (typeof isPublic !== "boolean") {
    throw new HTTPException(400, {
      message: `Expected public to be true or false, received ${JSON.stringify(isPublic)}.`,
    });
  }
  await sql`update sheet set public = ${isPublic} where sheet_id = ${sheet_id}`;
  invalidateSync(sheet_id.split(":")[1]);
  return c.json({ data: { public: isPublic } });
});

// A view-only link is a JWT scoped to one sheet. The sync socket already accepts
// ?auth=<jwt>, so this needs no new read path.
app.post("/library/:id/link", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const token = await sign(
    { share: sheet_id, exp: Math.floor(Date.now() / 1000) + 60 * 60 * 24 * 30 },
    JWT_SECRET,
    JWT_ALG,
  );
  return c.json({ data: { token } });
});

// CSV Import - parse CSV and create a new table sheet
// --- import/export

app.post("/import/csv", async (c) => {
  const usr_id = c.get("usr_id");
  const contentType = c.req.header("content-type") || "";

  let csvText: string;
  let sheetName = "Imported CSV";

  if (contentType.includes("multipart/form-data")) {
    const formData = await c.req.formData();
    const file = formData.get("file") as File | null;
    if (!file) {
      throw new HTTPException(400, {
        message: `Expected a multipart field named "file", received fields: ${
          [...formData.keys()].join(", ") || "(none)"
        }. Send the CSV as -F file=@data.csv, or post the raw text with Content-Type: text/csv.`,
      });
    }
    csvText = await file.text();
    sheetName = file.name.replace(/\.csv$/i, "") || sheetName;
  } else {
    // Raw CSV text in body
    csvText = await c.req.text();
  }

  if (!csvText.trim()) {
    throw new HTTPException(400, {
      message:
        `Expected CSV text, received ${csvText.length} characters of whitespace. Source: the request body. Send at least a header row.`,
    });
  }

  // Parse CSV
  const parseCSV = (text: string): string[][] => {
    const rows: string[][] = [];
    let currentRow: string[] = [];
    let currentField = "";
    let inQuotes = false;

    for (let i = 0; i < text.length; i++) {
      const char = text[i];
      const nextChar = text[i + 1];

      if (inQuotes) {
        if (char === '"' && nextChar === '"') {
          currentField += '"';
          i++; // Skip next quote
        } else if (char === '"')
          inQuotes = false;
        else
          currentField += char;
      } else {
        if (char === '"')
          inQuotes = true;
        else if (char === ",") {
          currentRow.push(currentField);
          currentField = "";
        } else if (char === "\n" || (char === "\r" && nextChar === "\n")) {
          currentRow.push(currentField);
          rows.push(currentRow);
          currentRow = [];
          currentField = "";
          if (char === "\r") i++; // Skip \n in \r\n
        } else if (char !== "\r") {
          currentField += char;
        }
      }
    }

    // Don't forget the last field/row
    if (currentField || currentRow.length > 0) {
      currentRow.push(currentField);
      rows.push(currentRow);
    }

    return rows;
  };

  const parsed = parseCSV(csvText);
  if (parsed.length < 1) {
    throw new HTTPException(400, {
      message:
        `Expected at least a header row, received ${parsed.length} parsed rows from ${csvText.length} characters. Source: the request body. Check the delimiter and the line endings.`,
    });
  }

  const [headerRow, ...dataRows] = parsed;

  // Infer types from data
  const inferType = (values: string[]): string => {
    let numericCount = 0;
    let boolCount = 0;
    let total = 0;

    for (const val of values) {
      if (!val.trim()) continue;
      total++;
      if (!isNaN(Number(val))) numericCount++;
      if (["true", "false", "t", "f", "1", "0", "yes", "no"].includes(val.toLowerCase())) boolCount++;
    }

    if (total === 0) return "text";
    if (numericCount / total > 0.8) return "num";
    if (boolCount / total > 0.8) return "bool";
    return "text";
  };

  // Build column definitions
  const cols: Col[] = headerRow.map((name, i) => {
    const colValues = dataRows.map((row) => row[i] || "");
    return {
      name: name.trim() || `Column ${i + 1}`,
      type: inferType(colValues) as Type,
      key: String(i),
    };
  });

  // Build rows with proper type conversion
  const rows: Row[] = dataRows.map((row) => {
    const obj: Row = {};
    cols.forEach((col, i) => {
      const val = row[i] ?? "";
      if (col.type === "num" && val.trim())
        obj[col.key] = Number(val);
      else if (col.type === "bool")
        obj[col.key] = ["true", "t", "1", "yes"].includes(val.toLowerCase());
      else
        obj[col.key] = val;
    });
    return obj;
  });

  // Create automerge document
  const colsRow: Row<Col> = {};
  cols.forEach((col, i) => {
    colsRow[i] = col;
  });

  const tableData: Table = [colsRow, ...rows];
  const handle = automerge.create<{ type: string; data: Table }>();
  handle.change((doc) => {
    doc.type = "table";
    doc.data = tableData;
  });

  const doc_id = handle.documentId;

  // Create sheet record in database
  const sheet = {
    name: sheetName,
    tags: ["imported"],
    type: "table",
    doc_id,
    row_0: colsRow,
    created_by: usr_id,
  };

  const [created] = await sql`
    with s as (
      insert into sheet ${sql(sheet, "type", "doc_id", "name", "tags", "created_by", "row_0")}
      returning sheet_id, created_by
    )
    insert into sheet_usr (sheet_id, usr_id, role) select sheet_id, created_by, 'owner' from s
    returning sheet_id
  `;

  return c.json({ sheet_id: created.sheet_id, rows: rows.length, cols: cols.length }, 201);
});

app.get("/export/:id{.+\\.csv}", async (c) => {
  const sheet_id = c.req.param("id").replace(/\.csv$/, "");
  // sheet() paginates net and query sheets at 50 rows; an export wants the whole sheet.
  const { data } = await sheet(c, sheet_id, { limit: "100000", ...c.req.query() });
  const [colsRow, ...rows] = data;
  if (!colsRow) {
    throw new HTTPException(400, {
      message:
        `Expected sheet ${sheet_id} to have a column row, received a document with no rows at all. Source: data[0] of the automerge document. Add a column before exporting.`,
    });
  }
  const cols = Object.values(colsRow) as Col[];

  const escapeCSV = (val: unknown): string => {
    if (val === null || val === undefined) return "";
    const str = String(val);
    if (str.includes(",") || str.includes('"') || str.includes("\n"))
      return '"' + str.replace(/"/g, '""') + '"';
    return str;
  };

  const csv = [
    cols.map((col) => escapeCSV(col.name)).join(","),
    ...rows.map((row) => cols.map((col) => escapeCSV((row as Row)[col.key])).join(",")),
  ].join("\n");

  return new Response(csv, {
    headers: {
      "Content-Type": "text/csv; charset=utf-8",
      "Content-Disposition": `attachment; filename="${sheet_id.replace(/[^a-zA-Z0-9-_]/g, "_")}.csv"`,
    },
  });
});

// The one stable JSON read: every sheet type, access and pagination inherited from sheet().
app.get("/sheet/:id", async (c) => page(c)(await sheet(c, c.req.param("id"), c.req.query())));

app.get("/net/:id", async (c) => {
  const id = c.req.param("id");
  const sheet_id = id.includes(":")
    ? id
    : await sql`select sheet_id from sheet where doc_id = ${id}`.then(([s]: [{ sheet_id: string }?]) => s?.sheet_id);
  if (!sheet_id) {
    throw new HTTPException(404, {
      message: `Expected a net sheet id or doc_id, received ${
        JSON.stringify(id)
      }, which matches no sheet. Source: the sheet table. Pass the full id, e.g. net-hook:abc123.`,
    });
  }
  return page(c)(await sheet(c, sheet_id, c.req.query()));
});

app.post("/query", async (c) => {
  return page(c)(await querify(c, await c.req.json(), c.req.query()));
});

// --- codex (external databases)

app.get("/codex/:id", async (c) => {
  if (!rateLimit(`codex:${c.get("usr_id")}`))
    throw new HTTPException(429, { message: "Too many codex queries. Please slow down." });
  const sheet_id = c.req.param("id");
  const [type, _doc_id] = sheet_id.split(":");
  switch (type) {
    case "codex-db": {
      const [db] = await sql`select dsn from db where sheet_id = ${sheet_id}`;
      if (!db) {
        throw new HTTPException(400, {
          message: `No DSN found.`,
        });
      }
      db.dsn = await decryptDsn(db.dsn);
      // Block connections to the application's own database
      const appDbUrl = Deno.env.get("DATABASE_URL") ?? "postgresql://postgres@127.0.0.1:5434/postgres";
      try {
        const app_ = new URL(appDbUrl);
        const ext = new URL(db.dsn);
        const blockedHosts = ["localhost", "127.0.0.1", "0.0.0.0", "::1"];
        const appHost = blockedHosts.includes(app_.hostname) ? "127.0.0.1" : app_.hostname;
        const extHost = blockedHosts.includes(ext.hostname) ? "127.0.0.1" : ext.hostname;
        if (extHost === appHost && (ext.port || "5432") === (app_.port || "5432"))
          throw new HTTPException(403, { message: "Cannot connect to this database." });
      } catch (e) {
        if (e instanceof HTTPException) throw e;
        throw new HTTPException(400, {
          message: `Expected a parseable postgres DSN, received one that failed to parse: ${
            e instanceof Error ? e.message : String(e)
          }. Source: the dsn stored for ${
            c.req.param("id")
          }. Re-save it as postgresql://user:password@host:port/database.`,
        });
      }
      const sql_ = pg(db.dsn, {
        onnotice: (msg: { severity?: string }) => msg.severity !== "DEBUG" && console.log(msg),
        connect_timeout: 5,
        idle_timeout: 10,
      });
      try {
        await sql_`SET statement_timeout = '10s'`;
        await sql_`SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY`;
        const rows = await sql_`
          select
            table_name as name,
            '[[{"name":"name","type":"text","key":"column_name"},{"name":"type","type":"text","key":"data_type"},{"name":"key","type":"int","key":"ordinal_position"}]]'::jsonb || jsonb_agg(t)::jsonb as columns
          from information_schema.tables t
          inner join information_schema.columns c using (table_catalog,table_schema,table_name)
          where table_schema = 'public'
          group by table_name, table_type
        `;
        const cols = rows.columns.map((col: { name: string }) => ({
          name: col.name,
          type: "text",
          key: col.name,
        })); // TODO:
        return c.json({ data: [cols, ...rows] }, 200);
      } finally {
        await sql_.end();
      }
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
  const dsn = await c.req.json();
  await sql`
    insert into db (sheet_id, dsn)
    select ${sheet_id}, ${await encryptDsn(dsn)}
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
  const sheet_id = c.req.param("id");
  const [type, doc_id] = sheet_id.split(":");
  const { provider } = c.req.query();

  // Verify user has access to this codex
  const [access] = await sql`
    select true from sheet_usr
    where sheet_id = ${`codex-${type}:${doc_id}`}
      and usr_id = ${c.get("usr_id")}
  `;

  if (!access) {
    throw new HTTPException(403, {
      message: `Expected membership of codex-${type}:${doc_id} for usr ${
        c.get("usr_id")
      }, received none. Source: sheet_usr. Ask an owner to share the codex with you.`,
    });
  }

  // For now, only support direct PostgreSQL connection strings
  // Future: add OAuth flows for Google Sheets, Airtable, etc.
  switch (provider || type) {
    case "db":
    case "postgres":
    case "postgresql":
      return c.json({
        provider: "postgresql",
        method: "dsn",
        instructions: "POST a PostgreSQL connection string (DSN) to /codex-db/:id",
        example: "postgresql://user:pass@host:5432/database",
        connect_url: `/codex-db/${doc_id}`,
      }, 200);

    case "google-sheets":
    case "airtable":
    case "notion":
      // Placeholder for future OAuth providers
      return c.json({
        provider,
        method: "oauth",
        status: "not_implemented",
        message: `OAuth integration with ${provider} is coming soon.`,
      }, 501);

    default:
      return c.json({
        available_providers: [
          { id: "postgresql", name: "PostgreSQL", status: "available" },
          { id: "google-sheets", name: "Google Sheets", status: "coming_soon" },
          { id: "airtable", name: "Airtable", status: "coming_soon" },
          { id: "notion", name: "Notion", status: "coming_soon" },
        ],
      }, 200);
  }
});

app.get("/codex/:id/callback", (c) => {
  // OAuth callback handler - placeholder for future OAuth flows
  const { provider, code, state: _state } = c.req.query();

  if (!provider || !code) {
    throw new HTTPException(400, {
      message: `Expected both "provider" and "code" query parameters, received provider=${
        JSON.stringify(provider ?? null)
      } code=${code ? "(present)" : "(missing)"}. Start the flow at GET /codex/:id/connect.`,
    });
  }

  // For now, return not implemented
  return c.json({
    error: "oauth_not_implemented",
    message: `OAuth callback for ${provider} is not yet implemented.`,
  }, 501);
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
  if (!sheet_) {
    throw new HTTPException(404, {
      message: `Expected a purchased portal for usr ${c.get("usr_id")}, received none for portal:${
        c.req.param("id")
      }. Source: your sheet_usr rows joined to the seller's listing. Buy it with POST /buy/:sell_id first.`,
    });
  }
  return page(c)(await sheet(c, sheet_.sheet_id, c.req.query()));
});

app.get("/stats/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const sheetData = await sheet(c, sheet_id, c.req.query());
  const [colsRow, ...rows] = sheetData.data;
  const cols = Object.values(colsRow) as Col[];

  // Compute statistics for each column
  const stats = cols.map((col) => {
    const values = rows.map((row) => row[col.key]).filter((v) => v != null);
    const numericValues = values
      .map((v) => (typeof v === "number" ? v : parseFloat(String(v))))
      .filter((n) => !isNaN(n));

    const isNumeric = numericValues.length > values.length / 2;

    if (isNumeric && numericValues.length > 0) {
      const sum = numericValues.reduce((a, b) => a + b, 0);
      const min = Math.min(...numericValues);
      const max = Math.max(...numericValues);
      const mean = sum / numericValues.length;
      const sorted = [...numericValues].sort((a, b) => a - b);
      const median = sorted[Math.floor(sorted.length / 2)];

      return {
        column: col.name,
        type: "numeric",
        count: numericValues.length,
        null_count: rows.length - values.length,
        min,
        max,
        sum,
        mean,
        median,
      };
    } else {
      // Text/categorical stats
      const lengths = values.map((v) => String(v).length);
      const histogram: Record<string, number> = {};
      for (const v of values) {
        const key = String(v).slice(0, 100); // Truncate long values
        histogram[key] = (histogram[key] || 0) + 1;
      }

      // Get top 10 most frequent values
      const topValues = Object.entries(histogram)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 10)
        .map(([value, count]) => ({ value, count }));

      return {
        column: col.name,
        type: "text",
        count: values.length,
        null_count: rows.length - values.length,
        unique_count: Object.keys(histogram).length,
        min_length: lengths.length ? Math.min(...lengths) : 0,
        max_length: lengths.length ? Math.max(...lengths) : 0,
        avg_length: lengths.length ? lengths.reduce((a, b) => a + b, 0) / lengths.length : 0,
        top_values: topValues,
      };
    }
  });

  // Format as a table
  const statsCols = [
    { name: "column", type: "text", key: "column" },
    { name: "type", type: "text", key: "type" },
    { name: "count", type: "int", key: "count" },
    { name: "null_count", type: "int", key: "null_count" },
    { name: "unique_count", type: "int", key: "unique_count" },
    { name: "min", type: "text", key: "min" },
    { name: "max", type: "text", key: "max" },
    { name: "mean", type: "text", key: "mean" },
    { name: "top_values", type: "json", key: "top_values" },
  ];

  return c.json({
    data: [
      arrayify(statsCols),
      ...stats.map((s) => ({
        ...s,
        min: s.type === "numeric" ? s.min : s.min_length,
        max: s.type === "numeric" ? s.max : s.max_length,
        mean: s.type === "numeric" ? s.mean?.toFixed(2) : s.avg_length?.toFixed(1),
      })),
    ],
  }, 200);
});

// --- mcp
// Hand-rolled Model Context Protocol server (JSON-RPC 2.0 over POST, no
// streaming). :id is the default sheet scope; tools may override via sheet_id.

type McpTool = {
  description: string;
  inputSchema: Record<string, unknown>;
  handler: (c: Context, args: Record<string, unknown>) => Promise<unknown>;
};

const mcpSheetId = (c: Context, args: Record<string, unknown>): string =>
  typeof args.sheet_id === "string" ? args.sheet_id : c.req.param("id") ?? "";

const mcpTools: Record<string, McpTool> = {
  read_sheet: {
    description: "Read a sheet's columns and rows.",
    inputSchema: {
      type: "object",
      properties: {
        sheet_id: { type: "string", description: "type:doc_id; defaults to the sheet in the URL" },
        limit: { type: "integer" },
        offset: { type: "integer" },
      },
    },
    handler: async (c, args) => {
      const qs: Record<string, string> = {};
      if (args.limit !== undefined) qs.limit = String(args.limit);
      if (args.offset !== undefined) qs.offset = String(args.offset);
      const { data, count, offset } = await sheet(c, mcpSheetId(c, args), qs);
      const [cols, ...rows] = data;
      return { cols: Object.values(cols), rows, count, offset };
    },
  },
  query_sheet: {
    description: "Run SQL (AlaSQL dialect; reference sheets as @type:doc_id) or PRQL.",
    inputSchema: {
      type: "object",
      required: ["code"],
      properties: {
        lang: { type: "string", enum: ["sql", "prql"], default: "sql" },
        code: { type: "string" },
      },
    },
    handler: async (c, args) => {
      if (typeof args.code !== "string") {
        throw new HTTPException(400, {
          message: `query_sheet needs a "code" string, got: ${JSON.stringify(args.code)}`,
        });
      }
      const lang = args.lang ?? "sql";
      if (lang !== "sql" && lang !== "prql") {
        throw new HTTPException(400, {
          message: `query_sheet lang must be "sql" or "prql", got: ${JSON.stringify(lang)}`,
        });
      }
      const { data, count } = await querify(c, { lang, code: args.code, args: [] }, {});
      const [cols, ...rows] = data;
      return { cols: Object.values(cols), rows, count };
    },
  },
  list_sheets: {
    description: "List the sheets in the caller's library.",
    inputSchema: { type: "object", properties: {} },
    handler: async (c) => {
      const sheets = await sql`
        select s.sheet_id, s.type, s.doc_id, s.name, s.tags, s.created_at
        from sheet_usr su
        inner join sheet s using (sheet_id)
        where su.usr_id = ${c.get("usr_id")}
        order by s.created_at
      `;
      return { sheets: [...sheets] };
    },
  },
  write_cells: {
    description:
      "Write cells in a table sheet. row is a 0-based data-row index; row equal to the current row count appends one new row.",
    inputSchema: {
      type: "object",
      required: ["cells"],
      properties: {
        sheet_id: { type: "string", description: "type:doc_id; defaults to the sheet in the URL" },
        cells: {
          type: "array",
          items: {
            type: "object",
            required: ["row", "col", "value"],
            properties: {
              row: { type: "integer", minimum: 0 },
              col: { type: "string", description: "column key or column name" },
              value: {
                description:
                  "must match the column type: number for num/int/float/usd, boolean for bool, string otherwise",
              },
            },
          },
        },
      },
    },
    handler: async (c, args) => {
      const sheet_id = mcpSheetId(c, args);
      const [type, doc_id] = sheet_id.split(":");
      if (type !== "table")
        throw new HTTPException(400, { message: `write_cells only works on table sheets, got: ${sheet_id}` });
      if (!Array.isArray(args.cells) || !args.cells.length)
        throw new HTTPException(400, { message: `write_cells needs a non-empty "cells" array.` });
      await assertSheetAccess(c, sheet_id);
      const hand = await automerge.find<{ type: string; data: Table }>(doc_id as AnyDocumentId).catch(() => {
        throw new HTTPException(404, {
          message:
            `Expected an automerge document for sheet ${sheet_id}, received none. Source: doc_id ${doc_id}. List the sheets you can reach with the list_sheets tool.`,
        });
      });
      const doc = hand.doc();
      if (!doc?.data) throw new HTTPException(500, { message: `Sheet ${sheet_id} has no data.` });
      const [colsRow, ...rows] = doc.data;
      if (!colsRow) throw new HTTPException(500, { message: `Sheet ${sheet_id} has no columns row.` });
      const cols = Object.values(colsRow);
      // Validate every cell before mutating anything: no partial writes.
      const writes: { rowIndex: number; key: string | number; value: unknown }[] = [];
      for (const [i, cell] of args.cells.entries()) {
        const { row, col, value } = cell as { row: unknown; col: unknown; value: unknown };
        const target = cols.find((x) => x.key === col) ?? cols.find((x) => x.name === col);
        if (!target) {
          throw new HTTPException(400, {
            message: `cells[${i}]: no column ${JSON.stringify(col)}. Columns: ` +
              cols.map((x) => `${x.name} (key ${JSON.stringify(x.key)})`).join(", "),
          });
        }
        if (typeof row !== "number" || !Number.isInteger(row) || row < 0 || row > rows.length) {
          throw new HTTPException(400, {
            message: `cells[${i}]: row ${JSON.stringify(row)} is out of range. The sheet has ${rows.length} rows` +
              ` (row ${rows.length} appends one new row).`,
          });
        }
        if (typeof target.type !== "string") {
          throw new HTTPException(400, {
            message:
              `cells[${i}]: column ${target.name} has a structured type; write_cells only writes scalar columns.`,
          });
        }
        const t = target.type;
        const bad = ["num", "int", "float", "usd", "percentage"].includes(t)
          ? typeof value !== "number" || (t === "int" && !Number.isInteger(value))
          : t === "bool"
          ? typeof value !== "boolean"
          : t === "json"
          ? value === undefined
          : typeof value !== "string";
        if (bad) {
          throw new HTTPException(400, {
            message: `cells[${i}] (row ${row}, ${target.name}): expected ${t}, got ${typeof value} ${
              JSON.stringify(value)
            }`,
          });
        }
        writes.push({ rowIndex: row, key: target.key, value });
      }
      const appends = writes.some((w) => w.rowIndex === rows.length);
      hand.change((doc) => {
        if (appends) doc.data.push({});
        for (const { rowIndex, key, value } of writes) doc.data[rowIndex + 1][key] = value;
      });
      return { written: writes.length, rows: rows.length + (appends ? 1 : 0) };
    },
  },
};

app.post("/mcp/:id", async (c) => {
  const msg: {
    jsonrpc?: string;
    id?: unknown;
    method?: string;
    params?: { name?: string; arguments?: Record<string, unknown>; protocolVersion?: string };
  } = await c.req.json().catch(() => {
    throw new HTTPException(400, { message: "MCP requests must be JSON." });
  });
  if (msg?.jsonrpc !== "2.0")
    throw new HTTPException(400, { message: `Expected JSON-RPC 2.0, got: ${JSON.stringify(msg?.jsonrpc)}` });
  if (msg.id === undefined) return c.body(null, 202);
  const rpc = (result: unknown) => c.json({ jsonrpc: "2.0", id: msg.id, result });
  const rpcErr = (code: number, message: string) => c.json({ jsonrpc: "2.0", id: msg.id, error: { code, message } });
  switch (msg.method) {
    case "initialize":
      return rpc({
        protocolVersion: ["2025-06-18", "2025-03-26"].includes(msg.params?.protocolVersion ?? "")
          ? msg.params?.protocolVersion
          : "2025-06-18",
        capabilities: { tools: {} },
        serverInfo: { name: "scrapsheets", version: "0" },
      });
    case "ping":
      return rpc({});
    case "tools/list":
      return rpc({
        tools: Object.entries(mcpTools).map(([name, tool]) => ({
          name,
          description: tool.description,
          inputSchema: tool.inputSchema,
        })),
      });
    case "tools/call": {
      const tool = mcpTools[msg.params?.name ?? ""];
      if (!tool)
        return rpcErr(-32602, `Unknown tool: ${msg.params?.name}. Available: ${Object.keys(mcpTools).join(", ")}`);
      try {
        const out = await tool.handler(c, msg.params?.arguments ?? {});
        return rpc({ content: [{ type: "text", text: JSON.stringify(out) }], structuredContent: out, isError: false });
      } catch (err) {
        if (!(err instanceof HTTPException)) console.error(`mcp tools/call ${msg.params?.name}:`, err);
        return rpc({
          content: [{ type: "text", text: err instanceof HTTPException ? err.message : String(err) }],
          isError: true,
        });
      }
    }
    default:
      return rpcErr(-32601, `Method not found: ${msg.method}`);
  }
});
app.all("/mcp/:id", (c) => c.body(null, 405, { Allow: "POST" }));

export default app;
