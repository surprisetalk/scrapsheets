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
import {
  applyWindows,
  chartSql,
  checkQueryRows,
  checkResultColumns,
  DESCRIBE_COLUMNS,
  describeRef,
  describeRows,
  explain,
  formatQueryError,
  loadRefs,
  MAX_QUERY_MS,
  nearest,
  planQuery,
  register,
  scanRefs,
  WINDOW_TYPES,
} from "./src/sql.mjs";
import Stripe from "stripe";

// --- secrets & crypto

// A secret-less boot is not a recoverable state, so it is not a warning. These
// three used to fall back to Math.random(), which meant a restart silently
// re-rolled them: every session dropped, every encrypted DSN unreadable, and --
// since TOKEN_SECRET is the root hookSecret() derives every sender's signing key
// from -- every webhook delivery refused with a message blaming the sender.
export const requireSecret = (name: string): string => {
  const value = Deno.env.get(name);
  if (!value) {
    throw new Error(explain(`The server cannot start without ${name}.`, {
      Received: "an unset environment variable",
      Expected: `${name} set to a long random string, the same one across every restart`,
      Source: "the process environment",
      Fix: "add it to .env locally, or to the deployment's environment, then start again",
    }));
  }
  return value;
};

const JWT_SECRET = requireSecret("JWT_SECRET");
const JWT_ALG = "HS256";
const TOKEN_SECRET = requireSecret("TOKEN_SECRET");
const DSN_KEY = requireSecret("DSN_ENCRYPTION_KEY");

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

// Application-level AES-256-GCM under DSN_ENCRYPTION_KEY, for the two things
// that must not sit in the database in the clear: a codex connection string and
// a sheet's own secrets. `what` is only ever read back in the failure message,
// which has to name the thing that could not be decrypted -- one key, two
// kinds of value, and "could not decrypt" alone does not say which.
const encrypt = async (plain: string): Promise<string> => {
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const cipher = new Uint8Array(
    await crypto.subtle.encrypt({ name: "AES-GCM", iv }, await dsnAesKey, new TextEncoder().encode(plain)),
  );
  const buf = new Uint8Array(iv.length + cipher.length);
  buf.set(iv);
  buf.set(cipher, iv.length);
  return b64(buf);
};

const decrypt = async (what: string, stored: string): Promise<string> => {
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
      message: explain(`Could not decrypt the ${what}.`, {
        Received: "ciphertext this key does not open",
        Expected: "a value encrypted under the current DSN_ENCRYPTION_KEY",
        Source: "DSN_ENCRYPTION_KEY, which every stored secret and DSN is sealed with",
        Fix: `restore the previous DSN_ENCRYPTION_KEY, or save the ${what} again under this one`,
      }),
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

const hex = (buf: Uint8Array): string => Array.from(buf).map((b) => b.toString(16).padStart(2, "0")).join("");

// --- webhook signing
//
// A net sheet's signing secret is derived from TOKEN_SECRET and the sheet_id
// rather than stored. Nothing new is persisted, and the secret never enters the
// automerge document -- which a viewer, or anyone holding a share link, can
// read. The cost is that rotating one sheet's secret means rotating
// TOKEN_SECRET, which also invalidates every outstanding email-verification
// token, since createToken() hashes the same secret. Per-sheet rotation is its
// own item.
const hmacKey = (secret: string) =>
  crypto.subtle.importKey("raw", new TextEncoder().encode(secret), { name: "HMAC", hash: "SHA-256" }, false, [
    "sign",
    "verify",
  ]);

const hookMac = async (secret: string, message: Uint8Array<ArrayBuffer>): Promise<Uint8Array> =>
  new Uint8Array(await crypto.subtle.sign("HMAC", await hmacKey(secret), message));

// The "hook:" prefix domain-separates this from createToken(), which derives
// email-verification tokens from the same root secret. One secret, two uses,
// and neither may be able to produce the other's output.
export const hookSecret = async (sheet_id: string): Promise<string> =>
  hex(await hookMac(TOKEN_SECRET, new TextEncoder().encode(`hook:${sheet_id}`)));

// What is actually signed. v1 is the timestamp exactly as it appears in the
// header, a dot, then the body's own bytes. v2 puts the request target between
// them, because under v1 a delivery's identity was the body alone: two
// genuinely different deliveries carrying the same body in the same second were
// one delivery, so a fan-out that discriminates by query string lost every copy
// after the first to a replay refusal. The delimiter is a newline, since a URL
// cannot carry a raw one while a path can carry any number of dots.
//
// Bytes rather than a decoded string, because decoding a non-UTF-8 payload
// replaces characters and would make a correctly signed delivery unverifiable.
const hookMessage = (scheme: "v1" | "v2", t: string, target: string, body: Uint8Array): Uint8Array<ArrayBuffer> => {
  const prefix = new TextEncoder().encode(scheme === "v1" ? `${t}.` : `${t}\n${target}\n`);
  const message = new Uint8Array(prefix.length + body.length);
  message.set(prefix);
  message.set(body, prefix.length);
  return message;
};

/** The `scrapsheets-signature` header value for one delivery, in the current
 * scheme. Takes the secret rather than the sheet id, because that is what a
 * sender holds -- and because a sheet's secret is no longer always the derived
 * one. Exported so the tests sign the same way a sender does. */
export const hookSign = async (
  secret: string,
  target: string,
  body: string,
  t: number = Math.floor(Date.now() / 1000),
): Promise<string> => {
  const mac = await hookMac(secret, hookMessage("v2", String(t), target, new TextEncoder().encode(body)));
  return `t=${t},v2=${hex(mac)}`;
};

// crypto.subtle.verify does the comparison, so no digest equality is written by
// hand. The signature reaches here already matched against [0-9a-f]{64}; a pair
// that is not hex would silently become a zero byte, so it throws instead.
const hookVerify = async (secret: string, message: Uint8Array<ArrayBuffer>, signature: string): Promise<boolean> => {
  const bytes = Uint8Array.from((signature.match(/../g) ?? []).map((pair) => {
    const byte = parseInt(pair, 16);
    if (!Number.isInteger(byte)) throw new Error(`Expected two hex digits in a signature digest, received ${pair}.`);
    return byte;
  }));
  return await crypto.subtle.verify("HMAC", await hmacKey(secret), bytes, message);
};

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
    if (rateLimitBuckets.size > RATE_LIMIT_KEYS_MAX)
      rateLimitBuckets.delete(rateLimitBuckets.keys().next().value!);
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

// Bounded, and not only swept: sweeping is by idle time, so a caller who can
// vary its own key mints entries faster than a 60-second broom removes them.
// Insertion order is iteration order, so the oldest key is the first one out.
const RATE_LIMIT_KEYS_MAX = 10_000;

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
  | "date"
  | "timestamp"
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
// An alert is a query plus somewhere to send it. The condition is the query's
// own where clause: it fires when the query returns a row, which is the only
// definition that needs no second language.
export type Alert = { code: string; to: string; interval: number; digest?: boolean };
// A chart is a sheet: where the numbers come from, and which two columns to draw.
export type Chart = { source: string; kind: string; x: string; y: string };
// A dashboard owns no data: it names the sheets to show, and each tile is that
// sheet. Its own rows are therefore the list of what it names.
export type Dashboard = { tiles: string[] };
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
    // Deliberately no owner emails: any signed-in user can name any sheet_id, so
    // the message says how access is granted without saying who holds it.
    const [exists] = await sql`select type from sheet where sheet_id = ${sheet_id}`;
    throw new HTTPException(403, {
      message: explain(`You do not have read access to ${sheet_id}.`, {
        Received: exists ? `no membership, no purchase, and the sheet is not public` : `no sheet with that id exists`,
        Expected: "a share on the sheet, a purchase of its listing, or sheet.public",
        Source: "sheet_usr, the payment-derived buy_id/sell_id link, and sheet.public",
        Fix: exists
          ? `ask an owner to run POST /library/${sheet_id}/share with your email, or POST /library/${sheet_id}/public`
          : "check the id: it is type:doc_id, e.g. table:abc123",
      }),
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
    const [mine] = await sql`select role from sheet_usr where sheet_id = ${sheet_id} and usr_id = ${c.get("usr_id")}`;
    throw new HTTPException(403, {
      message: explain(`Only an owner can change ${sheet_id}.`, {
        Received: mine ? `your role on this sheet is ${mine.role}` : "you have no role on this sheet",
        Expected: "role owner, or having created the sheet",
        Source: "sheet_usr.role and sheet.created_by",
        Fix: `ask an owner to run POST /library/${sheet_id}/share with your email and role owner`,
      }),
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
  // Sheets the server computes rather than stores answer before the document
  // lookup: they have no automerge document and no sheet row, and their own
  // where clause is the access rule.
  if (sheet_id === FRESHNESS_SHEET) return await freshness(c, { limit, offset });
  // The failure log answers before the membership check, because a caller with
  // no share on it still owns the failures their own requests caused. The where
  // clause is the access rule: the operator reads every row through their
  // viewer grant, everybody else reads the rows their own account caused, and
  // an anonymous caller owns nothing and reads nothing. Safe to widen because
  // the row stores header and query names only, never values.
  if (sheet_id === ERROR_SHEET) {
    const usr_id = c.get("usr_id") ?? null;
    const [operator] = await sql`
      select true from sheet_usr where sheet_id = ${ERROR_SHEET} and usr_id = ${usr_id}
    `;
    return await cselect({
      cols: null,
      // A 5xx body is the stack `onError` recorded: file paths, line numbers,
      // dependency internals, and the failing SQL out of a postgres.js error.
      // The 500 response deliberately answers "Sorry, something went wrong",
      // and this read must not undo that. A 4xx body is the explain() block
      // about the caller's own request and is theirs to read.
      select: operator
        ? sql`select n.created_at, n.body, n.method,
                     n.req_headers::text, n.query_params::text, n.meta::text`
        : sql`select n.created_at,
                     case when coalesce(substring(n.meta->>'status' from '^[0-9]{1,9}$')::int, 500) < 500
                          then n.body
                          else 'The server failed on this request. The message is in the operator log.' end as body,
                     n.method, n.req_headers::text, n.query_params::text, n.meta::text`,
      from: sql`from net n`,
      where: operator
        ? [sql`n.sheet_id = ${ERROR_SHEET}`]
        : [sql`n.sheet_id = ${ERROR_SHEET}`, sql`n.meta->>'usr_id' = ${usr_id}`],
      order: sql`order by n.created_at desc, n.net_id desc`,
      limit,
      offset,
    });
  }
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
    case "alert":
      return await cselect({
        cols: null,
        // Appended after body, so existing `select body from @net-hook:x` queries
        // and existing column positions are untouched. `meta` is how a run reads
        // as a run: status, milliseconds, bytes.
        //
        // Cast to text on the way out. These three are jsonb in the table, so the
        // status check can read `meta->>'status'` in SQL, but a cell holds text --
        // and `json_extract(req_headers, '$.names')` is how a sheet reads one,
        // which needs the string postgresjs would otherwise have parsed away.
        select: sql`select n.created_at, n.body, n.method,
                           n.req_headers::text, n.query_params::text, n.meta::text`,
        from: sql`from sheet_usr su inner join net n using (sheet_id)`,
        where: [
          sql`(su.sheet_id,su.usr_id) = (${sheet_id},${c.get("usr_id")})`,
        ],
        // Without an order the heap decides, so paging a log repeats and skips rows.
        order: sql`order by n.created_at desc, n.net_id desc`,
        limit,
        offset,
      });
    case "dashboard": {
      const { tiles } = hand.doc().data[0] as unknown as Dashboard;
      const rows = (Array.isArray(tiles) ? tiles : []).map((tile) => ({ tile: String(tile).replace(/^@/, "") }));
      return {
        data: [arrayify([{ name: "tile", type: "text" as Type, key: "tile" }]), ...rows],
        count: rows.length,
        offset: 0,
      };
    }
    // A chart's rows are the query its settings describe, so it reads, pages and
    // exports like any other sheet -- the picture is the page's job, not the API's.
    case "chart": {
      let code: string;
      try {
        code = chartSql(hand.doc().data[0] as unknown as Chart);
      } catch (err) {
        throw new HTTPException(400, { message: err instanceof Error ? err.message : String(err) });
      }
      return await executeSql(c, code, path_);
    }
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
        message:
          `Expected sheet type table, net-hook, net-http, net-socket, alert, chart, dashboard, or query, received ${
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
  // `describe @table:abc` never reaches the engine: it loads the one sheet it
  // names and reports its shape. describeRef is shared with the page.
  const described = describeRef(sqlCode);
  // scanRefs is shared with the page, so @type:doc_id resolves identically here.
  const { sql: scanned, ids: sheet_ids, cells } = described
    ? { sql: "", ids: [described], cells: [] }
    : scanRefs(sqlCode);

  // A check that fires on the query itself is the author's problem: 400. One
  // that came out of loading a sheet already carries its own status -- 403 for a
  // sheet that is not theirs, 404 for one that is not there -- and re-wrapping
  // it would turn "you cannot see this" into "your SQL is wrong".
  const asBadRequest = (err: unknown): never => {
    if (err instanceof HTTPException) throw err;
    throw new HTTPException(400, { message: err instanceof Error ? err.message : String(err) });
  };

  // Source column types by name, which is what the result columns are typed from.
  const nameToType: Record<string, Type> = {};
  let loaded = 0;
  const { docs, colsOf } = await loadRefs(sheet_ids, {
    path: path_,
    describing: !!described,
    // Where a sheet comes from is the only real difference between the engines:
    // here it is sheet(), which enforces access and recurses into a query sheet
    // on its own; in the page it is a library entry or an automerge document.
    fetch: (sheet_id: string) =>
      sheet(c, sheet_id, {}, [...path_, sheet_id]).then((r) => r.data).catch(async (err) => {
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
      }),
    // The row budget, spent as each sheet lands rather than after all of them:
    // this is the guard that stops a runaway before it starts, so it has to
    // refuse while there is still something left to refuse.
    onLoad: (sheet_id: string, rows: Record<string, unknown>[]) => {
      loaded = checkQueryRows(loaded + rows.length, sheet_id);
    },
  }).catch(asBadRequest) as {
    docs: Record<string, Record<string, unknown>[]>;
    colsOf: Record<string, Col[]>;
  };

  for (const cols of Object.values(colsOf)) for (const col of cols) nameToType[col.name] = col.type;

  if (described) {
    const rows = ((): Record<string, unknown>[] => {
      try {
        return describeRows(described, colsOf[described], docs[described]);
      } catch (err) {
        return asBadRequest(err);
      }
    })();
    return {
      data: [
        arrayify(
          DESCRIBE_COLUMNS.map((name) => ({
            name,
            type: (["rows", "nulls"].includes(name) ? "int" : "text") as Type,
            key: name,
          })),
        ),
        ...rows,
      ],
      count: rows.length,
      offset: 0,
    };
  }

  // Everything the engine cannot be trusted with, in the order it has to happen.
  // planQuery is shared with the page, which is what keeps that order one fact
  // rather than two.
  let plan: {
    sql: string;
    windows: { fn: string; alias: string; args: string[] }[];
    qualify: string | null;
    limit: number | null;
    offset: number;
  };
  try {
    plan = planQuery(scanned, cells, docs, colsOf);
  } catch (err) {
    return asBadRequest(err);
  }
  for (const w of plan.windows) {
    const declared = (WINDOW_TYPES as Record<string, Type | null>)[w.fn];
    nameToType[w.alias] = declared ?? nameToType[w.args[0]] ?? "num";
  }

  let result: { columns: { columnid: string }[]; data: Record<string, unknown>[] };
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    // The engine is single-threaded JS and cannot be interrupted, so this bounds
    // how long the caller waits, not how long the CPU burns. checkQueryRows above
    // is the guard that actually keeps a runaway from starting. The page has no
    // equivalent: there is no other request waiting on it.
    result = await Promise.race<typeof result>([
      ala(plan.sql, [docs]),
      new Promise<never>((_, reject) => {
        timer = setTimeout(
          () =>
            reject(
              new HTTPException(504, {
                message: explain(`This query ran longer than one request is allowed.`, {
                  Received: `still running after ${MAX_QUERY_MS / 1000}s over ${loaded} loaded rows`,
                  Limit: `${MAX_QUERY_MS / 1000}s per query`,
                  Source: "the SQL in this query sheet",
                  Fix: "narrow the joins, or filter each @sheet in its own query sheet first",
                }),
              }),
            ),
          MAX_QUERY_MS,
        );
      }),
    ]);
  } catch (err) {
    // An AlaSQL parse error used to surface as a generic 500. Say where it is.
    if (err instanceof HTTPException) throw err;
    throw new HTTPException(400, { message: formatQueryError(err, sqlCode) });
  } finally {
    clearTimeout(timer);
  }
  let { columns: cols, data: rows } = result;
  try {
    if (plan.windows.length) {
      ({ columns: cols, data: rows } = applyWindows(
        result,
        plan,
        (q: string, params: unknown[]) => (ala(q, params) as { data: Record<string, unknown>[] }).data,
      ));
    }
    checkResultColumns(cols, rows, Object.keys(nameToType), sqlCode);
  } catch (err) {
    return asBadRequest(err);
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
  return `${epoch}:${hex(new Uint8Array(hash))}`;
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

/** The address a rate-limit bucket is keyed on. The **rightmost**
 * x-forwarded-for entry, not the leftmost: the rightmost is the one the proxy in
 * front of us appended, and the leftmost is whatever the caller typed -- so
 * keying on it let a flood rotate its own bucket for free, one header value per
 * request. No header at all means nothing proxied us, so the socket's own peer
 * is the caller. x-real-ip is gone for the same reason: nothing in front of us
 * sets it, so a caller who sets it chooses its own bucket. */
export const callerIp = (c: Context): string =>
  c.req.header("x-forwarded-for")?.split(",").at(-1)?.trim() ||
  (c.env as { remoteAddr?: { hostname?: string } } | undefined)?.remoteAddr?.hostname ||
  "127.0.0.1";

app.use("*", async (c, next) => {
  if (!rateLimit(callerIp(c)))
    throw new HTTPException(429, { message: "Too many requests. Please slow down." });

  await next();
});

// --- seeding

// Every sheet seed() puts in the shop. Named rather than inlined because the
// status check grades how many of them are still there.
const SEEDED = [
  ...DATASETS,
  { doc_id: "webhook-inbox", name: "webhook inbox", tags: ["example", "net"], doc: { type: "net-hook", data: [] } },
];

// Idempotent: examples.sql and every dataset upsert on doc_id.
export const seed = async () => {
  // schema/db.sql is schema only, so pg-schema-diff can diff it. This sentinel
  // user owns every seeded sheet and must exist before examples.sql runs.
  await sql`insert into usr (name, email) values ('Scrapsheets', '') on conflict (email) do nothing`;
  await sql.unsafe(examplesSql);
  for (const { doc_id, name, tags, doc } of SEEDED) {
    await sql`
      insert into sheet (sell_price, created_by, type, name, tags, doc_id, row_0)
      values (0, (select usr_id from usr where email = ''), 'template', ${name}, ${tags},
              ${"dataset-" + doc_id}, ${sql.json({ name, ...doc })})
      on conflict (doc_id) do update set name = excluded.name, tags = excluded.tags, row_0 = excluded.row_0
    `;
  }
  // The operator's failure log. It is a net-hook sheet because that is already
  // "a sheet whose rows live in the net table" -- so it reads, pages, exports
  // and trims with no new code, and `select * from @net-hook:errors` works the
  // day it lands. The sentinel owns it and has no password, so reading it in
  // the app means sharing it to a real account once.
  await sql`
    insert into sheet (created_by, type, doc_id, name, tags)
    values ((select usr_id from usr where email = ''), 'net-hook', ${ERROR_DOC}, 'errors', '{"system"}')
    on conflict (doc_id) do nothing
  `;
  // The sentinel cannot be logged into, so the log needs a reader who can. The
  // account row is created here rather than waited for: an operator who has not
  // signed up yet would otherwise get nothing, and signing up later adopts this
  // row, because POST /signup/:token upserts on the email.
  const operator = Deno.env.get("OPERATOR_EMAIL")?.trim();
  if (operator) {
    await sql`insert into usr (email) values (${operator}) on conflict (email) do nothing`;
    await sql`
      insert into sheet_usr (sheet_id, usr_id, role)
      select ${ERROR_SHEET}, usr_id, 'viewer' from usr where email = ${operator}
      on conflict (sheet_id, usr_id) do nothing
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

// Every failure lands here as a row, so "what is breaking, how often, on which
// path" is a query instead of a scroll through a console that has already
// scrolled. Header values are never stored, only their names: a caller's
// Authorization must not sit in a log forever. trimNet bounds it at NET_KEEP.
const ERROR_DOC = "errors";
const ERROR_SHEET = `net-hook:${ERROR_DOC}`;

// How many log writes have failed in a row. It resets on the next success, so
// it reads as "the log is broken right now" rather than "it was once". Without
// it, a log that cannot be written grades as a service with no failures: the
// two conditions below it are counted out of this very sheet.
let logWriteFailures = 0;

// One row per (status, path) per minute. Every 4xx used to cost an insert and a
// trimNet behind it, so refusing a request was more expensive than serving it --
// which is the wrong way round for the path that exists to shed load. The count
// a suppressed minute hid rides the next row that key writes, as `meta.folded`,
// and the two conditions counted out of this sheet add it back.
//
// Two things folding costs, both of them real:
//
// The row keeps one message per key per minute. A different failure that shares
// a status and a path inside that minute is counted but its own message is not
// kept -- and on /query, where a debounced editor fires one request per
// keystroke, that is most of them. The alternative is a key that includes the
// message, which folds nothing at all on exactly the flood it exists for.
//
// A message is kept for the first failure of a window and not for the rest, so
// the count is complete and the diagnosis is a sample. flushFolds below is what
// keeps the count complete when a burst stops rather than retrying.
export const LOG_EVERY_MS = 60_000;
// Bounded, because a scanner walking unknown paths mints a key per request.
// Insertion order is iteration order, so the oldest key is the first one out.
const LOG_KEYS_MAX = 1_000;
const logSeen = new Map<string, { at: number; folded: number }>();

/** Writes one failure to the error sheet. Answers whether a row actually
 * landed: a suppressed call resolves like a written one, and `logWriteFailures`
 * may only be cleared by a write that happened. Cleared by a suppression, a
 * database that is refusing every insert reads as a service with no failures --
 * which is the one thing this log cannot report about itself. */
const logFailure = async (c: Context, status: number, message: string): Promise<boolean> => {
  const path = new URL(c.req.url).pathname;
  const usr_id = c.get("usr_id");
  const key = `${status} ${path}`;
  const seen = logSeen.get(key) ?? { at: 0, folded: 0 };
  const at = Date.now();
  if (seen.at && at - seen.at < LOG_EVERY_MS) {
    seen.folded++;
    return false;
  }
  // Deleted first so the key moves to the back: Map.set on a key it already
  // holds keeps its original position, which would evict a busy key ahead of an
  // idle one.
  const folded = seen.folded;
  seen.at = at;
  logSeen.delete(key);
  logSeen.set(key, seen);
  if (logSeen.size > LOG_KEYS_MAX) logSeen.delete(logSeen.keys().next().value!);
  await sql`insert into net ${
    sql({
      sheet_id: ERROR_SHEET,
      body: message,
      method: c.req.method,
      // Names, never values, on both. A query string carries secrets here by
      // design -- the sync socket takes ?auth=<jwt> and a share link rides the
      // same parameter -- so a failing request would otherwise write a live
      // token into a sheet that can be shared and exported.
      req_headers: sql.json({ names: [...c.req.raw.headers.keys()].sort().join(", ") || "(none)" }),
      query_params: sql.json({ names: Object.keys(c.req.query()).sort().join(", ") || "(none)" }),
      // Who caused it, so a caller can read back their own failures without a
      // share on the operator's sheet. Absent on an unauthenticated failure and
      // on one the jwt middleware itself raised, which is the honest answer:
      // nobody owns it.
      meta: sql.json({ status, path, ...(folded ? { folded } : {}), ...(usr_id ? { usr_id } : {}) }),
    })
  }`;
  // Subtract what this row carried rather than zeroing: onError does not await
  // this call, so every failure arriving during the insert's round trip has
  // already folded onto the same entry, and zeroing would drop occurrences that
  // were never written. A throw above skips this line entirely, so the count
  // stays on the entry for the next row that key writes.
  seen.folded -= folded;
  await trimNet(ERROR_SHEET);
  return true;
};

/** Writes what a burst that stopped never got to say. logFailure folds repeats
 * onto the entry in `logSeen`, and they ride the next row that key writes --
 * which never comes once the burst ends, so 6,000 refused deliveries inside one
 * minute that then stops used to read as one.
 *
 * A row in this sheet stands for itself plus its folds, because that is how the
 * two conditions counted out of it add suppression back (`sum(1 + folded)`). So
 * `n` pending occurrences are written as `folded: n - 1`: declaring `n` would
 * count the burst once too often, which is the same lie in the other direction.
 *
 * No usr_id and no header names: the key is a status and a path, so the row may
 * span several callers and there is no request left to read anything off. That
 * is also what tells "one failure" apart from "one failure we kept a message
 * for". */
export const flushFolds = async (now = Date.now()): Promise<void> => {
  for (const [key, seen] of logSeen) {
    if (now - seen.at < LOG_EVERY_MS) continue;
    if (!seen.folded) {
      // An expired window with nothing pending has nothing left to say, and the
      // next failure on this key writes a fresh row anyway. This is the only
      // eviction by time logSeen has; LOG_KEYS_MAX is the one by count.
      logSeen.delete(key);
      continue;
    }
    // Claimed synchronously, before the await. logFailure subtracts after its
    // insert instead, because it sets `seen.at` first and so every failure
    // arriving during the round trip is suppressed onto the same entry. This
    // one leaves `at` expired on purpose -- the next failure on this key should
    // write a row with its own message -- so a failure landing mid-flush takes
    // the write path, and both writers would carry the same count. Zeroing
    // first is what makes the flush the only owner of it.
    const folded = seen.folded;
    seen.folded = 0;
    const cut = key.indexOf(" ");
    const status = Number(key.slice(0, cut));
    const path = key.slice(cut + 1);
    try {
      await sql`insert into net ${
        sql({
          sheet_id: ERROR_SHEET,
          body: `${folded} more failures on this status and path inside one minute. The first kept its ` +
            `message; this row is the rest of the count.`,
          method: "FOLD",
          req_headers: sql.json({ names: "(none)" }),
          query_params: sql.json({ names: "(none)" }),
          meta: sql.json({ status, path, ...(folded > 1 ? { folded: folded - 1 } : {}) }),
        })
      }`;
      logWriteFailures = 0;
      await trimNet(ERROR_SHEET);
    } catch (err) {
      // Added back rather than assigned, because a failure suppressed during
      // the round trip has folded onto the entry since it was claimed.
      seen.folded += folded;
      // A logging failure must never replace the failure being logged, and
      // there is no response here to replace it with. It is counted instead,
      // which is the one fact about this log that cannot be read out of it.
      logWriteFailures++;
      console.error(`error log ${ERROR_SHEET}:`, err);
    }
  }
};

// One broom, every RATE_LIMIT_WINDOW_MS. It evicts rate-limit buckets that have
// gone idle and flushes the fold counts of bursts that stopped -- both are
// bounded maps that a caller minting keys can grow, and both are swept on the
// same minute LOG_EVERY_MS folds on.
setInterval(() => {
  const now = Date.now();
  for (const [key, bucket] of rateLimitBuckets) {
    if (now - bucket.lastRefill > RATE_LIMIT_WINDOW_MS)
      rateLimitBuckets.delete(key);
  }
  flushFolds().catch((err) => console.error("fold flush:", err));
}, RATE_LIMIT_WINDOW_MS);

app.onError((err, c) => {
  const known = err instanceof HTTPException;
  const res = known ? err.getResponse() : c.json({ error: "Sorry, something went wrong." }, 500);
  if (!known) console.error(err);
  // 429 is the one status that is not logged. Shedding load is the cheap path
  // by definition, and paying two round trips to record each shed request
  // inverts the point of shedding it.
  //
  // Not awaited: when the database is what failed, the error response must not
  // wait for the logger to discover that too. And a failure to write the log
  // must never replace the failure being logged -- which happens, because the
  // rate limiter throws before seed() has run, so the sheet row may not exist.
  if (res.status !== 429) {
    logFailure(c, res.status, known ? err.message : String(err?.stack ?? err))
      .then((wrote) => {
        if (wrote) logWriteFailures = 0;
      })
      .catch((logErr) => {
        logWriteFailures++;
        console.error(`error log ${ERROR_SHEET}:`, logErr);
      });
  }
  return res;
});

// --- status
//
// One graded condition per likely failure mode. 1.0 is the minimum passing
// grade and 0.0 is total failure, so a number above 1.0 is headroom rather than
// a score to maximize -- which is the whole point: a check written to be
// maximized measures the wrong thing the moment somebody notices it.
//
// Each key is the sentence the grade is about. Each value maps a seconds-ago
// offset to the grade as of then, so the reader sees a trend and not a
// snapshot. A condition that can only be measured now carries "0" alone.
// What a good run looks like, for each of the two kinds of run there are. Both
// GET /status and the library:freshness sheet read these, because two hand-
// copied copies had already drifted: a run that said "sent" with a delivery
// that failed was a failure to one and a success to the other, so the sheet
// whose whole job is naming the rotten alert read healthy on exactly that one.
//
// Both name the alias `n`, and every read out of jsonb is guarded inside a
// case rather than beside it with `and`: Postgres does not promise to evaluate
// `and` left to right, so a guard next to the cast is a guard the planner may
// run second.
const POLL_OK = () => sql`substring(n.meta->>'status' from '^[0-9]{1,9}$')::int between 200 and 299`;
const ALERT_OK = () =>
  sql`(case when n.body is json then n.body::jsonb end)->>'status' <> 'error'
      and ((case when n.body is json then n.body::jsonb end)->>'delivery' in ('sent', ${HELD})
           or (case when n.body is json then n.body::jsonb end)->>'status' in ('clear', 'unchanged', 'idle'))`;

const STATUS_AGO = [0, 3600, 86400];

// The numbers each condition's sentence quotes. They are read into the sentence
// rather than typed into it twice, so a changed limit cannot leave a sentence
// claiming the old one. DB_BYTES_CAP is Neon's free-tier ceiling and
// HEAP_BYTES_CAP the heap a Deno Deploy isolate gets: both live outside this
// code, which is why they are written down rather than derived.
// 250, not 100: a select 1 from Deno Deploy to Neon is ~98ms of network before
// the database does anything, so a 100ms bar sat 2ms inside its own threshold
// and failed one run in four on jitter alone. An alarm that fires every hour
// with nothing wrong is one that gets muted, and it takes the twelve real
// conditions with it. At 250 the condition means the database is degraded --
// a saturated pool, a cold start, a query queue -- rather than a slow packet.
const LATENCY_MS = 250;
const REFUSALS_MAX = 20;
const POLL_STALE_S = 7200;
const DB_BYTES_CAP = 4_000_000_000;
const HEAP_BYTES_CAP = 512_000_000;

// Floor, not round: a condition half a percent short of its own bar would
// otherwise report exactly 1.0 and pass. A grade that will not compute is a
// broken check, and a broken check that answers "fine" is worse than no check.
const grade = (condition: string, n: number): number => {
  const value = Math.floor(Number(n) * 100) / 100;
  if (!Number.isFinite(value)) {
    throw new Error(explain(`The status condition "${condition}" did not grade to a number.`, {
      Received: JSON.stringify(n) ?? String(n),
      Expected: "a finite grade, where 1.0 is the minimum pass",
      Source: "status()",
      Fix: "check the SQL alias this condition reads; a renamed column grades as undefined",
    }));
  }
  return value;
};

export const status = async (): Promise<Record<string, Record<string, number>>> => {
  // A round trip, not a workload: timing the aggregates below would grade the
  // heaviest statement in the file as "a query".
  //
  // The first one is thrown away. An isolate that has just started pays TCP,
  // TLS and auth on it, and a Neon instance that has suspended pays its own
  // wake-up -- about a second against 98ms warm. Timing that graded how cold
  // the caller was, not how fast the database answers, and since nothing keeps
  // a 15-minute cron warm it failed nearly every scheduled run. Connection
  // setup is a different failure mode, and the endpoint timing out is where it
  // already shows.
  await sql`select 1`;
  const started = Date.now();
  await sql`select 1`;
  const dbMs = Date.now() - started;

  // One round trip per shape: the historical conditions are the same SQL
  // evaluated at three instants, so the offsets are a join rather than three
  // queries.
  //
  // Every cast out of jsonb is guarded, because a single row this code did not
  // write would otherwise take the whole endpoint to 500 -- and an uptime
  // checker cannot tell that from a dead server. `substring(x from
  // '^[0-9]{1,9}$')` answers null rather than raising, and `is json` sits in
  // the where clause, which an aggregate's filter is evaluated after.
  //
  // Nine digits, not `+`: shape is not magnitude, and `'99999999999999999999'`
  // is all digits and still out of range for an int. That took this endpoint to
  // 500 on one row nobody here wrote, which is the exact failure this guard is
  // for -- and this endpoint is the alarm.
  const overTime = await sql`
    with at as (select seconds, now() - make_interval(secs => seconds) as t
                from unnest(${STATUS_AGO}::int[]) as seconds)
    select
      at.seconds,
      -- Each row stands for itself plus whatever logFailure folded into it, or
      -- suppression would turn "how many requests failed" into "how many
      -- minutes did failures span", and any burst inside REFUSALS_MAX minutes
      -- would pass at any volume.
      1.0 / (1 + (
        select coalesce(sum(1 + coalesce(substring(n.meta->>'folded' from '^[0-9]{1,9}$')::int, 0)), 0) from net n
        where n.sheet_id = ${ERROR_SHEET}
          and n.created_at > at.t - interval '1 hour' and n.created_at <= at.t
          and substring(n.meta->>'status' from '^[0-9]{1,9}$')::int >= 500
      )) as no_5xx,
      ${REFUSALS_MAX}::numeric / greatest(1, (
        select coalesce(sum(1 + coalesce(substring(n.meta->>'folded' from '^[0-9]{1,9}$')::int, 0)), 0) from net n
        where n.sheet_id = ${ERROR_SHEET}
          and n.created_at > at.t - interval '1 hour' and n.created_at <= at.t
          and substring(n.meta->>'status' from '^[0-9]{1,9}$')::int = 401
          and n.meta->>'path' like '/net/%'
      )) as refusals,
      (select case when count(*) = 0 then 1
                   else count(*) filter (where ${POLL_OK()})::numeric / count(*) end
       from net n inner join sheet s using (sheet_id)
       where s.type = 'net-http' and n.method = 'GET'
         and n.created_at > at.t - interval '1 hour' and n.created_at <= at.t) as polls_ok,
      (select case when count(*) = 0 then 1
                   else count(*) filter (where ${ALERT_OK()})::numeric / count(*) end
       from net n
       where n.method = 'ALERT' and n.body is json
         and n.created_at > at.t - interval '1 day' and n.created_at <= at.t) as alerts_delivered,
      (select count(*) from sheet s
       where s.created_at > at.t - interval '1 day' and s.created_at <= at.t
         and s.created_by <> (select usr_id from usr where email = '')) as sheets_created
    from at
  `;

  // Current state, with no history to read it against: how stale the feeds are,
  // how full the disk is, whether the seed is intact.
  const [live] = await sql`
    select
      (select coalesce(min(${POLL_STALE_S}::numeric / greatest(1, extract(epoch from (now() - last)))), 1)
       from (select max(n.created_at) as last
             from sheet s inner join net n using (sheet_id)
             where s.type = 'net-http' and n.method = 'GET' group by s.sheet_id) feeds) as polls_fresh,
      -- Each alert against its own interval, taken off its newest run rather
      -- than an automerge document. Twice, not once: one missed tick is a slow
      -- poll, two in a row is a poller that stopped.
      --
      -- A left join, so an alert that has never run at all is graded from the
      -- moment it was created. An inner join would have excluded exactly the
      -- failure this condition is for -- a poller that never fired once, on a
      -- cold isolate -- and an empty set grades as a pass. The 3600 an
      -- interval-less run falls back to is pollAlertOnce's own default, so a
      -- sheet that never said otherwise is graded against what it would use.
      (select coalesce(min((2 * interval_s)::numeric / greatest(1, extract(epoch from (now() - last)))), 1)
       from (select coalesce(max(n.created_at), s.created_at) as last,
                    coalesce(
                      (array_agg(substring(n.meta->>'interval' from '^[0-9]{1,9}$')::int
                                 order by n.created_at desc))[1], 3600) as interval_s
             from sheet s left join net n on n.sheet_id = s.sheet_id and n.method = 'ALERT'
             where s.type = 'alert' group by s.sheet_id, s.created_at) runs) as alerts_fresh,
      ${NET_KEEP}::numeric / greatest(1, coalesce(
        (select max(c) from (select count(*) as c from net group by sheet_id) logs), 0)) as log_capped,
      ${DB_BYTES_CAP}::numeric / greatest(1, pg_database_size(current_database())) as db_size,
      (select count(*) from sheet where doc_id like 'dataset-%')::numeric / ${SEEDED.length} as datasets_present
  `;

  const byAgo = (
    condition: string,
    pick: (row: Record<string, number>) => number,
  ): [string, Record<string, number>] => [
    condition,
    Object.fromEntries(
      overTime.map((row: Record<string, number>) => [String(row.seconds), grade(condition, pick(row))]),
    ),
  ];
  const now = (condition: string, n: number): [string, Record<string, number>] => [condition, {
    "0": grade(condition, n),
  }];

  return Object.fromEntries([
    now(`The database answers a query in under ${LATENCY_MS}ms.`, LATENCY_MS / Math.max(1, dbMs)),
    byAgo("No request failed with a 5xx in the past hour.", (r) => r.no_5xx),
    // Deliveries refused, not requests rejected: a scanner walking unknown
    // paths would otherwise hold this red with nothing broken, and an alarm
    // anyone can trigger is an alarm that gets muted. A 401 on /net/ is a real
    // sender that cannot sign -- usually a rotation nobody finished.
    byAgo(`No more than ${REFUSALS_MAX} deliveries were refused in the past hour.`, (r) => r.refusals),
    now("Every failure is reaching the error log.", 1 / (1 + logWriteFailures)),
    byAgo("Every net-http poll in the past hour returned 2xx.", (r) => r.polls_ok),
    now(
      `Every net-http sheet that has ever polled did so in the past ${POLL_STALE_S / 3600} hours.`,
      live.polls_fresh,
    ),
    byAgo("Every alert run in the past day either delivered or had nothing to deliver.", (r) => r.alerts_delivered),
    now("Every alert sheet ran within twice its own interval.", live.alerts_fresh),
    now("No sheet's net log has grown past its retention cap.", live.log_capped),
    now(`The database is under ${DB_BYTES_CAP / 1e9} GB.`, live.db_size),
    now(`The server heap is under ${HEAP_BYTES_CAP / 1e6} MB.`, HEAP_BYTES_CAP / Deno.memoryUsage().heapUsed),
    byAgo("Somebody created a sheet in the past 24 hours.", (r) => r.sheets_created),
    now("Every seeded dataset is still in the shop.", live.datasets_present),
  ]);
};

// Reported, but not paged on. A product nobody is using is failing, and that
// grade belongs in the answer -- but a service with no users is not a service
// that is down, and an alarm that fires every fifteen minutes about it is an
// alarm that gets muted, taking the eleven technical conditions with it.
const REPORTED_ONLY = ["Somebody created a sheet in the past 24 hours."];

// Public, because an uptime check carries no bearer token. It answers grades and
// no rows -- no ids, no names, no addresses -- though a grade is a ratio against
// a limit named in this file, so a reader can invert one back to the count it
// came from. 503 rather than 200 when a condition that pages is failing now, so
// a checker that reads only the status line still learns the answer, and
// `deno task status` reads exactly that rather than deciding again.
app.get("/status", async (c) => {
  const grades = await status();
  const missing = REPORTED_ONLY.filter((condition) => !(condition in grades));
  if (missing.length) {
    throw new HTTPException(500, {
      message: explain(`A condition excused from paging is not one this check grades.`, {
        Received: missing.join("; "),
        Expected: "a sentence that status() actually returns",
        Source: "REPORTED_ONLY",
        Fix: "match the sentence exactly, or it silently starts paging again",
      }),
    });
  }
  const failing = Object.entries(grades)
    .filter(([condition]) => !REPORTED_ONLY.includes(condition))
    .filter(([, series]) => !(series["0"] >= 1));
  return c.json(grades, failing.length ? 503 : 200);
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
      // name is not unique, so it cannot decide the order on its own.
      order: sql`order by name, sell_id`,
      limit,
      offset,
    }),
  );
});

// One delivery may not be larger than this. The net table has no retention policy
// yet, so an unbounded body is an unbounded table.
const NET_BODY_CAP = 1_048_576;

// How far a delivery's signed timestamp may sit from ours. It is replay
// protection only in the crude sense -- a delivery id dedupe is a separate item
// -- but it bounds how long a captured request stays usable.
const HOOK_SKEW = 300;

// Rows kept per sheet. Everything that writes to `net` trims behind itself, so
// a webhook that fires every second cannot fill the disk. It is a cap on the
// log, not on the data: a sheet that must keep everything should write to a
// table, which is never trimmed.
export const NET_KEEP = 1_000;

// Deletes the rows past the cap for one sheet. The identity column is
// monotonic, so "the newest NET_KEEP" is exactly "net_id at or above the
// smallest of the newest NET_KEEP", and a sheet under the cap matches nothing.
export const trimNet = async (sheet_id: string): Promise<void> => {
  await sql`
    delete from net
    where sheet_id = ${sheet_id}
      and net_id < (
        select min(net_id) from (
          select net_id from net where sheet_id = ${sheet_id} order by net_id desc limit ${NET_KEEP}
        ) keep
      )
  `.catch((err: unknown) => console.error(`net retention ${sheet_id}:`, err));
};

// --- delivery signatures
//
// Ours is one scheme and every provider signs its own way, so a sheet's stored
// secrets decide which verifier runs. Never the request: a header the sender
// chose must not be able to pick the check it is measured against, or a spoofed
// stripe-signature would select the verifier for a secret nobody set.
const HOOK_HEADERS: Record<string, string> = {
  "hook": "scrapsheets-signature",
  "hook:stripe": "stripe-signature",
  "hook:github": "x-hub-signature-256",
  "hook:shopify": "x-shopify-hmac-sha256",
};

// Current and previous, tried in that order. Two is what a rollover needs and
// one more than that is a secret nobody meant to leave working.
const SECRET_KEEP = 2;

type HookKey = { value: string; at: string };

/** Which scheme this sheet is signed with, and the keys a delivery may carry,
 * newest first.
 *
 * The derived key rides along as the implicit previous one until a second
 * rotation retires it. Without that, storing a sheet's first secret would drop
 * every sender still using the derived one at the instant it was written --
 * which is the missed delivery the rollover exists to avoid. */
const hookKeys = async (sheet_id: string): Promise<{ name: string; keys: HookKey[] }> => {
  const rows = await sql`
    select name, value_encrypted, created_at from secret
    where sheet_id = ${sheet_id} and (name = 'hook' or name like 'hook:%')
    order by created_at desc, secret_id desc
  `;
  const names: string[] = [...new Set<string>(rows.map((row: { name: string }) => String(row.name)))];
  if (names.length > 1) {
    // POST /library/:id/secret refuses the second one, so reaching here means
    // the table was written around the route. Guessing which scheme was meant
    // is the one thing this must not do.
    throw new HTTPException(500, {
      message: explain(`Sheet ${sheet_id} is configured for more than one signing scheme.`, {
        Received: `secrets named ${names.sort().join(", ")}`,
        Expected: "at most one of " + Object.keys(HOOK_HEADERS).join(", "),
        Source: "the secret table",
        Fix: `delete the ones that do not apply with DELETE /library/${sheet_id}/secret`,
      }),
    });
  }
  const name = names[0] ?? "hook";
  if (!HOOK_HEADERS[name]) {
    throw new HTTPException(500, {
      message: explain(`Sheet ${sheet_id} names a signing scheme this server does not know.`, {
        Received: `a secret named ${JSON.stringify(name)}`,
        Expected: Object.keys(HOOK_HEADERS).join(", "),
        Source: "the secret table",
        Fix: `delete it with DELETE /library/${sheet_id}/secret`,
      }),
    });
  }
  const keys: HookKey[] = [];
  for (const row of rows.slice(0, SECRET_KEEP)) {
    keys.push({
      value: await decrypt(`${name} secret`, String(row.value_encrypted)),
      at: new Date(row.created_at as string).toISOString(),
    });
  }
  if (name === "hook" && keys.length < SECRET_KEEP) keys.push({ value: await hookSecret(sheet_id), at: "derived" });
  return { name, keys };
};

// None of these prints the secret or the expected digest: a rejection that does
// is a signing oracle. Each names its own check, because "401" tells a sender
// nothing about which half of the handshake it got wrong. The type annotation
// is on the binding rather than the arrow, which is what lets a call to it read
// as terminal and spare every caller a trailing throw.
const unsigned: (message: string, fields: Record<string, string>) => never = (message, fields) => {
  throw new HTTPException(401, { message: explain(message, fields) });
};

const skewed = (sheet_id: string, sent: number): void => {
  const nowSec = Math.floor(Date.now() / 1000);
  if (Math.abs(nowSec - sent) > HOOK_SKEW) {
    unsigned(`The signature on this delivery to ${sheet_id} is outside the replay window.`, {
      Received: `t=${sent}, which is ${Math.abs(nowSec - sent)} seconds ${sent < nowSec ? "old" : "in the future"}`,
      Expected: `a t within ${HOOK_SKEW} seconds of ${nowSec}, this server's clock`,
      Source: "the t field of the signature header",
      Fix: "sign with the current time rather than a stored header, and check the sender's clock",
    });
  }
};

/** Checks one delivery against the sheet's own scheme and its own keys, and
 * answers which scheme and which key said yes -- both of which ride the row, so
 * "is anyone still sending the old secret" is a query rather than a guess, and
 * a rollover has a visible end. */
const verifyDelivery = async (
  c: Context,
  sheet_id: string,
  raw: Uint8Array<ArrayBuffer>,
  size: number,
  heard: string,
): Promise<{ scheme: string; secret_at: string; sig: string }> => {
  const { name, keys } = await hookKeys(sheet_id);
  const provider = name.slice("hook:".length);
  const header = HOOK_HEADERS[name];
  const url = new URL(c.req.url);
  const target = url.pathname + url.search;
  const signature = c.req.header(header);
  if (!signature) {
    unsigned(`This delivery to ${sheet_id} is not signed.`, {
      Received: `headers ${heard}`,
      Expected: name === "hook"
        ? "a scrapsheets-signature header, formatted t=<unix seconds>,v2=<64 lowercase hex characters>"
        : `a ${header} header, which is what ${provider} signs with`,
      Source: `the request headers, checked against this sheet's ${name} secret`,
      Fix: name === "hook"
        ? `read this sheet's signing secret with GET /library/${sheet_id}/hook, then sign ` +
          `"<t>\\n<path and query>\\n<body>" with HMAC-SHA256`
        : `send this delivery from ${provider}, or delete the ${name} secret to use scrapsheets' own scheme`,
    });
  }

  if (name === "hook") {
    // Lowercase only. parseInt is case-insensitive, so an upper-cased digest
    // verifies against the same secret while reading as a different string --
    // and the uniqueness that refuses a replay is over the header as sent, so
    // every one of the 2^64 case variants of one captured signature was a free
    // replay. A hex digest has one canonical spelling; this is where it is
    // required.
    //
    // v1 is still accepted: it is what every sender writing against the old
    // readme implements, and refusing it would be the missed delivery this
    // whole section exists to avoid. It signs the body alone, so it keeps the
    // old identity semantics -- no worse than before, and meta.scheme is how
    // you find out who is still on it.
    const parts = signature.match(/^t=(\d{1,10}),(v1|v2)=([0-9a-f]{64})$/);
    if (!parts) {
      unsigned(`The signature on this delivery to ${sheet_id} is malformed.`, {
        Received: JSON.stringify(signature),
        Expected: "t=<unix seconds>,v2=<64 lowercase hex characters>, in that order, with no spaces",
        Source: "the scrapsheets-signature header",
        Fix: 'build it as `t=$(date +%s),v2=$(printf "%s\\n%s\\n%s" "$t" "$path" "$body" | openssl dgst -sha256 ' +
          `-hmac "$secret" -r | cut -d' ' -f1)\``,
      });
    }
    const t = parts[1];
    // The regex above admits v1 and v2 and nothing else, which is what makes
    // this narrowing total rather than a default.
    const scheme = parts[2] as "v1" | "v2";
    const digest = parts[3];
    skewed(sheet_id, Number(t));
    for (const key of keys) {
      if (await hookVerify(key.value, hookMessage(scheme, t, target, raw), digest))
        return { scheme, secret_at: key.at, sig: `t=${t},${scheme}=${digest}` };
    }
    unsigned(`The signature on this delivery to ${sheet_id} does not match its body.`, {
      Received: `${scheme} starting ${digest.slice(0, 8)}, over ${size} bytes`,
      Expected: scheme === "v2"
        ? `HMAC-SHA256 of "${t}\\n${target}\\n<body>" under this sheet's signing secret`
        : `HMAC-SHA256 of "${t}.<body>" under this sheet's signing secret`,
      Source: `the ${scheme} field of the scrapsheets-signature header, tried against ${keys.length} current keys`,
      // The target is the path as it travels, percent-encoding and all, which
      // is not the spelling `type:doc_id` has in a sheet id.
      Fix: "sign the exact bytes you send, unmodified, sign the path exactly as it appears in the request line, " +
        "and check the secret is this sheet's",
    });
  }

  if (name === "hook:stripe") {
    // Stripe's header is a comma-separated scheme list and may carry more than
    // one v1, which is exactly how it rolls its own secrets over. Every one is
    // tried.
    const stamp = signature.match(/(?:^|,)t=(\d{1,10})(?=,|$)/);
    const digests = [...signature.matchAll(/(?:^|,)v1=([0-9a-f]{64})(?=,|$)/g)].map((m) => m[1]);
    if (!stamp || !digests.length) {
      unsigned(`The stripe-signature on this delivery to ${sheet_id} is malformed.`, {
        Received: JSON.stringify(signature),
        Expected: "t=<unix seconds> and at least one v1=<64 lowercase hex characters>, comma separated",
        Source: "the stripe-signature header",
        Fix: "forward the header Stripe sent, unmodified",
      });
    }
    skewed(sheet_id, Number(stamp[1]));
    for (const key of keys) {
      for (const digest of digests) {
        if (await hookVerify(key.value, hookMessage("v1", stamp[1], target, raw), digest))
          return { scheme: "stripe", secret_at: key.at, sig: `t=${stamp[1]},v1=${digest}` };
      }
    }
    unsigned(`The stripe-signature on this delivery to ${sheet_id} does not match its body.`, {
      Received: `${digests.length} v1 digests over ${size} bytes`,
      Expected: `HMAC-SHA256 of "${stamp[1]}.<body>" under this sheet's stored Stripe signing secret`,
      Source: `the stripe-signature header, tried against ${keys.length} stored keys`,
      Fix: "check the endpoint secret stored on this sheet is the one for this Stripe endpoint",
    });
  }

  // GitHub and Shopify sign no timestamp, so neither has a replay window and
  // the unique index is the whole of their replay protection -- which trimNet
  // can eventually free on a very busy sheet. Said out loud because it is a
  // real difference from our own scheme, not an oversight.
  const digest = name === "hook:github"
    ? signature.match(/^sha256=([0-9a-f]{64})$/)?.[1]
    : /^[A-Za-z0-9+/]{43}=$/.test(signature)
    ? signature
    : undefined;
  if (!digest) {
    unsigned(`The ${header} on this delivery to ${sheet_id} is malformed.`, {
      Received: JSON.stringify(signature),
      Expected: name === "hook:github"
        ? "sha256=<64 lowercase hex characters>"
        : "a base64 HMAC-SHA256 digest, 44 characters",
      Source: `the ${header} header`,
      Fix: `forward the header ${provider} sent, unmodified`,
    });
  }
  for (const key of keys) {
    if (name === "hook:github") {
      if (await hookVerify(key.value, raw, digest))
        return { scheme: provider, secret_at: key.at, sig: `sha256=${digest}` };
    } else {
      // Re-encoded from the bytes, not taken from the header. The last base64
      // character of a 32-byte digest carries four data bits and two ignored
      // ones, so four header spellings decode to the same digest -- and
      // Shopify signs no timestamp, so each of those spellings would be a
      // replay that never goes stale.
      const bytes = unb64(digest);
      if (await crypto.subtle.verify("HMAC", await hmacKey(key.value), bytes, raw))
        return { scheme: provider, secret_at: key.at, sig: b64(bytes) };
    }
  }
  unsigned(`The ${header} on this delivery to ${sheet_id} does not match its body.`, {
    Received: `a digest over ${size} bytes`,
    Expected: `HMAC-SHA256 of the body alone under this sheet's stored ${provider} signing secret`,
    Source: `the ${header} header, tried against ${keys.length} stored keys`,
    Fix: `check the secret stored on this sheet is the one ${provider} signs this endpoint with`,
  });
};

app.post("/net/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const heard = [...c.req.raw.headers.keys()].sort().join(", ") || "(none)";
  // Every rejection below names the delivery, not just the status: a webhook
  // sender sees only the response body, so the body has to carry the diagnosis.
  const [target] = await sql`select type from sheet where sheet_id = ${sheet_id}`;
  if (!target) {
    throw new HTTPException(404, {
      message: explain(`No sheet with id ${sheet_id} accepts deliveries.`, {
        Received: `POST /net/${sheet_id} carrying headers ${heard}`,
        Expected: "the sheet_id of an existing net-hook, net-http, or net-socket sheet",
        Source: "the sheet table",
        Fix: `create the sheet first with PUT /library/net-hook:<doc_id>, then post to /net/net-hook:<doc_id>`,
      }),
    });
  }
  if (!String(target.type).startsWith("net-")) {
    throw new HTTPException(400, {
      message: explain(`Sheet ${sheet_id} does not accept deliveries.`, {
        Received: `a ${target.type} sheet`,
        Expected: "a net-hook, net-http, or net-socket sheet",
        Source: "sheet.type",
        Fix: "post to a net-hook sheet, or change this sheet's type prefix",
      }),
    });
  }
  const declared = Number(c.req.header("content-length") ?? NaN);
  // Bytes, not text: the signature covers what was sent, and decoding a
  // non-UTF-8 payload first would replace characters and make a correctly
  // signed delivery unverifiable. The column is text, so it is decoded after.
  const raw = declared > NET_BODY_CAP
    ? new Uint8Array(new ArrayBuffer(0))
    : new Uint8Array<ArrayBuffer>(await c.req.arrayBuffer());
  const size = declared > NET_BODY_CAP ? declared : raw.byteLength;
  const body = new TextDecoder().decode(raw);
  // Postgres text cannot hold a NUL, and the column is text because a body is
  // a body. Verified bytes that cannot be stored used to reach the insert and
  // come back as an unexplained 500 -- the one 500 a sheet's own owner can
  // trigger by accident, by pointing a protobuf or a gzip sender at it.
  const nul = raw.indexOf(0);
  if (nul >= 0) {
    throw new HTTPException(400, {
      message: explain(`This delivery to ${sheet_id} carries a byte that cannot be stored.`, {
        Received: `a NUL byte at offset ${nul} of ${size}`,
        Expected: "a body with no NUL bytes; every other byte, valid UTF-8 or not, is kept as sent",
        Source: "the request body, against the text column it is stored in",
        Fix: "send text or JSON; base64 the payload if it is binary",
      }),
    });
  }
  if (size > NET_BODY_CAP) {
    throw new HTTPException(413, {
      message: explain(`This delivery to ${sheet_id} is too large to store.`, {
        Received: `${size} bytes`,
        Limit: `${NET_BODY_CAP} bytes per delivery`,
        Source: "the request body",
        Fix: "send the payload in pages, or post a URL the sheet can fetch instead",
      }),
    });
  }
  // Every delivery is signed. There is no per-sheet opt-out: without this,
  // anyone who learns a net sheet's id can append rows to it. Which scheme is
  // checked comes off the sheet's stored secrets, never off the request.
  const { scheme, secret_at, sig } = await verifyDelivery(c, sheet_id, raw, size, heard);
  // The skew window bounds a replay to HOOK_SKEW seconds, which is not the same
  // as never: a delivery captured off the wire can be sent again, unchanged,
  // until its t goes stale. The unique index on (sheet_id, signature) is what
  // refuses the repeat, and the insert is what asks -- selecting first and
  // inserting after would let ten parallel copies of one captured delivery all
  // see no prior row and all land.
  //
  // The one hole is the log's own retention: trimNet keeps NET_KEEP rows per
  // sheet, so a sheet taking more than that can evict the record and free the
  // signature -- at which point the skew check has long since refused it anyway.
  const [stored] = await sql`insert into net ${
    sql({
      sheet_id,
      body,
      method: c.req.method,
      req_headers: sql.json(Object.fromEntries(c.req.raw.headers)),
      query_params: sql.json(c.req.query()),
      // Which scheme and which key said yes, and the exact signature that did.
      // The first two make "is anyone still sending the old secret, or the old
      // scheme" a query, so a rollover has a visible end. The third is what the
      // unique index keys on: a header name cannot be the key once a sheet may
      // be signed by a provider, because then the sender chooses which header
      // the index reads and a replay costs one junk header.
      meta: sql.json({ bytes: size, scheme, secret_at, sig }),
    })
  } on conflict (sheet_id, (meta->>'sig')) do nothing returning net_id`;
  if (!stored) {
    throw new HTTPException(409, {
      message: explain(`This delivery to ${sheet_id} has already been stored.`, {
        Received: "a signature this sheet has already accepted",
        // Under v2 a signature covers the second, the request target and the
        // bytes, so what identifies a delivery is what the sender varies. Under
        // v1 it was the bytes alone, which made two genuinely different
        // deliveries carrying the same body in the same second one delivery.
        Expected: scheme === "v2"
          ? "one delivery per signature, where a signature is this second, this path and these bytes"
          : scheme === "v1"
          ? "one delivery per signature, where a v1 signature is this second and these bytes alone"
          : `one delivery per ${scheme} signature`,
        Source: "the signature header, against this sheet's log",
        Fix: scheme === "v2"
          ? "put a delivery id in the body or the query string; this exact request has already landed"
          : scheme === "v1"
          ? "sign with v2, which covers the path and query too, or put a delivery id in the body"
          : `${scheme} sends each delivery one signature; this one has already landed`,
      }),
    });
  }
  await trimNet(sheet_id);
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

// A header value may name a secret instead of carrying one. The document keeps
// the reference and never the value, which is the whole point: sync hands that
// document to every viewer and every share-link holder, so a token written into
// a header is a token a share link can be pointed at.
const SECRET_REF = /\{\{secret:([a-z0-9][a-z0-9:_-]{0,63})\}\}/g;

/** The headers to actually send, with every `{{secret:name}}` replaced by the
 * newest secret of that name on this sheet. Answers a fresh object rather than
 * editing the one it was given, so the caller keeps the unresolved headers and
 * only names can reach a log. */
const resolveSecrets = async (
  sheet_id: string,
  headers: Record<string, string>,
): Promise<Record<string, string>> => {
  const wanted = [
    ...new Set<string>(Object.values(headers).flatMap((v) => [...v.matchAll(SECRET_REF)].map((m) => m[1]))),
  ];
  if (!wanted.length) return headers;
  // Newest per name, which is the same current-secret rule hookKeys reads by:
  // rotating a key a feed uses is a write, not an outage.
  const rows = await sql`
    select distinct on (name) name, value_encrypted from secret
    where sheet_id = ${sheet_id} and name in ${sql(wanted)}
    order by name, created_at desc, secret_id desc
  `;
  const held = new Map<string, string>();
  for (const row of rows) held.set(String(row.name), await decrypt(`secret ${row.name}`, String(row.value_encrypted)));
  const missing = wanted.filter((name) => !held.has(name));
  if (missing.length) {
    // Sending the request without the header instead would come back as
    // somebody else's 401 and read as the API's fault. This names ours.
    throw new Error(
      explain(`This sheet's headers name ${missing.length > 1 ? "secrets" : "a secret"} it does not hold.`, {
        Received: missing.map((name) => `{{secret:${name}}}`).join(", "),
        Expected: `a secret of ${missing.length > 1 ? "each of those names" : "that name"} on this sheet`,
        Source: "the headers on this net-http sheet, against the secret table",
        Fix: `store it with POST /library/${sheet_id}/secret, or take the reference out of the header`,
      }),
    );
  }
  return Object.fromEntries(
    Object.entries(headers).map(([k, v]) => [k, v.replace(SECRET_REF, (_, name) => held.get(name)!)]),
  );
};

// A failure the user can reproduce. The curl line names the header keys the sheet
// sent but never their values: a net-http header may carry a token.
const curlFor = (url: string, headers: Record<string, string>): string =>
  ["curl -i", ...Object.keys(headers).map((k) => `-H '${k}: <value>'`), `'${url.replace(/'/g, "'\\''")}'`].join(" ");

const fetchFailure = (
  url: string,
  headers: Record<string, string>,
  res: Response | null,
  detail: string,
): Record<string, unknown> => ({
  error: res ? `HTTP ${res.status}${res.statusText ? " " + res.statusText : ""}` : detail,
  status: res?.status ?? null,
  // The URL actually fetched, which is not the configured one once a redirect ran.
  url: res?.url || url,
  content_type: res?.headers.get("content-type") ?? null,
  body: res ? detail.slice(0, 1000) : null,
  repro: curlFor(res?.url || url, headers),
});

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
    // Declared out here so the catch can name the URL and headers it failed on.
    let url = `sheet ${sheet_id}`, headers: Record<string, string> = {};
    try {
      const config = (await automerge.find<{ data: [NetHttp] }>(doc_id)).doc()?.data?.[0];
      if (!config) throw new Error("The document has no config in data[0].");
      netDue.set(sheet_id, now + Math.max(60, Number(config.interval) || 3600) * 1000);
      if (!config.url) continue;
      url = config.url;
      headers = parseNetHeaders(config.headers);
      // Resolved into a separate object. `headers` is what a failure row is
      // built from, so a resolved token cannot reach the log even if curlFor
      // one day prints more than the keys.
      const sending = await resolveSecrets(sheet_id, headers);
      const started = Date.now();
      const res = await fetcher(url, sending);
      const text = await readCapped(res, 65536);
      // Errors become log rows too: the user who typed the URL must see them, and
      // must be able to run the same request by hand.
      const body = res.ok ? text : JSON.stringify(fetchFailure(url, headers, res, text));
      // The run beside the payload: whether a feed is slow, or 200-ing an error
      // page, is a question about the poll and not about the body it returned.
      const meta = sql.json({ status: res.status, ms: Date.now() - started, bytes: text.length });
      await sql`
        insert into net (sheet_id, method, body, meta) values (${sheet_id}, 'GET', ${body}, ${meta})
      `;
      await trimNet(sheet_id);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      console.error(`net-http poll ${sheet_id}:`, message);
      const failure = fetchFailure(url, headers, null, message);
      await sql`
        insert into net (sheet_id, method, body, meta)
        values (${sheet_id}, 'GET', ${JSON.stringify(failure)}, ${sql.json({ status: 0, ms: 0, bytes: 0 })})
      `.catch((dbErr: unknown) => console.error(`net-http poll ${sheet_id}: could not record the error:`, dbErr));
    }
  }
};

setInterval(() => pollNetOnce().catch((err) => console.error("net-http poll:", err)), 15_000);

// --- alerts
//
// An alert sheet is a query plus a destination. It fires when the query returns
// a row, which means the condition is the query's own where clause and there is
// no second expression language to learn. Every evaluation lands in `net`, so
// the alert's own history is a sheet you can query -- and a quiet alert is
// distinguishable from a dead one, which it is not when only changes are kept.

const alertDue = new Map<string, number>();

// How many matched rows an alert keeps, and so the most it can diff against the
// run before. Past this the rows are still counted and still sent, but the run
// says it could not tell you which of them are new.
const ALERT_ROWS = 200;

// What a run says when the alert asked to be folded into the daily summary
// instead of mailed on its own. The summary is a convenience over runs that are
// already recorded, so a digest that fails to send loses nothing but the email.
const HELD = "held for the daily digest";
const DIGEST_EVERY_MS = 24 * 60 * 60 * 1000;

// Only what the alert decided, never the rows themselves: the digest is what
// tells one firing from the next without keeping the data twice.
const digest = async (value: unknown) =>
  Array.from(
    new Uint8Array(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(JSON.stringify(value)))),
  ).slice(0, 8).map((b) => b.toString(16).padStart(2, "0")).join("");

export const sendAlertEmail = async (
  to: string,
  sheet_id: string,
  name: string,
  rows: Row[],
  diff: { added: Row[]; removed: number } | null,
): Promise<string> => {
  const key = Deno.env.get(`RESEND_API_KEY`);
  if (!key) return "no RESEND_API_KEY, so nothing was sent";
  const plural = (n: number, word: string) => `${n} ${word}${n === 1 ? "" : "s"}`;
  // What changed is the news; the whole matching set is the context underneath.
  const headline = diff
    ? `${plural(diff.added.length, "new row")}, ${plural(diff.removed, "gone")}, ${plural(rows.length, "row")} in all`
    : plural(rows.length, "row");
  const res = await fetch("https://api.resend.com/emails", {
    method: "POST",
    headers: { Authorization: `Bearer ${key}`, "Content-Type": "application/json" },
    body: JSON.stringify({
      to,
      from: "hello@sheets.scrap.land",
      subject: `${name || sheet_id}: ${headline}`,
      text: [
        `${name || sheet_id}: ${headline}.`,
        ...(diff?.added.length
          ? [``, `New since the last run:`, ...diff.added.slice(0, 20).map((row) => JSON.stringify(row))]
          : []),
        ``,
        `Matching now:`,
        ...rows.slice(0, 20).map((row) => JSON.stringify(row)),
        ...(rows.length > 20 ? [`... and ${rows.length - 20} more`] : []),
        ``,
        `https://sheets.scrap.land/${sheet_id}`,
      ].join("\n"),
    }),
  });
  if (res.ok) return "sent";
  const detail = await res.text();
  console.error(`alert ${sheet_id}: resend refused the message:`, res.status, detail);
  return `resend refused it with ${res.status}: ${detail.slice(0, 200)}`;
};

export const pollAlertOnce = async (send = sendAlertEmail, now = Date.now()): Promise<void> => {
  const sheets = await sql`select sheet_id, doc_id, name, created_by from sheet where type = 'alert'`;
  for (const { sheet_id, doc_id, name, created_by } of sheets) {
    if ((alertDue.get(sheet_id) ?? 0) > now) continue;
    alertDue.set(sheet_id, now + 3600_000);
    let record: Record<string, unknown>;
    // Recorded beside the run so the status check can grade liveness in SQL,
    // instead of opening an automerge document per alert. A run whose document
    // would not open keeps the hour the due map just assumed.
    let interval = 3600;
    // The row a repeated unchanged tick updates instead of duplicating: set only
    // when the previous run was itself unchanged, so the run that last said
    // something different is never overwritten.
    let quiet: string | null = null;
    const started = Date.now();
    try {
      const config = (await automerge.find<{ data: [Alert] }>(doc_id)).doc()?.data?.[0];
      if (!config) throw new Error("The document has no config in data[0].");
      // The status check reads this number back out of the run and reports it as
      // the alert's own interval, so a value nobody can parse has to be a crash
      // rather than a silent hour. Rounded, because the guard the check reads it
      // through is anchored on digits: a fractional interval would drop out and
      // be graded against the default instead.
      if (config.interval !== undefined && config.interval !== null && !(Number(config.interval) > 0)) {
        throw new Error(explain(`The interval on ${sheet_id} is not a number of seconds.`, {
          Received: JSON.stringify(config.interval),
          Expected: "a positive number of seconds, or no interval at all for the default 3600",
          Source: "data[0].interval on the alert document",
          Fix: "put seconds in the interval cell, or clear it",
        }));
      }
      interval = Math.max(60, Math.round(Number(config.interval) || 3600));
      alertDue.set(sheet_id, now + interval * 1000);
      const code = config.code?.trim() ?? "";
      let rows: Row[] = [];
      if (code) {
        // Run it as the owner would, through the same authenticated path, so an
        // alert can never read a sheet its owner cannot.
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({
            Authorization: `Bearer ${await createJwt(created_by)}`,
            "Content-Type": "application/json",
          }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        const text = await res.text();
        if (!res.ok) throw new Error(`the query failed with ${res.status}: ${text.slice(0, 400)}`);
        rows = (JSON.parse(text) as { data: Row[] }).data.slice(1);
      }
      const fingerprint = await digest(rows);
      const [last]: { net_id: string; body: string }[] = await sql`
        select net_id, body from net where sheet_id = ${sheet_id} order by net_id desc limit 1
      `;
      const before = last
        ? JSON.parse(last.body) as {
          fingerprint?: string;
          matched?: Row[];
          truncated?: boolean;
          status?: string;
          delivery?: string;
          to?: string;
        }
        : null;
      // Same answer as last time means the same alert, and sending it again
      // every interval is how people learn to filter alerts into a folder they
      // never open. The run is still recorded: a healthy quiet alert and a
      // poller that died both write nothing otherwise, and nothing outside this
      // log can tell them apart. The row carries the same fingerprint, matched
      // and truncated the next run reads back, so the de-dupe and the diff both
      // keep working off "the last row" as they did.
      // ...unless the last run never got through. A send that failed and is
      // then de-duped away is an alert one Resend outage silences for good, so
      // the same rows are sent again rather than counted as already delivered.
      // A run with no destination is not stuck: retrying sends nothing, and
      // recording that every interval would page on a sheet nobody finished.
      quiet = before?.status === "unchanged" && last ? last.net_id : null;
      const stuck = before?.status === "firing" && !!before?.to &&
        before?.delivery !== "sent" && before?.delivery !== HELD;
      const unchanged = before?.fingerprint === fingerprint && !stuck;
      // The diff is over the rows the last run kept, so it is only honest when
      // neither run had more rows than it keeps. Say so rather than guess.
      const truncated = rows.length > ALERT_ROWS;
      const comparable = !truncated && before !== null && !before.truncated;
      const gone = new Set((before?.matched ?? []).map((row) => JSON.stringify(row)));
      const here = new Set(rows.map((row) => JSON.stringify(row)));
      const diff = comparable
        ? {
          added: rows.filter((row) => !gone.has(JSON.stringify(row))),
          removed: [...gone].filter((row) => !here.has(row)).length,
        }
        : null;
      record = {
        status: !code ? "idle" : unchanged ? "unchanged" : rows.length ? "firing" : "clear",
        rows: rows.length,
        fingerprint,
        to: config.to ?? "",
        truncated,
        matched: rows.slice(0, ALERT_ROWS),
        added: diff ? diff.added.length : null,
        removed: diff ? diff.removed : null,
        ...(diff ? {} : {
          diff_skipped: truncated
            ? `this run matched more than ${ALERT_ROWS} rows`
            : before === null
            ? "this is the first run, so there is nothing to compare it with"
            : `the run before matched more than ${ALERT_ROWS} rows`,
        }),
        delivery: !code
          ? "no query to run, so nothing was sent"
          : unchanged
          ? "the same answer as the run before, so nothing was sent"
          : !rows.length
          ? "cleared, so nothing was sent"
          : config.digest
          ? HELD
          : config.to
          ? await send(config.to, sheet_id, name, rows, diff)
          : "no destination, so nothing was sent",
      };
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      console.error(`alert ${sheet_id}:`, message);
      record = { status: "error", rows: 0, fingerprint: await digest(message), error: message };
    }
    // A tick that says what the tick before it said moves that row's timestamp
    // rather than adding another. Liveness reads max(created_at) and the
    // de-dupe reads the last row, so both still work -- and a minute-interval
    // alert stops writing 1440 rows a day, which would push the run the daily
    // digest still has to find out past NET_KEEP in under 17 hours.
    const repeat = record.status === "unchanged" && quiet;
    await (repeat
      ? sql`
        update net set created_at = now(), meta = ${sql.json({ ms: Date.now() - started, interval })}
        where net_id = ${quiet}
      `
      : sql`
        insert into net (sheet_id, method, body, meta)
        values (${sheet_id}, 'ALERT', ${JSON.stringify(record)}, ${sql.json({ ms: Date.now() - started, interval })})
      `).catch((dbErr: unknown) => console.error(`alert ${sheet_id}: could not record the run:`, dbErr));
    if (!repeat) await trimNet(sheet_id);
  }
};

setInterval(() => pollAlertOnce().catch((err) => console.error("alert poll:", err)), 15_000);

export const sendDigestEmail = async (
  to: string,
  runs: { sheet_id: string; name: string; body: string }[],
): Promise<string> => {
  const key = Deno.env.get(`RESEND_API_KEY`);
  if (!key) return "no RESEND_API_KEY, so nothing was sent";
  const line = ({ sheet_id, name, body }: { sheet_id: string; name: string; body: string }) => {
    const run = JSON.parse(body) as { status: string; rows: number; added: number | null; removed: number | null };
    const moved = run.added === null ? "" : ` (+${run.added} -${run.removed})`;
    return `${run.status.padEnd(7)} ${String(run.rows).padStart(5)} rows${moved}  ${name || sheet_id}`;
  };
  const res = await fetch("https://api.resend.com/emails", {
    method: "POST",
    headers: { Authorization: `Bearer ${key}`, "Content-Type": "application/json" },
    body: JSON.stringify({
      to,
      from: "hello@sheets.scrap.land",
      subject: `Your alert digest: ${runs.length} change${runs.length === 1 ? "" : "s"}`,
      text: [...runs.map(line), ``, `https://sheets.scrap.land/`].join("\n"),
    }),
  });
  if (res.ok) return "sent";
  const detail = await res.text();
  console.error(`alert digest for ${to}: resend refused the message:`, res.status, detail);
  return `resend refused it with ${res.status}: ${detail.slice(0, 200)}`;
};

// One email per account per day, holding every held run since the last one. The
// watermark moves whether or not the send worked: the runs are all still on
// their own sheets, and retrying a summary forever is worse than missing one.
export const sendDigestOnce = async (send = sendDigestEmail, now = Date.now()): Promise<void> => {
  const users = await sql`
    select u.usr_id, u.email, u.digest_at from usr u
    where exists (select 1 from sheet s where s.created_by = u.usr_id and s.type = 'alert')
  `;
  for (const { usr_id, email, digest_at } of users) {
    const since = digest_at ? new Date(digest_at) : new Date(0);
    if (digest_at && now - since.getTime() < DIGEST_EVERY_MS) continue;
    const runs = await sql`
      select s.sheet_id, s.name, n.body
      from net n inner join sheet s using (sheet_id)
      where s.created_by = ${usr_id} and n.method = 'ALERT' and n.created_at > ${since}
      order by n.created_at
    `;
    const held = runs.filter((run: { body: string }) => {
      try {
        return (JSON.parse(run.body) as { delivery?: string }).delivery === HELD;
      } catch {
        return false;
      }
    });
    if (!held.length) continue;
    await send(email, held as { sheet_id: string; name: string; body: string }[]);
    await sql`update usr set digest_at = ${new Date(now)} where usr_id = ${usr_id}`;
  }
};

setInterval(() => sendDigestOnce().catch((err) => console.error("alert digest:", err)), 900_000);

// CORS proxy for external data sources (unauthenticated, rate-limited)
app.get("/proxy", async (c) => {
  const url = c.req.query("url");
  if (!url) return c.json({ error: "Missing url parameter" }, 400);

  if (!rateLimit(`proxy:${callerIp(c)}`))
    return c.json({ error: "Rate limit exceeded" }, 429);

  try {
    const res = await safeFetch(url);
    const contentType = res.headers.get("content-type") || "application/octet-stream";
    const body = await res.text();
    if (!res.ok) return c.json(fetchFailure(url, {}, res, body), 502);
    return c.text(body, res.status as 200, {
      "Content-Type": contentType,
      "X-Proxy-Status": String(res.status),
    });
  } catch (err) {
    const message = err instanceof Error ? err.message : "Unknown error";
    if (err instanceof HTTPException) return c.json(fetchFailure(url, {}, null, err.message), err.status);
    return c.json(fetchFailure(url, {}, null, `Fetch failed: ${message}`), 502);
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
  // An alert holds someone's email address and sends mail on a timer. Selling
  // copies would hand a stranger a thing that emails the seller.
  if (c.req.param("id").startsWith("alert:")) {
    throw new HTTPException(400, {
      message: explain(`An alert cannot be listed for sale.`, {
        Received: c.req.param("id"),
        Cause: "an alert carries a destination address and sends mail on its own, so a copy would mail its author",
        Fix: "sell the query the alert watches instead, and let the buyer point their own alert at it",
      }),
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

// --- freshness
//
// GET /status grades the feeds in aggregate -- "every net-http poll returned
// 2xx", "every net-http sheet was polled in the past two hours" -- and names
// none of them, so the alarm says something is rotten without saying what. A
// dashboard over `net` cannot answer it either: `net` is not a sheet, and every
// @net-http:x ref reads one feed's rows, so there is nothing to group by.
//
// It is a sheet rather than a route, so it pages, exports and can be selected
// from a query sheet through the paths every other sheet already uses. It has
// no automerge document and no sheet row, so it answers before the lookup and
// before the membership check in sheet(): the join to sheet_usr below is the
// access rule.
const FRESHNESS_SHEET = "library:freshness";

const freshness = async (c: Context, { limit, offset }: Record<string, string>): Promise<Page> =>
  await cselect({
    cols: null,
    select: sql`select f.*`,
    // The whole query is the from clause because cselect counts with
    // `select count(*) ${from} ${where}` and reuses neither the order nor a
    // group by -- so a grouped query written any other way pages against a
    // count that is not its own.
    from: sql`from (
      select s.sheet_id, s.name, s.type,
             last.created_at as last_run,
             ok.created_at as last_ok,
             -- Rows since the last good one. For an alert this counts rows and
             -- not runs, because a repeated quiet tick moves the previous row's
             -- created_at rather than adding another -- which is the same thing
             -- for this question, since a quiet run is not a failure.
             -- Compared on the same key the laterals order by. On created_at
             -- alone, a good run and a bad one sharing a timestamp left
             -- last_ok equal to last_run and the count at zero, so the sheet
             -- this read exists to surface reported itself healthy.
             (select count(*) from net n
               where n.sheet_id = s.sheet_id
                 and n.method = case when s.type = 'alert' then 'ALERT' else 'GET' end
                 and (n.created_at, n.net_id)
                     > (coalesce(ok.created_at, '-infinity'::timestamp), coalesce(ok.net_id, -1)))
               as failures_since_ok,
             last.meta::text as last_meta
      from sheet s
      inner join sheet_usr su using (sheet_id)
      -- Left, both of them. A sheet that has never run at all is exactly the
      -- failure this read is for, and an inner join drops it.
      left join lateral (
        select n.created_at, n.meta from net n
        where n.sheet_id = s.sheet_id and n.method = case when s.type = 'alert' then 'ALERT' else 'GET' end
        order by n.created_at desc, n.net_id desc limit 1
      ) last on true
      left join lateral (
        select n.created_at, n.net_id from net n
        where n.sheet_id = s.sheet_id
          and n.method = case when s.type = 'alert' then 'ALERT' else 'GET' end
          -- The same two predicates GET /status grades on, read from the same
          -- place, so the two cannot drift into disagreeing about what a good
          -- run is. They already had.
          and case when s.type = 'alert' then (${ALERT_OK()}) else (${POLL_OK()}) end
        order by n.created_at desc, n.net_id desc limit 1
      ) ok on true
      where su.usr_id = ${c.get("usr_id") ?? null} and s.type in ('net-http', 'alert')
    ) f`,
    where: [],
    // Stalest first, because that is the question. A sheet that has never run
    // sorts above one that ran a month ago, and sheet_id breaks the ties.
    order: sql`order by f.last_run asc nulls first, f.sheet_id`,
    limit,
    offset,
  });

app.get("/library/freshness", async (c) => page(c)(await freshness(c, c.req.query())));

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
      // Without an order the heap decides, so paging the library repeats and
      // skips rows; sheet_id breaks ties between sheets created in the same tick.
      order: sql`order by s.created_at desc, s.sheet_id`,
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

// The signing secret for one net sheet, plus a shell line that sends a signed
// delivery. Owner-only: the secret is what stands between the sheet and anyone
// who learns its id, so it may not travel with the document the way a share
// link does.
app.get("/library/:id/hook", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const [target] = await sql`select type from sheet where sheet_id = ${sheet_id}`;
  if (!target || !String(target.type).startsWith("net-")) {
    throw new HTTPException(400, {
      message: explain(`Sheet ${sheet_id} has no delivery endpoint.`, {
        Received: target ? `a ${target.type} sheet` : "no sheet row",
        Expected: "a net-hook, net-http, or net-socket sheet",
        Source: "sheet.type",
        Fix: "ask for the hook of a net-hook sheet",
      }),
    });
  }
  // The current key, which is the newest stored one if this sheet has any and
  // the derived one otherwise. A sheet configured for a provider has no line to
  // print: the secret is the provider's, and we never had it to give back.
  const { name, keys } = await hookKeys(sheet_id);
  if (name !== "hook") {
    throw new HTTPException(400, {
      message: explain(`Sheet ${sheet_id} is signed by ${name.slice("hook:".length)}, not by scrapsheets.`, {
        Received: `a sheet holding a ${name} secret`,
        Expected: "a sheet on scrapsheets' own signing scheme",
        Source: "the secret table",
        Fix: `read the endpoint secret from ${name.slice("hook:".length)}, or delete the ${name} secret with ` +
          `DELETE /library/${sheet_id}/secret`,
      }),
    });
  }
  const secret = keys[0].value;
  // Escaped the way curlFor escapes a url: a secret is pasted by hand, and one
  // apostrophe in it otherwise ends the quoting, so the documented line runs
  // and quietly signs the wrong thing.
  const quoted = secret.replace(/'/g, "'\\''");
  const path = `/net/${sheet_id}`;
  const url = `${new URL(c.req.url).origin}${path}`;
  // A long-lived secret may not sit in a shared cache.
  c.header("Cache-Control", "no-store");
  return c.json({
    data: {
      url,
      secret,
      // The path is signed alongside the body, so a query string makes a second
      // delivery instead of a replay. That is why $path is a variable here: a
      // sender that varies it has to sign what it varied.
      repro: [
        `body='{"hello":"world"}'`,
        `path='${path}'`,
        `t=$(date +%s)`,
        `sig=$(printf '%s\\n%s\\n%s' "$t" "$path" "$body" | openssl dgst -sha256 -hmac '${quoted}' -r | cut -d' ' -f1)`,
        `curl -X POST '${new URL(c.req.url).origin}'"$path" -H 'Content-Type: application/json' \\`,
        `  -H "scrapsheets-signature: t=$t,v2=$sig" -d "$body"`,
      ].join("\n"),
    },
  });
});

// --- secrets
//
// A sheet's own secrets. Owner-only to write, and there is no read: a value
// that can be read back is a value a share link can eventually be pointed at,
// and these never enter the automerge document, which is what sync hands a
// viewer. Writing one IS rotating it -- the newest is current, the one before
// it still verifies, and everything older is trimmed behind the write.
const SECRET_NAME = /^[a-z0-9][a-z0-9:_-]{0,63}$/;
const SECRET_VALUE_CAP = 4096;
// Bounded, because this is an owner-writable store and nothing else trims it
// across names -- SECRET_KEEP trims within one.
const SECRET_NAMES_MAX = 32;

app.post("/library/:id/secret", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  // A missing or malformed body is a 400 naming what arrived. Unguarded, the
  // json() throw became an unexplained 500, and cost a row in the error log
  // for a request the caller could have fixed from the message.
  const { name, value } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  if (typeof name !== "string" || !SECRET_NAME.test(name)) {
    throw new HTTPException(400, {
      message: explain(`That is not a usable secret name on ${sheet_id}.`, {
        Received: JSON.stringify(name),
        Expected: "1 to 64 characters of a-z, 0-9, colon, underscore or hyphen, starting with a letter or digit",
        Source: "the name field of the request body",
        Fix: `name it for what it is: hook for this sheet's own signing secret, or one of ${
          Object.keys(HOOK_HEADERS).filter((k) => k !== "hook").join(", ")
        } to have that provider verify deliveries instead`,
      }),
    });
  }
  if (typeof value !== "string" || !value || value.length > SECRET_VALUE_CAP) {
    throw new HTTPException(400, {
      message: explain(`That is not a usable secret value on ${sheet_id}.`, {
        Received: typeof value !== "string" ? `a ${typeof value}` : `${value.length} characters`,
        Expected: `a non-empty string of at most ${SECRET_VALUE_CAP} characters`,
        Source: "the value field of the request body",
        Fix: "send the secret as a JSON string; a key file belongs in a codex connection, not here",
      }),
    });
  }
  // `hook` and `hook:*` are the names verifyDelivery reads, so a name in that
  // space that no verifier knows would be written happily here and then fail
  // every delivery to this sheet. Refused where it is typed instead.
  const scheme = name === "hook" || name.startsWith("hook:");
  if (scheme && !HOOK_HEADERS[name]) {
    throw new HTTPException(400, {
      message: explain(`${JSON.stringify(name)} is not a signing scheme this server knows.`, {
        Received: name,
        Expected: Object.keys(HOOK_HEADERS).join(", "),
        Source: "the name field of the request body",
        Fix: "use one of those, or a name that does not start with hook",
      }),
    });
  }
  const [{ names }] = await sql`
    select count(distinct name)::int as names from secret where sheet_id = ${sheet_id} and name <> ${name}
  `;
  if (names >= SECRET_NAMES_MAX) {
    throw new HTTPException(409, {
      message: explain(`Sheet ${sheet_id} holds as many secrets as it may.`, {
        Received: `${names} names already, and a request to add ${name}`,
        Expected: `at most ${SECRET_NAMES_MAX} distinct names per sheet`,
        Source: "the secret table",
        Fix: `delete one you no longer need with DELETE /library/${sheet_id}/secret`,
      }),
    });
  }
  // At most one signing scheme per sheet. With two, which verifier runs would
  // have to be decided by the headers the sender chose to send, which is the
  // one thing that must never pick the check -- so the **insert** asks, rather
  // than a select before it: two concurrent writes of different schemes both
  // pass a check and both land, and the sheet then refuses every delivery.
  //
  // The name cap above is a plain check because losing that race costs one name
  // over the limit; losing this one costs the sheet.
  const value_encrypted = await encrypt(value);
  // The check and the insert are one transaction behind a lock on the sheet
  // row, because `insert ... where not exists` is only atomic against another
  // statement on the same connection. Two isolates each hold their own, so
  // under read committed both saw no clashing row and both landed -- and a
  // sheet with two schemes refuses every delivery, since verifyDelivery will
  // not guess which one was meant. The lock is what serializes them; the row
  // exists already, because assertSheetOwner just read it.
  const clash = await sql.begin(async (tx: typeof sql) => {
    await tx`select 1 from sheet where sheet_id = ${sheet_id} for update`;
    const taken = scheme
      ? await tx`
        select distinct name from secret
        where sheet_id = ${sheet_id} and (name = 'hook' or name like 'hook:%') and name <> ${name}
      `
      : [];
    if (taken.length) return taken as unknown as { name: string }[];
    await tx`
      insert into secret (sheet_id, name, value_encrypted) values (${sheet_id}, ${name}, ${value_encrypted})
    `;
    return null;
  });
  if (clash) {
    throw new HTTPException(409, {
      message: explain(`Sheet ${sheet_id} already has a signing scheme.`, {
        Received: `a request to add ${name} beside ${clash.map((r: { name: string }) => r.name).sort().join(", ")}`,
        Expected: "at most one of " + Object.keys(HOOK_HEADERS).join(", ") + " per sheet",
        Source: "the secret table",
        Fix: `delete the other one first with DELETE /library/${sheet_id}/secret`,
      }),
    });
  }
  // Keep current and previous. A third is a secret nobody meant to leave
  // working, and the rollover has to end somewhere visible.
  await sql`
    delete from secret
    where sheet_id = ${sheet_id} and name = ${name}
      and secret_id not in (
        select secret_id from secret
        where sheet_id = ${sheet_id} and name = ${name}
        order by created_at desc, secret_id desc limit ${SECRET_KEEP}
      )
  `;
  return c.json(null, 201);
});

app.get("/library/:id/secret", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  // Names and timestamps, never values. `previous_at` is what says a rollover
  // is still open, and `meta.secret_at` on this sheet's deliveries is what says
  // whether anyone is still using the older one.
  const rows = await sql`
    select name,
           max(created_at) as created_at,
           (array_agg(created_at order by created_at desc, secret_id desc))[2] as previous_at
    from secret where sheet_id = ${sheet_id}
    group by name order by name
  `;
  c.header("Cache-Control", "no-store");
  return c.json({ data: { secrets: rows } });
});

app.delete("/library/:id/secret", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { name } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  const removed = typeof name === "string"
    ? await sql`delete from secret where sheet_id = ${sheet_id} and name = ${name} returning secret_id`
    : [];
  if (!removed.length) {
    throw new HTTPException(404, {
      message: explain(`Sheet ${sheet_id} holds no secret named ${JSON.stringify(name)}.`, {
        Received: JSON.stringify(name),
        Expected: "a name GET /library/" + sheet_id + "/secret lists",
        Source: "the secret table",
        Fix: "read the names back first; a value is never readable, but a name always is",
      }),
    });
  }
  return c.json(null, 200);
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

  // Parse CSV. Each row keeps the line it started on and its raw text, because a
  // rejection has to point at the line in the file the user is looking at.
  const parseCSV = (text: string): { fields: string[]; line: number; raw: string }[] => {
    const rows: { fields: string[]; line: number; raw: string }[] = [];
    let currentRow: string[] = [];
    let currentField = "";
    let inQuotes = false;
    let start = 0;
    let line = 1; // where the scanner is
    let rowLine = 1; // where the row being built started

    const push = (end: number) => {
      currentRow.push(currentField);
      rows.push({ fields: currentRow, line: rowLine, raw: text.slice(start, end) });
      currentRow = [];
      currentField = "";
    };

    for (let i = 0; i < text.length; i++) {
      const char = text[i];
      const nextChar = text[i + 1];

      if (inQuotes) {
        if (char === '"' && nextChar === '"') {
          currentField += '"';
          i++; // Skip next quote
        } else if (char === '"')
          inQuotes = false;
        else {
          if (char === "\n") line++;
          currentField += char;
        }
      } else {
        if (char === '"')
          inQuotes = true;
        else if (char === ",") {
          currentRow.push(currentField);
          currentField = "";
        } else if (char === "\n" || (char === "\r" && nextChar === "\n")) {
          push(i);
          if (char === "\r") i++; // Skip \n in \r\n
          start = i + 1;
          rowLine = ++line;
        } else if (char !== "\r") {
          currentField += char;
        }
      }
    }

    // Don't forget the last field/row
    if (currentField || currentRow.length > 0) push(text.length);

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

  // A short or long row is the single most common broken CSV, and coercing it
  // loses data silently. Name the line, the counts, and the column it stops at.
  const ragged = dataRows.find((row) => row.fields.length !== headerRow.fields.length);
  if (ragged) {
    const at = Math.min(ragged.fields.length, headerRow.fields.length);
    throw new HTTPException(400, {
      message: explain(`Line ${ragged.line} of the CSV does not match its header.`, {
        Expected: `${headerRow.fields.length} fields: ${headerRow.fields.join(", ")}`,
        Received: `${ragged.fields.length} fields: ${ragged.raw.slice(0, 200)}`,
        Column: ragged.fields.length < headerRow.fields.length
          ? `nothing for "${headerRow.fields[at]}"`
          : `an extra field after "${headerRow.fields[at - 1]}"`,
        Source: `line ${ragged.line} of the uploaded file`,
        Fix: "quote the field that contains a comma, or fill in the missing column",
      }),
    });
  }

  // Every non-blank value must parse, not four in five. At 80% the other fifth
  // became NaN or false silently, which is data loss dressed up as inference.
  const inferType = (values: string[]): string => {
    const filled = values.filter((val) => val.trim());
    if (!filled.length) return "text";
    if (filled.every((val) => !isNaN(Number(val)))) return "num";
    if (filled.every((val) => ["true", "false", "t", "f", "1", "0", "yes", "no"].includes(val.toLowerCase())))
      return "bool";
    return "text";
  };

  // Build column definitions
  const cols: Col[] = headerRow.fields.map((name, i) => {
    const colValues = dataRows.map((row) => row.fields[i] || "");
    return {
      name: name.trim() || `Column ${i + 1}`,
      type: inferType(colValues) as Type,
      key: String(i),
    };
  });

  // Build rows with proper type conversion
  const rows: Row[] = dataRows.map(({ fields }) => {
    const obj: Row = {};
    cols.forEach((col, i) => {
      const val = fields[i] ?? "";
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

// One export route, five renderings. Every format goes through sheet(), so it
// inherits assertSheetAccess and the query/net recursion rather than repeating it.

const csvCell = (val: unknown): string => {
  const str = val === null || val === undefined ? "" : String(val);
  return /[",\n\r]/.test(str) ? '"' + str.replace(/"/g, '""') + '"' : str;
};

// RFC 5545: escape the reserved characters, then fold so no line exceeds 75
// octets. Folding counts bytes but must cut on codepoints: splitting a UTF-8
// sequence in half turns an emoji into two replacement characters.
const icsLine = (name: string, val: string): string => {
  const line = `${name}:${val.replace(/\\/g, "\\\\").replace(/\n/g, "\\n").replace(/([;,])/g, "\\$1")}`;
  const parts: string[] = [];
  let part = "", used = 0;
  for (const ch of line) {
    const size = new TextEncoder().encode(ch).byteLength;
    // A continuation line spends one of its 75 octets on the leading space.
    if (used + size > (parts.length ? 74 : 75)) {
      parts.push(part);
      part = "";
      used = 0;
    }
    part += ch;
    used += size;
  }
  parts.push(part);
  return parts.join("\r\n ");
};

// A date-only value stays a date: an all-day event must not shift by a timezone.
const icsStamp = (val: string): string => {
  const d = new Date(/^\d{4}-\d{2}-\d{2}$/.test(val) ? `${val}T00:00:00Z` : val);
  if (Number.isNaN(d.getTime())) return "";
  const iso = d.toISOString().replace(/[-:]/g, "").replace(/\.\d+/, "");
  return /^\d{4}-\d{2}-\d{2}$/.test(val) ? `;VALUE=DATE:${iso.slice(0, 8)}` : `:${iso}`;
};

const DATE_TYPES = ["date", "timestamp", "create"];

// Records keyed by column name, for the formats that carry names into every row.
const named = (sheet_id: string, cols: Col[], rows: Row[]): Record<string, unknown>[] => {
  const seen = cols.map((col) => col.name).filter((name, i, all) => all.indexOf(name) !== i);
  if (seen.length) {
    throw new HTTPException(400, {
      message: explain(`Sheet ${sheet_id} has two columns with the same name.`, {
        Received: `${seen.join(", ")} appears more than once`,
        Source: "the column row of the sheet, or the select list of its query",
        Fix: "alias one of them, e.g. select a, b as b2, then export again",
      }),
    });
  }
  return rows.map((row) => Object.fromEntries(cols.map((col) => [col.name, row[col.key] ?? null])));
};

const EXPORTS: Record<string, { mime: string; render: (id: string, cols: Col[], rows: Row[]) => string }> = {
  csv: {
    mime: "text/csv; charset=utf-8",
    render: (_id, cols, rows) =>
      [
        cols.map((col) => csvCell(col.name)).join(","),
        ...rows.map((row) => cols.map((col) => csvCell(row[col.key])).join(",")),
      ].join("\n"),
  },
  json: {
    mime: "application/json; charset=utf-8",
    render: (id, cols, rows) => JSON.stringify(named(id, cols, rows), null, 2),
  },
  ndjson: {
    mime: "application/x-ndjson; charset=utf-8",
    render: (id, cols, rows) => named(id, cols, rows).map((row) => JSON.stringify(row)).join("\n"),
  },
  md: {
    mime: "text/markdown; charset=utf-8",
    render: (_id, cols, rows) => {
      const cell = (val: unknown) =>
        (val === null || val === undefined ? "" : String(val)).replace(/\|/g, "\\|").replace(/\n/g, "<br>");
      return [
        `| ${cols.map((col) => cell(col.name)).join(" | ")} |`,
        `| ${cols.map(() => "---").join(" | ")} |`,
        ...rows.map((row) => `| ${cols.map((col) => cell(row[col.key])).join(" | ")} |`),
      ].join("\n");
    },
  },
  ics: {
    mime: "text/calendar; charset=utf-8",
    render: (id, cols, rows) => {
      const when = cols.find((col) => DATE_TYPES.includes(String(col.type)));
      if (!when) {
        throw new HTTPException(400, {
          message: explain(`Sheet ${id} has no date column to build a calendar from.`, {
            Received: cols.map((col) => `${col.name} (${JSON.stringify(col.type)})`).join(", ") || "(no columns)",
            Expected: `one column typed ${DATE_TYPES.join(", ")}`,
            Source: "the column row of the sheet",
            Fix: "set a column's type to date, then export .ics again",
          }),
        });
      }
      const title = cols.find((col) => col.key !== when.key && String(col.type) === "text") ?? when;
      const stamp = new Date().toISOString().replace(/[-:]/g, "").replace(/\.\d+/, "");
      const events = rows.flatMap((row, i) => {
        const at = icsStamp(String(row[when.key] ?? ""));
        if (!at) return [];
        return [
          "BEGIN:VEVENT",
          icsLine("UID", `${id}-${i}@sheets.scrap.land`),
          `DTSTAMP:${stamp}`,
          `DTSTART${at}`,
          icsLine("SUMMARY", String(row[title.key] ?? `${id} row ${i + 1}`)),
          icsLine(
            "DESCRIPTION",
            cols.filter((col) => col.key !== when.key && col.key !== title.key)
              .map((col) => `${col.name}: ${row[col.key] ?? ""}`).join("\n"),
          ),
          "END:VEVENT",
        ];
      });
      return [
        "BEGIN:VCALENDAR",
        "VERSION:2.0",
        "PRODID:-//Scrapsheets//EN",
        "CALSCALE:GREGORIAN",
        ...events,
        "END:VCALENDAR",
      ].join("\r\n");
    },
  },
};

app.get(`/export/:id{.+\\.(${Object.keys(EXPORTS).join("|")})}`, async (c) => {
  const raw = c.req.param("id");
  const format = raw.slice(raw.lastIndexOf(".") + 1);
  const sheet_id = raw.slice(0, raw.lastIndexOf("."));
  // sheet() paginates net and query sheets at 50 rows; an export wants the whole sheet.
  const { data } = await sheet(c, sheet_id, { limit: "100000", ...c.req.query() });
  const [colsRow, ...rows] = data;
  if (!colsRow) {
    throw new HTTPException(400, {
      message:
        `Expected sheet ${sheet_id} to have a column row, received a document with no rows at all. Source: data[0] of the automerge document. Add a column before exporting.`,
    });
  }
  const { mime, render } = EXPORTS[format];
  return new Response(render(sheet_id, Object.values(colsRow) as Col[], rows), {
    headers: {
      "Content-Type": mime,
      "Content-Disposition": `attachment; filename="${sheet_id.replace(/[^a-zA-Z0-9-_]/g, "_")}.${format}"`,
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
      db.dsn = await decrypt("connection string", db.dsn);
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
    select ${sheet_id}, ${await encrypt(dsn)}
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
