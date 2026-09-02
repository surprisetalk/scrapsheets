// The whole server, in one file on purpose.
//
// It was measured against splitting at the `// ---` sections below. Every
// candidate -- polling, status, the MCP endpoint -- needs `sql`, `app`, `sheet`
// and `safeFetch`, all defined here, so each new file either imports back from
// this one (a cycle) or takes them as arguments (which changes the signatures
// main_test.ts calls). Both cost more than the size does. The section comments
// are the navigation; keep them accurate and keep new code inside one of them.

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
import * as path from "@std/path";
import examplesSql from "./examples.sql" with { type: "text" };
import { DATASETS } from "./src/examples.mjs";
import { PORTALS } from "./src/portals.mjs";
import {
  applyWindows,
  chartSql,
  checkColumnTypes,
  checkQueryRows,
  checkResultColumns,
  canonicalType,
  COLUMN_TYPES,
  DESCRIBE_COLUMNS,
  describeRef,
  describeRows,
  explain,
  formatQueryError,
  loadRefs,
  MAX_QUERY_MS,
  nearest,
  NUMERIC_TYPES,
  planQuery,
  register,
  scanRefs,
  selectTypes,
  show,
  WINDOW_TYPES,
} from "./src/sql.mjs";
import Stripe from "stripe";

// --- refusals
//
// Every refusal this server raises is one call, and every one carries the four
// fields: what was expected, what arrived, where it came from, and what to do
// about it. A message without them is visibly the odd one out here, which is
// the point -- a vague error outranks the bug behind it.
//
// The few `throw new HTTPException` left are passthroughs: their message is an
// explain() block src/sql.mjs already built, and re-wrapping it would replace a
// message about the query with one about the request.

type Status = ConstructorParameters<typeof HTTPException>[0];
type Fields = Record<string, string | undefined>;

// The type annotation is on the binding rather than the arrow, which is what
// lets a call read as terminal: TypeScript narrows control flow past a
// never-returning call only when the callee is declared that way, and without
// it every `bad()` standing in for a `throw` would leave the code after it
// re-checking values the refusal already ruled out.
const bad: (status: Status, headline: string, fields: Fields) => never = (status, headline, fields) => {
  throw new HTTPException(status, { message: explain(headline, fields) });
};

// What a caught thing has to say for itself. `throw "nope"` is legal and lands
// here as the string, rather than as "undefined".
const reason = (err: unknown) => (err instanceof Error ? err.message : String(err));

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
    bad(500, `Could not decrypt the ${what}.`, {
      Received: "ciphertext this key does not open",
      Expected: "a value encrypted under the current DSN_ENCRYPTION_KEY",
      Source: "DSN_ENCRYPTION_KEY, which every stored secret and DSN is sealed with",
      Fix: `restore the previous DSN_ENCRYPTION_KEY, or save the ${what} again under this one`,
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

/** Caps a map by insertion order, oldest key first out. Sweeping by idle time
 * alone loses to a caller minting keys faster than the 60-second broom runs, so
 * every map that grows with traffic is capped here as well. */
const bound = <V>(map: Map<string, V>, max: number): void => {
  while (map.size > max) map.delete(map.keys().next().value!);
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
    bound(rateLimitBuckets, RATE_LIMIT_KEYS_MAX);
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

// How many keys one traffic-keyed map may hold. Shared by the rate-limit
// buckets, the per-sheet delivery budgets and the poller's two due-time maps,
// because a second number would be a second thing to tune and none of the four
// wants a different one.
export const RATE_LIMIT_KEYS_MAX = 10_000;

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
    bad(400, `I could not load the sheet "@${id}".`, {
      "Did you mean": hit ? `@${hit}` : undefined,
      Loaded: hit ? undefined : Object.keys(loaded).join(", ") || "(no @sheet is referenced)",
      Source: "the @sheet refs in this query",
      Fix: hit ? `write @${hit} instead` : "reference the sheet as @type:doc_id so it loads before the query runs",
    });
  }
  return typeof cb === "function" ? cb(rows, idx, query) : rows;
};

export type Tag<T extends string, X extends Row[]> = {
  type: T;
  data: X;
};

// --- types

// The compile-time half of COLUMN_TYPES in src/sql.mjs, which is the runtime
// list. TypeScript cannot read a union out of an untyped .mjs, so the names are
// written twice here and browser_test.ts fails when the two stop agreeing.
export type Type =
  | "text"
  | "num"
  | "int"
  | "float"
  | "usd"
  | "percentage"
  | "bool"
  | "date"
  | "timestamp"
  | "json"
  | "link"
  | "image"
  | "sheet_id"
  | "form"
  | "create"
  // The read-only aliases. They are here because a document holds them and this
  // is the type a Col carries. Nothing writes one, which COLUMN_TYPES says.
  | "number"
  | "string"
  | "pct"
  | "percent"
  | "datetime"
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
export type Query = { lang: "sql"; code: string; args: Args };
// `cursor` names the query parameter this feed takes a since-value in. The
// watermark itself is not here: it is the poller's, not the user's.
export type NetHttp = { url: string; interval: number; headers?: string; cursor?: string };
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
  // A prefix, the way Template already spells it and the way the sheet table's
  // own check constraint reads it. No sheet is ever typed the bare "codex":
  // that arm named a type that cannot exist while refusing the two that do.
  | Tag<`codex-${string}`, []>;

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
    bad(403, `You do not have read access to ${sheet_id}.`, {
      Received: exists ? `no membership, no purchase, and the sheet is not public` : `no sheet with that id exists`,
      Expected: "a share on the sheet, a purchase of its listing, or sheet.public",
      Source: "sheet_usr, the payment-derived buy_id/sell_id link, and sheet.public",
      Fix: exists
        ? `ask an owner to run POST /library/${sheet_id}/share with your email, or POST /library/${sheet_id}/public`
        : "check the id: it is type:doc_id, e.g. table:abc123",
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
    bad(403, `Only an owner can change ${sheet_id}.`, {
      Received: mine ? `your role on this sheet is ${mine.role}` : "you have no role on this sheet",
      Expected: "role owner, or having created the sheet",
      Source: "sheet_usr.role and sheet.created_by",
      Fix: `ask an owner to run POST /library/${sheet_id}/share with your email and role owner`,
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
  // viewer grant, everybody else reads the rows their own account caused, and a
  // failure nobody was signed in for carries no usr_id, so it stays the
  // operator's alone. Safe to widen because the row stores header and query
  // names only, never values.
  if (sheet_id === ERROR_SHEET) {
    // A string, always: the auth middleware refuses a token that names no
    // account, so no route reaching this line has an anonymous caller.
    const usr_id = c.get("usr_id");
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
        bad(404, `Sheet ${sheet_id} has a row but no document.`, {
          Expected: "an automerge document",
          Received: "none",
          Source: `doc_id ${doc_id}`,
          Fix:
            `the document is missing or unreadable; re-create the sheet, or claim it again with PUT /library/${sheet_id}`,
        });
      },
    }));
  switch (type) {
    case "table": {
      const [colsRow, ...rows] = hand.doc().data as Table;
      const cols = Object.values(colsRow ?? {}) as Col[];
      // Values with no names left to key them by. Every read is name-keyed, so
      // this sheet's rows would come back as one empty object each -- N rows of
      // nothing, reported as a healthy answer. Refused instead, and refused
      // only when there is something to lose: an empty sheet is empty, and a
      // document with no column row at all is handed back as it is, because the
      // callers that refuse that by name read data[0] to decide.
      if (!cols.length && rows.length) {
        bad(400, `Sheet ${sheet_id} holds rows under no columns.`, {
          Received: `${rows.length} rows and a column row naming nothing`,
          Expected: "one column per value in a row",
          Source: "data[0] of the automerge document",
          Fix: "undo the column deletion in the sheet, which is where the values are still reachable",
        });
      }
      if (!colsRow) return { data: hand.doc().data as Table, count: 0, offset: 0 };
      // Keyed by name, the way every other branch of this switch already
      // answers -- cselect stamps `key: col.name` off the postgres column and
      // executeSql stamps AlaSQL's columnid -- and the way POST /sheet/:id
      // already takes them. `key` is the document's own spelling: minted as a
      // position, stable across a rename, and none of the caller's business. It
      // stays inside the document, which is what the page syncs and edits.
      //
      // Restamped onto the column row as well, so every consumer that reads a
      // cell as row[col.key] -- toRecords, all five exports -- keeps working
      // off the answer rather than off a second convention.
      return {
        data: [arrayify(cols.map((col) => ({ ...col, key: col.name }))), ...named(sheet_id, cols, rows)],
        count: rows.length,
        offset: 0,
      };
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
        throw new HTTPException(400, { message: reason(err) });
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
      bad(400, `A template is a listing, not a sheet with rows.`, {
        Expected: "a readable sheet",
        Received: `the template ${sheet_id}`,
        Source: "the type prefix on this sheet id",
        Fix: "buy it with POST /buy/:sell_id and read the copy you get back",
      });
    case "portal":
      bad(400, `A portal has no stored rows.`, {
        Expected: "a readable sheet",
        Received: `the portal ${sheet_id}`,
        Source: "the type prefix on this sheet id",
        Fix: `read it live over GET /portal/${doc_id}/sync, or through GET /portal/:id if you bought it`,
      });
    default:
      bad(400, `That is not a sheet type this server can read.`, {
        Expected: "table, net-hook, net-http, net-socket, alert, chart, dashboard, or query",
        Received: show(type),
        Source: `the type prefix on sheet id ${sheet_id}`,
        Fix: "fix the type prefix on the id",
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
    throw new HTTPException(400, { message: reason(err) });
  };

  // Source column types by name, and then what each select item makes of them.
  // Two sheets with a column of one name and two types still collide here; the
  // select list is read last, so a query's own alias wins over both.
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
        bad(400, `I could not load the sheet "@${sheet_id}".`, {
          "Did you mean": `@${hit}`,
          Source: "the @sheet refs in this query",
          Fix: `write @${hit} instead`,
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
  // The columns the referenced sheets actually have, taken before anything the
  // query invents is mixed in: this is what a typo is matched against, and a
  // name only this query knows is not evidence that the sheets hold it.
  // `min(code) as lowest` typed itself text and so excused its own empty column.
  const known = Object.keys(nameToType);
  // The author's own text, not the scanned rewrite, so the page infers from the
  // same characters. An item selectTypes cannot type is left out, which leaves
  // the source column of that name below -- what typing did before this existed.
  Object.assign(nameToType, selectTypes(sqlCode, nameToType) as Record<string, Type>);

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
    const followed = nameToType[w.args[0]] ?? "num";
    // An average of whole numbers is not a whole number. The one demotion
    // itemType() makes for a select item, made here too, because a window and a
    // select item spelling one name have to mean one type.
    nameToType[w.alias] = declared ?? (w.fn === "avg" && followed === "int" ? "num" : followed);
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
    checkResultColumns(cols, rows, known, sqlCode);
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
  if (lang !== "sql") {
    bad(400, `A query sheet is SQL.`, {
      Expected: `"sql"`,
      Received: show(lang ?? null),
      Source: `the "lang" field of this query sheet`,
      Fix: `set lang to "sql"; it is the only language this engine runs`,
    });
  }
  return await executeSql(c, code, path_);
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
  if (!rateLimit(callerIp(c))) {
    bad(429, `Too many requests from this address.`, {
      Expected: `a burst of at most ${RATE_LIMIT_MAX_TOKENS}, refilling at ${RATE_LIMIT_REFILL_RATE} per second`,
      Received: "one request past that",
      Source: "the address this request arrived from",
      Fix: "wait a second and send it again",
    });
  }

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
    throw new Error(`seed() failed applying examples.sql/DATASETS: ${reason(err)}`);
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
    const query = c.req.query();
    const auth = await verifyWsAuth(query.auth, query.pass);
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

// What a share-link password is proved against. The "link:" prefix
// domain-separates it from hookSecret(), which derives every sender's signing
// key from the same root: one secret, two uses, and neither may produce the
// other's output. The sheet id is inside the message, so a password that opens
// one link does not open a link to another sheet.
const linkMessage = (sheet_id: string, password: string): Uint8Array<ArrayBuffer> =>
  new TextEncoder().encode(`link:${sheet_id}:${password}`);

const verifyWsAuth = async (auth: string | undefined, pass?: string): Promise<WsAuth> => {
  if (!auth) return { usr_id: null, share: null };
  const token = auth.startsWith("Bearer ") ? auth.slice(7) : auth;
  const payload = await verify(token, JWT_SECRET, JWT_ALG).catch(() => null);
  // An unreadable or expired token is anonymous, not an error: an expired
  // share link has to read as "no access", which is what expiring it means.
  if (!payload) return { usr_id: null, share: null };
  const share = (payload.share as string) ?? null;
  const lock = payload.lock as string | undefined;
  // This is the one place the share claim is read, so it is the one place the
  // lock can be enforced -- a link gated on the socket and open anywhere else
  // is worse than no gate. A locked link is refused rather than quietly
  // downgraded to no access, so the reader is told which check failed instead
  // of watching a sheet never load. Neither refusal prints the password or the
  // digest: the digest is what an offline guesser would grind against, and the
  // same oracle rule the webhook signature refusals follow applies here.
  if (share && lock) {
    if (!pass) {
      bad(401, `This link to ${share} is locked.`, {
        Received: "a link with no password beside it",
        Expected: "the password the link was minted with",
        Source: "the lock claim on the share token",
        Fix: "add &pass=<password> to the link, or ask whoever sent it for the password",
      });
    }
    // crypto.subtle.verify does the comparison, so no digest equality is
    // written by hand here either.
    if (!await hookVerify(TOKEN_SECRET, linkMessage(share, pass), lock)) {
      bad(401, `That password does not open this link to ${share}.`, {
        Received: `a password of ${pass.length} characters`,
        Expected: "the password the link was minted with",
        Source: "the lock claim on the share token, recomputed under the server's own key",
        Fix: "check the password for a typo, or ask the owner to mint a new link",
      });
    }
  }
  return { usr_id: (payload.sub as string) ?? null, share };
};

// --- live portals

// deno-lint-ignore no-explicit-any
const portal = (name: string, ms: number, init: () => any, tick: (s: any) => { cols: any[]; rows: any[] }) =>
  app.get(
    `/portal/${name}/sync`,
    upgradeWebSocket(async (c) => {
      const { auth, pass } = c.req.query();
      // A portal is a public synthetic stream: this validates the token and
      // discards the role on purpose, since a portal honours no share claim and
      // an absent token is anonymous rather than an error. `pass` rides along so
      // that a locked share link is not refused by the one socket where it was
      // never going to grant anything anyway.
      await verifyWsAuth(auth, pass);
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

// The feeds themselves are synthetic demo data with no server logic in them,
// so they live in src/portals.mjs beside the rest of the bundled make-believe.
// This loop and the wrapper above are the server's half: one socket per feed,
// and the auth check.
for (const feed of PORTALS) portal(feed.name, feed.ms, feed.init, feed.tick);

// --- public routes

app.use("*", cors());

app.notFound((c) => {
  bad(404, `No route answers that.`, {
    Expected: "a known path and method",
    Received: `${c.req.method} ${new URL(c.req.url).pathname}`,
    Source: "this request line",
    Fix: "check the path and the method",
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
  bound(logSeen, LOG_KEYS_MAX);
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

// One broom, every RATE_LIMIT_WINDOW_MS. It evicts rate-limit buckets and
// per-sheet delivery budgets that have gone idle, drops the poller's due times
// that have already passed, and flushes the fold counts of bursts that stopped
// -- every one of those maps grows with traffic, every one is capped by bound(),
// and every one is swept on the same minute LOG_EVERY_MS folds on. An idle
// budget is a full one, so dropping it loses nothing.
setInterval(() => {
  const now = Date.now();
  for (const [key, bucket] of rateLimitBuckets) {
    if (now - bucket.lastRefill > RATE_LIMIT_WINDOW_MS)
      rateLimitBuckets.delete(key);
  }
  for (const [key, bucket] of hookBuckets) {
    if (now - bucket.lastRefill > HOOK_WINDOW_S * 1000)
      hookBuckets.delete(key);
  }
  // A due time already past says exactly what the map's absence says, so the
  // entry is dead weight -- and this is how a sheet that was deleted, or a host
  // nothing points at any more, stops being remembered.
  for (const [key, due] of netDue) if (due <= now) netDue.delete(key);
  for (const [key, due] of hostDue) if (due <= now) hostDue.delete(key);
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

// Which rows of `net` are one sheet's runs, and which of those went well --
// per sheet type, because a run is a different event for each. A net-http
// sheet's run is the poll it made; an alert's is the tick it fired; a codex
// sheet's is the connection GET /codex/:id opened; a net-socket sheet's is a
// browser reporting what it saw of the socket; a net-hook sheet's run is
// the delivery it was sent, and every delivery that reached the table landed,
// since a refused one is never stored. Both name `s` and `n`, and RUN_OF is
// spliced into freshness() three times -- the count subquery and both laterals
// -- so it lives here rather than as three hand-copied copies, which is exactly
// how POLL_OK and ALERT_OK came to live here.
//
// Searched rather than `case s.type when`, because a codex type is a prefix
// (codex-db, codex-scrapsheets) and not one string to compare against. A codex
// run is graded by POLL_OK: it records the same `meta.status` a poll does, so
// grading it by a second predicate spelling the same rule is how the two copies
// this pair exists to prevent get made.
const RUN_OF = () =>
  sql`case when s.type = 'alert' then n.method = 'ALERT'
           when s.type = 'net-http' then n.method = 'GET'
           when s.type like 'codex-%' then n.method = 'CODEX'
           when s.type = 'net-socket' then n.method = 'SOCKET'
           else true end`;
const RUN_OK = () =>
  sql`case when s.type = 'alert' then (${ALERT_OK()})
           when s.type = 'net-http' or s.type = 'net-socket' or s.type like 'codex-%' then (${POLL_OK()})
           else true end`;

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
      Received: show(n),
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
    bad(500, `A condition excused from paging is not one this check grades.`, {
      Received: missing.join("; "),
      Expected: "a sentence that status() actually returns",
      Source: "REPORTED_ONLY",
      Fix: "match the sentence exactly, or it silently starts paging again",
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
    bad(502, `Stripe did not answer.`, {
      Expected: `Stripe to ${what}`,
      Received: reason(err),
      Source: "api.stripe.com",
      Fix: "check STRIPE_SECRET_KEY and that api.stripe.com is reachable",
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
    bad(404, `No shop listing with that sell_id.`, {
      Expected: `a live listing with sell_id ${sell_id}`,
      Received: "no row",
      Source: "the sheet table, where a listing is a row with a sell_id",
      Fix: "the listing may have been taken down; re-read GET /shop",
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
    bad(400, `That listing does not say what it sells.`, {
      Expected: `a sell_type on listing ${sell_id}`,
      Received: "null",
      Source: "the sheet row behind this listing",
      Fix: "only templates and live sheets can be sold; the seller must re-list it",
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
    bad(403, `That listing is not yours to buy.`, {
      Expected: `listing ${sell_id} to be purchasable by usr ${usr_id}`,
      Received: "no row",
      Source: "the insert selects from sheet where sell_id matches and the listing is live",
      Fix: "the listing was taken down, or you already own it, or you are its seller",
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
    bad(500, `This server cannot verify a Stripe webhook.`, {
      Expected: "STRIPE_WEBHOOK_SECRET, a Stripe webhook signing secret starting with whsec_",
      Received: "nothing",
      Source: "the process environment",
      Fix: "set STRIPE_WEBHOOK_SECRET in the environment and restart",
    });
  }
  const sig = c.req.header("stripe-signature");
  if (!sig) {
    bad(400, `This delivery is not signed.`, {
      Expected: "a stripe-signature header",
      Received: "none",
      Source: "the request headers",
      Fix: "Stripe signs every webhook; configure the endpoint and STRIPE_WEBHOOK_SECRET",
    });
  }
  const raw = await c.req.text();
  let event: Stripe.Event;
  try {
    event = await Stripe.webhooks.constructEventAsync(raw, sig, secret);
  } catch (err) {
    bad(400, `That stripe-signature does not verify.`, {
      Expected: "a stripe-signature matching STRIPE_WEBHOOK_SECRET",
      Received: reason(err),
      Source: "the raw request body, as sent",
      Fix: "check STRIPE_WEBHOOK_SECRET and that the raw body is not parsed before verification",
    });
  }
  if (event.type !== "checkout.session.completed") return c.json(null, 200);
  const session = event.data.object;
  if (session.payment_status !== "paid") return c.json(null, 200);
  if (typeof session.id !== "string" || !session.id) {
    bad(400, `That event carries no Checkout Session id.`, {
      Expected: "a checkout.session id",
      Received: show(session.id),
      Source: "the Stripe event body",
      Fix: "create the session through POST /buy/:id",
    });
  }
  const usr_id = session.metadata?.usr_id;
  const sell_id = session.metadata?.sell_id;
  if (!usr_id || !sell_id) {
    bad(400, `That Checkout Session says nothing about what was bought.`, {
      Expected: "metadata.usr_id and metadata.sell_id",
      Received: show(session.metadata),
      Source: "the Checkout Session's metadata",
      Fix: "create the session through POST /buy/:id",
    });
  }
  const [buyer] = await sql`select usr_id from usr where usr_id::text = ${usr_id}`;
  if (!buyer) {
    bad(400, `That Checkout Session names no buyer this server knows.`, {
      Expected: "metadata.usr_id to be a usr",
      Received: show(usr_id),
      Source: "the usr table",
      Fix: "create the session through POST /buy/:id",
    });
  }
  const amount_total = session.amount_total;
  if (typeof amount_total !== "number" || !Number.isInteger(amount_total) || amount_total < 1) {
    bad(400, `That Checkout Session names no amount.`, {
      Expected: "amount_total, a whole number of cents above zero",
      Received: show(amount_total),
      Source: "the Checkout Session",
      Fix: "the session must be a paid Checkout Session",
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
    bad(500, `Sheet ${sheet_id} is configured for more than one signing scheme.`, {
      Received: `secrets named ${names.sort().join(", ")}`,
      Expected: "at most one of " + Object.keys(HOOK_HEADERS).join(", "),
      Source: "the secret table",
      Fix: `delete the ones that do not apply with DELETE /library/${sheet_id}/secret`,
    });
  }
  const name = names[0] ?? "hook";
  if (!HOOK_HEADERS[name]) {
    bad(500, `Sheet ${sheet_id} names a signing scheme this server does not know.`, {
      Received: `a secret named ${JSON.stringify(name)}`,
      Expected: Object.keys(HOOK_HEADERS).join(", "),
      Source: "the secret table",
      Fix: `delete it with DELETE /library/${sheet_id}/secret`,
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

// bad(401) under a name that says which refusal it is. None of these prints the
// secret or the expected digest: a rejection that does is a signing oracle. Each
// names its own check, because "401" tells a sender nothing about which half of
// the handshake it got wrong.
const unsigned: (message: string, fields: Fields) => never = (message, fields) => bad(401, message, fields);

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
        Received: show(signature),
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
        Received: show(signature),
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
      Received: show(signature),
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

// --- delivery budgets
//
// NET_BODY_CAP bounds one delivery and trimNet bounds one sheet, but nothing
// bounded the sender: inside the global limiter's 100 requests a second, one
// machine churns all NET_KEEP rows in ten seconds and every delivery the sheet
// held before it is gone. The key is the **sheet**, not callerIp(): a webhook
// sender is one machine that will not rotate its address, and what is being
// protected is this sheet's row budget rather than this caller's share of the
// server.
//
// Two bounds, because they fail differently. A count alone lets one 1 MB body a
// second through; a byte budget alone lets a million empty rows through. They
// share one bucket rather than two maps: one key, one refill, one broom, one
// thing to bound.
export const hookBuckets = new Map<string, { rows: number; bytes: number; lastRefill: number }>();
export const HOOK_WINDOW_S = 60;
export const HOOK_ROWS_PER_WINDOW = 60;
export const HOOK_BYTES_PER_WINDOW = 4 * NET_BODY_CAP;

/** This sheet's budget, refilled for the time since it was last read. Exported
 * because the test drives the key cap through the same function the handler
 * calls, rather than a second one that could agree with a bug in the first. */
export const hookBucket = (sheet_id: string): { rows: number; bytes: number; lastRefill: number } => {
  const now = Date.now();
  let bucket = hookBuckets.get(sheet_id);
  if (!bucket) {
    bucket = { rows: HOOK_ROWS_PER_WINDOW, bytes: HOOK_BYTES_PER_WINDOW, lastRefill: now };
    hookBuckets.set(sheet_id, bucket);
    // A sheet id is not mintable at will, but a bound nothing enforces is not a
    // bound.
    bound(hookBuckets, RATE_LIMIT_KEYS_MAX);
  }
  const elapsed = (now - bucket.lastRefill) / 1000;
  bucket.rows = Math.min(HOOK_ROWS_PER_WINDOW, bucket.rows + elapsed * HOOK_ROWS_PER_WINDOW / HOOK_WINDOW_S);
  bucket.bytes = Math.min(HOOK_BYTES_PER_WINDOW, bucket.bytes + elapsed * HOOK_BYTES_PER_WINDOW / HOOK_WINDOW_S);
  bucket.lastRefill = now;
  return bucket;
};

app.post("/net/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const heard = [...c.req.raw.headers.keys()].sort().join(", ") || "(none)";
  // Every rejection below names the delivery, not just the status: a webhook
  // sender sees only the response body, so the body has to carry the diagnosis.
  //
  // This 404 and the 400 under it answer before the signature is checked, which
  // is an existence oracle to anyone already holding a doc_id. Accepted: a
  // doc_id is 22 unguessable characters, and a sender who cannot tell "no such
  // sheet" from "wrong secret" has nothing to debug with.
  const [target] = await sql`select type from sheet where sheet_id = ${sheet_id}`;
  if (!target) {
    bad(404, `No sheet with id ${sheet_id} accepts deliveries.`, {
      Received: `POST /net/${sheet_id} carrying headers ${heard}`,
      Expected: "the sheet_id of an existing net-hook, net-http, or net-socket sheet",
      Source: "the sheet table",
      Fix: `create the sheet first with PUT /library/net-hook:<doc_id>, then post to /net/net-hook:<doc_id>`,
    });
  }
  if (!String(target.type).startsWith("net-")) {
    bad(400, `Sheet ${sheet_id} does not accept deliveries.`, {
      Received: `a ${target.type} sheet`,
      Expected: "a net-hook, net-http, or net-socket sheet",
      Source: "sheet.type",
      Fix: "post to a net-hook sheet, or change this sheet's type prefix",
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
    bad(400, `This delivery to ${sheet_id} carries a byte that cannot be stored.`, {
      Received: `a NUL byte at offset ${nul} of ${size}`,
      Expected: "a body with no NUL bytes; every other byte, valid UTF-8 or not, is kept as sent",
      Source: "the request body, against the text column it is stored in",
      Fix: "send text or JSON; base64 the payload if it is binary",
    });
  }
  if (size > NET_BODY_CAP) {
    bad(413, `This delivery to ${sheet_id} is too large to store.`, {
      Received: `${size} bytes`,
      Limit: `${NET_BODY_CAP} bytes per delivery`,
      Source: "the request body",
      Fix: "send the payload in pages, or post a URL the sheet can fetch instead",
    });
  }
  // Placed here on purpose, between the checks that cost nothing and the four
  // round trips that follow. After the 404 and the 400, because a budget must
  // be keyed on a sheet that exists -- keyed before them, a caller minting ids
  // mints map keys -- and because those two already answer before the signature
  // is checked, so a 429 here tells a caller holding a doc_id nothing the 404
  // did not. Before verifyDelivery, the insert and the trim, which is what a
  // refusal is worth shedding. Not before the body is read: the byte bound must
  // spend the bytes actually sent rather than the content-length a flooder
  // declares, and the body is on the wire either way, so refusing sooner saves
  // nothing. A 429 is the one status app.onError does not log, so shedding load
  // stays the cheap path.
  const budget = hookBucket(sheet_id);
  if (budget.rows < 1) {
    bad(429, `Sheet ${sheet_id} has taken too many deliveries.`, {
      Received: `a delivery of ${size} bytes with no delivery budget left`,
      Limit: `${HOOK_ROWS_PER_WINDOW} deliveries per ${HOOK_WINDOW_S} seconds, for this sheet`,
      Source: "this sheet's delivery budget, which refills continuously",
      Fix: `batch the events into fewer deliveries, or retry in ${
        Math.ceil((1 - budget.rows) * HOOK_WINDOW_S / HOOK_ROWS_PER_WINDOW)
      } seconds`,
    });
  }
  if (budget.bytes < size) {
    bad(429, `Sheet ${sheet_id} has taken too many bytes.`, {
      Received: `${size} bytes against ${Math.floor(budget.bytes)} left in this window`,
      Limit: `${HOOK_BYTES_PER_WINDOW} bytes per ${HOOK_WINDOW_S} seconds, for this sheet`,
      Source: "this sheet's byte budget, which refills continuously",
      Fix: `send a smaller body, or retry in ${
        Math.ceil((size - budget.bytes) * HOOK_WINDOW_S / HOOK_BYTES_PER_WINDOW)
      } seconds`,
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
    bad(409, `This delivery to ${sheet_id} has already been stored.`, {
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
    });
  }
  // Charged only by a delivery that landed. A 401 or a 409 spends nothing:
  // charging every attempt would let one attacker replaying one captured
  // delivery, or posting junk at an id it guessed, exhaust the budget of the
  // sender that sheet belongs to -- the opposite of what the budget protects.
  // The cost is that an unsigned flood is shed by callerIp()'s global limiter
  // alone, and each of its requests still pays the secret lookup a spent budget
  // would have refused before. Read again rather than reusing the bucket
  // checked above, because the awaits between them span an eviction, and
  // charging a bucket the map no longer holds charges nobody.
  const spent = hookBucket(sheet_id);
  spent.rows -= 1;
  spent.bytes -= size;
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

// How many hops one fetch may follow. Named rather than written twice: the
// refusal at the end of the loop quotes the same number the loop counts to.
const REDIRECT_MAX = 5;

// SSRF guard: block internal hosts by literal IP and by resolved DNS; follow redirects manually re-validating each hop.
const safeFetch = async (start: string, headers: Record<string, string> = {}): Promise<Response> => {
  let url = start;
  for (let hop = 0; hop < REDIRECT_MAX; hop++) {
    const u = new URL(url);
    if (!["http:", "https:"].includes(u.protocol)) {
      bad(400, `That is not a url this server will fetch.`, {
        Expected: "an http:// or https:// url",
        Received: show(u.protocol),
        Source: "the url this sheet was pointed at",
        Fix: "point it at an http or https url",
      });
    }
    const host = u.hostname.replace(/^\[|\]$/g, "");
    if (host === "localhost" || host.endsWith(".local")) {
      bad(400, `That url points inside this server's own network.`, {
        Expected: "a public host",
        Received: u.hostname,
        Source: "the url this sheet was pointed at",
        Fix: "point it at a host reachable from the public internet",
      });
    }
    const isLiteral = /^[0-9.]+$/.test(host) || host.includes(":");
    const ips = isLiteral ? [host] : (await Promise.all(
      (["A", "AAAA"] as const).map((t) => Deno.resolveDns(host, t).catch(() => [] as string[])),
    )).flat();
    if (ips.some(ipBlocked)) {
      bad(400, `That host resolves inside this server's own network.`, {
        Expected: "a host resolving to a public address",
        Received: `${u.hostname} -> ${ips.join(", ")}`,
        Source: "the DNS answer for this url",
        Fix: "point it at a host reachable from the public internet",
      });
    }
    const res = await fetch(url, {
      redirect: "manual",
      // Ten seconds, not thirty: the poller runs on a 15s tick, and a request
      // that can outlive two of them is a cycle that overlaps the next. The
      // page waiting on /proxy has less patience than that anyway.
      signal: AbortSignal.timeout(10_000),
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
  bad(400, `That url redirected too many times.`, {
    Expected: `at most ${REDIRECT_MAX} redirects`,
    Received: `${REDIRECT_MAX} of them, still not a final answer`,
    Source: `the redirect chain from ${url}`,
    Fix: "fetch the url the chain ends at directly",
  });
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

// Both hold a *future* due time: one entry per net-http sheet that ever polled
// and one per host that ever answered, deleted sheets and dead hosts included,
// so neither is bounded by anything the deployment holds. Evicting a live entry
// re-polls that sheet or host early and loses nothing, which is what makes
// insertion order a safe thing to evict on -- the oldest key can go without
// asking what it was for.
export const netDue = new Map<string, number>();
// A host that asks to be retried later asked every sheet pointed at it, not
// only the one that heard it. Two sheets on one API otherwise take turns
// stampeding the host that just said stop.
export const hostDue = new Map<string, number>();

// The most failures in a row one feed retries before it stops and waits for its
// own interval again. The row that gives up names this number and the count.
const RETRY_MAX = 3;
// The first retry delay, doubled per attempt. A retry is scheduled and never
// slept: a cycle that waited here would still be waiting when the next tick
// arrived, and a Retry-After of an hour would hold every other sheet with it.
const RETRY_BACKOFF_MS = 30_000;
// A cycle starts no further sheets past this. That budget, plus the single
// request a sheet makes and safeFetch's 10s timeout, is how one cycle stays
// inside the 15s tick that drives it; the tick itself refuses to start a second
// cycle while one is still running, which covers a database that hangs too.
const POLL_CYCLE_MS = 3_000;
// A body arrives in chunks, and a host that trickles empty ones is a loop the
// byte cap alone cannot end.
const BODY_CHUNKS_MAX = 10_000;

// RFC 9110: Retry-After is a delay in seconds or an HTTP-date. One that is
// neither is this sheet's failure row -- the poller keeps its other sheets.
const retryAfterMs = (raw: string, now: number): number => {
  // Nine digits, because shape is not magnitude and this value is multiplied.
  if (/^[0-9]{1,9}$/.test(raw.trim())) return Number(raw.trim()) * 1000;
  const at = Date.parse(raw);
  if (Number.isNaN(at)) {
    throw new Error(
      explain("This host asked to be retried at a time that cannot be read.", {
        Received: `Retry-After: ${JSON.stringify(raw)}`,
        Expected: "a delay in seconds, or an HTTP-date",
        Source: "the response headers on this poll",
        Fix: "the header is the host's to fix; until it is, this sheet waits its own interval",
      }),
    );
  }
  // A date already past means now.
  return Math.max(0, at - now);
};

// A validator is only worth keeping if it can be sent back exactly as it
// arrived, so an absurd one is dropped rather than stored and replayed forever.
const validator = (raw: string | null): string | null => (raw && raw.length <= 200 ? raw : null);

// The body, up to one byte past the cap. That byte is all it takes to know the
// response is too large, and reading the rest of a runaway is the cost the cap
// exists to refuse.
const readBody = async (res: Response): Promise<Uint8Array> => {
  const reader = res.body?.getReader();
  if (!reader) return new Uint8Array();
  const chunks: Uint8Array[] = [];
  let size = 0;
  for (let chunk = 0; size <= NET_BODY_CAP; chunk++) {
    if (chunk >= BODY_CHUNKS_MAX) {
      throw new Error(
        explain("This response arrived in more pieces than a body may take.", {
          Received: `${chunk} chunks holding ${size} bytes`,
          Expected: `at most ${BODY_CHUNKS_MAX} chunks`,
          Source: "the response stream on this poll",
          Fix: "the host is trickling or sending empty chunks; point the sheet at another endpoint",
        }),
      );
    }
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    size += value.byteLength;
  }
  await reader.cancel().catch(() => {});
  const buf = new Uint8Array(size);
  let offset = 0;
  for (const chunk of chunks) {
    buf.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return buf;
};

// Every write to this log trims behind itself, the failures included: a feed
// that only ever fails used to grow until the first success trimmed it.
const netRow = async (sheet_id: string, body: string, meta: Record<string, unknown>): Promise<void> => {
  await sql`insert into net (sheet_id, method, body, meta) values (${sheet_id}, 'GET', ${body}, ${sql.json(meta)})`;
  await trimNet(sheet_id);
};

export const pollNetOnce = async (fetcher = safeFetch, now = Date.now()): Promise<void> => {
  const sheets = await sql`select sheet_id, doc_id from sheet where type = 'net-http'`;
  const cycleStart = Date.now();
  for (const { sheet_id, doc_id } of sheets) {
    if ((netDue.get(sheet_id) ?? 0) > now) continue;
    // A cycle that has spent its budget stops starting sheets rather than
    // running long. Nothing here has moved this sheet's due time yet, so the
    // next tick takes it.
    if (Date.now() - cycleStart > POLL_CYCLE_MS) break;
    netDue.set(sheet_id, now + 3600_000);
    // The only set here that can introduce a key: every later one this cycle is
    // this same sheet, and Map.set on a key it holds keeps its position.
    bound(netDue, RATE_LIMIT_KEYS_MAX);
    // Declared out here so the catch can name the URL and headers it failed on.
    let url = `sheet ${sheet_id}`, headers: Record<string, string> = {};
    try {
      const config = (await automerge.find<{ data: [NetHttp] }>(doc_id)).doc()?.data?.[0];
      if (!config) throw new Error("The document has no config in data[0].");
      netDue.set(sheet_id, now + Math.max(60, Number(config.interval) || 3600) * 1000);
      if (!config.url) continue;
      url = config.url;
      headers = parseNetHeaders(config.headers);
      const host = new URL(url).hostname;
      const holdoff = hostDue.get(host) ?? 0;
      // Waiting out what another sheet on this host was told. Nothing ran, so
      // there is nothing to log.
      if (holdoff > now) {
        netDue.set(sheet_id, holdoff);
        continue;
      }
      // The last row is where this feed's state lives: the validators the last
      // good body carried, the watermark it was fetched at, and how many
      // failures have happened in a row since. It lives in `net.meta` rather
      // than in the automerge document because that document is what sync hands
      // every viewer and what the user edits -- a poller writing to it every
      // tick would fight those edits and mint a change for every open browser.
      const [prev]: {
        net_id: string;
        meta: { etag?: string; last_modified?: string; cursor?: string; attempt?: number };
      }[] = await sql`
        select net_id, meta from net where sheet_id = ${sheet_id} order by net_id desc limit 1
      `;
      const was = prev?.meta ?? {};
      // Resolved into a separate object. `headers` is what a failure row is
      // built from, so a resolved token cannot reach the log even if curlFor
      // one day prints more than the keys. The conditional headers are ours and
      // not the sheet's, for that rule in reverse: the repro line's job is to
      // fetch the body by hand, and a conditional request answers 304.
      const sending = {
        ...await resolveSecrets(sheet_id, headers),
        ...(was.etag ? { "If-None-Match": was.etag } : {}),
        ...(was.last_modified ? { "If-Modified-Since": was.last_modified } : {}),
      };
      // The watermark is when the last good poll started, not when it finished:
      // a row created while that request was in flight is asked for twice
      // rather than missed once. It rides every good run, so a sheet that
      // gains a cursor later resumes from its last poll instead of re-reading
      // history it has already logged.
      if (config.cursor) {
        if (!/^[A-Za-z0-9_.-]{1,64}$/.test(config.cursor)) {
          throw new Error(
            explain("This sheet's cursor is not the name of a query parameter.", {
              Received: show(config.cursor),
              Expected: "letters, digits, dot, dash or underscore, at most 64 of them",
              Source: "the cursor field on this net-http sheet",
              Fix: "name the parameter this feed takes a since-value in, such as `since` or `updated_after`",
            }),
          );
        }
        const target = new URL(url);
        if (was.cursor) target.searchParams.set(config.cursor, was.cursor);
        url = target.href;
      }
      const started = Date.now();
      // A string instead of a response is a failure off the wire: a timeout, a
      // reset. Everything safeFetch refuses on its own arrives as an
      // HTTPException instead, because a private address or a redirect loop
      // answers a retry exactly as it answered this one.
      const res = await fetcher(url, sending).catch((err) => {
        if (err instanceof HTTPException) throw err;
        return reason(err);
      });
      // A 5xx and a 429 are the host saying "later". A 404, a 401, an SSRF
      // refusal are a "no", and retrying a "no" is noise on top of the failure
      // row that already answered it.
      if (typeof res === "string" || res.status === 429 || res.status >= 500) {
        const answered = typeof res === "string" ? null : res;
        const detail = typeof res === "string" ? res : new TextDecoder().decode(await readBody(res));
        const after = answered?.headers.get("retry-after") ?? null;
        // The wait is the host's and not this sheet's, so it holds every sheet
        // pointed there. It is a floor and not a schedule: this sheet takes the
        // later of it and its own backoff, which is what the host asked for.
        if (after !== null) {
          hostDue.set(host, now + retryAfterMs(after, now));
          bound(hostDue, RATE_LIMIT_KEYS_MAX);
        }
        const attempt = Number(was.attempt ?? 0) + 1;
        // Read back out of jsonb, so it is guarded like every other read out of
        // jsonb: a count that is not a whole number lands as NaN in the next
        // due time, and a sheet due at NaN is due on every tick forever.
        if (!Number.isInteger(attempt) || attempt < 1) {
          throw new Error(
            explain("This feed's last run recorded a failure count that cannot be read.", {
              Received: `attempt: ${JSON.stringify(was.attempt)}`,
              Expected: "a whole number of failures in a row, or nothing at all",
              Source: `meta on the newest row of ${sheet_id}`,
              Fix: "delete that row; the next poll starts the count again",
            }),
          );
        }
        if (attempt >= RETRY_MAX) {
          throw new Error(
            explain(`This feed has failed ${attempt} polls in a row, which is the retry bound.`, {
              Received: `${attempt} failures, the last of them: ${detail.slice(0, 200)}`,
              Expected: `a 2xx within ${RETRY_MAX} attempts`,
              Source: url,
              Fix: "fix the feed or the sheet; the next scheduled poll starts the count again",
            }),
          );
        }
        const wait = Math.max(hostDue.get(host) ?? 0, now + RETRY_BACKOFF_MS * 2 ** (attempt - 1));
        netDue.set(sheet_id, wait);
        await netRow(
          sheet_id,
          JSON.stringify({
            ...fetchFailure(url, headers, answered, detail),
            attempt,
            retry_at: new Date(wait).toISOString(),
          }),
          { status: answered?.status ?? 0, ms: Date.now() - started, bytes: detail.length, attempt },
        );
        continue;
      }
      const kept = {
        etag: validator(res.headers.get("etag")) ?? was.etag ?? null,
        last_modified: validator(res.headers.get("last-modified")) ?? was.last_modified ?? null,
        cursor: new Date(now).toISOString(),
      };
      if (res.status === 304) {
        // A 304 is a healthy poll that appended nothing: the feed answered, and
        // what it answered is that the row already here is still current.
        // POLL_OK and library:freshness both grade meta->>'status' between 200
        // and 299, so the run is recorded as the 200 it semantically is and
        // `not_modified` keeps the status that came off the wire -- otherwise a
        // daily file polled hourly reads as a dead feed. Moving that row's
        // created_at rather than appending an empty one is what a quiet alert
        // tick does: liveness reads max(created_at), and 23 blank rows a day
        // would push the feed's own data out past NET_KEEP.
        if (!prev || !(was.etag || was.last_modified)) {
          throw new Error(
            explain("This feed answered 304 to a request that carried no validator.", {
              Received: "HTTP 304 Not Modified",
              Expected: "a 2xx, because this poll sent no If-None-Match and no If-Modified-Since",
              Source: url,
              Fix: "the host is answering a conditional request nobody made; point the sheet elsewhere",
            }),
          );
        }
        await sql`
          update net
          set created_at = now(),
              meta = ${sql.json({ ...kept, status: 200, not_modified: true, ms: Date.now() - started, bytes: 0 })}
          where net_id = ${prev.net_id}
        `;
        continue;
      }
      const raw = await readBody(res);
      // A body over the cap is a failure row naming both numbers. A truncated
      // success is a parse error further downstream, blamed on the data.
      if (res.ok && raw.byteLength > NET_BODY_CAP) {
        throw new Error(
          explain("This feed's response is too large to store.", {
            Received: `${res.headers.get("content-length") ?? `at least ${raw.byteLength}`} bytes`,
            Limit: `${NET_BODY_CAP} bytes per response`,
            Source: url,
            Fix: "point the sheet at a paged or filtered endpoint, or give it a cursor",
          }),
        );
      }
      const text = new TextDecoder().decode(raw);
      // Errors become log rows too: the user who typed the URL must see them, and
      // must be able to run the same request by hand.
      const body = res.ok ? text : JSON.stringify(fetchFailure(url, headers, res, text));
      // The run beside the payload: whether a feed is slow, or 200-ing an error
      // page, is a question about the poll and not about the body it returned.
      // The validators and the watermark ride the good rows only, so the next
      // poll asks its question about the body this sheet actually holds.
      await netRow(sheet_id, body, {
        status: res.status,
        ms: Date.now() - started,
        bytes: raw.byteLength,
        ...(res.ok ? kept : {}),
      });
    } catch (err) {
      const message = reason(err);
      console.error(`net-http poll ${sheet_id}:`, message);
      const failure = fetchFailure(url, headers, null, message);
      // No attempt count: giving up, a malformed Retry-After and a sheet that
      // cannot be read all land here, and the next scheduled poll starts over.
      await netRow(sheet_id, JSON.stringify(failure), { status: 0, ms: 0, bytes: 0 })
        .catch((dbErr: unknown) => console.error(`net-http poll ${sheet_id}: could not record the error:`, dbErr));
    }
  }
};

let polling = false;
setInterval(() => {
  // A tick that finds the cycle before it still running does nothing rather
  // than starting a second one over the same due sheets.
  if (polling) return;
  polling = true;
  pollNetOnce()
    .catch((err) => console.error("net-http poll:", err))
    .finally(() => polling = false);
}, 15_000);

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
          Received: show(config.interval),
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
      const message = reason(err);
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
    if (err instanceof HTTPException) return c.json(fetchFailure(url, {}, null, err.message), err.status);
    return c.json(fetchFailure(url, {}, null, `Fetch failed: ${reason(err)}`), 502);
  }
});

// --- authenticated routes
//
// Two ways in. A person carries a JWT and reaches everything their account can.
// A script carries a key for one sheet, minted by POST /library/:id/secret under
// the name `api`: it is stored encrypted beside every other sheet secret, it
// names its own sheet, and it opens that sheet and nothing else. A key that
// carried the minter's whole authority is the thing this exists to prevent.
const API_KEY_HEADER = "scrapsheets-key";
const API_KEY_NAME = "api";

/** A key for one sheet: the sheet id, a dot, then 32 random bytes. The sheet id
 * rides along so verifying reads at most SECRET_KEEP rows of one sheet rather
 * than decrypting the whole table looking for a match. */
const apiKeyFor = (sheet_id: string): string => `${sheet_id}.${hex(crypto.getRandomValues(new Uint8Array(32)))}`;

// crypto.subtle.verify does the comparison, so no key equality is written by
// hand: HMAC both under one fixed message and let WebCrypto compare the digests.
const API_KEY_PROBE = new TextEncoder().encode(API_KEY_NAME);
const sameKey = async (presented: string, stored: string): Promise<boolean> =>
  await crypto.subtle.verify(
    "HMAC",
    await hmacKey(stored),
    await hookMac(presented, API_KEY_PROBE) as Uint8Array<ArrayBuffer>,
    API_KEY_PROBE,
  );

/** Who a presented key is, or a 401 that is not a key oracle: it never prints a
 * key, a digest, or how close the one it received was. */
const apiKeyScope = async (presented: string): Promise<{ usr_id: string; sheet_id: string }> => {
  const cut = presented.lastIndexOf(".");
  const sheet_id = cut < 0 ? "" : presented.slice(0, cut);
  const refuse = () =>
    new HTTPException(401, {
      message: explain(`That ${API_KEY_HEADER} is not a key this server issued.`, {
        Received: `a key naming ${sheet_id ? sheet_id : "no sheet at all"}`,
        Expected: `the key POST /library/:id/secret answered with for that sheet`,
        Source: `the ${API_KEY_HEADER} request header`,
        Fix: `mint one with POST /library/<sheet_id>/secret {"name":"${API_KEY_NAME}"}; it is shown once, so a lost ` +
          `key is replaced rather than recovered`,
      }),
    });
  // Current and previous, the same rotation rule every other secret reads by.
  const rows = sheet_id
    ? await sql`
      select value_encrypted from secret
      where sheet_id = ${sheet_id} and name = ${API_KEY_NAME}
      order by created_at desc, secret_id desc
      limit ${SECRET_KEEP}
    `
    : [];
  for (const row of rows) {
    if (!await sameKey(presented, await decrypt(`${API_KEY_NAME} key`, String(row.value_encrypted)))) continue;
    // The identity a key borrows is the sheet's creator: the key stands for one
    // sheet, and the path check below is what bounds it, not the account.
    const [owner] = await sql`select created_by from sheet where sheet_id = ${sheet_id}`;
    if (!owner?.created_by) throw refuse();
    return { usr_id: String(owner.created_by), sheet_id };
  }
  throw refuse();
};

const jwtAuth = jwt({ secret: JWT_SECRET, alg: JWT_ALG });

app.use("*", async (c, next) => {
  const presented = c.req.header(API_KEY_HEADER);
  // No key header and the request takes exactly the path it always did.
  if (!presented) return await jwtAuth(c, next);
  const { usr_id, sheet_id } = await apiKeyScope(presented);
  // A colon may arrive percent-encoded. One that cannot be decoded at all is
  // not this key's sheet either, so it falls to the refusal below rather than
  // to a URIError and a 500.
  const path = (() => {
    try {
      return decodeURIComponent(c.req.path);
    } catch {
      return c.req.path;
    }
  })();
  // The scope is checked on the path, before routing, so no handler has to
  // remember to ask -- and a key that reached POST /library/:id/secret could
  // mint itself a key for a sheet it was never given.
  if (path !== `/sheet/${sheet_id}` && path !== `/openapi/${sheet_id}`) {
    bad(403, `This key opens ${sheet_id} and nothing else.`, {
      Received: `${c.req.method} ${path}`,
      Expected: `GET or POST /sheet/${sheet_id}, or GET /openapi/${sheet_id}`,
      Source: `the ${API_KEY_HEADER} request header, which names the one sheet it is for`,
      Fix: "mint a key on that sheet too, or call this route with the Authorization header of an account that " +
        "can reach it",
    });
  }
  c.set("usr_id", usr_id);
  await next();
});

// The one place a token becomes an identity, and so the one place a token that
// carries none is refused. A share link is a real token this server minted, but
// it claims a sheet and no `sub`: verifyWsAuth reads that claim on the sync
// socket, and no HTTP route reads it at all. Every route past here interpolates
// usr_id into SQL, so an identity-less one arrived at postgres.js as
// UNDEFINED_VALUE -- a 500, a row in the operator's log and a point off the 5xx
// grade, for a caller using the credential we handed them. The guard lives here
// rather than beside each of those queries because there is one way in and six
// places to forget, and because the honest answer is about the token and not
// about the sheet it was pointed at.
app.use("*", async (c, next) => {
  const usr_id = c.get("jwtPayload")?.sub ?? c.get("usr_id");
  if (!usr_id) {
    const share = c.get("jwtPayload")?.share;
    bad(403, "That token opens the sync socket and nothing else.", {
      Received: share ? `a share link to ${share}, which names a sheet and no account` : "a token with no sub claim",
      Expected: "the token POST /login answered with",
      Source: "the Authorization header",
      Fix: share
        ? "open the link in the app, which reads it over the sync socket, or sign in and call this route with " +
          "that account's token"
        : "sign in again with POST /login",
    });
  }
  c.set("usr_id", usr_id);
  await next();
});

app.post("/buy/:id", async (c) => {
  const sell_id = c.req.param("id");
  const usr_id = c.get("usr_id");
  const [sheet] = await sql`select * from sheet where sell_id = ${sell_id} and sell_price >= 0`;
  if (!sheet) {
    bad(404, `No shop listing with that sell_id.`, {
      Expected: `a live listing with sell_id ${sell_id}`,
      Received: "no row",
      Source: "the sheet table, where a listing is a row with a sell_id",
      Fix: "the listing may have been taken down; re-read GET /shop",
    });
  }
  if (!sheet.sell_type) {
    bad(400, `That listing does not say what it sells.`, {
      Expected: `a sell_type on listing ${sell_id}`,
      Received: "null",
      Source: "the sheet row behind this listing",
      Fix: "only templates and live sheets can be sold; the seller must re-list it",
    });
  }
  const dollars = Number(sheet.sell_price);
  if (!Number.isFinite(dollars)) {
    bad(400, `That listing has no price this server can charge.`, {
      Expected: "sell_price, a number of dollars",
      Received: show(sheet.sell_price),
      Source: `sheet ${sheet.sheet_id}`,
      Fix: "re-list the sheet with a numeric price",
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
    bad(400, `That price is smaller than a cent.`, {
      Expected: "a sell_price of at least $0.01, or 0 to give it away",
      Received: String(sheet.sell_price),
      Source: `sheet ${sheet.sheet_id}`,
      Fix: "re-list at a whole-cent price or 0",
    });
  }
  const key = Deno.env.get("STRIPE_SECRET_KEY");
  if (!key) {
    bad(500, `This server cannot take a payment.`, {
      Expected: "STRIPE_SECRET_KEY, a Stripe secret key starting with sk_",
      Received: "nothing",
      Source: "the process environment",
      Fix: "set STRIPE_SECRET_KEY in the environment and restart",
    });
  }
  const stripe = new Stripe(key);
  const [usr] = await sql`select email, stripe_customer_id from usr where usr_id = ${usr_id}`;
  if (!usr) {
    bad(401, `This token names an account that no longer exists.`, {
      Expected: `a usr row for usr_id ${usr_id}`,
      Received: "no row",
      Source: "the usr table",
      Fix: "sign in again",
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
      bad(500, `The Stripe customer was created and not written down.`, {
        Expected: "usr.stripe_customer_id to be set after creating a Stripe customer",
        Received: `null for usr_id ${usr_id}`,
        Source: "the usr row this purchase updated",
        Fix: "retry the purchase",
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
    bad(502, `Stripe made a session with nowhere to send you.`, {
      Expected: "a Checkout Session url",
      Received: `none, for session ${session.id}`,
      Source: "the Stripe Checkout Session",
      Fix: "retry the purchase",
    });
  }
  return c.json({ data: { checkout_url: session.url } }, 200);
});

app.post("/sell/:id", async (c) => {
  const body = await c.req.json();
  const { price } = body;
  if (price === undefined) {
    bad(400, `A listing needs a price.`, {
      Expected: `a "price" field in the body`,
      Received: show(body),
      Source: "the request body",
      Fix: `post {"price": 0} to list it for free`,
    });
  }
  // An alert holds someone's email address and sends mail on a timer. Selling
  // copies would hand a stranger a thing that emails the seller.
  if (c.req.param("id").startsWith("alert:")) {
    bad(400, `An alert cannot be listed for sale.`, {
      Received: c.req.param("id"),
      Cause: "an alert carries a destination address and sends mail on its own, so a copy would mail its author",
      Fix: "sell the query the alert watches instead, and let the buyer point their own alert at it",
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
  if (!updated.length) {
    bad(400, `A purchased sheet cannot be resold.`, {
      Expected: "a sheet you created",
      Received: `${c.req.param("id")}, which was bought, or is not yours`,
      Source: "the sheet row this update looked for",
      Fix: "sell a sheet of your own, or fork this one and sell the fork",
    });
  }
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
                 and (${RUN_OF()})
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
        where n.sheet_id = s.sheet_id and (${RUN_OF()})
        order by n.created_at desc, n.net_id desc limit 1
      ) last on true
      left join lateral (
        select n.created_at, n.net_id from net n
        where n.sheet_id = s.sheet_id
          and (${RUN_OF()})
          -- The same predicates GET /status grades on, read from the same
          -- place, so the two cannot drift into disagreeing about what a good
          -- run is. They already had.
          and (${RUN_OK()})
        order by n.created_at desc, n.net_id desc limit 1
      ) ok on true
      -- Every type whose runs are recorded here: a net-http sheet's polls, a
      -- net-hook sheet's deliveries, an alert's ticks, a codex sheet's
      -- connections. Deliberately not "sheets that have rows in net", because a
      -- webhook nobody has delivered to is the failure this read is for -- the
      -- same argument the two left joins above carry.
      --
      -- A codex sheet is in, and its "never run" is not a false alarm: a
      -- connection has no poller, so the only moment anybody learns whether the
      -- far database still answers is when somebody opens the sheet, and a
      -- connection nobody has ever opened is a connection nobody has ever
      -- verified. That is the fact, and the read states it.
      --
      -- A net-socket sheet is in only once it has a run, which is the one
      -- exception to the paragraph above, and POST /library/:id/socket says
      -- why: only a browser can witness that socket, so the column means "last
      -- seen open by a browser" and a sheet nobody has watched is absent rather
      -- than "never run" forever.
      where su.usr_id = ${c.get("usr_id")}
        and (
          s.type in ('net-http', 'net-hook', 'alert')
          or s.type like 'codex-%'
          or (s.type = 'net-socket'
              and exists (select 1 from net n2 where n2.sheet_id = s.sheet_id and n2.method = 'SOCKET'))
        )
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
    bad(403, `That sheet is already claimed by someone else.`, {
      Expected: `write access to ${sheet_id} for usr ${usr_id}`,
      Received: "no owner or editor row",
      Source: "sheet_usr membership",
      Fix: "ask an owner to share it with you",
    });
  }

  // Sheet doesn't exist - create it (user is claiming a new automerge doc)
  const [type, doc_id] = sheet_id.split(":");
  if (!type || !doc_id) {
    bad(400, `That is not a sheet id.`, {
      Expected: "type:doc_id, e.g. table:abc123",
      Received: show(sheet_id),
      Source: "the :id in this path",
      Fix: "pass the full id, type prefix included",
    });
  }

  // Verify the automerge document exists. The doc may live only on the caller's
  // client, so grant sync access for the fetch window.
  grantSync(usr_id, doc_id);
  const row_0 = await automerge
    .find<{ data: Table }>(doc_id as AnyDocumentId)
    .then((hand) => hand.doc()?.data?.[0] ?? {})
    .catch(() => {
      bad(404, `The document behind that sheet never arrived.`, {
        Expected: `automerge document ${doc_id} to be reachable`,
        Received: "none",
        Source: "the sync server, which reads it from your client during the claim",
        Fix: "keep the tab open and retry, so the document can be pushed",
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
    bad(404, `This server has never heard of that sheet.`, {
      Expected: `a sheet row for ${sheet_id}`,
      Received: "none",
      Source: "the sheet table",
      Fix: `claim it first with PUT /library/${sheet_id}`,
    });
  }
  return c.json({ data: { members: rows, public: sheet.public } });
});

app.post("/library/:id/share", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  // A missing or malformed body is a 400 naming what arrived. Unguarded, the
  // json() throw became an unexplained 500, and cost a row in the error log
  // for a request the caller could have fixed from the message.
  const { email, role } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  if (typeof email !== "string" || !email.includes("@")) {
    bad(400, `A share needs somebody to share with.`, {
      Expected: "an email address",
      Received: show(email),
      Source: `the "email" field of the request body`,
      Fix: `post {"email": "them@example.com", "role": "viewer"}`,
    });
  }
  if (!ROLES.includes(role)) {
    bad(400, `That is not a role a share can grant.`, {
      Expected: ROLES.join(", "),
      Received: show(role),
      Source: `the "role" field of the request body`,
      Fix: `pick one of ${ROLES.join(", ")}`,
    });
  }

  const [target] = await sql`select usr_id from usr where email = ${email}`;
  if (!target) {
    bad(404, `Nobody here goes by that address.`, {
      Expected: `a usr with email ${email}`,
      Received: "no row",
      Source: "the usr table",
      Fix: "they need to sign up before the sheet can be shared with them",
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
  const { email } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  // Unvalidated, a missing email reached the delete and came back as the 404
  // "undefined is not a non-owner member of this sheet" -- a sentence about the
  // sheet for a mistake in the request.
  if (typeof email !== "string" || !email.includes("@")) {
    bad(400, `A removal needs somebody to remove.`, {
      Expected: "an email address",
      Received: show(email),
      Source: `the "email" field of the request body`,
      Fix: `post {"email": "them@example.com"}`,
    });
  }
  const [removed] = await sql`
    delete from sheet_usr
    where sheet_id = ${sheet_id}
      and usr_id = (select usr_id from usr where email = ${email})
      and role <> 'owner'
    returning usr_id
  `;
  if (!removed) {
    bad(404, `There was nothing to remove.`, {
      Expected: `${email} to be an editor or viewer of this sheet`,
      Received: "no such row",
      Source: "sheet_usr membership; an owner cannot be removed this way",
      Fix: "check the address, or read the members with GET /library/:id/share",
    });
  }
  invalidateSync(sheet_id.split(":")[1]);
  return c.json(null, 200);
});

// Public toggles the anonymous read path; both go through syncRole.
app.post("/library/:id/public", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { public: isPublic } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  if (typeof isPublic !== "boolean") {
    bad(400, `A sheet is either public or it is not.`, {
      Expected: "true or false",
      Received: show(isPublic),
      Source: `the "public" field of the request body`,
      Fix: `post {"public": true} or {"public": false}`,
    });
  }
  await sql`update sheet set public = ${isPublic} where sheet_id = ${sheet_id}`;
  invalidateSync(sheet_id.split(":")[1]);
  return c.json({ data: { public: isPublic } });
});

// What a browser saw of a net-socket sheet's connection.
//
// Nothing server-side ever opens that socket, so a tab with the sheet open is
// the only witness there is. `library:freshness` grades what lands here, which
// is why the row carries `meta.status`: a poll, a codex connection and a socket
// report are graded by one predicate rather than by three spellings of it.
//
// Two states, because only two can be told honestly. A close is not reported --
// changeId() closes this socket on every navigation, so a close is a fact about
// which sheet you are looking at rather than about the feed.
//
// Nothing rides along beside the status. The one thing a page could add -- which
// url failed -- is already on the sheet, and it is unbounded text the page did
// not choose, so a field for it is a field the page overflows by accident.
const SOCKET_STATES: Record<string, number> = { connected: 200, error: 502 };

app.post("/library/:id/socket", async (c) => {
  const sheet_id = c.req.param("id");
  // Owner or editor, read the way the sync socket reads it, and read before
  // anything is written. A viewer holding a share link watches the same socket
  // and must not write its health; and a stranger holding a doc_id must not
  // write failures into somebody else's freshness, which is why GET /codex/:id
  // checks access before its clock starts too.
  const role = await syncRole({ usr_id: c.get("usr_id") ?? null, share: null }, sheet_id.split(":")[1] ?? "");
  if (role !== "owner" && role !== "editor") {
    bad(403, `You do not have write access to ${sheet_id}.`, {
      Received: role ? `your role on this sheet is ${role}` : "you have no role on this sheet",
      Expected: "role owner or editor",
      Source: "sheet_usr.role, the same read the sync socket makes",
      Fix: `ask an owner to run POST /library/${sheet_id}/share with your email and role editor`,
    });
  }
  // syncRole answers on doc_id alone, so a role on table:abc passes the check
  // above for net-socket:abc, and a second colon leaves the tail out of it
  // entirely. The row this insert names has to exist or the foreign key refuses
  // it as an unexplained 500 -- and the budget below would be keyed on an id the
  // caller minted, which is how one editor evicts every other sheet's budget.
  const [target] = await sql`select type from sheet where sheet_id = ${sheet_id}`;
  if (!target || target.type !== "net-socket") {
    bad(404, `No net-socket sheet answers to ${sheet_id}.`, {
      Received: target ? `a ${target.type} sheet` : "no sheet with that id",
      Expected: "an existing sheet of type net-socket",
      Source: "the sheet table",
      Fix: "post to /library/net-socket:<doc_id>/socket, with the id the sheet itself carries",
    });
  }
  // Parsed, not destructured. `.catch` covers a body that is not JSON; a body
  // that is JSON `null` parses fine and then throws on the destructuring, which
  // is a 500 anyone can send.
  const body = await c.req.json().catch(() => null);
  const status = (body as { status?: unknown } | null)?.status;
  // hasOwn, not `in`. `in` walks the prototype, so "__proto__", "toString" and
  // every other Object.prototype key answered 200 and wrote a run whose
  // meta.status was an object -- which POLL_OK grades as a failure forever, on a
  // socket that never failed.
  if (typeof status !== "string" || !Object.hasOwn(SOCKET_STATES, status)) {
    bad(400, `That is not something a socket can be.`, {
      Received: show(status),
      Expected: Object.keys(SOCKET_STATES).join(" or "),
      Source: "the status field of the request body",
      Fix: 'post {"status": "connected"} when it opens and {"status": "error"} when it fails',
    });
  }
  // The same budget a webhook sender spends, keyed on the same sheet: a page
  // stuck in a reconnect ladder is a sender that will not stop, and one bucket
  // with one refill and one broom is the whole answer to that. Taken after the
  // 403 and the 404 for the reason POST /net/:id takes it there.
  const budget = hookBucket(sheet_id);
  if (budget.rows < 1) {
    bad(429, `Sheet ${sheet_id} has reported on its socket too many times.`, {
      Received: "a report with no budget left",
      Limit: `${HOOK_ROWS_PER_WINDOW} reports per ${HOOK_WINDOW_S} seconds, for this sheet`,
      Source: "this sheet's delivery budget, which refills continuously",
      Fix: `report only when the connection changes state, or retry in ${
        Math.ceil((1 - budget.rows) * HOOK_WINDOW_S / HOOK_ROWS_PER_WINDOW)
      } seconds`,
    });
  }
  await sql`
    insert into net (sheet_id, method, body, meta)
    values (${sheet_id}, 'SOCKET', ${status}, ${sql.json({ status: SOCKET_STATES[status] })})
  `;
  // Read again rather than charging the object above, for the reason
  // POST /net/:id reads again: the await between them spans an eviction, and
  // charging a bucket the map no longer holds charges nobody.
  hookBucket(sheet_id).rows -= 1;
  await trimNet(sheet_id);
  return c.json({ data: { status } });
});

// A view-only link is a JWT scoped to one sheet. The sync socket already accepts
// ?auth=<jwt>, so this needs no new read path.
//
// A bearer credential that travels in a URL may not be immortal, so the owner
// picks how long it lives, bounded rather than free. An empty body is the
// page's own call and must keep minting the unlocked 30-day link it always did.
const LINK_DAYS_MAX = 365;
// A password a person types and reads out over the phone, not a key file.
const LINK_PASSWORD_MAX = 128;

app.post("/library/:id/link", async (c) => {
  const sheet_id = c.req.param("id");
  await assertSheetOwner(c, sheet_id);
  const { days, password } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  if (days !== undefined && (typeof days !== "number" || !(days > 0) || days > LINK_DAYS_MAX)) {
    bad(400, `That is not a usable lifetime for a link to ${sheet_id}.`, {
      Received: show(days),
      Expected: `a number above 0 and at most ${LINK_DAYS_MAX}`,
      Source: "the days field of the request body",
      Fix: "send days as a JSON number, or leave it out for a link that lives 30 days",
    });
  }
  if (password !== undefined && (typeof password !== "string" || !password || password.length > LINK_PASSWORD_MAX)) {
    bad(400, `That is not a usable password for a link to ${sheet_id}.`, {
      // The value only reaches the message when it is not a string, so a real
      // password can never be printed back.
      Received: typeof password !== "string" ? JSON.stringify(password) : `${password.length} characters`,
      Expected: `a non-empty string of at most ${LINK_PASSWORD_MAX} characters`,
      Source: "the password field of the request body",
      Fix: "send the password as a JSON string, or leave it out for a link anyone holding the url can open",
    });
  }
  const token = await sign(
    {
      share: sheet_id,
      // The password is never stored and never travels. What rides the token is
      // an HMAC of it under TOKEN_SECRET, which the reader's password has to
      // reproduce -- keyed by a server secret, so holding the link buys nobody
      // an offline guess at the password.
      ...(password === undefined ? {} : { lock: hex(await hookMac(TOKEN_SECRET, linkMessage(sheet_id, password))) }),
      exp: Math.floor(Date.now() / 1000 + 60 * 60 * 24 * (days ?? 30)),
    },
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
    bad(400, `Sheet ${sheet_id} has no delivery endpoint.`, {
      Received: target ? `a ${target.type} sheet` : "no sheet row",
      Expected: "a net-hook, net-http, or net-socket sheet",
      Source: "sheet.type",
      Fix: "ask for the hook of a net-hook sheet",
    });
  }
  // The current key, which is the newest stored one if this sheet has any and
  // the derived one otherwise. A sheet configured for a provider has no line to
  // print: the secret is the provider's, and we never had it to give back.
  const { name, keys } = await hookKeys(sheet_id);
  if (name !== "hook") {
    bad(400, `Sheet ${sheet_id} is signed by ${name.slice("hook:".length)}, not by scrapsheets.`, {
      Received: `a sheet holding a ${name} secret`,
      Expected: "a sheet on scrapsheets' own signing scheme",
      Source: "the secret table",
      Fix: `read the endpoint secret from ${name.slice("hook:".length)}, or delete the ${name} secret with ` +
        `DELETE /library/${sheet_id}/secret`,
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
    bad(400, `That is not a usable secret name on ${sheet_id}.`, {
      Received: show(name),
      Expected: "1 to 64 characters of a-z, 0-9, colon, underscore or hyphen, starting with a letter or digit",
      Source: "the name field of the request body",
      Fix: `name it for what it is: hook for this sheet's own signing secret, or one of ${
        Object.keys(HOOK_HEADERS).filter((k) => k !== "hook").join(", ")
      } to have that provider verify deliveries instead`,
    });
  }
  // `api` is this sheet's own API key, and nothing else in that space is a name
  // the middleware knows. An unknown one is refused where it is typed, the same
  // way an unknown hook scheme is: written happily and then useless is worse.
  if (name.startsWith(`${API_KEY_NAME}:`)) {
    bad(400, `${JSON.stringify(name)} is not a key name this server knows.`, {
      Received: name,
      Expected: API_KEY_NAME,
      Source: "the name field of the request body",
      Fix: `use ${API_KEY_NAME}, or a name that does not start with ${API_KEY_NAME}:`,
    });
  }
  // The server mints an API key rather than taking one: a key the owner chose is
  // a key the owner reused somewhere else, and there is no way to check that
  // from here. Writing one is rotating it, like every other secret on the sheet.
  const minted = name === API_KEY_NAME ? apiKeyFor(sheet_id) : null;
  if (minted && value !== undefined) {
    bad(400, `An ${API_KEY_NAME} key is minted here, not supplied.`, {
      Received: `a value field alongside name ${API_KEY_NAME}`,
      Expected: `just {"name":"${API_KEY_NAME}"}; the key comes back in the answer, once`,
      Source: "the value field of the request body",
      Fix: "drop the value field, and store what this route answers with",
    });
  }
  if (!minted && (typeof value !== "string" || !value || value.length > SECRET_VALUE_CAP)) {
    bad(400, `That is not a usable secret value on ${sheet_id}.`, {
      Received: typeof value !== "string" ? `a ${typeof value}` : `${value.length} characters`,
      Expected: `a non-empty string of at most ${SECRET_VALUE_CAP} characters`,
      Source: "the value field of the request body",
      Fix: "send the secret as a JSON string; a key file belongs in a codex connection, not here",
    });
  }
  // `hook` and `hook:*` are the names verifyDelivery reads, so a name in that
  // space that no verifier knows would be written happily here and then fail
  // every delivery to this sheet. Refused where it is typed instead.
  const scheme = name === "hook" || name.startsWith("hook:");
  if (scheme && !HOOK_HEADERS[name]) {
    bad(400, `${JSON.stringify(name)} is not a signing scheme this server knows.`, {
      Received: name,
      Expected: Object.keys(HOOK_HEADERS).join(", "),
      Source: "the name field of the request body",
      Fix: "use one of those, or a name that does not start with hook",
    });
  }
  const [{ names }] = await sql`
    select count(distinct name)::int as names from secret where sheet_id = ${sheet_id} and name <> ${name}
  `;
  if (names >= SECRET_NAMES_MAX) {
    bad(409, `Sheet ${sheet_id} holds as many secrets as it may.`, {
      Received: `${names} names already, and a request to add ${name}`,
      Expected: `at most ${SECRET_NAMES_MAX} distinct names per sheet`,
      Source: "the secret table",
      Fix: `delete one you no longer need with DELETE /library/${sheet_id}/secret`,
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
  const value_encrypted = await encrypt(minted ?? value);
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
    bad(409, `Sheet ${sheet_id} already has a signing scheme.`, {
      Received: `a request to add ${name} beside ${clash.map((r: { name: string }) => r.name).sort().join(", ")}`,
      Expected: "at most one of " + Object.keys(HOOK_HEADERS).join(", ") + " per sheet",
      Source: "the secret table",
      Fix: `delete the other one first with DELETE /library/${sheet_id}/secret`,
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
  if (!minted) return c.json(null, 201);
  // Once, here, and never again: GET answers names and timestamps, and the
  // value is sealed under DSN_ENCRYPTION_KEY behind a comparison that only ever
  // answers yes or no. A long-lived key may not sit in a shared cache either.
  const url = `${new URL(c.req.url).origin}/sheet/${sheet_id}`;
  c.header("Cache-Control", "no-store");
  return c.json({
    data: {
      url,
      key: minted,
      // A key and a sheet id are hex and base58, so neither can close the
      // quoting the way a hand-pasted provider secret can.
      repro: [
        `curl '${url}' -H '${API_KEY_HEADER}: ${minted}'`,
        `# the columns a row must carry, and their types:`,
        `curl '${new URL(c.req.url).origin}/openapi/${sheet_id}' -H '${API_KEY_HEADER}: ${minted}'`,
        `curl -X POST '${url}' -H '${API_KEY_HEADER}: ${minted}' \\`,
        `  -H 'Content-Type: application/json' -d '{"rows":[{"column":"value"}]}'`,
      ].join("\n"),
    },
  }, 201);
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
    bad(404, `Sheet ${sheet_id} holds no secret named ${JSON.stringify(name)}.`, {
      Received: show(name),
      Expected: "a name GET /library/" + sheet_id + "/secret lists",
      Source: "the secret table",
      Fix: "read the names back first; a value is never readable, but a name always is",
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
      bad(400, `That upload carries no file.`, {
        Expected: `a multipart field named "file"`,
        Received: `fields: ${[...formData.keys()].join(", ") || "(none)"}`,
        Source: "the multipart request body",
        Fix: "send the CSV as -F file=@data.csv, or post the raw text with Content-Type: text/csv",
      });
    }
    csvText = await file.text();
    sheetName = file.name.replace(/\.csv$/i, "") || sheetName;
  } else {
    // Raw CSV text in body
    csvText = await c.req.text();
  }

  if (!csvText.trim()) {
    bad(400, `There is nothing in that file.`, {
      Expected: "CSV text",
      Received: `${csvText.length} characters of whitespace`,
      Source: "the request body",
      Fix: "send at least a header row",
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
    bad(400, `Nothing in that file parsed as a row.`, {
      Expected: "at least a header row",
      Received: `${parsed.length} parsed rows from ${csvText.length} characters`,
      Source: "the request body",
      Fix: "check the delimiter and the line endings",
    });
  }

  const [headerRow, ...dataRows] = parsed;

  // A short or long row is the single most common broken CSV, and coercing it
  // loses data silently. Name the line, the counts, and the column it stops at.
  const ragged = dataRows.find((row) => row.fields.length !== headerRow.fields.length);
  if (ragged) {
    const at = Math.min(ragged.fields.length, headerRow.fields.length);
    bad(400, `Line ${ragged.line} of the CSV does not match its header.`, {
      Expected: `${headerRow.fields.length} fields: ${headerRow.fields.join(", ")}`,
      Received: `${ragged.fields.length} fields: ${ragged.raw.slice(0, 200)}`,
      Column: ragged.fields.length < headerRow.fields.length
        ? `nothing for "${headerRow.fields[at]}"`
        : `an extra field after "${headerRow.fields[at - 1]}"`,
      Source: `line ${ragged.line} of the uploaded file`,
      Fix: "quote the field that contains a comma, or fill in the missing column",
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

  // Two columns of one name have no name-keyed row, and every read is
  // name-keyed -- so a file imported with a duplicate header would land as a
  // sheet nothing can read back, refused by a message about the sheet rather
  // than about the file. Refuse it here, where the header that has to change is
  // in front of the person who can change it.
  const headers = headerRow.fields.map((name, i) => name.trim() || `Column ${i + 1}`);
  const twice = headers.filter((name, i) => headers.indexOf(name) !== i);
  if (twice.length) {
    bad(400, `The CSV header names a column more than once.`, {
      Received: `${[...new Set(twice)].map((name) => JSON.stringify(name)).join(", ")} appears more than once`,
      Expected: `one column per name: ${headers.join(", ")}`,
      Source: `line ${headerRow.line} of the uploaded file`,
      Fix: "rename one of them in the file, because a row is keyed by column name",
    });
  }

  // Build column definitions
  const cols: Col[] = headers.map((name, i) => ({
    name,
    type: inferType(dataRows.map((row) => row.fields[i] || "")) as Type,
    key: String(i),
  }));

  // A blank is no value, and it stays one. Number("") is 0, and a blank is in
  // neither half of the boolean list, so the old shape stored "" in a num column
  // and an invented false in a bool one -- a reading nobody took and a fact the
  // file never stated. checkColumnTypes() in src/sql.mjs is the one place a
  // blank becomes a null; an importer that disagrees with it makes the stored
  // document and the query answer two different questions about one file. Only
  // a text column keeps the empty string, because there it is a value.
  const rows: Row[] = dataRows.map(({ fields }) => {
    const obj: Row = {};
    cols.forEach((col, i) => {
      const val = fields[i] ?? "";
      if (col.type === "text")
        obj[col.key] = val;
      else if (!val.trim())
        obj[col.key] = null;
      else if (col.type === "num")
        obj[col.key] = Number(val);
      else
        obj[col.key] = ["true", "t", "1", "yes"].includes(val.toLowerCase());
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

// Records keyed by column name. This is the shape of every read -- sheet()
// projects a table sheet through it, and the name-keyed exports re-label
// through it -- so the refusal below is the one place a name that cannot key a
// row is caught. Two columns of one name collapse into one, which is a wrong
// answer rather than a missing one; a query already collapses them silently in
// toRecords, so refusing here is the loud version of a bug that exists either
// way. An unnamed column is a name, so two of them collide like any other pair.
const named = (sheet_id: string, cols: Col[], rows: Row[]): Record<string, unknown>[] => {
  const seen = cols.map((col) => col.name).filter((name, i, all) => all.indexOf(name) !== i);
  if (seen.length) {
    bad(400, `Sheet ${sheet_id} has two columns with the same name.`, {
      // Quoted, because the commonest pair is two columns nobody has named
      // yet, and `  appears more than once` names nothing.
      Received: `${[...new Set(seen)].map((name) => JSON.stringify(name)).join(", ")} appears more than once`,
      Expected: "one column per name, since a row is keyed by name",
      Source: "the column row of the sheet, or the select list of its query",
      Fix: "rename one of them in the sheet, or alias one in the query, e.g. select a, b as b2",
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
        bad(400, `Sheet ${id} has no date column to build a calendar from.`, {
          Received: cols.map((col) => `${col.name} (${JSON.stringify(col.type)})`).join(", ") || "(no columns)",
          Expected: `one column typed ${DATE_TYPES.join(", ")}`,
          Source: "the column row of the sheet",
          Fix: "set a column's type to date, then export .ics again",
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
    bad(400, `That sheet has no columns to export.`, {
      Expected: `sheet ${sheet_id} to have a column row`,
      Received: "a document with no rows at all",
      Source: "data[0] of the automerge document",
      Fix: "add a column before exporting",
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

// At most one batch, because the whole batch is checked before the document is
// touched and the check holds every row in memory.
const APPEND_ROWS_MAX = 1000;

// The write half of that read. Rows arrive keyed by column **name** -- the
// header a CSV would carry, and the shape checkColumnTypes reads -- and are
// stored keyed by column key, which is what the document holds.
app.post("/sheet/:id", async (c) => {
  const sheet_id = c.req.param("id");
  const [type, doc_id] = sheet_id.split(":");
  // Only a table sheet owns rows. A query, chart or dashboard computes its rows
  // from somewhere else, and a net sheet's arrive signed over POST /net/:id --
  // appending to either would be writing into an answer, not into a sheet.
  if (type !== "table") {
    bad(400, `Sheet ${sheet_id} has no rows of its own to append to.`, {
      Received: `a ${type} sheet`,
      Expected: "a table sheet",
      Source: "the type prefix of the sheet id",
      Fix: type?.startsWith("net-")
        ? `send a signed delivery to POST /net/${sheet_id} instead`
        : "append to the table this sheet reads from instead",
    });
  }
  // Owner or editor, read the way sync reads it, so the two paths cannot
  // disagree about who may write: a viewer holding a share link, and anyone at
  // all on a public sheet, can read this sheet and must not append to it.
  const role = await syncRole({ usr_id: c.get("usr_id") ?? null, share: null }, doc_id);
  if (role !== "owner" && role !== "editor") {
    bad(403, `You do not have write access to ${sheet_id}.`, {
      Received: role ? `your role on this sheet is ${role}` : "you have no role on this sheet",
      Expected: "role owner or editor",
      Source: "sheet_usr.role, the same read the sync socket makes",
      Fix: `ask an owner to run POST /library/${sheet_id}/share with your email and role editor`,
    });
  }
  const { rows } = await c.req.json().catch(() => ({} as Record<string, unknown>));
  if (!Array.isArray(rows) || !rows.length) {
    bad(400, `That is not a batch of rows to append to ${sheet_id}.`, {
      Received: Array.isArray(rows) ? "an empty rows array" : `a ${typeof rows} rows field`,
      Expected: `{"rows":[{...}]}, each row an object keyed by column name`,
      Source: "the rows field of the request body",
      Fix: `read the shape this sheet takes from GET /openapi/${sheet_id}`,
    });
  }
  if (rows.length > APPEND_ROWS_MAX) {
    bad(413, `That is more rows than one append may carry.`, {
      Received: `${rows.length} rows`,
      Expected: `at most ${APPEND_ROWS_MAX} rows per request`,
      Source: "the rows field of the request body",
      Fix: `send it in batches of ${APPEND_ROWS_MAX}, or import the whole file with POST /import/csv`,
    });
  }
  const hand = await automerge.find<{ type: string; data: Table }>(doc_id as AnyDocumentId).catch(() => {
    bad(404, `Sheet ${sheet_id} has a row but no document.`, {
      Expected: "an automerge document",
      Received: "none",
      Source: `doc_id ${doc_id}`,
      Fix:
        `the document is missing or unreadable; re-create the sheet, or claim it again with PUT /library/${sheet_id}`,
    });
  });
  const [colsRow] = hand.doc().data;
  const cols = Object.values(colsRow ?? {}) as Col[];
  if (!cols.length) {
    bad(400, `Sheet ${sheet_id} has no columns to append under.`, {
      Received: "a document whose first row names no columns",
      Expected: "a column row, which is data[0] of the document",
      Source: "the automerge document",
      Fix: "add a column in the sheet first, then append",
    });
  }
  const names = cols.map((col) => col.name);
  // A short or long row is the commonest broken batch, and filling one in loses
  // data silently, exactly as it does in a CSV. Name both counts, the field it
  // stops at, and the row as sent.
  for (const [i, row] of rows.entries()) {
    if (!row || typeof row !== "object" || Array.isArray(row)) {
      bad(400, `Row ${i + 1} of the request is not an object.`, {
        Received: Array.isArray(row) ? `an array: ${JSON.stringify(row).slice(0, 200)}` : `${JSON.stringify(row)}`,
        Expected: `an object keyed by column name: ${names.join(", ")}`,
        Source: `rows[${i}] of the request body`,
        Fix: "key each row by the column names, not by position",
      });
    }
    const keys = Object.keys(row);
    const missing = names.filter((name) => !(name in row));
    const extra = keys.filter((key) => !names.includes(key));
    if (missing.length || extra.length) {
      bad(400, `Row ${i + 1} of the request does not match the sheet's columns.`, {
        Expected: `${names.length} fields: ${names.join(", ")}`,
        Received: `${keys.length} fields: ${JSON.stringify(row).slice(0, 200)}`,
        Column: missing.length ? `nothing for "${missing[0]}"` : `an extra field "${extra[0]}"`,
        Source: `rows[${i}] of the request body`,
        Fix: missing.length
          ? `send every column, with null for the ones you have no value for`
          : `drop "${extra[0]}", or add a column of that name to the sheet`,
      });
    }
    for (const name of names) {
      const val = (row as Row)[name];
      if (val === null || ["string", "number", "boolean"].includes(typeof val)) continue;
      bad(400, `Row ${i + 1}, column "${name}" holds a value no cell can hold.`, {
        Received: `a ${Array.isArray(val) ? "array" : typeof val}: ${JSON.stringify(val ?? null).slice(0, 200)}`,
        Expected: "a string, a number, a boolean, or null",
        Source: `rows[${i}]["${name}"] of the request body`,
        Fix: "send it as a string; a json column holds the JSON text, the way the net read casts jsonb to text",
      });
    }
  }
  // The declared types are checked by the same function every table sheet is
  // checked by as it loads into a query, so a value this route lets through can
  // never be one a query then refuses. Its row numbers are the request's, since
  // nothing has landed yet.
  try {
    checkColumnTypes(sheet_id, cols, rows as Row[]);
  } catch (err) {
    throw new HTTPException(400, { message: reason(err) });
  }
  // All or nothing. Every row is checked above and the append is one change,
  // because automerge cannot roll a change back: a batch half-written under a
  // 201 is the silent failure this endpoint exists without.
  hand.change((doc) => {
    for (const row of rows as Row[])
      doc.data.push(Object.fromEntries(cols.map((col) => [col.key, row[col.name]])) as Row);
  });
  return c.json({ data: { appended: rows.length, rows: hand.doc().data.length - 1 } }, 201);
});

// A column type, as JSON Schema says it. Read off COLUMN_TYPES rather than
// written again here, so a type the engine accepts cannot be missing from the
// spec that documents it. A structured type (an array, a tuple, an object) has
// no scalar spelling, so the lookup below falls back to "anything" rather than
// to a lie.
type JsonShape = { type: string; format?: string };
// The cast is what src/sql.mjs checks when it loads: every entry is a type that
// states a shape, or an alias of one that does. A second check here would be a
// second place to be wrong about the same table.
const JSON_TYPES: Record<string, JsonShape> = Object.fromEntries(
  Object.keys(COLUMN_TYPES as Record<string, unknown>).map((
    type,
  ) => [type, (COLUMN_TYPES as Record<string, { json?: JsonShape }>)[canonicalType(type)].json as JsonShape]),
);

// The spec for one sheet, derived from its own columns at request time and
// never stored, so it cannot drift from the schema it describes. It documents
// the two routes a key opens and the header the key is presented in -- which is
// the whole point: a key with nothing to point at is a key nobody can use.
app.get("/openapi/:id", async (c) => {
  const sheet_id = c.req.param("id");
  // sheet() is the access check and the column row in one read. One row,
  // because the spec is about the shape and not about the rows.
  const [colsRow] = (await sheet(c, sheet_id, { limit: "1" })).data;
  const cols = Object.values(colsRow ?? {}) as Col[];
  const url = `/sheet/${sheet_id}`;
  const row = {
    type: "object",
    additionalProperties: false,
    required: cols.map((col) => col.name),
    properties: Object.fromEntries(cols.map((col) => [col.name, JSON_TYPES[String(col.type)] ?? {}])),
  };
  return c.json({
    openapi: "3.1.0",
    info: { title: sheet_id, version: "1" },
    servers: [{ url: new URL(c.req.url).origin }],
    security: [{ sheetKey: [] }],
    components: {
      securitySchemes: { sheetKey: { type: "apiKey", in: "header", name: API_KEY_HEADER } },
      schemas: { Row: row },
    },
    paths: {
      [url]: {
        get: {
          summary: `Read ${sheet_id}`,
          parameters: ["limit", "offset"].map((name) => ({ name, in: "query", schema: { type: "integer" } })),
          responses: {
            "200": {
              // The same Row the write takes. One schema for both halves is the
              // whole point: the read used to key a table sheet's rows by
              // column key while the write took them by name, so a generated
              // client had a type for what it sent and none for what came back.
              description: "The column row, then the data rows, each keyed by column name.",
              content: {
                "application/json": {
                  schema: {
                    type: "object",
                    properties: {
                      data: {
                        type: "array",
                        // data[0] is the column row and is not a Row; every
                        // element after it is the same Row the write takes.
                        prefixItems: [{ type: "object", description: "the columns, by position" }],
                        items: { $ref: "#/components/schemas/Row" },
                      },
                    },
                  },
                },
              },
            },
          },
        },
        // Only a table sheet has a write. A query or chart computes its rows, so
        // a spec that offered a POST would be documenting a 400.
        ...(sheet_id.startsWith("table:")
          ? {
            post: {
              summary: `Append rows to ${sheet_id}`,
              requestBody: {
                required: true,
                content: {
                  "application/json": {
                    schema: {
                      type: "object",
                      required: ["rows"],
                      properties: {
                        rows: { type: "array", maxItems: APPEND_ROWS_MAX, items: { $ref: "#/components/schemas/Row" } },
                      },
                    },
                  },
                },
              },
              responses: {
                "201": {
                  description: "Every row was appended, or none was.",
                  content: {
                    "application/json": {
                      schema: {
                        type: "object",
                        properties: {
                          data: {
                            type: "object",
                            properties: { appended: { type: "integer" }, rows: { type: "integer" } },
                          },
                        },
                      },
                    },
                  },
                },
              },
            },
          }
          : {}),
      },
    },
  });
});

app.get("/net/:id", async (c) => {
  const id = c.req.param("id");
  const sheet_id = id.includes(":")
    ? id
    : await sql`select sheet_id from sheet where doc_id = ${id}`.then(([s]: [{ sheet_id: string }?]) => s?.sheet_id);
  if (!sheet_id) {
    bad(404, `No net sheet answers to that.`, {
      Expected: "a net sheet id or doc_id",
      Received: show(id),
      Source: "the sheet table",
      Fix: "pass the full id, e.g. net-hook:abc123",
    });
  }
  return page(c)(await sheet(c, sheet_id, c.req.query()));
});

app.post("/query", async (c) => {
  return page(c)(await querify(c, await c.req.json(), c.req.query()));
});

// --- codex (external databases)

// One row of `net` per connection attempt. A codex sheet has no poller and no
// delivery, so opening it is the only moment anybody learns whether the far
// database still answers -- which is what library:freshness reads back, and
// what tells a rotated credential from a quiet afternoon. `net` is where a run
// already lives: trimNet bounds it, RUN_OF/RUN_OK grade it, and the freshness
// laterals needed one arm rather than a second log with its own retention,
// index and read.
//
// `status` is the shape a poll writes, because POLL_OK grades both: a status
// this code invents rather than reads off a wire, but the same field, so the
// two are not graded by two spellings of one rule.
//
// Written after the answer is known, never in front of it, and its own failure
// is swallowed the way trimNet's is: a log that cannot be written must not fail
// the read it is about. The failure message is stored as raised -- it names the
// host or the parse error, which is what the sheet's own members need in order
// to fix it, and neither the DSN nor its password reaches it.
const codexRun = async (sheet_id: string, started: number, status: number, body: string) => {
  await sql`
    insert into net (sheet_id, method, body, meta)
    values (${sheet_id}, 'CODEX', ${body}, ${sql.json({ status, ms: Date.now() - started })})
  `.catch((err: unknown) => console.error(`codex run ${sheet_id}:`, err));
  await trimNet(sheet_id);
};

app.get("/codex/:id", async (c) => {
  if (!rateLimit(`codex:${c.get("usr_id")}`)) {
    bad(429, `Too many codex queries from this account.`, {
      Expected: `a burst of at most ${RATE_LIMIT_MAX_TOKENS}, refilling at ${RATE_LIMIT_REFILL_RATE} per second`,
      Received: "one query past that",
      Source: `the codex budget for usr ${c.get("usr_id")}`,
      Fix: "wait a second and ask again",
    });
  }
  const sheet_id = c.req.param("id");
  const [type, _doc_id] = sheet_id.split(":");
  // Before the clock starts, because a caller with no share on this sheet is
  // not a connection that failed: logging their refusal would let anyone
  // holding a doc_id write failures into somebody else's freshness.
  await assertSheetAccess(c, sheet_id);
  const started = Date.now();
  try {
    switch (type) {
      case "codex-db": {
        const [db] = await sql`select dsn from db where sheet_id = ${sheet_id}`;
        if (!db) {
          bad(400, `That codex sheet is not connected to anything.`, {
            Expected: `a stored dsn for ${sheet_id}`,
            Received: "no row",
            Source: "the db table",
            Fix: "save a connection string with PUT /codex/:id first",
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
          if (extHost === appHost && (ext.port || "5432") === (app_.port || "5432")) {
            bad(403, `A codex cannot point at this server's own database.`, {
              Expected: "a host other than this server's own",
              Received: `${extHost}:${ext.port || "5432"}`,
              Source: "the dsn stored for this codex sheet",
              Fix: "point it at an external database",
            });
          }
        } catch (e) {
          if (e instanceof HTTPException) throw e;
          bad(400, `That connection string will not parse.`, {
            Expected: "a postgres DSN",
            Received: reason(e),
            Source: `the dsn stored for ${c.req.param("id")}`,
            Fix: "re-save it as postgresql://user:password@host:port/database",
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
          await codexRun(sheet_id, started, 200, "");
          return c.json({ data: [cols, ...rows] }, 200);
        } finally {
          await sql_.end();
        }
      }
      case "codex-scrapsheets": {
        await codexRun(sheet_id, started, 200, "");
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
        bad(400, `That is not a codex this server can open.`, {
          Expected: "codex-db",
          Received: show(type),
          Source: `the type prefix on sheet id ${sheet_id}`,
          Fix: "fix the type prefix on the id",
        });
    }
  } catch (err) {
    // A refusal about the id itself is not a connection that failed, and
    // `net` refuses the row anyway: its check constraint takes a net-, an
    // alert: or a codex- sheet and nothing else.
    if (type.startsWith("codex-")) {
      await codexRun(
        sheet_id,
        started,
        err instanceof HTTPException ? err.status : 500,
        reason(err),
      );
    }
    throw err;
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
    bad(403, `That codex is not shared with you.`, {
      Expected: `membership of codex-${type}:${doc_id} for usr ${c.get("usr_id")}`,
      Received: "no row",
      Source: "sheet_usr membership",
      Fix: "ask an owner to share the codex with you",
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
    bad(400, `That callback is missing half of what it needs.`, {
      Expected: `both "provider" and "code" query parameters`,
      Received: `provider=${JSON.stringify(provider ?? null)} code=${code ? "(present)" : "(missing)"}`,
      Source: "this request's query string",
      Fix: "start the flow at GET /codex/:id/connect",
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
    bad(404, `You have not bought that portal.`, {
      Expected: `a purchased portal for usr ${c.get("usr_id")}`,
      Received: `none for portal:${c.req.param("id")}`,
      Source: "your sheet_usr rows joined to the seller's listing",
      Fix: "buy it with POST /buy/:sell_id first",
    });
  }
  return page(c)(await sheet(c, sheet_.sheet_id, c.req.query()));
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
    description: "Run SQL (AlaSQL dialect; reference sheets as @type:doc_id).",
    inputSchema: {
      type: "object",
      required: ["code"],
      properties: {
        code: { type: "string" },
      },
    },
    handler: async (c, args) => {
      if (typeof args.code !== "string") {
        bad(400, `query_sheet has nothing to run.`, {
          Expected: `a "code" string`,
          Received: show(args.code),
          Source: "the tool call's arguments",
          Fix: `pass {"code": "select 1"}`,
        });
      }
      const { data, count } = await querify(c, { lang: "sql", code: args.code, args: [] }, {});
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
                  `must match the column type: number for ${
                    (NUMERIC_TYPES as string[]).join("/")
                  }, boolean for bool, string otherwise`,
              },
            },
          },
        },
      },
    },
    handler: async (c, args) => {
      const sheet_id = mcpSheetId(c, args);
      const [type, doc_id] = sheet_id.split(":");
      if (type !== "table") {
        bad(400, `Only a table sheet has cells to write.`, {
          Expected: "a table sheet",
          Received: sheet_id,
          Source: "the type prefix on that sheet id",
          Fix: "point write_cells at a table: sheet, or edit the query behind a computed one",
        });
      }
      if (!Array.isArray(args.cells) || !args.cells.length) {
        bad(400, `write_cells has nothing to write.`, {
          Expected: `a non-empty "cells" array`,
          Received: show(args.cells ?? null),
          Source: "the tool call's arguments",
          Fix: `pass {"cells": [{"row": 0, "col": "name", "value": "x"}]}`,
        });
      }
      await assertSheetAccess(c, sheet_id);
      const hand = await automerge.find<{ type: string; data: Table }>(doc_id as AnyDocumentId).catch(() => {
        bad(404, `Sheet ${sheet_id} has a row but no document.`, {
          Expected: "an automerge document",
          Received: "none",
          Source: `doc_id ${doc_id}`,
          Fix: "list the sheets you can reach with the list_sheets tool",
        });
      });
      const doc = hand.doc();
      if (!doc?.data) {
        bad(500, `That document holds no rows at all.`, {
          Expected: `a data array on sheet ${sheet_id}`,
          Received: "nothing",
          Source: `the automerge document ${doc_id}`,
          Fix: "re-create the sheet; the document is corrupt",
        });
      }
      const [colsRow, ...rows] = doc.data;
      if (!colsRow) {
        bad(500, `That document has rows under no columns.`, {
          Expected: `a column row at data[0] of sheet ${sheet_id}`,
          Received: "nothing",
          Source: `the automerge document ${doc_id}`,
          Fix: "add a column before writing cells",
        });
      }
      const cols = Object.values(colsRow);
      // Validate every cell before mutating anything: no partial writes.
      const writes: { rowIndex: number; key: string | number; value: unknown }[] = [];
      for (const [i, cell] of args.cells.entries()) {
        const { row, col, value } = cell as { row: unknown; col: unknown; value: unknown };
        const target = cols.find((x) => x.key === col) ?? cols.find((x) => x.name === col);
        if (!target) {
          bad(400, `That sheet has no such column.`, {
            Expected: cols.map((x) => `${x.name} (key ${JSON.stringify(x.key)})`).join(", "),
            Received: show(col),
            Source: `cells[${i}] of this tool call`,
            Fix: "name a column by its name or its key",
          });
        }
        if (typeof row !== "number" || !Number.isInteger(row) || row < 0 || row > rows.length) {
          bad(400, `That row is outside the sheet.`, {
            Expected:
              `a whole number from 0 to ${rows.length}, since the sheet has ${rows.length} rows and row ${rows.length} appends one`,
            Received: show(row),
            Source: `cells[${i}] of this tool call`,
            Fix: "read the sheet first, then write within its row count",
          });
        }
        if (typeof target.type !== "string") {
          bad(400, `That column does not hold a scalar.`, {
            Expected: "a column of a scalar type",
            Received: `column ${target.name}, which has a structured type`,
            Source: `cells[${i}] of this tool call`,
            Fix: "write_cells only writes scalar columns; edit a structured column in the page",
          });
        }
        const t = target.type;
        const mismatched = (NUMERIC_TYPES as string[]).includes(t)
          ? typeof value !== "number" || (t === "int" && !Number.isInteger(value))
          : t === "bool"
          ? typeof value !== "boolean"
          : t === "json"
          ? value === undefined
          : typeof value !== "string";
        if (mismatched) {
          bad(400, `That value does not match the column it is written to.`, {
            Expected: `${t}, which column ${target.name} declares`,
            Received: `${typeof value} ${JSON.stringify(value)}`,
            Source: `cells[${i}] of this tool call, row ${row}`,
            Fix: `send a ${t}, or change the column's type first`,
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
    bad(400, `That is not a request this endpoint can read.`, {
      Expected: "a JSON body",
      Received: "something that would not parse",
      Source: "the request body",
      Fix: "post JSON-RPC 2.0 with Content-Type: application/json",
    });
  });
  if (msg?.jsonrpc !== "2.0") {
    bad(400, `That is not JSON-RPC 2.0.`, {
      Expected: `"2.0"`,
      Received: show(msg?.jsonrpc),
      Source: `the "jsonrpc" field of the request body`,
      Fix: `send {"jsonrpc": "2.0", "id": 1, "method": "tools/list"}`,
    });
  }
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
