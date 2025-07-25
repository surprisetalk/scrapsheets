import { HTTPException } from "jsr:@hono/hono/http-exception";
import { Context, Hono } from "jsr:@hono/hono";
import { logger } from "jsr:@hono/hono/logger";
import { cors } from "jsr:@hono/hono/cors";
import { jwt, sign } from "jsr:@hono/hono/jwt";
import type { JwtVariables } from "jsr:@hono/hono/jwt";
import sg from "npm:@sendgrid/mail";
import pg from "https://deno.land/x/postgresjs@v3.4.5/mod.js";
import { upgradeWebSocket } from "jsr:@hono/hono/deno";
import { Repo } from "npm:@automerge/automerge-repo";
import * as AM from "npm:@automerge/automerge-repo";
import type { AnyDocumentId, DocHandle } from "npm:@automerge/automerge-repo";
import { NodeWSServerAdapter } from "npm:@automerge/automerge-repo-network-websocket";
import ala from "npm:alasql";

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
  | "string"
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

const QuerySql = (
  cols: Record<string, string>,
  code: string,
  ...examples: string[]
) => ({
  type: "query",
  data: [
    {
      lang: "sql",
      code,
      cols,
      args: {},
      examples,
    },
  ],
});
const examples = {
  // "table:calculated-columns": Table(
  //   [
  //     { key: 0, name: "a", type: "int" },
  //     { key: 1, name: "b", type: "int" },
  //     {
  //       key: 2,
  //       name: "c",
  //       type: { lang: "formula", code: "a + b" },
  //     },
  //   ],
  //   ...[
  //     [1, 2],
  //     [2, 4],
  //     [3, 6],
  //   ],
  // ),
  // "table:postman": Table(
  //   [
  //     { key: 0, name: "method", type: "text" },
  //     { key: 1, name: "url", type: "text" },
  //     { key: 2, name: "fields", type: "list field" },
  //     {
  //       key: 3,
  //       name: "form",
  //       type: {
  //         lang: "scrapscript",
  //         code: "form {method,url,fields}",
  //       },
  //     },
  //   ],
  //   ...[
  //     [
  //       "GET",
  //       "example.com/foo",
  //       [
  //         { label: "hello", type: "text" },
  //         { label: "world", type: "date" },
  //       ],
  //     ],
  //     [
  //       "POST",
  //       "example.com/bar",
  //       [
  //         {
  //           label: "my dropdown",
  //           key: "my_dropdown",
  //           type: { 1: "label a", 2: "label b" },
  //           default: 2,
  //         },
  //       ],
  //     ],
  //   ],
  // ),
  // "table:nested": Table(
  //   [
  //     { key: 0, name: "a", type: "list text" },
  //     { key: 1, name: "b", type: "dict text text" },
  //     { key: 1, name: "c", type: "dict text (list text)" },
  //   ],
  //   ...[
  //     [
  //       ["a", "b"],
  //       { c: "d", e: "f" },
  //       { g: ["h", "i"], j: ["k", "l", "m"] },
  //     ],
  //     [["n"], { o: "p" }, { q: [], r: ["s", "t"] }],
  //   ],
  // ),
  "query:http-get": QuerySql(
    {},
    "select http('https://taylor.town/random')->res as random_number",
  ),
  // "query:python": QuerySql({},
  //   "return [[{'name':'a','type':'text','key':'a'}], {'a':1}, {'a':2}, {'a':3}]",
  // ),
  // "query:stonks": QuerySql({},
  //   "select stonks->>'id', stonks->>'price' from http('https://taylor.town/stonks'), json(res) stonks",
  // ),
  "query:bluesky-actors": QuerySql(
    {
      avatar: "image",
      did: "link",
      handle: "text",
      displayName: "text",
    },
    "" +
      "search actors/ return (" +
      "\n  avatar," +
      "\n  'http://bsky.app/profile/'||(@a->did) as did," +
      "\n  handle," +
      "\n  displayName" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://public.api.bsky.app/xrpc/app.bsky.actor.searchActors'," +
      "\n  @{q:(coalesce(@params->(''),'example'))}" +
      "\n)",
    "taylor.town",
    "memes",
  ),
  "query:github-repos": QuerySql(
    {
      repo: "link",
      full_name: "text",
      description: "text",
      lang: "text",
      license: "text",
      stargazers: "int",
      open_issues: "int",
    },
    "" +
      "search items/ return (" +
      "\n  'http://github.com/'||(@x->full_name) as repo," +
      "\n  full_name," +
      "\n  description," +
      "\n  language as lang," +
      "\n  license->key as license," +
      "\n  stargazers_count as stargazers," +
      "\n  open_issues_count as open_issues" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.github.com/search/repositories'," +
      "\n  @{q:(coalesce(@params->(''),'example'))}" +
      "\n)",
    "scrapscript sort=stars",
    "surprisetalk sort=updated",
    "jquery in:name,description",
    "jquery in:readme ",
    "repo:octocat/hello-world",
    "user:surprisetalk forks:>100 ",
    "org:scrapscript",
    "size:>=30000",
    "size:<50",
    "size:50..120",
    "node followers:>=10000",
    "stars:>=500 language:php",
    "css pushed:>2013-02-01",
    "webos created:<2011-01-01",
    "rails language:javascript",
    "license:apache-2.0",
    "archived:true GNOME",
    "help-wanted-issues:>n",
    "good-first-issues:>n",
    "is:sponsorable",
  ),
  "query:github-users": QuerySql(
    {
      avatar: "image",
      url: "link",
      handle: "text",
      type: "text",
    },
    "" +
      "search items/ as @x return (" +
      "\n  avatar_url as avatar," +
      "\n  url," +
      "\n  login as handle," +
      "\n  type" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.github.com/search/users'," +
      "\n  @{q:(coalesce(@params->(''),'example'))}" +
      "\n)",
    "surprisetalk sort=followers",
    "foo sort=joined",
    "mike in:name created:<2011-01-01 type:user",
    "data in:email type:org",
    "user:octocat",
    "org:electron type:user",
    "kenya in:login",
    "bolton in:name",
    "fullname:nat friedman",
    "data in:email",
    "repos:>9000",
    "repos:1 location:iceland",
    "language:javascript location:russia",
    "jenny language:javascript in:fullname",
    "created:2010-01-01..2011-01-01 john in:login",
    "followers:>=1000",
    "is:sponsorable",
  ),
  // "query:github-commits": QuerySql({},
  //   "select * from @(http('https://api.github.com/search/commits', @{q:(@params->(''))})->res->items)",
  //   "repo:tekknolagi/scrapscript",
  //   "surprisetalk",
  // ),
  "query:github-prs-issues": QuerySql(
    {
      id: "text",
      number: "number",
      title: "text",
      url: "link",
      avatar: "image",
      login: "text",
      labels: "text",
      state: "text",
      locked: "bool",
      assignee_avatar: "image",
      assignee_login: "text",
      milestone: "text",
      comments: "text",
      created_at: "datetime",
      updated_at: "datetime",
      closed_at: "datetime",
      author_association: "text",
      type: "text",
      draft: "text",
      html_url: "link",
      body: "text",
      reactions: "text",
    },
    "" +
      "search items/ as @x labels/ as @label return (" +
      "\n  @x->id as id," +
      "\n  @x->number as number," +
      "\n  @x->title as title," +
      "\n  @x->html_url as url," +
      "\n  @x->user->avatar_url as avatar," +
      "\n  @x->user->login as login," +
      "\n  @label->name as labels," +
      "\n  @x->state as state," +
      "\n  @x->locked as locked," +
      "\n  @x->assignee->avatar_url as assignee_avatar," +
      "\n  @x->assignee->login as assignee_login," +
      "\n  @x->milestone as milestone," +
      "\n  @x->comments as comments," +
      "\n  @x->created_at as created_at," +
      "\n  @x->updated_at as updated_at," +
      "\n  @x->closed_at as closed_at," +
      "\n  @x->author_association as author_association," +
      "\n  @x->type as type," +
      "\n  @x->draft as draft," +
      "\n  @x->pull_request->html_url as html_url," +
      "\n  @x->body as body," +
      "\n  @x->reactions->total_count as reactions" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.github.com/search/issues'," +
      "\n  @{q:(coalesce(@params->(''),'example'))}" +
      "\n)",
    "repo:tekknolagi/scrapscript",
    "user:surprisetalk",
    "author:surprisetalk",
    "assignee:surprisetalk",
    "commenter:surprisetalk",
    "language:ruby",
    "state:closed comments:>100",
    "draft:true",
    "interactions:>2000",
    "reactions:>1000",
    "archived:false gnome",
    "gnome is:unmerged",
    "language:javascript merged:<2011-01-01",
    "language:c# created:<2011-01-01 state:open",
    "type:pr review-requested:benbalter",
    "type:pr reviewed-by:gjtorikian",
    "type:pr review:changes_requested",
    "mentions:surprisetalk",
    "team:example/example",
    "bug type:issue sort=reactions-+1",
    "type:pr sort=comments",
    "oops in:title,body,comments",
    "state:open",
    "sort=reactions-smile",
    "sort=updated",
  ),
  "query:crypto-markets": QuerySql(
    {
      image: "image",
      rank: "int",
      name: "text",
      symbol: "text",
      price: "usd",
      market_cap: "usd",
      high_24h: "usd",
      low_24h: "usd",
      change_24h: "usd",
      ath: "usd",
      ath_date: "datetime",
    },
    "" +
      "search / return (" +
      "\n  image," +
      "\n  market_cap_rank as rank," +
      "\n  name," +
      "\n  symbol," +
      "\n  current_price as price," +
      "\n  market_cap," +
      "\n  high_24h," +
      "\n  low_24h," +
      "\n  price_change_percentage_24h as change_24h," +
      "\n  ath," +
      "\n  ath_date" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.coingecko.com/api/v3/coins/markets'," +
      "\n  @{vs_currency:'usd', `order`:'market_cap_desc', per_page:100}" +
      "\n)",
  ),
  "query:crypto-search": QuerySql(
    {
      thumb: "image",
      rank: "int",
      name: "text",
      symbol: "text",
    },
    "" +
      "search coins/ return (" +
      "\n  market_cap_rank as rank," +
      "\n  thumb," +
      "\n  symbol," +
      "\n  name" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.coingecko.com/api/v3/search'," +
      "\n  @{`query`:(coalesce(@params->(''),'bitcoin'))}" +
      "\n)",
    "bitcoin",
    "ethereum",
    "doge",
  ),
  "query:hackernews-search": QuerySql(
    {
      id: "text",
      url: "link",
      title: "text",
      author: "text",
      points: "num",
      comments: "num",
      created_at: "datetime",
      updated_at: "datetime",
    },
    "" +
      "search hits/ return (" +
      "\n  story_id as id," +
      "\n  url," +
      "\n  title," +
      "\n  author," +
      "\n  points," +
      "\n  num_comments as comments," +
      "\n  created_at," +
      "\n  updated_at" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://hn.algolia.com/api/v1/search'," +
      "\n  @{`query`:(coalesce(@params->(''),'example'))}" +
      "\n)",
    "taylor.town",
  ),
  "query:hackernews-top": QuerySql(
    {
      title: "text",
      author: "text",
      hn_url: "link",
      url: "link",
      published_at: "datetime",
    },
    "" +
      "search items/ return (" +
      "\n  title," +
      "\n  author->name as author," +
      "\n  external_url as hn_url," +
      "\n  url," +
      "\n  date_published as published_at" +
      "\n)" +
      "\nfrom http('https://corsproxy.io/?url=https://hnrss.org/frontpage.jsonfeed')",
  ),
  "query:arxiv-search": QuerySql(
    {
      id: "link",
      title: "text",
      summary: "text",
      published: "datetime",
      updated: "datetime",
      authors: "text",
      cat: "text",
    },
    "" +
      "search entries / return (" +
      "\n  id," +
      "\n  categories as cat," +
      "\n  title," +
      "\n  summary," +
      "\n  authors," +
      "\n  published," +
      "\n  updated" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://export.arxiv.org/api/query'," +
      "\n  @{search_query:('all:' || (@params->(''))), max_results:25}" +
      "\n)",
  ),
  "query:wikipedia-search": QuerySql(
    { i: "number", thumb: "image", title: "text", url: "link" },
    "" +
      "search `query` pages / return (" +
      // "\n  `index` as i," +
      "\n  thumbnail->source as thumb," +
      "\n  title," +
      "\n  fullurl as url" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://en.wikipedia.org/w/api.php'," +
      "\n  @{" +
      "\n     action:'query'," +
      "\n     generator:'search'," +
      "\n     prop:'pageimages|info'," +
      "\n     piprop:'thumbnail'," +
      "\n     pithumbsize:100," +
      "\n     inprop:'url'," +
      "\n     exintro:''," +
      "\n     explaintext:''," +
      "\n     `gsrsearch`:(@params->(''))," +
      "\n     gsrlimit:25," +
      "\n     format:'json'," +
      "\n     origin:'*'" +
      "\n   }" +
      "\n)",
  ),
  "query:reddit-search": QuerySql(
    {
      preview: "image",
      ups: "int",
      downs: "int",
      comms: "int",
      created: "datetime",
      thumbnail: "image",
      subreddit: "text",
      subs: "int",
      title: "text",
      selftext: "text",
      author: "text",
      is_self: "bool",
      spoiler: "bool",
      type: "text",
      domain: "text",
      reddit_url: "link",
      url: "link",
      dest_url: "link",
    },
    "" +
      "search data children / data return (" +
      // "\n  preview->images->(0)->resolutions->(0)->url as preview," +
      "\n  thumbnail," +
      "\n  title," +
      // "\n  selftext," +
      "\n  'https://old.reddit.com'||permalink as reddit_url," +
      "\n  url," +
      "\n  url_overridden_by_dest as dest_url," +
      "\n  domain," +
      "\n  post_hint as type," +
      "\n  ups," +
      "\n  downs," +
      "\n  is_self," +
      "\n  spoiler," +
      "\n  num_comments as comms," +
      "\n  author," +
      "\n  subreddit," +
      "\n  subreddit_subscribers as subs," +
      "\n  created" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://www.reddit.com/search.json'," +
      "\n  @{q:(@params->('')), limit:25}" +
      "\n)",
  ),
  "query:reddit-subreddit": QuerySql(
    {
      preview: "image",
      ups: "int",
      downs: "int",
      comms: "int",
      created: "datetime",
      thumbnail: "image",
      subreddit: "text",
      subs: "int",
      title: "text",
      selftext: "text",
      author: "text",
      is_self: "bool",
      spoiler: "bool",
      type: "text",
      domain: "text",
      reddit_url: "link",
      url: "link",
      dest_url: "link",
    },
    "" +
      "search data children / data return (" +
      // "\n  preview->images->(0)->resolutions->(0)->url as preview," +
      "\n  thumbnail," +
      "\n  title," +
      // "\n  selftext," +
      "\n  'https://old.reddit.com'||permalink as reddit_url," +
      "\n  url," +
      "\n  url_overridden_by_dest as dest_url," +
      "\n  domain," +
      "\n  post_hint as type," +
      "\n  ups," +
      "\n  downs," +
      "\n  is_self," +
      "\n  spoiler," +
      "\n  num_comments as comms," +
      "\n  author," +
      "\n  subreddit," +
      "\n  subreddit_subscribers as subs," +
      "\n  created" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://www.reddit.com/r/'||(coalesce(@params->(''),'all'))||'.json'," +
      "\n  @{limit:25}" +
      "\n)",
    "hmmm",
  ),
  "query:manifold-search": QuerySql(
    {
      creatorAvatarUrl: "image",
      url: "link",
      question: "text",
      creatorUsername: "text",
      createdTime: "datetime",
      closeTime: "datetime",
      resolutionTime: "datetime",
      resolution: "text",
      totalLiquidity: "float",
      volume: "float",
      volume24Hours: "float",
      uniqes: "int",
      pool: "text",
    },
    "" +
      "search / return (" +
      "\n  url," +
      "\n  question," +
      // "\n  id," +
      // "\n  creatorAvatarUrl," +
      // "\n  creatorId," +
      "\n  creatorUsername," +
      // "\n  creatorName," +
      "\n  createdTime," +
      "\n  closeTime," +
      "\n  resolutionTime," +
      "\n  resolution," +
      // "\n  outcome," +
      // "\n  mechanism," +
      "\n  volume24Hours," +
      "\n  volume," +
      "\n  totalLiquidity," +
      // "\n  isResolved," +
      "\n  uniqueBettorCount as uniqes," +
      // "\n  lastUpdatedTime," +
      // "\n  lastBetTime," +
      // "\n  lastCommentTime," +
      // "\n  token," +
      "\n  pool" +
      // "\n  probability," +
      // "\n  p," +
      // "\n  resolutionProbability," +
      // "\n  `value`," +
      // "\n  `min`," +
      // "\n  `max`," +
      // "\n  isLogScale" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.manifold.markets/v0/search-markets'," +
      "\n  @{term:(@params->('')), limit:25}" +
      "\n)",
  ),
  "query:manifold-trending": QuerySql(
    {
      creatorAvatarUrl: "image",
      url: "link",
      question: "text",
      creatorUsername: "text",
      createdTime: "datetime",
      closeTime: "datetime",
      resolutionTime: "datetime",
      resolution: "text",
      outcomeType: "text",
      totalLiquidity: "float",
      volume: "float",
      volume24Hours: "float",
      uniqes: "int",
      pool: "text",
    },
    "" +
      "search / return (" +
      "\n  url," +
      "\n  question," +
      // "\n  id," +
      // "\n  creatorAvatarUrl," +
      // "\n  creatorId," +
      "\n  creatorUsername," +
      // "\n  creatorName," +
      "\n  createdTime," +
      "\n  closeTime," +
      "\n  resolutionTime," +
      "\n  resolution," +
      // "\n  mechanism," +
      "\n  volume24Hours," +
      "\n  volume," +
      "\n  totalLiquidity," +
      // "\n  isResolved," +
      "\n  uniqueBettorCount as uniqes," +
      // "\n  lastUpdatedTime," +
      // "\n  lastBetTime," +
      // "\n  lastCommentTime," +
      // "\n  token," +
      "\n  pool," +
      // "\n  probability," +
      // "\n  p," +
      // "\n  resolutionProbability," +
      // "\n  `value`," +
      // "\n  `min`," +
      // "\n  `max`," +
      // "\n  isLogScale" +
      "\n  outcomeType" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.manifold.markets/v0/markets'," +
      "\n  @{limit:25}" +
      "\n)",
  ),
  // "query:fred-search": QuerySql({},
  //   "select * from @(http('https://corsproxy.io/?url=https://api.stlouisfed.org/fred/series/search', @{search_text:(@params->('')), file_type:'json'})->res)"
  // ),
  "query:stocks-search": QuerySql(
    {
      link: "link",
      thumb: "image",
      title: "text",
      publisher: "text",
      publishedAt: "datetime",
      tickers: "text",
    },
    "" +
      "search news / return (" +
      "\n  link," +
      "\n  thumbnail->resolutions->(1)->url as thumb," +
      "\n  title," +
      "\n  publisher," +
      "\n  providerPublishTime as publishedAt," +
      "\n  relatedTickers as tickers" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://corsproxy.io/?url=https://query1.finance.yahoo.com/v1/finance/search'," +
      "\n  @{q:(@params->(''))}" +
      "\n)",
    "AAPL",
  ),
  // "query:discogs-search": QuerySql({},
  //   "select * from @(http('https://api.discogs.com/database/search', @{q:(@params->('')), type:'release'})->res)"
  // ),
  "query:speedrun-games": QuerySql(
    {
      thumb: "image",
      url: "link",
      title: "text",
      release: "date",
      discord: "link",
    },
    "" +
      "search data / return (" +
      "\n  weblink as url," +
      "\n  assets->`cover-tiny`->uri as thumb," +
      "\n  names->international as title," +
      "\n  `release-date` as release," +
      "\n  discord" +
      "\n)" +
      "\nfrom http('https://www.speedrun.com/api/v1/games', @{name:(@params->(''))})",
    "mario",
    "zelda",
  ),
  "query:speedrun-users": QuerySql(
    {
      url: "link",
      thumb: "image",
      location: "text",
      signup: "datetime",
      twitch: "link",
      hitbox: "link",
      youtube: "link",
      twitter: "link",
      live: "link",
    },
    "" +
      "search data / return (" +
      "\n  weblink as url," +
      "\n  assets->image->uri as thumb," +
      "\n  names->international as name," +
      "\n  location->region->code as location," +
      "\n  twitch->uri as twitch," +
      "\n  hitbox->uri as hitbox," +
      "\n  youtube->uri as youtube," +
      "\n  twitter->uri as twitter," +
      "\n  speedrunslive->uri as live," +
      "\n  signup" +
      "\n)" +
      "\nfrom http('https://www.speedrun.com/api/v1/users', @{name:(@params->(''))})",
    "mario",
    "zelda",
  ),
  // "query:youtube-search": QuerySql({},
  //   "select * from @(http('https://www.googleapis.com/youtube/v3/search', @{part:'snippet', q:(@params->('')), key:'YOUR_API_KEY', maxResults:25})->res->items)",
  //   "videogamedunkey"
  // ),
  "query:podcast-search": QuerySql(
    {
      artistName: "text",
      collectionName: "text",
      trackName: "text",
      colUrl: "link",
      trackUrl: "link",
      feedUrl: "link",
      artworkUrl100: "image",
      releaseDate: "datetime",
      trackCount: "int",
      country: "text",
      genre: "text",
      genres: "text",
      trackTimeMillis: "int",
      artistUrl: "link",
    },
    "" +
      "search results / return (" +
      "\n feedUrl," +
      "\n trackViewUrl as trackUrl," +
      "\n artworkUrl100," +
      "\n artistName," +
      // "\n collectionName," +
      "\n trackName," +
      // "\n collectionViewUrl as colUrl," +
      // "\n trackCount," +
      // "\n country," +
      "\n primaryGenreName as genre," +
      // "\n genres," +
      // "\n trackTimeMillis," +
      // "\n artistViewUrl as artistUrl," +
      "\n releaseDate" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://itunes.apple.com/search'," +
      "\n  @{term:(@params->('')), media:'podcast', limit:25}" +
      "\n)",
    "articles of interest",
  ),
  "query:ossinsight-trends": QuerySql(
    {
      repo: "text",
      lang: "text",
      desc: "text",
      stars: "int",
      forks: "int",
      prs: "int",
      pushes: "int",
      total_score: "float",
      contributors: "text",
      cats: "text",
    },
    "" +
      "search data `rows` / return (" +
      "\n  repo_name as repo," +
      "\n  description as `desc`," +
      "\n  primary_language as lang," +
      "\n  contributor_logins as contributors," +
      // "\n  collections_names as cats," +
      "\n  stars," +
      "\n  forks," +
      "\n  pull_requests as prs," +
      "\n  pushes," +
      "\n  total_score" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.ossinsight.io/v1/trends/repos'," +
      "\n  @{keyword:(@params->(''))}" +
      "\n)",
    "language=All period=past_24_hours",
    "language=JavaScript period=past_3_months",
    "language=Erlang",
  ),
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
  { lang, code, args = [] }: Query,
  reqQuery: Record<string, string>,
): Promise<Page> => {
  if (lang === "sql") {
    // TODO: Use psql/etc if all sheets are from the same codex.
    const sheet_ids: string[] = [];
    const code_ = code.replace(
      /@[^ ]+/g,
      ref => (sheet_ids.push(ref.slice(1)), "?"),
    );
    const docs: Record<string, Record<string, any>[]> = {};
    for (const sheet_id of sheet_ids) {
      if (docs[sheet_id]) continue;
      const [cols, ...rows] = (await sheet(c, sheet_id, {})).data;
      docs[sheet_id] = rows.map(row =>
        Object.fromEntries(
          Object.values(cols).map(col => [col.name, row[col.key]]),
        ),
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
        sheet_ids.map(id => docs[id]),
      );
    return {
      data: [
        Object.fromEntries(
          cols.map((col, i) => [
            i,
            {
              name: col.columnid,
              type: "string",
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
}): Promise<Page> => {
  const where_ = where.filter(x => x).length
    ? sql`where true ${where.filter(x => x).map((x: any) => sql`and ${x}`)}`
    : sql``;
  const rows =
    await sql`${select} ${from} ${where_} ${order ?? sql``} limit ${limit} offset ${offset}`;
  const cols: Row<Col> = arrayify(
    rows.columns.map((col, i) => ({
      name: col.name,
      type: "string", // TODO: col.type
      key: col.name,
    })),
  );
  const [{ count }] = await sql`select count(*) ${from} ${where_}`;
  return { data: [cols, ...rows], count, offset: parseInt(offset) };
};

const page =
  (c: Context) =>
  ({ data, offset, count }: Page) => {
    c.header("Content-Range", `${offset}-${offset + data.length - 1}/${count}`);
    return c.json({ data }, 200);
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

app.get("/shop", async c => {
  return page(c)({
    data: [
      [
        { name: "price", type: "usd", key: "price" },
        { name: "id", type: "sheet_id", key: "id" },
        { name: "name", type: "text", key: "id" },
        { name: "", type: "create", key: "data" },
      ] as unknown as Row<Col>,
      ...Object.entries(examples).map(([k, v]) => ({
        id: k,
        price: 0,
        data: v,
      })),
    ],
    offset: 0,
    count: Object.keys(examples).length,
  });
  // TODO:
  const { limit, offset, ...qs } = c.req.query();
  return page(c)(
    await cselect({
      select: sql`select created_at, sell_id, sell_type, sell_price, name`,
      from: sql`from sheet s`,
      where: [
        sql`sell_price >= 0`,
        sql`sell_type is not null`,
        qs.name && sql`name ilike ${qs.name + "%"}`,
        qs.sell_type && sql`sell_type = ${qs.sell_type}`,
        qs.sell_price &&
          sql`sell_price between ${qs.sell_type.split("-")[0]}::numeric and ${qs.sell_type.split("-")[1]}::numeric`,
      ],
      order: undefined,
      limit,
      offset,
    }),
  );
});

app.post("/net/:id", async c => {
  await sql`insert into net ${sql({ sheet_id: c.req.param("id"), body: await c.req.text() })}`;
  return c.json(null, 200);
});

app.use("*", jwt({ secret: JWT_SECRET }));

app.use("*", async (c, next) => {
  c.set("usr_id", c.get("jwtPayload")?.sub);
  await next();
});

app.post("/buy/:id", async c => {
  const sheet_id = await sql.begin(async sql => {
    const [sheet] =
      await sql`select * from sheet where sell_id = ${c.req.param("id")} and sell_price >= 0`;
    if (!sheet) throw new HTTPException(404, { message: "Not found." });
    if (!sheet.sell_type)
      throw new HTTPException(400, { message: "Not for sale." });
    const row_0 = sheet.type === "template" ? sheet.row_0.data : {};
    const doc_id = sheet.sell_type.startsWith("codex-")
      ? Math.random().toString().slice(2)
      : automerge.create({ data: row_0 }).documentId;
    const [{ sheet_id }] = await sql`
      with sell as (
        select * from sheet where sell_id = ${c.req.param("id")} and sell_price >= 0
      ), buy as (
        insert into sheet (created_by, type, doc_id, name, buy_id, buy_price, row_0) 
        select ${c.get("usr_id")}, sell_type, ${doc_id}, name, sell_id, sell_price, ${row_0}
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

app.post("/sell/:id", async c => {
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

app.get("/library", async c => {
  const { limit, offset, ...qs } = c.req.query();
  return page(c)(
    await cselect({
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
app.put("/library/:id", async c => {
  const [sheet] = await sql`
    update sheet 
    set ${sql({ name: null, tags: [], ...(await c.req.json()) }, "name", "tags")} 
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
        .then(hand => hand.doc().data[0] ?? {}),
      created_by: c.get("usr_id"),
    };
    await sql`
      with s as (insert into sheet ${sql(sheet, "type", "doc_id", "name", "tags", "created_by", "row_0")} on conflict (sheet_id) do nothing returning *)
      insert into sheet_usr (sheet_id, usr_id) select sheet_id, created_by from s
    `;
  }
  return c.json(null, 200);
});

// TODO: This currently takes sheet_id, but we want it to take doc_id.
app.get("/net/:id", async c => {
  return page(c)(await sheet(c, c.req.param("id"), c.req.query()));
});

app.post("/query", async c => {
  return page(c)(await querify(c, await c.req.json(), c.req.query()));
});

app.get("/codex/:id", async c => {
  const sheet_id = c.req.param("id");
  const [type, doc_id] = sheet_id.split(":");
  switch (type) {
    case "codex-db": {
      const [db] = await sql`select dsn from db where sheet_id = ${sheet_id}`;
      if (!db)
        throw new HTTPException(400, {
          message: `No DSN found.`,
        });
      // TODO: Be really careful about arbitrary DB access! Don't let them access our DB haha
      const sql_ = pg(db.dsn, {
        onnotice: msg => msg.severity !== "DEBUG" && console.log(msg),
      });
      const rows = await sql_`
        select 
          table_name as name,
          '[[{"name":"name","type":"string","key":"column_name"},{"name":"type","type":"string","key":"data_type"},{"name":"key","type":"int","key":"ordinal_position"}]]'::jsonb || jsonb_agg(t)::jsonb as columns
        from information_schema.tables t
        inner join information_schema.columns c using (table_catalog,table_schema,table_name)
        where table_schema = 'public'
        group by table_name, table_type
      `;
      const cols = rows.columns.map((col, i) => ({
        name: col.name,
        type: "string",
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
              { name: "name", type: "string", key: "name" },
              { name: "columns", type: "table", key: "columns" },
            ],
            {
              name: "shop",
              columns: [
                [
                  { name: "name", type: "string", key: 0 },
                  { name: "type", type: "string", key: 1 },
                  { name: "key", type: "int", key: 2 },
                ],
                ["created_at", "string", 0],
                ["sell_id", "string", 1],
                ["sell_type", "string", 2],
                ["sell_price", "string", 3],
                ["name", "string", 4],
              ],
            },
            {
              name: "library",
              columns: [
                [
                  { name: "name", type: "string", key: 0 },
                  { name: "type", type: "string", key: 1 },
                  { name: "key", type: "int", key: 2 },
                ],
                ["s.created_at", "string", 0],
                ["s.type", "string", 1],
                ["s.doc_id", "string", 2],
                ["s.name", "string", 3],
                ["s.tags", "string", 4],
                ["s.sell_price", "string", 5],
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

app.post("/codex-db/:id", async c => {
  const sheet_id = `codex-db:${c.req.param("id")}`;
  await sql`
    insert into db (sheet_id, dsn) 
    select ${sheet_id}, ${await c.req.json()}
    where exists (select true from sheet_usr su where (su.sheet_id,su.usr_id) = (${sheet_id},${c.get("usr_id")}))
    on conflict (sheet_id) do update set dsn = excluded.dsn
  `;
  return c.json(null, 200);
});

app.get("/codex/:id/connect", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/codex/:id/callback", async c => {
  // TODO:
  return c.json(null, 500);
});

app.get("/portal/:id", async c => {
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

app.get("/stats/:id", async c => {
  // TODO: Get column/table stats about any sheet as a table.
  return c.json(null, 500);
});

app.all("/mcp/:id", async c => {
  // TODO: mcp server
  return c.json(null, 500);
});

export default app;
