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

export const examples = {
  "simple http get": QuerySql(
    {},
    "select http('https://taylor.town/random')->res as random_number",
  ),
  "bluesky users": QuerySql(
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
  "github repo": QuerySql(
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
  "github users": QuerySql(
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
  "github prs/issues": QuerySql(
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
  "crypto markets": QuerySql(
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
  "crypto search": QuerySql(
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
  "hackernews search": QuerySql(
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
  "hackernews frontpage": QuerySql(
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
  "arxiv search": QuerySql(
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
  "wikipedia search": QuerySql(
    { i: "number", thumb: "image", title: "text", url: "link" },
    "" +
      "search `query` pages / return (" +
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
  "reddit search": QuerySql(
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
      "\n  thumbnail," +
      "\n  title," +
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
  "subreddit posts": QuerySql(
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
      "\n  thumbnail," +
      "\n  title," +
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
  "manifold search": QuerySql(
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
      "\n  creatorUsername," +
      "\n  createdTime," +
      "\n  closeTime," +
      "\n  resolutionTime," +
      "\n  resolution," +
      "\n  volume24Hours," +
      "\n  volume," +
      "\n  totalLiquidity," +
      "\n  uniqueBettorCount as uniqes," +
      "\n  pool" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.manifold.markets/v0/search-markets'," +
      "\n  @{term:(@params->('')), limit:25}" +
      "\n)",
  ),
  "manifold trending": QuerySql(
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
      "\n  creatorUsername," +
      "\n  createdTime," +
      "\n  closeTime," +
      "\n  resolutionTime," +
      "\n  resolution," +
      "\n  volume24Hours," +
      "\n  volume," +
      "\n  totalLiquidity," +
      "\n  uniqueBettorCount as uniqes," +
      "\n  pool," +
      "\n  outcomeType" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://api.manifold.markets/v0/markets'," +
      "\n  @{limit:25}" +
      "\n)",
  ),
  "yahoo finance news": QuerySql(
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
  "speedrun.com games": QuerySql(
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
  "speedrun.com users": QuerySql(
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
  "apple podcast search": QuerySql(
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
      "\n trackName," +
      "\n primaryGenreName as genre," +
      "\n releaseDate" +
      "\n)" +
      "\nfrom http(" +
      "\n  'https://itunes.apple.com/search'," +
      "\n  @{term:(@params->('')), media:'podcast', limit:25}" +
      "\n)",
    "articles of interest",
  ),
  "ossinsights.io trends": QuerySql(
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