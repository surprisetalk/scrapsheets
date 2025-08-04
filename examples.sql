insert into sheet (sell_price, created_by, type, doc_id, row_0)
select
  0,
  (select usr_id from usr where email = ''),
  'template',
  replace(doc_id, ' ', '-'),
  jsonb_build_object('type','query', 'data',jsonb_build_array(jsonb_build_object('lang','sql', 'code',trim(code), 'cols',cols)))
from jsonb_to_recordset(replace($$[

{
  "doc_id": "simple http get",
  "cols": {},
  "code":"
  \n  select http('https://taylor.town/random')->res as random_number
  \n  "
}, 

{
  "doc_id": "bluesky users",
  "cols": {
    "avatar": "image",
    "did": "link",
    "handle": "text",
    "displayName": "text"
  },
  "code":"
  \n  search actors/ return (
  \n    avatar,
  \n    'http://bsky.app/profile/'||(@a->did) as did,
  \n    handle,
  \n    displayName
  \n  )
  \n  from http(
  \n    'https://public.api.bsky.app/xrpc/app.bsky.actor.searchActors',
  \n    @{q:(coalesce(@params->(''),'example'))}
  \n  )
  \n  "
},

{
  "doc_id": "github repo",
  "cols": {
    "repo": "link",
    "full_name": "text",
    "description": "text",
    "lang": "text",
    "license": "text",
    "stargazers": "int",
    "open_issues": "int"
  },
  "code":
  "\n  search items/ return (\n    'http://github.com/'||(@x->full_name) as repo,\n    full_name,\n    description,\n    language as lang,\n    license->key as license,\n    stargazers_count as stargazers,\n    open_issues_count as open_issues\n  )\n  from http(\n    'https://api.github.com/search/repositories',\n    @{q:(coalesce(@params->(''),'example'))}\n  )\n  "
},

{
  "doc_id": "github users",
  "cols": {
    "avatar": "image",
    "url": "link",
    "handle": "text",
    "type": "text"
  },
  "code":
  "\n  search items/ as @x return (\n    avatar_url as avatar,\n    url,\n    login as handle,\n    type\n  )\n  from http(\n    'https://api.github.com/search/users',\n    @{q:(coalesce(@params->(''),'example'))}\n  )\n  "
},

{
  "doc_id": "github prs/issues",
  "cols": {
    "id": "text",
    "number": "number",
    "title": "text",
    "url": "link",
    "avatar": "image",
    "login": "text",
    "labels": "text",
    "state": "text",
    "locked": "bool",
    "assignee_avatar": "image",
    "assignee_login": "text",
    "milestone": "text",
    "comments": "text",
    "created_at": "datetime",
    "updated_at": "datetime",
    "closed_at": "datetime",
    "author_association": "text",
    "type": "text",
    "draft": "text",
    "html_url": "link",
    "body": "text",
    "reactions": "text"
  },
  "code":
  "\n  search items/ as @x labels/ as @label return (\n    @x->id as id,\n    @x->number as number,\n    @x->title as title,\n    @x->html_url as url,\n    @x->user->avatar_url as avatar,\n    @x->user->login as login,\n    @label->name as labels,\n    @x->state as state,\n    @x->locked as locked,\n    @x->assignee->avatar_url as assignee_avatar,\n    @x->assignee->login as assignee_login,\n    @x->milestone as milestone,\n    @x->comments as comments,\n    @x->created_at as created_at,\n    @x->updated_at as updated_at,\n    @x->closed_at as closed_at,\n    @x->author_association as author_association,\n    @x->type as type,\n    @x->draft as draft,\n    @x->pull_request->html_url as html_url,\n    @x->body as body,\n    @x->reactions->total_count as reactions\n  )\n  from http(\n    'https://api.github.com/search/issues',\n    @{q:(coalesce(@params->(''),'example'))}\n  )\n  "
},

{
  "doc_id": "crypto markets",
  "cols": {
    "image": "image",
    "rank": "int",
    "name": "text",
    "symbol": "text",
    "price": "usd",
    "market_cap": "usd",
    "high_24h": "usd",
    "low_24h": "usd",
    "change_24h": "usd",
    "ath": "usd",
    "ath_date": "datetime"
  },
  "code":
  "\n  search / return (\n    image,\n    market_cap_rank as rank,\n    name,\n    symbol,\n    current_price as price,\n    market_cap,\n    high_24h,\n    low_24h,\n    price_change_percentage_24h as change_24h,\n    ath,\n    ath_date\n  )\n  from http(\n    'https://api.coingecko.com/api/v3/coins/markets',\n    @{vs_currency:'usd', `order`:'market_cap_desc', per_page:100}\n  )\n  "
},
{
  "doc_id": "crypto search",
  "cols": {
    "thumb": "image",
    "rank": "int",
    "name": "text",
    "symbol": "text"
  },
  "code":
  "\n  search coins/ return (\n    market_cap_rank as rank,\n    thumb,\n    symbol,\n    name\n  )\n  from http(\n    'https://api.coingecko.com/api/v3/search',\n    @{`query`:(coalesce(@params->(''),'bitcoin'))}\n  )\n  "
},

{
  "doc_id": "hackernews search",
  "cols": {
    "id": "text",
    "url": "link",
    "title": "text",
    "author": "text",
    "points": "num",
    "comments": "num",
    "created_at": "datetime",
    "updated_at": "datetime"
  },
  "code":
  "\n  search hits/ return (\n    story_id as id,\n    url,\n    title,\n    author,\n    points,\n    num_comments as comments,\n    created_at,\n    updated_at\n  )\n  from http(\n    'https://hn.algolia.com/api/v1/search',\n    @{`query`:(coalesce(@params->(''),'example'))}\n  )\n  "
},

{
  "doc_id": "hackernews frontpage",
  "cols": {
    "title": "text",
    "author": "text",
    "hn_url": "link",
    "url": "link",
    "published_at": "datetime"
  },
  "code":
  "\n  search items/ return (\n    title,\n    author->name as author,\n    external_url as hn_url,\n    url,\n    date_published as published_at\n  )\n  from http('https://corsproxy.io/?url=https://hnrss.org/frontpage.jsonfeed')\n  "
},

{
  "doc_id": "arxiv search",
  "cols": {
    "id": "link",
    "title": "text",
    "summary": "text",
    "published": "datetime",
    "updated": "datetime",
    "authors": "text",
    "cat": "text"
  },
  "code":
  "\n  search entries / return (\n    id,\n    categories as cat,\n    title,\n    summary,\n    authors,\n    published,\n    updated\n  )\n  from http(\n    'https://export.arxiv.org/api/query',\n    @{search_query:('all:' || (@params->(''))), max_results:25}\n  )\n  "
},

{
  "doc_id": "wikipedia search",
  "cols": {
    "thumb": "image",
    "title": "text",
    "url": "link"
  },
  "code":
  "\n  search `query` pages / return (\n    thumbnail->source as thumb,\n    title,\n    fullurl as url\n  )\n  from http(\n    'https://en.wikipedia.org/w/api.php',\n    @{\n       action:'query',\n       generator:'search',\n       prop:'pageimages|info',\n       piprop:'thumbnail',\n       pithumbsize:100,\n       inprop:'url',\n       exintro:'',\n       explaintext:'',\n       `gsrsearch`:(@params->('')),\n       gsrlimit:25,\n       format:'json',\n       origin:'*'\n     }\n  )\n  "
},
{
  "doc_id": "reddit search",
  "cols": {
    "thumbnail": "image",
    "ups": "int",
    "downs": "int",
    "comms": "int",
    "created": "datetime",
    "subreddit": "text",
    "subs": "int",
    "title": "text",
    "selftext": "text",
    "author": "text",
    "is_self": "bool",
    "spoiler": "bool",
    "type": "text",
    "domain": "text",
    "reddit_url": "link",
    "url": "link",
    "dest_url": "link"
  },
  "code":
  "\n  search data children / data return (\n    thumbnail,\n    title,\n    'https://old.reddit.com'||permalink as reddit_url,\n    url,\n    url_overridden_by_dest as dest_url,\n    domain,\n    post_hint as type,\n    ups,\n    downs,\n    is_self,\n    spoiler,\n    num_comments as comms,\n    author,\n    subreddit,\n    subreddit_subscribers as subs,\n    created\n  )\n  from http(\n    'https://www.reddit.com/search.json',\n    @{q:(@params->('')), limit:25}\n  )\n  "
},

{
  "doc_id": "subreddit posts",
  "cols": {
    "thumbnail": "image",
    "ups": "int",
    "downs": "int",
    "comms": "int",
    "created": "datetime",
    "subreddit": "text",
    "subs": "int",
    "title": "text",
    "selftext": "text",
    "author": "text",
    "is_self": "bool",
    "spoiler": "bool",
    "type": "text",
    "domain": "text",
    "reddit_url": "link",
    "url": "link",
    "dest_url": "link"
  },
  "code":
  "\n  search data children / data return (\n    thumbnail,\n    title,\n    'https://old.reddit.com'||permalink as reddit_url,\n    url,\n    url_overridden_by_dest as dest_url,\n    domain,\n    post_hint as type,\n    ups,\n    downs,\n    is_self,\n    spoiler,\n    num_comments as comms,\n    author,\n    subreddit,\n    subreddit_subscribers as subs,\n    created\n  )\n  from http(\n    'https://www.reddit.com/r/'||(coalesce(@params->(''),'all'))||'.json',\n    @{limit:25}\n  )\n  "
},
{
  "doc_id": "manifold search",
  "cols": {
    "creatorAvatarUrl": "image",
    "url": "link",
    "question": "text",
    "creatorUsername": "text",
    "createdTime": "datetime",
    "closeTime": "datetime",
    "resolutionTime": "datetime",
    "resolution": "text",
    "totalLiquidity": "float",
    "volume": "float",
    "volume24Hours": "float",
    "uniqes": "int",
    "pool": "text"
  },
  "code":
  "\n  search / return (\n    url,\n    question,\n    creatorUsername,\n    createdTime,\n    closeTime,\n    resolutionTime,\n    resolution,\n    volume24Hours,\n    volume,\n    totalLiquidity,\n    uniqueBettorCount as uniqes,\n    pool\n  )\n  from http(\n    'https://api.manifold.markets/v0/search-markets',\n    @{term:(@params->('')), limit:25}\n  )\n  "
},

{
  "doc_id": "manifold trending", 
  "cols": {
    "creatorAvatarUrl": "image",
    "url": "link",
    "question": "text",
    "creatorUsername": "text",
    "createdTime": "datetime",
    "closeTime": "datetime",
    "resolutionTime": "datetime",
    "resolution": "text",
    "outcomeType": "text",
    "totalLiquidity": "float",
    "volume": "float",
    "volume24Hours": "float",
    "uniqes": "int",
    "pool": "text"
  },
  "code":
  "\n  search / return (\n    url,\n    question,\n    creatorUsername,\n    createdTime,\n    closeTime,\n    resolutionTime,\n    resolution,\n    volume24Hours,\n    volume,\n    totalLiquidity,\n    uniqueBettorCount as uniqes,\n    pool,\n    outcomeType\n  )\n  from http(\n    'https://api.manifold.markets/v0/markets',\n    @{limit:25}\n  )\n  "
},

{
  "doc_id": "yahoo finance news",
  "cols": {
    "link": "link",
    "thumb": "image", 
    "title": "text",
    "publisher": "text",
    "publishedAt": "datetime",
    "tickers": "text"
  },
  "code":
  "\n  search news / return (\n    link,\n    thumbnail->resolutions->(1)->url as thumb,\n    title,\n    publisher,\n    providerPublishTime as publishedAt,\n    relatedTickers as tickers\n  )\n  from http(\n    'https://corsproxy.io/?url=https://query1.finance.yahoo.com/v1/finance/search',\n    @{q:(@params->(''))}\n  )\n  "
},

{
  "doc_id": "speedrun.com games",
  "cols": {
    "thumb": "image",
    "url": "link",
    "title": "text",
    "release": "date",
    "discord": "link"
  },
  "code":
  "\n  search data / return (\n    weblink as url,\n    assets->`cover-tiny`->uri as thumb,\n    names->international as title,\n    `release-date` as release,\n    discord\n  )\n  from http('https://www.speedrun.com/api/v1/games', @{name:(@params->(''))})\n  "
},

{
  "doc_id": "speedrun.com users",
  "cols": {
    "url": "link",
    "thumb": "image",
    "name": "text",
    "location": "text",
    "signup": "datetime",
    "twitch": "link",
    "hitbox": "link",
    "youtube": "link",
    "twitter": "link",
    "live": "link"
  },
  "code":
  "\n  search data / return (\n    weblink as url,\n    assets->image->uri as thumb,\n    names->international as name,\n    location->region->code as location,\n    twitch->uri as twitch,\n    hitbox->uri as hitbox,\n    youtube->uri as youtube,\n    twitter->uri as twitter,\n    speedrunslive->uri as live,\n    signup\n  )\n  from http('https://www.speedrun.com/api/v1/users', @{name:(@params->(''))})\n  "
},

{
  "doc_id": "apple podcast search",
  "cols": {
    "artistName": "text",
    "trackName": "text",
    "trackUrl": "link",
    "feedUrl": "link",
    "artworkUrl100": "image",
    "releaseDate": "datetime",
    "genre": "text"
  },
  "code":
  "\n  search results / return (\n    feedUrl,\n    trackViewUrl as trackUrl,\n    artworkUrl100,\n    artistName,\n    trackName,\n    primaryGenreName as genre,\n    releaseDate\n  )\n  from http(\n    'https://itunes.apple.com/search',\n    @{term:(@params->('')), media:'podcast', limit:25}\n  )\n  "
},

{
  "doc_id": "ossinsights.io trends",
  "cols": {
    "repo": "text",
    "lang": "text",
    "desc": "text",
    "stars": "int",
    "forks": "int",
    "prs": "int",
    "pushes": "int",
    "total_score": "float",
    "contributors": "text"
  },
  "code":
  "\n  search data `rows` / return (\n    repo_name as repo,\n    description as `desc`,\n    primary_language as lang,\n    contributor_logins as contributors,\n    stars,\n    forks,\n    pull_requests as prs,\n    pushes,\n    total_score\n  )\n  from http(\n    'https://api.ossinsight.io/v1/trends/repos',\n    @{keyword:(@params->(''))}\n  )\n  "
}

]$$,$$
  \n  $$,'\n')::jsonb) as s(doc_id text, code text, cols jsonb)
on conflict (doc_id) 
do update set row_0 = excluded.row_0;
