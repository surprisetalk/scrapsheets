<!-- deno-fmt-ignore-file -->

# Scrapsheets Roadmap

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

---

## Misc

- [ ] https://github.com/bgreenwell/xleak
- [ ] https://github.com/CodeOne45/vex-tui
- [ ] https://github.com/vkobinski/CacTui

---

## Phase 0 — Patch

Close the gaps that prevent daily use and public demo.

- [x] **DocDelta efficiency**: send actual Automerge deltas instead of full doc on every change (Main.elm:194, index.html docChanged handler)
- [x] **Boolean cell editing guard**: don't open text editor on boolean/non-editable cells; toggle checkbox instead (Main.elm:1389)
- [x] **DSN encryption**: encrypt external database connection strings with pgcrypto (db.sql:30)
- [ ] **Automerge WebSocket adapter**: replace custom HonoWebSocketAdapter with official NodeWebSocketAdapter (main.ts:465)
- [x] **Net sheet ID normalization**: accept doc_id instead of sheet_id for /net/:id endpoint (main.ts:1547)
- [ ] **Sheet preview thumbnails**: generate mini sparkline/heatmap SVG for library view (Main.elm:335)
- [x] **URL query operators**: support ?q=+any, ?q=-any, ?q==any for bookmarkable filtered views (Main.elm:902)
- [x] **row_0 validation**: add check constraint ensuring row_0 is a JSONB array (db.sql:24)
- [x] **Net table headers**: store req/res metadata and HTTP headers alongside webhook body (db.sql:40)

---

## Phase 1 — Foundation

Make Scrapsheets reliable enough to be someone's primary data tool.

### Marketplace Payments

- [ ] **Stripe Checkout integration**: create checkout session on /buy/:id with sell_price, redirect to Stripe
- [ ] **Stripe webhooks**: complete purchase on payment confirmation, record transaction
- [ ] **Payment table**: add `payment` table tracking buyer, seller, amount, sheet, timestamp
- [ ] **stripe_customer_id on usr**: link users to Stripe customers for future payouts
- [ ] **Seller payouts**: Stripe Connect for marketplace payouts (can defer, collect platform-side first)

### MCP Server

- [ ] **Implement /mcp/:id endpoint**: replace 501 stub with Model Context Protocol server (main.ts:1824)
- [ ] **read_sheet tool**: return sheet data as structured JSON via MCP
- [ ] **write_cells tool**: update specific cells via MCP
- [ ] **query_sheet tool**: execute SQL/PRQL against sheets via MCP
- [ ] **list_sheets tool**: enumerate user's library via MCP

### Net Sheet UI

- [ ] **net-http config view**: URL input, polling interval selector, header editor
- [ ] **net-socket config view**: WebSocket URL input, connection status indicator
- [ ] **net-hook log view**: table of incoming webhook payloads from `net` table
- [ ] **Net sheet creation**: add net-hook/http/socket options to "new sheet" menu

### Security

- [x] **Codex DSN validation**: block connections resolving to application's own database (main.ts:1567)
- [x] **Codex read-only mode**: SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY for external DBs
- [x] **Codex query timeout**: add connection and statement timeouts for external DB queries
- [x] **Codex rate limiting**: per-user query rate limits for external database access

---

## Phase 2 — Growth

Build the features that create network effects. This is where the spreadsheet OS vision comes alive.

### Scrapscript Integration

- [ ] **Scrapscript WASM runtime**: compile Scrapscript interpreter to WASM for browser execution
- [ ] **Scrapscript query language**: wire up the existing `Scrapscript` lang type (Main.elm:380) to execute Scrapscript programs against sheet data
- [ ] **Scrapscript cell formulas**: `=#` prefix in cells triggers Scrapscript evaluation
- [ ] **Content-addressable formulas**: cross-sheet references via Scrapscript hashes instead of fragile @sheet_id strings
- [ ] **Scrapscript marketplace**: sell/share self-contained Scrapscript functions as composable sheet utilities

### Cell Formulas

- [ ] **Formula parser**: `=` prefix in cells triggers formula mode using the existing `Formula` lang type (Main.elm:379)
- [ ] **Basic arithmetic**: =A1 + B1, =SUM(A1:A10), =AVERAGE, =COUNT, =MIN, =MAX
- [ ] **Cross-sheet references**: =@table:abc123.A1
- [ ] **Dependency tracking**: topological sort for evaluation order, cycle detection
- [ ] **Reactive recalculation**: formulas re-evaluate when referenced cells change

### Sharing & Permissions

- [ ] **Role column on sheet_usr**: add owner/editor/viewer roles (currently flat access)
- [ ] **Share dialog UI**: email input + role selector in sheet settings
- [ ] **Public/private toggle**: wire up existing Peers = Private | Public type in Elm
- [ ] **Shareable view-only links**: unauthenticated read access via signed URLs
- [ ] **Role-aware sync policy**: extend sharePolicy (main.ts:471) to respect viewer vs editor

### Data Pipelines

- [ ] **Schedule field on sheet**: cron expression or interval for recurring query execution
- [ ] **Server-side query runner**: Deno cron job executing scheduled queries
- [ ] **Pipeline composition**: net-http (extract) -> query (transform) -> table (load)
- [ ] **Execution log**: track pipeline runs, successes, failures per sheet

### External Integrations

- [ ] **Google Sheets codex**: OAuth flow for reading Google Sheets as codex data source (main.ts:1683)
- [ ] **Airtable codex**: OAuth flow for reading Airtable bases as codex data source
- [ ] **Notion codex**: OAuth flow for reading Notion databases as codex data source
- [ ] **Bidirectional sync**: write Scrapsheet data back to external sources

---

## Phase 3 — Moonshot

Transform Scrapsheets into a platform. Each feature creates a new axis of composability.

### Sheet-as-API

- [ ] **Stable REST endpoint**: GET /api/v1/sheet/:id returns JSON, POST writes rows
- [ ] **API key auth**: per-sheet API keys (not just JWT) for programmatic access
- [ ] **OpenAPI auto-generation**: derive OpenAPI spec from sheet column types
- [ ] **Change webhooks**: notify external URLs when sheet data changes
- [ ] **Rate limits per sheet**: configurable throttling for public sheet APIs

### Custom Portals

- [ ] **User-defined portal sources**: provide a WebSocket URL or HTTP polling config to create custom portals
- [ ] **Portal marketplace**: sell live data feeds as portal sheets
- [ ] **Portal composition**: query across multiple live portals in real-time

### Embeddable Sheets

- [ ] **Embed endpoint**: /embed/:id renders minimal-chrome read-only sheet view
- [ ] **Embed code generator**: copy-pasteable iframe snippet in sheet settings
- [ ] **Interactive embeds**: viewers can sort/filter embedded sheets
- [ ] **Live dashboard mode**: combine portal data + embeds for real-time dashboards

### Collaboration

- [ ] **Cursor presence**: show other users' cursor positions with colored indicators
- [ ] **Active user list**: display connected collaborators in sheet header
- [ ] **Cell comments**: annotation system with threads on individual cells
- [ ] **Presence indicators in library**: show which sheets have active editors

### Version History

- [ ] **Timeline view**: slider showing document state over time (Automerge stores full history)
- [ ] **Diff view**: visual diff between any two versions
- [ ] **Named snapshots**: user-created bookmarks in the version timeline
- [ ] **Rollback**: restore sheet to any historical state

### Offline & Mobile

- [ ] **PWA manifest**: installable progressive web app with offline support
- [ ] **IndexedDB-first sync**: Automerge already uses IndexedDB; optimize for offline editing
- [ ] **Responsive mobile layout**: touch-friendly cell editing, swipe navigation
- [ ] **Conflict resolution UI**: surface Automerge merge conflicts for user review

---

## Strategic Notes

**The flywheel**: marketplace payments (1) attract template creators -> MCP server (1) makes sheets AI-accessible -> Scrapscript (2) is the moat no one can replicate -> pipelines (2) make sheets self-updating -> sheet-as-API (3) makes every sheet a microservice.

**What ships at 70%**: Phase 0 + Stripe + MCP is enough to launch publicly. Everything else compounds on top.

**The unique position**: Scrapsheets is not Google Sheets. It is not Airtable. It is a programmable data OS where every table is a queryable database, every query result is a shareable table, every portal is a live data stream, every sheet is an API, and every formula is a content-addressable program.

---

<!--
    https://lexega.com/blog/how-lexega-turns-sql-into-signals

    - [ ] Reach out to Ellen Chisa (via brandon?) with scrapsheets demo

    Become the worlds data authority for public entities. Orgs, people, parcels, colors, songs, everything!

    - eat ultorg
    - eat rowboat
    - eat excel addons
    - eat wolfram
    - eat airtable
    - eat retool
    - eat gsuite
    - eat linear/jira

    - library
      - examples
      - key shortcuts
      - tutorial

    giant table/database of events (esp. local events)

    https://www.airtable.com/universe
    https://sourcetable.com/excel-templates

    instead of sending pitch deck, just post cool demo and say looking for angels/funding
    set up customer funnel
    start advertising scrapsheets on reddit, etc
    scrapsheets podcast circuit interviews
    pitch customers for scrapsheets

    1. for devs: gh issues and demo and blog post ; rally help
    2. user testing and create issues ; clark, kirk, jake ?
    3. for vcs: pitch deck / demo ; /scrapland
    4. podcast circuit
    5. customers
    6. community: capture the data community. become the face of data analysis, curation, sanitization, etc.

    potential customers:
      ## Financial/Crypto Data
      - **Marketstack** - Simple stock market API, relatively new player
      - **CoinAPI** - Smaller crypto data provider with flexible terms
      - **Finnhub** - Nordic startup with stock/forex/crypto APIs
      - **FMP (Financial Modeling Prep)** - Growing financial data provider
      - **CoinGecko** - Popular but still indie crypto data provider
      - **Nomics** - Crypto API company (recently shut down but team might be starting something new)
      ## Alternative Data
      - **Thinknum** - Alternative data tracking company websites
      - **SimilarWeb API** - Web traffic analytics (smaller than competitors)
      - **Apify** - Web scraping platform with data feeds
      - **Bright Data** - Web data collection platform
      - **ScrapingBee** - Web scraping API service
      ## News & Sentiment
      - **Newsdata.io** - Smaller news aggregation API
      - **MediaStack** - News API from the Marketstack team
      - **CurrentsAPI** - Latest news API service
      - **GNews API** - Google News aggregation service
      ## Weather & Environmental
      - **ClimaCell (now Tomorrow.io)** - Weather tech startup
      - **Visual Crossing** - Weather data API with reasonable pricing
      - **Meteomatics** - Swiss weather data company
      ## Sports & Gaming
      - **API-Football** - Soccer/football data API
      - **SportDataAPI** - Smaller sports data provider
      - **PandaScore** - Esports data API company
      ## Shipping & Logistics
      - **Shippo** - Shipping API that might have tracking data
      - **EasyPost** - Another shipping API with tracking feeds
      - **AfterShip** - Shipment tracking API
      ## Real Estate
      - **Rentberry** - Rental market data
      - **RentSpree** - Rental listing data
      - **Estated** - Property data API

    potential customers:
      Financial & Market Data:
      - Refinitiv (formerly Thomson Reuters) - Real-time market data feeds
      - Bloomberg Data License - Enterprise financial data streaming
      - IEX Cloud - Stock market data with transparent pricing
      - Alpha Vantage - Stock, forex, and crypto data APIs
      - Twelve Data - Real-time and historical market data
      - Tiingo - Financial markets data with WebSocket support
      - Intrinio - Financial data feeds for various asset classes
      Social Media & Web Data:
      - Twitter/X Enterprise API - Full firehose of tweets (very expensive)
      - Reddit Data API - Commercial access to Reddit data streams
      - Webhose.io - Web crawling and news/forum/blog data
      - Brandwatch - Social media monitoring firehose
      - DataSift - Social media data aggregation (now part of Meltwater)
      Weather & Environmental:
      - Weather Source - Real-time weather data streams
      - AerisWeather - Weather data firehose with various endpoints
      - OpenWeather - Weather data with bulk/streaming options
      - Climate.com - Agricultural and weather data feeds
      Transportation & Logistics:
      - FlightAware - Real-time flight tracking data
      - FlightStats - Aviation data firehose
      - HERE Technologies - Traffic and location data streams
      - TomTom - Traffic flow and incident data
      News & Media:
      - NewsAPI - Real-time news aggregation
      - Aylien - News data and sentiment analysis
      - Benzinga - Financial news wire service
      - Accern - AI-powered news analytics
      Sports Data:
      - Sportradar - Comprehensive sports data feeds
      - Stats Perform - Sports statistics and live data
      - The Odds API - Sports betting odds data
      Crypto & Blockchain:
      - CryptoCompare - Cryptocurrency market data
      - Kaiko - Institutional-grade crypto data
      - Messari - Crypto research and data feeds
      - Amberdata - Blockchain and crypto market data

    IT Portfolio Management | Smartsheet: https://www.smartsheet.com/solutions/project-portfolio-management/it-portfolio-management
    Services Delivery | Smartsheet: https://www.smartsheet.com/solutions/project-portfolio-management/services-delivery
    Marketing management | Smartsheet: https://www.smartsheet.com/solutions/marketing/marketing-management
    Business PMO | Smartsheet: https://www.smartsheet.com/solutions/project-portfolio-management/business-pmo
    Project Management Solutions | Smartsheet: https://www.smartsheet.com/solutions/project-management
    New Product Development | Smartsheet: https://www.smartsheet.com/solutions/project-portfolio-management/new-product-development
    Capital Projects | Smartsheet: https://www.smartsheet.com/solutions/project-portfolio-management/capital-projects
    Solutions for Higher Education | Smartsheet: https://www.smartsheet.com/solutions/education
    Construction | Smartsheet: https://www.smartsheet.com/solutions/construction
    Tech Project Management Software | Smartsheet: https://www.smartsheet.com/solutions/technology
    Marketing management | Smartsheet: https://www.smartsheet.com/solutions/marketing/marketing-management
    Agency Management | Smartsheet: https://www.smartsheet.com/solutions/marketing/agency-management
    Event Management | Smartsheet: https://www.smartsheet.com/solutions/marketing/event-management
    Serve Without Barriers | Smartsheet: https://www.smartsheet.com/solutions/government-state-and-local
    Save the drama for the movies. | Smartsheet: https://www.smartsheet.com/solutions/media-and-entertainment
    Manufacturing Project Management Software | Smartsheet: https://www.smartsheet.com/solutions/manufacturing
    Energy and Utilities Project Management Software | Smartsheet: https://www.smartsheet.com/solutions/energy-and-utilities


    YouTube Channels:
    - ExcelIsFun - Features hundreds of Excel tutorials from basic to advanced
    - Leila Gharani - Offers professional Excel and Office tutorials
    - Kevin Stratvert - Provides clear Excel tutorials for all skill levels
    - Chandoo.org - Known for data analysis and dashboard tutorials
    - Excel Campus with Jon - Focuses on Excel tips and automation
    Podcasts:
    - Spreadsheet Radio - Dedicated to spreadsheet topics and interviews
    - The Spreadsheet Show - Discusses Excel tips and tricks
    - MyExcelOnline Podcast - Covers Excel and business analytics
    - Humans of Data - Broader data topics including spreadsheet usage
    - Analytics on Fire - Business intelligence and Excel analytics


    We work with several different systems across the company. Finance tends to be the department that needs to pull everything together for the full picture.
    We have our main ERP (Enterprise Resource Planning) system that handles accounting, purchasing, and inventory. That's where most of our financial transactions live. Then there's our CRM (Customer Relationship Management) system for sales data and customer information.
    For payroll and HR, we use a separate HRIS platform. The marketing team has their own analytics tools that track campaign performance metrics. And we have a few specialized systems for production planning and supply chain management.
    The challenge is that none of these systems talk to each other perfectly, so I often need to export data from each one into spreadsheets, clean it up, standardize formatting, and then combine everything for analysis. That's where Power Query has been so helpful—it remembers the cleaning steps so I don't have to manually fix the same issues every month.

    In a typical week, most of my time goes toward monthly financial close and reporting. That means reconciling accounts, preparing financial statements, and creating the management reports that help leadership understand our current financial position. It's detail-oriented work that requires accuracy and consistency.
    My biggest value-add to the company is probably my financial modeling and scenario analysis. When the executive team is considering a major decision—like expanding into a new market or launching a product line—I build the models that show the potential financial impacts under different scenarios.

    There are definitely several manual processes that create bottlenecks in our financial workflows. The most time-consuming ones include:
    Accrual spreadsheets - We track expenses that haven't been invoiced yet through a complex spreadsheet with multiple tabs. Department managers email their accruals, which I manually consolidate. It's prone to errors when formulas get accidentally overwritten or links break between sheets.
    Allocation calculations - We allocate certain overhead costs across departments based on headcount or square footage. This requires maintaining separate allocation spreadsheets that feed into the main financial reports. When departments reorganize, updating these allocations becomes tedious.
    Revenue recognition - For long-term contracts and projects, we track percentage completion and revenue recognition in spreadsheets outside our ERP system. This requires manual updates from project managers and reconciliation with our accounting system.
    Variance analysis - After closing each month, I create variance reports comparing actual results to budget and prior periods. While the data extraction is semi-automated, the analysis of unusual variances still requires manual investigation and documentation in spreadsheets.
    Sales commission calculations - Our commission structure has exceptions and special incentives that the payroll system can't handle. So we maintain separate spreadsheets with complex formulas to calculate commissions, which then have to be manually entered into payroll.

    If I could wave a magic wand, I'd love to have these datasets seamlessly integrated:
    Real-time cash flow data - Currently, we only see our cash position when accounting reconciles the bank accounts. Having automated daily cash flow visibility would help tremendously with liquidity planning.
    Customer payment behavior analytics - We track basic aging reports, but having predictive insights on which customers are likely to pay late based on historical patterns would help with collections strategy.
    Supply chain cost fluctuations - Our purchasing data exists in a separate system, making it difficult to quickly assess how component price changes affect our overall product margins.
    Competitor pricing intelligence - We collect this ad hoc now, but having structured data on competitor pricing would improve our strategic pricing decisions.
    Marketing attribution data connected to revenue - Seeing exactly which marketing channels drive profitable sales would help optimize our marketing spend.

    If I could wave a magic wand for third-party data integration, I'd love to have:
    Economic indicators automatically linked to our forecasting models - Getting GDP growth rates, inflation metrics, and industry-specific indicators plugged directly into our planning spreadsheets would make our projections much more accurate.
    Industry benchmarking data - Having automated feeds of industry averages for metrics like gross margin, operating expenses, and inventory turnover would help us understand how we're performing relative to peers.
    Real-time market pricing data for raw materials - For companies in our supply chain, having commodities pricing integrated would help anticipate cost fluctuations before they hit our purchasing department.
    Credit risk scoring data - Automatic updates on customer credit scores and financial health would improve our credit decisions and reduce bad debt exposure.
    Weather pattern data for seasonal forecasting - This sounds strange, but our business has surprising seasonality tied to weather patterns. Having this data integrated would improve our inventory planning.

    *** Real-time market pricing data for raw materials - For companies in our supply chain, having commodities pricing integrated would help anticipate cost fluctuations before they hit our purchasing department.

    Currently, our process for tracking raw material pricing is pretty manual and reactive. We don't have a systematic approach - it's more of a patchwork:
    Our purchasing team gets pricing during the actual negotiation process with suppliers, which means we often don't know about price changes until we're placing orders.
    For key commodities, we have a junior analyst who manually pulls data from public sources like government websites and industry publications on a weekly basis and updates a tracking spreadsheet.
    We subscribe to a few industry newsletters that sometimes mention pricing trends, but it's not comprehensive data - just general directional information.
    For more specialized materials, we rely heavily on our suppliers to inform us about market conditions, which obviously creates a biased perspective since they have incentives to justify price increases.
    Occasionally, we'll purchase one-off market reports when we need to make major strategic decisions, but these quickly become outdated.
    This approach leaves us constantly reacting to price changes rather than anticipating them. We've discussed subscribing to commodity pricing data services, but it's been tough to justify the expense for the specific materials we use most. Having this data automatically integrated would help us identify pricing trends earlier and improve our negotiating position with suppliers.

    ERP integration is a fascinating and often challenging aspect of finance work! Here's what I've experienced:
    ERP (Enterprise Resource Planning) systems are meant to be the central nervous system of a company's operations, but integrating them with other software can be quite complex. Our ERP system houses our core financial data, but struggles to connect seamlessly with other platforms.
    The main integration challenges we face include:
    Data mapping inconsistencies - Our ERP uses different field names and data structures than our other systems. For example, customer IDs in our ERP might be formatted differently than in our CRM system.
    API limitations - Our older ERP has limited API capabilities, which restricts how other systems can pull or push data to it. We often resort to scheduled data exports and imports rather than real-time connections.
    Customization complexities - Over the years, we've customized our ERP to fit specific business processes, which makes standard integration tools less effective since they expect vanilla implementations.
    Point-to-point connections - We've built several one-off integrations between specific systems, resulting in a spiderweb of connections that's difficult to maintain. When one system changes, multiple integrations can break.
    Some companies solve this with middleware or integration platforms like MuleSoft or Boomi, but we're still using more basic ETL (Extract, Transform, Load) tools and custom scripts. There's always a backlog of integration requests from different departments wanting their systems connected to the ERP data.
    The vendors always make it sound easier than it actually is in real-world implementation!

    Ah, niche financial problems that outsiders wouldn't understand? lowers voice slightly
    The absolute nightmare of intercompany transactions when subsidiaries are involved. Trying to track funds flowing between different parts of the same company might sound simple, but it creates these circular references that make reconciliation a puzzle from hell.
    Accrual accounting versus cash flow is another one. We report financials on an accrual basis, but executives often want to know about actual cash movement. Having to constantly switch between these two perspectives and explain why they don't match is exhausting.
    Foreign currency variances can ruin a perfectly balanced forecast. We have some international clients, and even small exchange rate fluctuations can turn a profitable deal into a loss-maker overnight through no fault of our operations.
    Oh, and depreciation schedules! We have ongoing debates about useful life estimates for our assets. Sounds boring, but it directly impacts our profit numbers and tax position.
    Then there's the LIFO versus FIFO inventory valuation debate that makes my accounting colleagues passionate but puts everyone else to sleep. It drastically changes our cost of goods sold depending which method we use.

    For tracking these complex financial processes, we rely on several specialized tools. Our tech stack has evolved over time as the company has grown.
    Our enterprise resource planning (ERP) system is the backbone—we use NetSuite, which integrates our accounting, inventory management, and financial reporting. It handles both accrual-based accounting and cash flow tracking in one system, though sometimes the reporting capabilities aren't as flexible as I'd like.
    For deeper financial analysis and forecasting, Excel is still king. I've built some pretty complex models with multiple scenarios for capital expenditures and their depreciation impacts. We're slowly moving some of this to Power BI for better visualization and data connections.
    For cash flow projections specifically, we use Adaptive Planning. It lets us create rolling 13-week cash flow forecasts that pull in data from our accounts receivable aging reports and accounts payable schedules. This helps us spot potential cash crunches weeks in advance.
    Treasury management is another area with specialized tools. We use Kyriba to manage our actual bank accounts, track daily cash positions, and handle any foreign exchange needs for our international transactions.
    The most frustrating part is that these systems don't always talk to each other seamlessly. I often find myself exporting data from one system and importing it into another, with manual adjustments along the way. We're working on better integrations, but that's a slow process requiring both IT resources and budget.

    For finance specifically, Anaplan is gaining popularity. It's designed for financial planning and would let us create those investor reports directly by pulling from our source systems. The downside is that it's quite expensive.
    Domo is one I've been pushing for recently. It's cloud-based with strong data integration capabilities and customizable dashboards that would work well for both our internal financial reporting and investor presentations.

    Organizations tackle this problem in several ways:
    Data warehouses are one traditional approach. Companies like Snowflake and Amazon Redshift pull data from various systems into a centralized repository where it's standardized and cleaned. The finance team and other departments then query this warehouse rather than individual systems.
    Master data management (MDM) systems focus specifically on harmonizing core business data across systems. They establish governance processes to ensure data consistency and quality. This is especially important for financial data where accuracy is critical.
    Data virtualization is a newer approach that creates a logical layer over existing systems without physically moving data. Tools like Denodo let you query data across systems as if they were one source. This can be more cost-effective than building a data warehouse but may have performance limitations.
    Some organizations even take a more manual approach with designated "data stewards" - people responsible for reconciling differences between systems and publishing official numbers. We currently use this approach, with me being the unofficial data steward, but it's time-consuming and error-prone.

    Oh, you're asking about the most universally despised financial tools that everyone seems stuck with anyway? I can definitely share some thoughts on that!
    Excel is simultaneously the most loved and hated tool in finance. Despite its limitations, it's still where most financial work happens. The version control is abysmal - you end up with files named "Budget_v3_FINAL_ACTUALLY_FINAL_2_March23.xlsx" and nobody knows which is current. The crashes when working with large datasets are legendary, especially when pivot tables are involved.
    QuickBooks is another pain point, particularly for small to mid-sized businesses. It's often the first accounting system companies use, but they outgrow it quickly while remaining dependent on it. The reporting is clunky, customization options are limited, and extracting data requires workarounds.
    Legacy ERP systems like older versions of SAP and Oracle are notorious for terrible user interfaces that feel designed in the 1990s. They're powerful but incredibly unintuitive, requiring specialized knowledge to navigate effectively. Many finance professionals dread having to dig into these systems.
    Concur for expense reporting generates universal groans. The mobile app is frustrating, receipt matching is hit-or-miss, and the approval workflows often get stuck in mysterious ways.
    Finally, there's the special hatred reserved for tax preparation software. Annual updates introduce new bugs, and navigating complex tax scenarios often requires bizarre workarounds that tax professionals pass around like secret knowledge.


    The most annoying niche part of my job that other departments don't understand is probably revenue accruals.
    When clinical departments provide care, they think the money should show up right away in their budget reports. But in reality, we have to recognize revenue when it's "reasonably assured" we'll collect it, which can be months after service was provided. Insurance claims processing, denials, appeals, patient payment plans - all these create timing differences between when care happens and when we can officially count the money.
    So I'm constantly explaining to department directors why their "profitable" month doesn't show full revenue yet, or why last quarter's procedures are still affecting this quarter's numbers. The physicians especially get frustrated by this - they want real-time financial data but healthcare finance just doesn't work that way.

    I use a mix of tools for calculating and tracking revenue accruals:
    For the actual calculations, we work primarily in Excel with some complex financial models. We've built spreadsheets with formulas that factor in our historical collection rates by payer (Medicare, Medicaid, private insurance, etc.) and service line (cardiology, oncology, etc.).
    These models pull data from our hospital's EHR (Electronic Health Record) system - we use Epic - and our billing system. We have SQL queries that extract charge data, claims status, payment history, and denial patterns.
    For more sophisticated analysis, we use Power BI dashboards that visualize collection trends over time. This helps us adjust our accrual rates regularly based on actual performance.
    The most tedious part is reconciliation - making sure our accrual estimates match what we eventually collect. We track this monthly in our financial management system (Oracle Hyperion) and make adjustments quarterly.
    It's not the most elegant system, to be honest. There's a lot of manual work connecting these different tools. Many hospitals are moving toward more automated solutions with predictive analytics, but healthcare finance tends to lag behind other industries in technology adoption.



    Healthcare financial reporting is heavily regulated to ensure transparency, accuracy, and standardization across the industry. Here are the key aspects:
    Medicare and Medicaid regulations are among the most stringent. The Centers for Medicare & Medicaid Services (CMS) requires detailed cost reports that must follow specific accounting methodologies. These reports determine reimbursement rates and require accurate revenue recognition to avoid potential False Claims Act violations.
    For hospitals with bond financing, the Securities and Exchange Commission (SEC) requires regular financial disclosures following Generally Accepted Accounting Principles (GAAP). GAAP has specific rules for revenue recognition in healthcare that we must follow.
    The Healthcare Financial Management Association (HFMA) provides industry-standard guidelines for revenue cycle management and financial reporting that most hospitals adopt.
    Tax-exempt hospitals face additional IRS reporting requirements on Form 990, including community benefit reporting and charity care documentation.
    HIPAA has financial reporting implications too - when reporting on service line revenues, we must ensure patient privacy is maintained in how we categorize and report certain specialized services.
    Internally, our hospital board's finance committee has oversight responsibilities, requiring monthly financial reports with detailed accrual methodologies and variance explanations.

    For Medicare reporting specifically, there's a quarterly process of reconciling our accrual estimates against the Medicare remittance advice data.


    The quarterly Medicare reconciliation process is one of those thankless, complex tasks that few people outside healthcare finance understand. Here's what it actually looks like:
    The process starts when we receive the Medicare Provider Statistics and Reimbursement (PS&R) report, which shows all claims Medicare has processed for our facility in the quarter. This massive data dump arrives via a secure CMS portal.
    I spend about a day extracting this data and formatting it to match our internal tracking systems. Medicare uses their own unique identifiers and coding systems that don't perfectly align with our charge master, so there's a lot of mapping work.
    The heart of the reconciliation is comparing what Medicare actually paid us against what we accrued as expected revenue. I pull this data into a specialized Excel template with tabs for:
    - Inpatient DRG analysis
    - Outpatient APC analysis
    - Professional services
    - Bad debt reimbursement
    - Disproportionate share (DSH) payments
    - Graduate medical education offsets
    The biggest challenge is timing differences. Medicare might adjust claims from previous quarters, recoup payments, or bundle services differently than we anticipated. I have to track each variance back to its source.
    For any significant discrepancies, I work with our billing team to investigate - this might involve pulling individual claims to see why Medicare paid differently than expected. Common issues include coding errors, documentation problems, or changes in Medicare's own payment rules.
    Finally, I prepare a reconciliation report for the CFO that highlights major variances and their causes, along with any adjustments we need to make to our accrual rates going forward. The whole process takes about three full days of work spread across two weeks.
    The most frustrating part? Even after all this work, Medicare can still reopen and adjust claims up to three years later, so our "final" numbers are never truly final.


    Other industries definitely share similar financial reporting and reconciliation headaches. While healthcare has its unique complexities, these industries face comparable challenges:
    Insurance companies have a very similar accrual challenge with the "incurred but not reported" (IBNR) claims - they need to estimate and accrue for claims that have happened but haven't been submitted yet, much like our revenue accruals.
    Construction companies deal with percentage-of-completion accounting, where they have to recognize revenue based on project milestones rather than when cash changes hands. They face similar reconciliation and audit documentation requirements.
    Government contractors have incredibly complex regulatory reporting with the Federal Acquisition Regulation (FAR) and Cost Accounting Standards (CAS) that dictate specific ways to recognize revenue and allocate costs.
    Universities with research grants face similar challenges tracking restricted funds across different fiscal periods and reconciling to sponsor requirements.
    Telecommunications companies deal with complex revenue recognition for bundled services, promotional pricing, and multi-period contracts.
    The oil and gas industry has specialized accounting for reserves and exploration costs that requires significant accrual estimates and reconciliations.
    What makes these industries similar to healthcare is the combination of regulatory oversight, complex revenue timing issues, and the need to constantly reconcile estimates against actual results. They all share the burden of maintaining extensive documentation to justify their accounting approaches.


    Looking at industries with similar reporting headaches, there are several cases where financial professionals regularly pull and respond to public reports:
    Public company financial analysts have to monitor SEC filings (10-Ks, 10-Qs, 8-Ks) on a strict calendar schedule. They need to quickly analyze these reports, identify variances from expectations, and prepare response documents for investors or management.
    Compliance officers at banks must regularly pull regulatory reports like FDIC Call Reports or Federal Reserve filings, reconcile them with internal data, and explain any discrepancies to regulators.
    Tax professionals dealing with multinational corporations face a web of country-specific reporting requirements with different deadlines and formats. They constantly pull public tax data and reporting standards to ensure compliance across jurisdictions.
    Investment fund managers need to track benchmark indices and performance attribution reports, then explain to clients why their returns differed from these benchmarks.
    Real estate investment professionals regularly pull property tax assessments, zoning reports, and market comparables to justify valuations and challenge tax assessments when needed.
    Economic forecasters at central banks or large financial institutions must pull a variety of government statistical releases (employment, inflation, etc.) on predetermined schedules and quickly produce analysis documents.
    The common thread across these examples is the combination of:
    - Externally imposed reporting schedules
    - Complex reconciliation against internal systems
    - The need to explain variances to stakeholders
    - Regulatory consequences for inaccuracies
    These professionals all share that feeling of "reporting day dread" that healthcare finance folks know all too well.

    The challenge is always keeping the data pipelines clean and ensuring that everything is properly tagged and categorized so we can trace decision-making back to specific inputs. We've invested heavily in data governance to make sure we're making apples-to-apples comparisons when blending these different data sources.

    Here are some other essential Excel add-ins that finance professionals commonly use:
    Power Query (Get & Transform) - Helps with data cleaning, transformation, and connecting to various data sources. It's excellent for automating repetitive data preparation tasks.
    Power Pivot - For building data models, creating relationships between tables, and performing advanced calculations with DAX formulas. This is crucial for financial modeling.
    Solver - Built into Excel but often overlooked. It's valuable for optimization problems like portfolio allocation.
    Analysis ToolPak - Provides data analysis tools for statistical and engineering analyses.
    Thomson Reuters Eikon Excel Add-in - Similar to Bloomberg's add-in but pulls data from the Thomson Reuters ecosystem.
    FactSet Office Integration - For those using FactSet, this provides Excel integration with their financial data.
    Capital IQ Excel Add-in - Popular for investment banking, providing access to S&P Capital IQ data.
    Morningstar Direct Excel Add-in - Useful for investment analysis, particularly for mutual funds and ETFs.
    Power BI - While technically separate from Excel, the integration allows for enhanced data visualization capabilities.
    VBA Editor - Not strictly an add-in, but essential for automating complex workflows in financial analysis.

    I'd be happy to share information about popular Excel add-ins. Excel add-ins are tools that extend Excel's functionality for specific tasks or industries. Here are some of the most popular Excel add-ins:
    - Power Query - Helps with data importing and transformation
    - Power Pivot - Enables advanced data modeling and analysis
    - Analysis ToolPak - Microsoft's built-in statistical analysis tool
    - Solver - For optimization problems and what-if analysis
    - PowerBI - Microsoft's business intelligence tool that integrates with Excel
    - Kutools - Collection of 300+ productivity tools and shortcuts
    - Ablebits - Suite of time-saving tools for data processing and management
    - XLSTAT - Statistical add-in for data analysis
    - Data Analysis Plus - Extension of Analysis ToolPak with more functions
    - QI Macros - Six Sigma and statistical analysis tools

    Based on industry trends and market analysis, these Excel add-ins likely generate the most revenue:
    - Enterprise BI solutions: PowerBI and Tableau (Excel integration) are extremely profitable due to their enterprise-wide deployments with recurring subscription models.
    - Industry-specific add-ins: Financial modeling tools like @RISK and Crystal Ball command premium prices ($1,000+ per license) because they serve specialized industries with high willingness to pay.
    - Productivity suites: Ablebits and Kutools target broad markets with accessible pricing, likely generating steady revenue through volume.
    - Statistical analysis tools: XLSTAT and JMP are profitable due to their scientific and research sector focus, allowing them to charge premium prices.
    - Automation solutions: Power Automate (formerly Flow) and other automation add-ins generate significant revenue through Microsoft's ecosystem.

    For Finance and Investment:
    - @RISK: This Monte Carlo simulation tool lets financial analysts model risk and uncertainty in forecasts and investments. Priced at $1,200-$2,500, it's widely used in financial services, insurance, and corporate finance.
    - Crystal Ball: Similar to @RISK but by Oracle, it's used for predictive modeling and forecasting with prices starting around $995.
    - FactSet: Professional-grade financial data and analytics add-in used by investment professionals, with enterprise pricing based on user count.
    For Engineering and Scientific Research:
    - JMP: Statistical discovery software popular in manufacturing and R&D, often costing $1,500+ per license.
    - Minitab: Statistical software for quality improvement and Six Sigma projects, priced at $1,500+ per license.
    - Origin: Data analysis and graphing software for scientists and engineers at about $800+ per license.
    For Project Management and Construction:
    - Primavera P6: Used for complex project management and scheduling with enterprise pricing models.
    - Procore: Construction management integration with Excel for budgeting and reporting.

      For Academic and Research:

    NVivo for Excel: Specialized for qualitative data analysis, popular among academic researchers analyzing interview transcripts and open-ended responses.
    SPSS Excel Integration: Bridges Excel with SPSS statistical software, used heavily in academic social science research.

    For Healthcare:
    - Clinical Research Data Tools: Add-ins like TrialKit that help manage clinical trial data and ensure compliance with healthcare regulations.
    - Medical Coding Assistants: Help with ICD-10 and medical coding integration directly in spreadsheets.
    For Legal Professionals:
    - LexisNexis for Excel: Allows legal professionals to access case law and legal information directly in spreadsheets.
    - Contract Analysis Tools: Help lawyers analyze contract terms across large document sets.
    For Energy and Utilities:
    - Energy Modeling Add-ins: TargetEnergy and EnergyCAP help utilities model consumption patterns and pricing scenarios.
    - Oil & Gas Production Analysis: Tools like PHDWin for petroleum reserve analysis and reporting.
    For Agriculture:
    - Farm Management Add-ins: Tools like FarmERP that integrate weather data, crop planning and yield analysis.
    - Livestock Tracking Systems: Specialized spreadsheet extensions for tracking animals, breeding, and production.
    For Retail and E-commerce:
    - Inventory Optimization Tools: Add-ins that connect with platforms like Shopify and Amazon to optimize inventory levels.
    - Pricing Strategy Tools: Dynamic pricing add-ins for competitive market analysis.

    For Government and Public Sector:
    - FEMA Benefit-Cost Analysis: Helps government agencies calculate disaster mitigation project benefits
    - Census Data Integrators: Connect directly to census APIs for demographic analysis and planning
    For Architecture and Design:
    - CAD-to-Excel bridges: Tools that extract bill of materials and specifications from CAD programs
    - Color Palette Generators: Help designers create and manage brand-compliant color schemes
    For Linguistics and Translation:
    - Terminology Management Add-ins: Help translators maintain consistent terminology across documents
    - Corpus Analysis Tools: Allow linguists to analyze word frequency and patterns
    For Music and Entertainment:
    - Royalty Calculation Tools: Help music publishers track and calculate complex royalty payments
    - Production Scheduling Add-ins: Specialized for film and TV production timelines
    For Sports:
    - Sports Analytics Platforms: Advanced statistical analysis for professional teams' player evaluation
    - Race Timing Systems: Integration with timing systems for marathons and other sporting events
    For Real Estate:
    - Property Valuation Models: Specialized add-ins with proprietary algorithms for real estate valuation
    - Mortgage Analysis Tools: Help brokers compare complex loan scenarios
    For Education:
    - Grade Book Extensions: Enhance Excel for teachers with specialized grading functionality
    - Learning Analytics Tools: Help schools analyze student performance data

    For ERP and Business Systems:
    - SAP Excel Add-In: Connects Excel with SAP ERP systems, allowing direct data exchange
    - Oracle SmartView: Bridges Excel with Oracle's enterprise planning applications
    - Dynamics 365 Excel Integration: Microsoft's own connector for its Dynamics business applications
    For CRM:
    - Salesforce Excel Connector: Enables bidirectional data flow between Salesforce and Excel
    - HubSpot Excel Integration: Synchronizes contact and sales data between platforms
    - Zoho Analytics Excel Bridge: Imports and exports data between Zoho CRM and Excel
    For Project Management:
    - Microsoft Project Excel Integrator: Bridges Excel with MS Project
    - Asana2Excel: Links Asana task management with Excel for reporting and analysis
    - Jira Excel Bridge: Connects Excel with Atlassian's Jira for issue tracking
    For Marketing and Analytics:
    - Google Analytics Excel Add-in: Pulls GA data directly into Excel
    - Mailchimp Excel Integrator: Synchronizes email marketing data with Excel
    - SEMrush Excel Bridge: Imports SEO and marketing data
    For Data Science:
    - R Excel Bridge (RExcel): Connects Excel with R statistical programming
    - Python Excel Libraries: Tools like xlwings that bridge Excel and Python
    - MATLAB Excel Link: Integrates MATLAB's analytical capabilities with Excel


    Strategy 5: Data Vendors Selling to Hedge Funds
    Feature focus: Data marketplace (seller side) + API connectivity
    Specific use case: Package and monetize proprietary datasets with live updates and customizable delivery
    Why it works: Many small data vendors struggle with distribution and real-time delivery infrastructure
    Entry point: Partner with alternative data aggregators like Eagle Alpha or Neudata

    Strategy 2: Supply Chain Analysts at Mid-Size Food Distributors
    Feature focus: Real-time data manipulation + ERP connectivity
    Specific use case: Live inventory reconciliation across multiple warehouses with automatic reorder triggers based on spoilage rates
    Why it works: Food distribution has razor-thin margins and spoilage is costly. Real-time visibility is crucial
    Entry point: Target companies with 50-200 employees using NetSuite or SAP Business One

    Strategy 3: Independent Financial Advisors Managing Multi-Family Offices
    Feature focus: Data marketplace + diverse cell types (especially financial formulas)
    Specific use case: Subscribe to real-time alternative data feeds (satellite imagery, web scraping data) to enhance portfolio analysis
    Why it works: These advisors need differentiated insights but can't afford Bloomberg terminals
    Entry point: Focus on RIAs managing $50-500M who already use tools like Addepar

    Strategy 7: Restaurant Group Controllers (5-20 locations)
    Feature focus: ERP connectivity + diverse cell types
    Specific use case: Live P&L by location pulling from Toast POS, payroll systems, and inventory management
    Why it works: Multi-location restaurants need daily visibility but franchise software is expensive/rigid
    Entry point: Focus on fast-casual chains in growth mode, especially those using Toast or Square
--->
