// The seven live demo feeds the server hosts, beside src/examples.mjs where the
// rest of the bundled make-believe lives. None of it is server logic: `init`
// makes a feed's starting state, `tick` advances it and returns the rows to
// send. main.ts owns the socket and the auth check; this file owns the data.
//
// It is the one list of portal names. src/page.mjs reads the names off it for
// the library, rather than writing them down a second time.

export const PORTALS = [
  {
    name: "time",
    ms: 10,
    init: () => null,
    tick: () => ({
      cols: [{ key: 0, name: "time", type: "int" }],
      rows: [{ 0: new Date().getTime() }],
    }),
  },
  {
    name: "stonks",
    ms: 100,
    init: () => ({
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
    }),
    tick: (stonks) => {
      for (const i in stonks) stonks[i] += 0.5 - Math.random();
      return {
        cols: [{ key: 1, name: "price", type: "usd" }, { key: 0, name: "ticker", type: "text" }],
        rows: Object.entries(stonks),
      };
    },
  },
  {
    name: "dice",
    ms: 500,
    init: () => {
      const dice = { d4: 4, d6: 6, d8: 8, d10: 10, d12: 12, d20: 20, d100: 100, coin: 2 };
      const state = {};
      for (const d in dice) state[d] = { roll: 0, total: 0, rolls: 0 };
      return { dice, state };
    },
    tick: ({ dice, state }) => {
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
        rows: Object.entries(state)
          .sort((a, b) => b[1].roll - a[1].roll)
          .map(([name, s]) => ({ 0: name, 1: s.roll, 2: s.total, 3: s.rolls, 4: s.total / s.rolls / dice[name] })),
      };
    },
  },
  {
    name: "orbit",
    ms: 100,
    init: () => ({
      planets: [
        ["Mercury", 40, 4],
        ["Venus", 55, 7],
        ["Earth", 75, 12],
        ["Mars", 95, 20],
        ["Jupiter", 130, 50],
        ["Saturn", 170, 80],
        ["Uranus", 210, 140],
        ["Neptune", 260, 250],
      ],
      seasons: ["spring", "summer", "autumn", "winter"],
    }),
    tick: ({ planets, seasons }) => {
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
        rows: planets.map(([name, dist, period]) => {
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
    },
  },
  {
    name: "cafe",
    ms: 500,
    init: () => ({
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
      ],
      orders: [],
      tick: 0,
    }),
    tick: (s) => {
      s.tick++;
      for (const o of s.orders) o.wait = s.tick - o._t;
      if (Math.random() < 0.3) {
        const b = s.orders.find((o) => o.status === "ordered");
        if (b) b.status = "brewing";
      }
      if (Math.random() < 0.3) {
        const r = s.orders.find((o) => o.status === "brewing");
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
      const rank = { ordered: 0, brewing: 1, ready: 2 };
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
    },
  },
  {
    name: "forest",
    ms: 1000,
    init: () =>
      ["oak", "pine", "maple", "birch", "willow", "cedar", "elm", "ash", "cherry", "palm", "bamboo", "cactus"]
        .map((name) => ({ name, age: Math.floor(Math.random() * 100), health: 0.5 + Math.random() * 0.5 })),
    tick: (trees) => {
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
        rows: trees.map((t) => ({
          0: t.name,
          1: t.age,
          2: ".".repeat(Math.min(20, Math.floor(t.age / 10))),
          3: t.health,
          4: t.age < 10 ? "seed" : t.age < 50 ? "sapling" : t.age < 200 ? "mature" : "ancient",
        })),
      };
    },
  },
  {
    name: "words",
    ms: 200,
    init: () => {
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
    },
    tick: (s) => {
      s.targetAge++;
      if (s.targetAge > 150) {
        s.target = s.targets[Math.floor(Math.random() * s.targets.length)];
        s.targetAge = 0;
      }
      let solved = false;
      for (const m of s.monkeys) {
        m.attempt = Array.from(
          { length: s.target.length },
          () => String.fromCharCode(97 + Math.floor(Math.random() * 26)),
        )
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
        rows: s.monkeys.map((m) => ({
          0: m.name,
          1: s.target,
          2: m.attempt,
          3: m.match,
          4: m.attempts,
          5: m.best,
        })),
      };
    },
  },
];
