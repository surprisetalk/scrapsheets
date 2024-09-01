import van from "https://cdn.jsdelivr.net/gh/vanjs-org/van/public/van-1.5.2.min.js";

const { div, table, tbody, tr, td, a, input, span, aside, main } = van.tags;

const Cell = () => "TODO";

const Sheet = rows =>
  table(tbody(rows.map(row => tr(row.map(cell => td(Cell(cell)))))));

// improve perf: https://vanjs.org/tutorial#stateful-binding
van.add(
  document.body,
  main(
    div(), // clipboard
    div(
      Sheet(new Array(20).fill(new Array(10).fill(""))),
      aside(
        div(), // channels
        div() // rules
      )
    )
  )
);
