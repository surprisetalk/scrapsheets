import van from "https://cdn.jsdelivr.net/gh/vanjs-org/van/public/van-1.5.2.min.js";

const {div, table, tbody, tr, td, a, input, span, aside, main} = van.tags;

const Cell = () => {
  return input({
    disabled: true,
    onblur: e => (e.target.disabled = true),
    onchange: e => e.target.blur(),
  });
};

const Sheet = rows => {
  const state = van.state(rows);
  // TODO: van.derive
  return table(
    {class: "sheet"},
    tbody(
      rows.map(row =>
        tr(
          row.map(cell =>
            td(
              {
                onclick: e => {
                  const x = e.target.closest("td").querySelector("input");
                  x.disabled = false;
                  x.focus();
                },
              },
              Cell(cell)
            )
          )
        )
      )
    )
  );
}

van.add(
  document.body,
  div({id: "clipboard"}),
  main(
    Sheet(new Array(35).fill(new Array(10).fill(""))),
    aside(
      div(), // channels
      div() // rules
    )
  )
);
