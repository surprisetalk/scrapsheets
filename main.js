import van from "https://cdn.jsdelivr.net/gh/vanjs-org/van/public/van-1.5.2.min.js";

const {div, table, tbody, tr, td, a, input, span, aside, main} = van.tags;

const p = (x, y) => ({x, y});

const Cell = () => {
  return input({
    disabled: true,
    onblur: e => (e.target.disabled = true),
    onchange: e => e.target.blur(),
  });
};

const Sheet = rows_ => {
  const rows = van.state(rows_);
  const sel = van.state({from: {}, to: {}});
  return () => table(
    {class: "sheet"},
    tbody(
      rows.val.map((row, y) =>
        tr(
          row.map((cell, x) =>
            td(
              {
                "data-x": x,
                "data-y": y,
                style: () => sel.val.from.x <= x && x <= sel.val.to.x && sel.val.from.y <= y && y <= sel.val.to.y ? `background: #eee;` : "",
                onclick: e => {
                  const x = e.target.closest("td").querySelector("input");
                  x.disabled = false;
                  x.focus();
                },
                onmousedown: () => sel.val = {from: p(x, y), to: {}},
                onmouseup: () => sel.val = {...sel.val, to: p(x, y)},
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
