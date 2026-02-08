package main

import (
	"fmt"
	"math"
	"os"
	"os/exec"
	"sort"
	"strconv"
	"strings"

	"github.com/automerge/automerge-go"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type editorDoneMsg struct{ err error }

var (
	titleStyle      = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("12"))
	headerStyle     = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("15"))
	cursorStyle     = lipgloss.NewStyle().Background(lipgloss.Color("236")).Foreground(lipgloss.Color("255"))
	editCurStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color("255"))
	editBorderStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("75"))
	dimStyle        = lipgloss.NewStyle().Foreground(lipgloss.Color("8"))
	cellDimStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color("242"))
	colHLStyle      = lipgloss.NewStyle().Background(lipgloss.Color("234")).Foreground(lipgloss.Color("242"))
	gutterStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("239"))
	gutterSelStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color("14"))
	statusStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("14"))
	editStatStyle   = lipgloss.NewStyle().Foreground(lipgloss.Color("75"))
	errorStyle      = lipgloss.NewStyle().Foreground(lipgloss.Color("9"))
	rowCurStyle     = lipgloss.NewStyle().Background(lipgloss.Color("235")).Foreground(lipgloss.Color("242"))
	editRowStyle    = lipgloss.NewStyle().Background(lipgloss.Color("235"))
	libHdrStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("8")).Bold(true)
	libDimStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("242"))
	visualSelStyle  = lipgloss.NewStyle().Background(lipgloss.Color("24")).Foreground(lipgloss.Color("255"))
	visualStatStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("12"))
)

type view int

const (
	viewLibrary view = iota
	viewTable
)

type mode int

const (
	modeNormal mode = iota
	modeEdit
	modeVisual
)

type sortMode int

// order matches display columns: TYPE ID TITLE COLS ROWS SIZE DATE
const (
	sortType sortMode = iota
	sortID
	sortTitle
	sortCols
	sortRows
	sortSize
	sortDate
	sortModeCount
)

type model struct {
	view    view
	dataDir string
	width   int
	height  int
	err     error

	// library
	docs       []docInfo
	libCursor  int
	libScroll  int
	libSort    sortMode
	libSortAsc bool
	filterBuf  string
	filterMode bool

	// table
	doc        *automerge.Doc
	docPath    string
	docID      string
	cols       []col
	rows       []map[string]any
	cx, cy     int
	scrollX    int
	scrollY    int
	mode       mode
	editBuf    string
	colRename  bool
	isMetadata bool
	isQuery    bool
	queryCode  string
	queryLang  string
	queryDirty bool
	editTmpFile string
	rowOrigIdx []int
	selected   map[int]bool // keyed by original row index (1-based)

	// table sort
	sortCol int // column index to sort by (-1 = unsorted)
	sortAsc bool

	// visual mode
	anchorX, anchorY int

	// vim state
	pending string
	yankBuf [][]any // 2D clipboard (region or single cell)

	// undo/redo
	undoStack [][]byte
	redoStack [][]byte
	lastSaved []byte
}

func initialModel(dataDir string) model {
	m := model{dataDir: dataDir, view: viewLibrary, libSort: sortDate}
	allDocs, err := discoverDocs(dataDir)
	if err != nil {
		m.err = err
	}
	for _, d := range allDocs {
		if d.docType != "empty" && d.docType != "unknown" {
			m.docs = append(m.docs, d)
		}
	}
	return m
}

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil
	case editorDoneMsg:
		if msg.err != nil {
			m.err = fmt.Errorf("editor: %w", msg.err)
			return m, nil
		}
		if m.editTmpFile != "" {
			data, err := os.ReadFile(m.editTmpFile)
			os.Remove(m.editTmpFile)
			m.editTmpFile = ""
			if err != nil {
				m.err = fmt.Errorf("read edited query: %w", err)
				return m, nil
			}
			newCode := string(data)
			if newCode != m.queryCode {
				m.queryCode = newCode
				m.queryDirty = true
				setQueryCode(m.doc, newCode)
				m.persist()
			}
		}
		return m, nil
	case tea.KeyMsg:
		if m.view == viewLibrary {
			return m.updateLibrary(msg)
		}
		if m.mode == modeEdit {
			return m.updateEdit(msg)
		}
		if m.mode == modeVisual {
			return m.updateVisual(msg)
		}
		return m.updateTable(msg)
	}
	return m, nil
}

// --- Library ---

func (m model) visibleDocs() []docInfo {
	var out []docInfo
	if m.filterBuf == "" {
		out = make([]docInfo, len(m.docs))
		copy(out, m.docs)
	} else {
		q := strings.ToLower(m.filterBuf)
		for _, d := range m.docs {
			if strings.Contains(strings.ToLower(d.id), q) ||
				strings.Contains(strings.ToLower(d.docType), q) ||
				strings.Contains(strings.ToLower(d.title), q) {
				out = append(out, d)
			}
		}
	}
	asc := m.libSortAsc
	cmp := func(less, greater bool) bool {
		if asc {
			return less
		}
		return greater
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i], out[j]
		switch m.libSort {
		case sortType:
			if a.docType != b.docType {
				return cmp(a.docType < b.docType, a.docType > b.docType)
			}
			return a.modTime.After(b.modTime)
		case sortID:
			return cmp(a.id < b.id, a.id > b.id)
		case sortTitle:
			return cmp(a.title < b.title, a.title > b.title)
		case sortCols:
			return cmp(a.nCols < b.nCols, a.nCols > b.nCols)
		case sortRows:
			return cmp(a.nRows < b.nRows, a.nRows > b.nRows)
		case sortSize:
			return cmp(a.size < b.size, a.size > b.size)
		default: // sortDate
			return cmp(a.modTime.Before(b.modTime), a.modTime.After(b.modTime))
		}
	})
	return out
}

func (m model) updateLibrary(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	if m.filterMode {
		switch msg.String() {
		case "enter":
			m.filterMode = false
		case "esc":
			m.filterMode = false
			m.filterBuf = ""
		case "backspace":
			if len(m.filterBuf) > 0 {
				m.filterBuf = m.filterBuf[:len(m.filterBuf)-1]
			}
		case "ctrl+u":
			m.filterBuf = ""
		default:
			if len(msg.String()) == 1 || msg.String() == " " {
				m.filterBuf += msg.String()
			}
		}
		visible := m.visibleDocs()
		if m.libCursor >= len(visible) {
			m.libCursor = max(len(visible)-1, 0)
		}
		return m, nil
	}

	visible := m.visibleDocs()
	last := max(len(visible)-1, 0)

	// pending key sequences
	if m.pending != "" {
		combo := m.pending + msg.String()
		m.pending = ""
		switch combo {
		case "gg":
			m.libCursor = 0
			return m, nil
		}
		// no match — fall through
	}

	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit
	case "up", "k":
		if m.libCursor > 0 {
			m.libCursor--
		}
	case "down", "j":
		if m.libCursor < last {
			m.libCursor++
		}
	case "G":
		m.libCursor = last
	case "g":
		m.pending = "g"
	case "ctrl+d":
		m.libCursor = min(m.libCursor+m.height/2, last)
	case "ctrl+u":
		m.libCursor = max(m.libCursor-m.height/2, 0)
	case "/":
		m.filterMode = true
	case ">", ".":
		m.libSort = (m.libSort + 1) % sortModeCount
		m.libCursor = 0
	case "<", ",":
		m.libSort = (m.libSort + sortModeCount - 1) % sortModeCount
		m.libCursor = 0
	case "^":
		m.libSortAsc = !m.libSortAsc
		m.libCursor = 0
	case "enter", "l":
		if m.libCursor >= 0 && m.libCursor < len(visible) {
			return m.openDoc(visible[m.libCursor])
		}
	}
	return m, nil
}

func (m model) openDoc(info docInfo) (tea.Model, tea.Cmd) {
	doc, _, err := loadDoc(info.path)
	if err != nil {
		m.err = err
		return m, nil
	}

	m.view = viewTable
	m.doc = doc
	m.docPath = info.path
	m.docID = info.id
	m.cx, m.cy = 0, 0
	m.scrollX, m.scrollY = 0, 0
	m.mode = modeNormal
	m.pending = ""
	m.err = nil
	m.undoStack = nil
	m.redoStack = nil
	m.isQuery = info.docType == "query"

	if m.isQuery {
		code, lang, cols, rows, err := readQueryDoc(doc)
		if err != nil {
			m.err = err
			return m, nil
		}
		m.queryCode = code
		m.queryLang = lang
		m.cols = cols
		m.rows = rows
		m.rowOrigIdx = make([]int, len(rows))
		for i := range rows {
			m.rowOrigIdx[i] = i + 1
		}
		m.selected = map[int]bool{}
		m.sortCol = -1
		m.sortAsc = true
	} else {
		m.queryCode = ""
		m.queryLang = ""
		if err := m.reloadTable(); err != nil {
			m.err = err
			return m, nil
		}
	}
	m.lastSaved = doc.Save()
	return m, nil
}

func (m *model) reloadTable() error {
	cols, rows, err := readTable(m.doc)
	if err != nil {
		return err
	}
	m.cols = cols
	m.rows = rows
	m.rowOrigIdx = make([]int, len(rows))
	for i := range rows {
		m.rowOrigIdx[i] = i + 1
	}
	m.selected = map[int]bool{}
	m.sortCol = -1
	m.sortAsc = true

	// detect metadata format
	m.isMetadata = false
	dataVal, err := m.doc.Path("data").Get()
	if err == nil && dataVal.Kind() == automerge.KindList {
		if first, err := dataVal.List().Get(0); err == nil && first.Kind() == automerge.KindMap {
			keys, _ := first.Map().Keys()
			if hasKey(keys, "data") && hasKey(keys, "type") {
				m.isMetadata = true
			}
		}
	}

	// clamp cursor
	if m.cy >= len(m.rows) {
		m.cy = max(len(m.rows)-1, 0)
	}
	if m.cx >= len(m.cols) {
		m.cx = max(len(m.cols)-1, 0)
	}
	return nil
}

// --- Table (normal) ---

func (m model) updateTable(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	lastRow := max(len(m.rows)-1, 0)
	lastCol := max(len(m.cols)-1, 0)

	// pending key sequences
	if m.pending != "" {
		combo := m.pending + msg.String()
		m.pending = ""
		switch combo {
		case "gg":
			m.cy, m.cx = 0, 0
			return m, nil
		case "dd":
			return m.deleteRow()
		case "dw":
			m.setCellValue(nil)
			return m, nil
		}
		// no match — fall through
	}

	switch msg.String() {
	case "q", "esc":
		m.view = viewLibrary
		m.doc = nil
		m.pending = ""
		m.undoStack = nil
		m.redoStack = nil
		m.lastSaved = nil
		return m, nil
	case "ctrl+c":
		return m, tea.Quit

	// -- cursor movement --
	case "left", "h":
		if m.cx > 0 {
			m.cx--
		}
	case "right", "l":
		if m.cx < lastCol {
			m.cx++
		}
	case "up", "k":
		if m.cy > 0 {
			m.cy--
		}
	case "down", "j":
		if m.cy < lastRow {
			m.cy++
		}
	case "w":
		if m.cx < lastCol {
			m.cx++
		} else if m.cy < lastRow {
			m.cx = 0
			m.cy++
		}
	case "b":
		if m.cx > 0 {
			m.cx--
		} else if m.cy > 0 {
			m.cx = lastCol
			m.cy--
		}
	case "0", "home":
		m.cx = 0
	case "$", "end":
		m.cx = lastCol
	case "G":
		m.cy = lastRow
	case "g":
		m.pending = "g"
	case "ctrl+d":
		m.cy = min(m.cy+m.height/2, lastRow)
	case "ctrl+u":
		m.cy = max(m.cy-m.height/2, 0)
	case "J":
		if !m.isQuery {
			return m.moveRowDown()
		}
	case "K":
		if !m.isQuery {
			return m.moveRowUp()
		}
	case "H":
		if !m.isQuery {
			return m.moveColLeft()
		}
	case "L":
		if !m.isQuery {
			return m.moveColRight()
		}
	case "M":
		dataH := max(m.height-6, 1)
		m.cy = min(m.scrollY+dataH/2, lastRow)
	case "tab":
		m.cx++
		if m.cx > lastCol {
			m.cx = 0
			m.cy = min(m.cy+1, lastRow)
		}
	case "shift+tab":
		m.cx--
		if m.cx < 0 {
			m.cx = lastCol
			m.cy = max(m.cy-1, 0)
		}

	// -- editing --
	case "enter", "i":
		if !m.isQuery && len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = formatCell(m.cellValue(), m.cols[m.cx].typ)
		}
	case "a":
		if !m.isQuery && len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = formatCell(m.cellValue(), m.cols[m.cx].typ)
		}
	case "c":
		if !m.isQuery && len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = ""
		}
	case "r":
		if !m.isQuery && len(m.cols) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.colRename = true
			m.editBuf = m.cols[m.cx].name
		}
	case "x":
		if !m.isQuery {
			m.setCellValue(nil)
		}

	// -- yank / paste --
	case "y":
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.yankBuf = [][]any{{m.cellValue()}}
		}
	case "p":
		if !m.isQuery {
			m.pasteYankBuf()
		}

	// -- visual mode --
	case "v":
		if len(m.cols) > 0 {
			m.mode = modeVisual
			m.anchorX, m.anchorY = m.cx, m.cy
		}

	// -- row operations --
	case "o":
		if !m.isQuery {
			return m.insertRowAt(m.cy + 1)
		}
	case "O":
		if !m.isQuery {
			return m.insertRowAt(m.cy)
		}
	case "d":
		if !m.isQuery {
			m.pending = "d"
		}

	// -- selection --
	case " ":
		if m.cy < len(m.rowOrigIdx) {
			orig := m.rowOrigIdx[m.cy]
			if m.selected[orig] {
				delete(m.selected, orig)
			} else {
				m.selected[orig] = true
			}
		}

	// -- column sort --
	case ">", ".":
		if len(m.cols) > 0 {
			m.sortCol = (m.sortCol + 1) % len(m.cols)
			m.sortTableRows()
		}
	case "<", ",":
		if len(m.cols) > 0 {
			m.sortCol = (m.sortCol + len(m.cols) - 1) % len(m.cols)
			m.sortTableRows()
		}
	case "^":
		m.sortAsc = !m.sortAsc
		if m.sortCol >= 0 {
			m.sortTableRows()
		}

	// -- column operations --
	case "A":
		if !m.isQuery {
			return m.appendCol()
		}
	case "X":
		if !m.isQuery {
			return m.deleteCol()
		}

	// -- query --
	case "e":
		if m.isQuery {
			return m.editQueryExternal()
		}
	case "E":
		if m.isQuery {
			return m.execQuery()
		}

	// -- undo/redo --
	case "u":
		return m.undo()
	case "ctrl+r":
		return m.redo()
	}
	return m, nil
}

// --- Edit mode ---

func (m model) updateEdit(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "enter":
		wasRename := m.colRename
		m.commitEdit()
		m.mode = modeNormal
		m.colRename = false
		if !wasRename && m.cy < len(m.rows)-1 {
			m.cy++
		}
	case "esc":
		m.mode = modeNormal
		m.colRename = false
	case "backspace":
		if len(m.editBuf) > 0 {
			m.editBuf = m.editBuf[:len(m.editBuf)-1]
		}
	case "ctrl+u":
		m.editBuf = ""
	case "ctrl+w":
		buf := strings.TrimRight(m.editBuf, " ")
		if i := strings.LastIndex(buf, " "); i >= 0 {
			m.editBuf = buf[:i+1]
		} else {
			m.editBuf = ""
		}
	case "tab":
		m.commitEdit()
		m.mode = modeNormal
		m.cx++
		if m.cx >= len(m.cols) {
			m.cx = 0
			m.cy = min(m.cy+1, len(m.rows)-1)
		}
	case "shift+tab":
		m.commitEdit()
		m.mode = modeNormal
		m.cx--
		if m.cx < 0 {
			m.cx = len(m.cols) - 1
			m.cy = max(m.cy-1, 0)
		}
	default:
		if len(msg.String()) == 1 || msg.String() == " " {
			m.editBuf += msg.String()
		}
	}
	return m, nil
}

// --- Visual mode ---

func (m model) updateVisual(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	lastRow := max(len(m.rows)-1, 0)
	lastCol := max(len(m.cols)-1, 0)

	switch msg.String() {
	case "esc", "v":
		m.mode = modeNormal
	case "ctrl+c":
		return m, tea.Quit

	// movement
	case "left", "h":
		if m.cx > 0 {
			m.cx--
		}
	case "right", "l":
		if m.cx < lastCol {
			m.cx++
		}
	case "up", "k":
		if m.cy > 0 {
			m.cy--
		}
	case "down", "j":
		if m.cy < lastRow {
			m.cy++
		}
	case "w":
		if m.cx < lastCol {
			m.cx++
		} else if m.cy < lastRow {
			m.cx = 0
			m.cy++
		}
	case "b":
		if m.cx > 0 {
			m.cx--
		} else if m.cy > 0 {
			m.cx = lastCol
			m.cy--
		}
	case "0", "home":
		m.cx = 0
	case "$", "end":
		m.cx = lastCol
	case "G":
		m.cy = lastRow
	case "ctrl+d":
		m.cy = min(m.cy+m.height/2, lastRow)
	case "ctrl+u":
		m.cy = max(m.cy-m.height/2, 0)

	// actions
	case "y":
		m.yankVisual()
		m.mode = modeNormal
	case "d", "x":
		m.yankVisual()
		if !m.isQuery {
			m.clearVisual()
		}
		m.mode = modeNormal
	case "D":
		if !m.isQuery {
			m.yankVisual()
			_, y1, _, y2 := m.visualRect()
			m.selected = map[int]bool{}
			for ri := y1; ri <= y2 && ri < len(m.rowOrigIdx); ri++ {
				m.selected[m.rowOrigIdx[ri]] = true
			}
			m.mode = modeNormal
			return m.deleteRow()
		}
		m.mode = modeNormal
	case "p":
		if !m.isQuery {
			m.pasteYankBuf()
		}
		m.mode = modeNormal
	case "f":
		if !m.isQuery {
			x1, y1, x2, y2 := m.visualRect()
			if y1 < len(m.rows) && x1 < len(m.cols) {
				val := m.rows[y1][m.cols[x1].key]
				for ri := y1; ri <= y2 && ri < len(m.rows); ri++ {
					for ci := x1; ci <= x2 && ci < len(m.cols); ci++ {
						m.rows[ri][m.cols[ci].key] = val
						m.setDocCell(ri, m.cols[ci].key, val)
					}
				}
				m.persist()
			}
		}
		m.mode = modeNormal
	}
	return m, nil
}

func (m model) visualRect() (x1, y1, x2, y2 int) {
	x1, x2 = m.anchorX, m.cx
	if x1 > x2 {
		x1, x2 = x2, x1
	}
	y1, y2 = m.anchorY, m.cy
	if y1 > y2 {
		y1, y2 = y2, y1
	}
	return
}

func (m *model) yankVisual() {
	x1, y1, x2, y2 := m.visualRect()
	h := y2 - y1 + 1
	w := x2 - x1 + 1
	m.yankBuf = make([][]any, h)
	for ri := 0; ri < h; ri++ {
		m.yankBuf[ri] = make([]any, w)
		for ci := 0; ci < w; ci++ {
			if y1+ri < len(m.rows) && x1+ci < len(m.cols) {
				m.yankBuf[ri][ci] = m.rows[y1+ri][m.cols[x1+ci].key]
			}
		}
	}
}

func (m *model) clearVisual() {
	x1, y1, x2, y2 := m.visualRect()
	for ri := y1; ri <= y2 && ri < len(m.rows); ri++ {
		for ci := x1; ci <= x2 && ci < len(m.cols); ci++ {
			m.rows[ri][m.cols[ci].key] = nil
			m.setDocCell(ri, m.cols[ci].key, nil)
		}
	}
	m.persist()
}

func (m *model) pasteYankBuf() {
	if m.yankBuf == nil || len(m.rows) == 0 || len(m.cols) == 0 {
		return
	}
	for ri, row := range m.yankBuf {
		ry := m.cy + ri
		if ry >= len(m.rows) {
			break
		}
		for ci, val := range row {
			cx := m.cx + ci
			if cx >= len(m.cols) {
				break
			}
			m.rows[ry][m.cols[cx].key] = val
			m.setDocCell(ry, m.cols[cx].key, val)
		}
	}
	m.persist()
}

// --- Mutations ---

func (m *model) commitEdit() {
	if m.colRename {
		if m.cx < len(m.cols) {
			m.cols[m.cx].name = m.editBuf
			c := m.cols[m.cx]
			if m.isMetadata {
				m.doc.Path("data", 0, "data", 0, c.key, "name").Set(m.editBuf)
			} else {
				m.doc.Path("data", 0, c.key, "name").Set(m.editBuf)
			}
			m.persist()
		}
		return
	}
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return
	}
	c := m.cols[m.cx]
	val := parseCell(m.editBuf, c.typ)
	m.rows[m.cy][c.key] = val
	m.setDocCell(m.cy, c.key, val)
	m.persist()
}

func (m *model) setCellValue(val any) {
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return
	}
	c := m.cols[m.cx]
	m.rows[m.cy][c.key] = val
	m.setDocCell(m.cy, c.key, val)
	m.persist()
}

func (m *model) setDocCell(rowIdx int, colKey string, val any) {
	dataIdx := rowIdx + 1
	if m.isMetadata {
		m.doc.Path("data", 0, "data", dataIdx, colKey).Set(val)
	} else {
		m.doc.Path("data", dataIdx, colKey).Set(val)
	}
}

func (m *model) persist() {
	if m.lastSaved != nil {
		m.undoStack = append(m.undoStack, m.lastSaved)
		if len(m.undoStack) > 50 {
			m.undoStack = m.undoStack[len(m.undoStack)-50:]
		}
		m.redoStack = nil
	}
	m.doc.Commit("tui edit")
	if err := saveDoc(m.doc, m.docPath); err != nil {
		m.err = err
		return
	}
	m.lastSaved = m.doc.Save()
}

func (m model) undo() (tea.Model, tea.Cmd) {
	if len(m.undoStack) == 0 {
		return m, nil
	}
	m.redoStack = append(m.redoStack, m.doc.Save())
	snapshot := m.undoStack[len(m.undoStack)-1]
	m.undoStack = m.undoStack[:len(m.undoStack)-1]
	doc, err := automerge.Load(snapshot)
	if err != nil {
		m.err = fmt.Errorf("undo: %w", err)
		return m, nil
	}
	m.doc = doc
	m.lastSaved = snapshot
	if err := m.reloadTable(); err != nil {
		m.err = fmt.Errorf("undo: %w", err)
		return m, nil
	}
	if err := saveDoc(m.doc, m.docPath); err != nil {
		m.err = err
	}
	return m, nil
}

func (m model) redo() (tea.Model, tea.Cmd) {
	if len(m.redoStack) == 0 {
		return m, nil
	}
	m.undoStack = append(m.undoStack, m.doc.Save())
	snapshot := m.redoStack[len(m.redoStack)-1]
	m.redoStack = m.redoStack[:len(m.redoStack)-1]
	doc, err := automerge.Load(snapshot)
	if err != nil {
		m.err = fmt.Errorf("redo: %w", err)
		return m, nil
	}
	m.doc = doc
	m.lastSaved = snapshot
	if err := m.reloadTable(); err != nil {
		m.err = fmt.Errorf("redo: %w", err)
		return m, nil
	}
	if err := saveDoc(m.doc, m.docPath); err != nil {
		m.err = err
	}
	return m, nil
}

func (m model) editQueryExternal() (tea.Model, tea.Cmd) {
	tmpFile, err := os.CreateTemp("", "scrapsheets-query-*.sql")
	if err != nil {
		m.err = fmt.Errorf("create temp file: %w", err)
		return m, nil
	}
	if _, err := tmpFile.WriteString(m.queryCode); err != nil {
		tmpFile.Close()
		os.Remove(tmpFile.Name())
		m.err = fmt.Errorf("write temp file: %w", err)
		return m, nil
	}
	tmpFile.Close()

	editor := os.Getenv("EDITOR")
	if editor == "" {
		editor = "vi"
	}
	m.editTmpFile = tmpFile.Name()
	c := exec.Command(editor, m.editTmpFile)
	return m, tea.ExecProcess(c, func(err error) tea.Msg {
		return editorDoneMsg{err: err}
	})
}

func (m model) execQuery() (tea.Model, tea.Cmd) {
	cols, rows, err := executeQuery(m.queryCode, m.dataDir)
	if err != nil {
		m.err = err
		return m, nil
	}
	m.cols = cols
	m.rows = rows
	m.rowOrigIdx = make([]int, len(rows))
	for i := range rows {
		m.rowOrigIdx[i] = i + 1
	}
	m.selected = map[int]bool{}
	m.sortCol = -1
	m.sortAsc = true
	m.cx, m.cy = 0, 0
	m.scrollX, m.scrollY = 0, 0
	m.err = nil
	m.queryDirty = false

	// write results back into the automerge doc
	m.writeQueryResults(cols, rows)
	return m, nil
}

func (m *model) writeQueryResults(cols []col, rows []map[string]any) {
	dataList := resolveDataList(m.doc)
	if dataList == nil {
		return
	}

	// clear cached results (everything after row 0 metadata)
	for dataList.Len() > 1 {
		dataList.Delete(dataList.Len() - 1)
	}

	// row 1: column definitions
	dataList.Insert(1, automerge.NewMap())
	for i, c := range cols {
		key := strconv.Itoa(i)
		m.doc.Path("data", 1, key).Set(automerge.NewMap())
		m.doc.Path("data", 1, key, "name").Set(c.name)
		m.doc.Path("data", 1, key, "type").Set(c.typ)
		m.doc.Path("data", 1, key, "key").Set(key)
	}

	// rows 2+: result data
	for i, row := range rows {
		idx := i + 2
		dataList.Insert(idx, automerge.NewMap())
		for j, c := range cols {
			key := strconv.Itoa(j)
			m.doc.Path("data", idx, key).Set(row[c.key])
		}
	}

	m.persist()
}

func (m model) cellValue() any {
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return nil
	}
	return m.rows[m.cy][m.cols[m.cx].key]
}

func (m model) colStats(ci int) string {
	if ci >= len(m.cols) || len(m.rows) == 0 {
		return ""
	}
	c := m.cols[ci]
	isNum := c.typ == "num" || c.typ == "int" || c.typ == "float" || c.typ == "usd" || c.typ == "percentage"

	if isNum {
		var sum, mn, mx float64
		count := 0
		for _, row := range m.rows {
			f, ok := toFloat(row[c.key])
			if !ok {
				continue
			}
			count++
			sum += f
			if count == 1 || f < mn {
				mn = f
			}
			if count == 1 || f > mx {
				mx = f
			}
		}
		if count == 0 {
			return fmt.Sprintf("n:%d", len(m.rows))
		}
		return fmt.Sprintf("n:%d Σ:%.4g μ:%.4g ↓:%.4g ↑:%.4g", count, sum, sum/float64(count), mn, mx)
	}

	count := 0
	empty := 0
	unique := map[string]bool{}
	for _, row := range m.rows {
		v := row[c.key]
		count++
		if v == nil {
			empty++
			continue
		}
		s := fmt.Sprintf("%v", v)
		if s == "" {
			empty++
		} else {
			unique[s] = true
		}
	}
	return fmt.Sprintf("n:%d ∅:%d ≠:%d", count, empty, len(unique))
}

func (m *model) sortTableRows() {
	if m.sortCol < 0 || m.sortCol >= len(m.cols) || len(m.rows) == 0 {
		return
	}
	key := m.cols[m.sortCol].key
	asc := m.sortAsc
	// build index pairs so we can sort rows + origIdx together
	type pair struct {
		row     map[string]any
		origIdx int
	}
	pairs := make([]pair, len(m.rows))
	for i := range m.rows {
		pairs[i] = pair{m.rows[i], m.rowOrigIdx[i]}
	}
	sort.SliceStable(pairs, func(i, j int) bool {
		return compareCells(pairs[i].row[key], pairs[j].row[key], asc)
	})
	for i := range pairs {
		m.rows[i] = pairs[i].row
		m.rowOrigIdx[i] = pairs[i].origIdx
	}
}

func compareCells(a, b any, asc bool) bool {
	fa, oka := toFloat(a)
	fb, okb := toFloat(b)
	if oka && okb {
		if asc {
			return fa < fb
		}
		return fa > fb
	}
	sa := fmt.Sprintf("%v", a)
	sb := fmt.Sprintf("%v", b)
	if a == nil {
		sa = ""
	}
	if b == nil {
		sb = ""
	}
	if asc {
		return sa < sb
	}
	return sa > sb
}

func toFloat(v any) (float64, bool) {
	switch n := v.(type) {
	case float64:
		return n, true
	case int64:
		return float64(n), true
	case uint64:
		return float64(n), true
	}
	return 0, false
}

// --- Move row/column ---

func (m model) moveRowDown() (tea.Model, tea.Cmd) {
	if m.cy >= len(m.rows)-1 {
		return m, nil
	}
	m.swapRows(m.cy, m.cy+1)
	m.cy++
	m.persist()
	return m, nil
}

func (m model) moveRowUp() (tea.Model, tea.Cmd) {
	if m.cy <= 0 {
		return m, nil
	}
	m.swapRows(m.cy, m.cy-1)
	m.cy--
	m.persist()
	return m, nil
}

func (m model) moveColLeft() (tea.Model, tea.Cmd) {
	if m.cx <= 0 {
		return m, nil
	}
	m.swapCols(m.cx, m.cx-1)
	m.cx--
	m.persist()
	return m, nil
}

func (m model) moveColRight() (tea.Model, tea.Cmd) {
	if m.cx >= len(m.cols)-1 {
		return m, nil
	}
	m.swapCols(m.cx, m.cx+1)
	m.cx++
	m.persist()
	return m, nil
}

func (m *model) swapRows(a, b int) {
	m.rows[a], m.rows[b] = m.rows[b], m.rows[a]
	m.rowOrigIdx[a], m.rowOrigIdx[b] = m.rowOrigIdx[b], m.rowOrigIdx[a]
	for _, c := range m.cols {
		m.setDocCell(a, c.key, m.rows[a][c.key])
		m.setDocCell(b, c.key, m.rows[b][c.key])
	}
}

func (m *model) swapCols(a, b int) {
	ka, kb := m.cols[a].key, m.cols[b].key
	m.cols[a].name, m.cols[b].name = m.cols[b].name, m.cols[a].name
	m.cols[a].typ, m.cols[b].typ = m.cols[b].typ, m.cols[a].typ
	for _, ci := range []int{a, b} {
		c := m.cols[ci]
		if m.isMetadata {
			m.doc.Path("data", 0, "data", 0, c.key, "name").Set(c.name)
			m.doc.Path("data", 0, "data", 0, c.key, "type").Set(c.typ)
		} else {
			m.doc.Path("data", 0, c.key, "name").Set(c.name)
			m.doc.Path("data", 0, c.key, "type").Set(c.typ)
		}
	}
	for ri := range m.rows {
		m.rows[ri][ka], m.rows[ri][kb] = m.rows[ri][kb], m.rows[ri][ka]
		m.setDocCell(ri, ka, m.rows[ri][ka])
		m.setDocCell(ri, kb, m.rows[ri][kb])
	}
}

// --- Row/Column ops ---

func (m model) deleteRow() (tea.Model, tea.Cmd) {
	if len(m.rows) == 0 {
		return m, nil
	}

	// if any rows selected, delete those; else delete current row
	toDelete := map[int]bool{}
	if len(m.selected) > 0 {
		for origIdx := range m.selected {
			toDelete[origIdx] = true
		}
	} else {
		toDelete[m.rowOrigIdx[m.cy]] = true
	}

	// delete from automerge in reverse order (highest index first)
	dataList := resolveDataList(m.doc)
	var delIndices []int
	for i, origIdx := range m.rowOrigIdx {
		if toDelete[origIdx] {
			delIndices = append(delIndices, i)
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(delIndices)))
	for _, i := range delIndices {
		if dataList != nil {
			dataList.Delete(i + 1) // +1 for column def row
		}
	}

	// rebuild rows and origIdx
	var newRows []map[string]any
	var newIdx []int
	for i, origIdx := range m.rowOrigIdx {
		if !toDelete[origIdx] {
			newRows = append(newRows, m.rows[i])
			newIdx = append(newIdx, origIdx)
		}
	}
	m.rows = newRows
	m.rowOrigIdx = newIdx
	m.selected = map[int]bool{}

	if m.cy >= len(m.rows) && m.cy > 0 {
		m.cy = len(m.rows) - 1
	}
	m.persist()
	return m, nil
}

func (m model) insertRowAt(idx int) (tea.Model, tea.Cmd) {
	if idx > len(m.rows) {
		idx = len(m.rows)
	}
	row := make(map[string]any)
	for _, c := range m.cols {
		row[c.key] = nil
	}

	// new origIdx = max existing + 1
	maxOrig := 0
	for _, o := range m.rowOrigIdx {
		if o > maxOrig {
			maxOrig = o
		}
	}

	// splice into local rows + origIdx
	m.rows = append(m.rows, nil)
	copy(m.rows[idx+1:], m.rows[idx:])
	m.rows[idx] = row
	m.rowOrigIdx = append(m.rowOrigIdx, 0)
	copy(m.rowOrigIdx[idx+1:], m.rowOrigIdx[idx:])
	m.rowOrigIdx[idx] = maxOrig + 1

	dataIdx := idx + 1
	dataList := resolveDataList(m.doc)
	if dataList != nil {
		dataList.Insert(dataIdx, automerge.NewMap())
	}
	m.cy = idx
	m.persist()
	return m, nil
}

func (m model) deleteCol() (tea.Model, tea.Cmd) {
	if len(m.cols) == 0 || m.cx >= len(m.cols) {
		return m, nil
	}
	c := m.cols[m.cx]

	// remove from automerge doc row 0 (column defs)
	if m.isMetadata {
		m.doc.Path("data", 0, "data", 0, c.key).Delete()
	} else {
		m.doc.Path("data", 0, c.key).Delete()
	}
	// remove from each data row in automerge
	for ri := range m.rows {
		dataIdx := ri + 1
		if m.isMetadata {
			m.doc.Path("data", 0, "data", dataIdx, c.key).Delete()
		} else {
			m.doc.Path("data", dataIdx, c.key).Delete()
		}
		delete(m.rows[ri], c.key)
	}

	// remove from local cols
	m.cols = append(m.cols[:m.cx], m.cols[m.cx+1:]...)
	if m.cx >= len(m.cols) && m.cx > 0 {
		m.cx--
	}
	m.persist()
	return m, nil
}

func (m model) appendCol() (tea.Model, tea.Cmd) {
	newKey := strconv.Itoa(len(m.cols))
	c := col{key: newKey, name: "col" + newKey, typ: "text"}
	m.cols = append(m.cols, c)

	colDef := automerge.NewMap()
	if m.isMetadata {
		m.doc.Path("data", 0, "data", 0, newKey).Set(colDef)
		m.doc.Path("data", 0, "data", 0, newKey, "name").Set(c.name)
		m.doc.Path("data", 0, "data", 0, newKey, "type").Set(c.typ)
		m.doc.Path("data", 0, "data", 0, newKey, "key").Set(newKey)
	} else {
		m.doc.Path("data", 0, newKey).Set(colDef)
		m.doc.Path("data", 0, newKey, "name").Set(c.name)
		m.doc.Path("data", 0, newKey, "type").Set(c.typ)
		m.doc.Path("data", 0, newKey, "key").Set(newKey)
	}

	for _, row := range m.rows {
		row[newKey] = nil
	}
	m.persist()
	return m, nil
}

// --- View ---

func (m model) View() string {
	if m.width == 0 {
		return "loading..."
	}
	switch m.view {
	case viewLibrary:
		return m.viewLibrary()
	case viewTable:
		return m.viewTable()
	}
	return ""
}

func (m model) viewLibrary() string {
	var lines []string

	title := titleStyle.Render(" Scrapsheets TUI")
	lines = append(lines, title)

	if m.err != nil {
		lines = append(lines, errorStyle.Render(" error: "+m.err.Error()))
	}

	// type summary
	if len(m.docs) > 0 {
		types := map[string]int{}
		for _, d := range m.docs {
			types[d.docType]++
		}
		var typeNames []string
		for t := range types {
			typeNames = append(typeNames, t)
		}
		sort.Strings(typeNames)
		var parts []string
		for _, t := range typeNames {
			parts = append(parts, fmt.Sprintf("%d %s", types[t], t))
		}
		lines = append(lines, dimStyle.Render(fmt.Sprintf(" %d docs: %s", len(m.docs), strings.Join(parts, ", "))))
	}

	visible := m.visibleDocs()

	if len(visible) == 0 && m.filterBuf == "" {
		lines = append(lines, dimStyle.Render(" no documents found"))
		lines = append(lines, dimStyle.Render(fmt.Sprintf(" (searched %s)", m.dataDir)))
	}

	// filter bar
	if m.filterMode {
		lines = append(lines, statusStyle.Render(fmt.Sprintf(" /%s_", m.filterBuf)))
	} else if m.filterBuf != "" {
		lines = append(lines, dimStyle.Render(fmt.Sprintf(" /%s", m.filterBuf)))
	}

	// column layout: TYPE ID TITLE COLS ROWS SIZE DATE
	const (
		colPad = 2
		colsCW = 4
		rowsCW = 5
		sizeCW = 6
		dateCW = 6
	)
	fixedW := colsCW + rowsCW + sizeCW + dateCW + (6 * colPad) // 6 gaps between 7 cols
	typeW := 0
	for _, d := range visible {
		if len(d.docType) > typeW {
			typeW = len(d.docType)
		}
	}
	typeW = max(typeW, 4)
	// split remaining space: 1/3 to ID, 2/3 to TITLE
	remain := max(m.width-fixedW-typeW, 20)
	idW := max(remain/3, 8)
	titleW := remain - idW

	arrow := "▼"
	if m.libSortAsc {
		arrow = "▲"
	}
	sortHdr := libHdrStyle.Copy().Foreground(lipgloss.Color("14"))
	fmtHdr := func(label string, w int, active bool, rightAlign bool) string {
		tag := label
		if active {
			tag = label + arrow
		}
		if rightAlign {
			txt := fmt.Sprintf("%*s", w, tag)
			if active {
				return sortHdr.Render(txt)
			}
			return libHdrStyle.Render(txt)
		}
		txt := fmt.Sprintf("%-*s", w, tag)
		if active {
			return sortHdr.Render(txt)
		}
		return libHdrStyle.Render(txt)
	}

	var hdrBuf strings.Builder
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("TYPE", typeW, m.libSort == sortType, false))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("ID", idW, m.libSort == sortID, false))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("TITLE", titleW, m.libSort == sortTitle, false))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("COLS", colsCW, m.libSort == sortCols, true))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("ROWS", rowsCW, m.libSort == sortRows, true))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("SIZE", sizeCW, m.libSort == sortSize, true))
	hdrBuf.WriteString(libHdrStyle.Render("  "))
	hdrBuf.WriteString(fmtHdr("DATE", dateCW, m.libSort == sortDate, false))

	hdrStr := hdrBuf.String()
	hdrVisLen := lipgloss.Width(hdrStr)
	if hdrVisLen < m.width {
		hdrStr += libHdrStyle.Render(strings.Repeat(" ", m.width-hdrVisLen))
	}
	lines = append(lines, hdrStr)

	sep := "  " + strings.Repeat("─", max(m.width-2, 0))
	lines = append(lines, dimStyle.Render(sep))

	var helpText string
	if m.filterMode {
		helpText = " type to filter  enter confirm  esc cancel  ctrl+u clear"
	} else {
		helpText = " j/k move  enter open  / filter  </> sort col  ^ reverse  gg top  G end  q quit"
	}
	help := dimStyle.Render(helpText)

	listHeight := max(m.height-len(lines)-2, 1)

	// clamp cursor
	last := max(len(visible)-1, 0)
	if m.libCursor > last {
		m.libCursor = last
	}

	if m.libCursor < m.libScroll {
		m.libScroll = m.libCursor
	}
	if m.libCursor >= m.libScroll+listHeight {
		m.libScroll = m.libCursor - listHeight + 1
	}

	prevType := ""
	for i := m.libScroll; i < len(visible) && i < m.libScroll+listHeight; i++ {
		d := visible[i]

		// when sorted by type, dim repeated type names for visual grouping
		typeStr := d.docType
		if m.libSort == sortType && typeStr == prevType && i != m.libCursor {
			typeStr = dimStyle.Render(fmt.Sprintf("%-*s", typeW, typeStr))
		} else {
			typeStr = fmt.Sprintf("%-*s", typeW, typeStr)
		}
		prevType = d.docType

		id := d.id
		if len(id) > idW {
			id = id[:idW-2] + ".."
		}
		t := d.title
		if len(t) > titleW {
			t = t[:titleW-2] + ".."
		}

		size := formatSize(d.size)

		line := fmt.Sprintf("  %s  %-*s  %-*s  %*d  %*d  %*s  %-*s",
			typeStr, idW, id, titleW, t,
			colsCW, d.nCols, rowsCW, d.nRows,
			sizeCW, size, dateCW, d.modTime.Format("Jan 02"))

		if lipgloss.Width(line) < m.width {
			line += strings.Repeat(" ", m.width-lipgloss.Width(line))
		}

		if i == m.libCursor {
			lines = append(lines, cursorStyle.Render(line))
		} else {
			lines = append(lines, libDimStyle.Render(line))
		}
	}

	for len(lines) < m.height-2 {
		lines = append(lines, "")
	}

	pos := ""
	if len(visible) > 0 {
		pos = dimStyle.Render(fmt.Sprintf(" %d/%d", m.libCursor+1, len(visible)))
	}
	lines = append(lines, pos)
	lines = append(lines, help)

	return strings.Join(lines, "\n")
}

func formatSize(b int64) string {
	if b < 1024 {
		return fmt.Sprintf("%dB", b)
	}
	return fmt.Sprintf("%.1fK", float64(b)/1024)
}

func (m model) viewTable() string {
	var lines []string

	title := m.docID
	if len(title) > 30 {
		title = title[:30] + ".."
	}
	if m.isQuery {
		titleLine := titleStyle.Render(" "+title) + dimStyle.Render(fmt.Sprintf("  %s  %dx%d", m.queryLang, len(m.cols), len(m.rows)))
		lines = append(lines, titleLine)
	} else {
		titleLine := titleStyle.Render(" "+title) + dimStyle.Render(fmt.Sprintf("  %dx%d", len(m.cols), len(m.rows)))
		lines = append(lines, titleLine)
	}

	if m.err != nil {
		lines = append(lines, errorStyle.Render(" error: "+m.err.Error()))
	}

	// query code pane
	if m.isQuery && m.queryCode != "" {
		codeLines := strings.Split(m.queryCode, "\n")
		maxShow := min(len(codeLines), 8)
		lineNumW := len(strconv.Itoa(len(codeLines)))
		if lineNumW < 2 {
			lineNumW = 2
		}
		maxLineW := m.width - lineNumW - 4
		for i := 0; i < maxShow; i++ {
			lineNum := gutterStyle.Render(fmt.Sprintf(" %*d ", lineNumW, i+1))
			line := codeLines[i]
			if len(line) > maxLineW {
				line = line[:maxLineW-1] + "."
			}
			lines = append(lines, lineNum+renderQueryLine(line))
		}
		if len(codeLines) > maxShow {
			lines = append(lines, dimStyle.Render(fmt.Sprintf(" %*s +%d lines", lineNumW, "", len(codeLines)-maxShow)))
		}
		sep := strings.Repeat("─", max(m.width-4, 0))
		if m.queryDirty {
			lines = append(lines, dimStyle.Render("  ")+errorStyle.Render("*")+dimStyle.Render(sep[:max(len(sep)-1, 0)]))
		} else {
			lines = append(lines, dimStyle.Render("  "+sep))
		}
	}

	if len(m.cols) == 0 {
		if m.isQuery {
			lines = append(lines, dimStyle.Render(" (no cached results)"))
		} else {
			lines = append(lines, dimStyle.Render(" (empty table)"))
		}
		for len(lines) < m.height {
			lines = append(lines, "")
		}
		return strings.Join(lines, "\n")
	}

	// gutter: marker + row number + separator
	digits := len(strconv.Itoa(len(m.rows)))
	if digits < 2 {
		digits = 2
	}
	gutterW := digits + 3 // "◆ NN " or "  NN "
	// subtract gutter from available width for column layout
	savedWidth := m.width
	m.width -= gutterW

	colWidths := m.computeColWidths()

	dataHeight := max(m.height-len(lines)-5, 1)

	if m.cy < m.scrollY {
		m.scrollY = m.cy
	}
	if m.cy >= m.scrollY+dataHeight {
		m.scrollY = m.cy - dataHeight + 1
	}

	visStart, visEnd := m.visibleColRange(colWidths)
	colWidths = m.expandColWidths(colWidths, visStart, visEnd)

	m.width = savedWidth

	// header
	sortHdrSt := headerStyle.Copy().Foreground(lipgloss.Color("14"))
	var hdr strings.Builder
	hdr.WriteString(gutterStyle.Render(fmt.Sprintf("%*s ", digits+2, "#")))
	for ci := visStart; ci < visEnd; ci++ {
		w := colWidths[ci]
		if m.mode == modeEdit && m.colRename && ci == m.cx {
			display := m.editBuf + "_"
			if len(display) > w {
				display = display[:w]
			}
			aligned := fmt.Sprintf("%-*s", w, display)
			hdr.WriteString(editBorderStyle.Render("│"))
			hdr.WriteString(editCurStyle.Render(aligned))
			hdr.WriteString(editBorderStyle.Render("│"))
		} else {
			name := m.cols[ci].name
			sorted := ci == m.sortCol
			if sorted {
				arrow := "▼"
				if m.sortAsc {
					arrow = "▲"
				}
				name = name + arrow
			}
			if len(name) > w {
				name = name[:w-1] + "."
			}
			cell := fmt.Sprintf(" %-*s ", w, name)
			if sorted {
				hdr.WriteString(sortHdrSt.Render(cell))
			} else {
				hdr.WriteString(headerStyle.Render(cell))
			}
		}
		if ci < visEnd-1 {
			hdr.WriteString(headerStyle.Render("│"))
		}
	}
	hdrStr := hdr.String()
	hdrVisLen := lipgloss.Width(hdrStr)
	if hdrVisLen < m.width {
		hdrStr += headerStyle.Render(strings.Repeat(" ", m.width-hdrVisLen))
	}
	lines = append(lines, hdrStr)

	// separator
	var sep strings.Builder
	sep.WriteString(strings.Repeat("─", gutterW))
	for ci := visStart; ci < visEnd; ci++ {
		w := colWidths[ci]
		sep.WriteString(strings.Repeat("─", w+2))
		if ci < visEnd-1 {
			sep.WriteString("┼")
		}
	}
	sepStr := sep.String()
	if len(sepStr) < m.width {
		sepStr += strings.Repeat("─", m.width-len(sepStr))
	}
	lines = append(lines, dimStyle.Render(sepStr))

	// pick styles based on mode
	cellCur := cursorStyle
	rowHL := rowCurStyle
	colHL := colHLStyle
	inVisual := m.mode == modeVisual
	var vx1, vy1, vx2, vy2 int
	if inVisual {
		vx1, vy1, vx2, vy2 = m.visualRect()
	}
	if m.mode == modeEdit {
		cellCur = editCurStyle
		rowHL = cellDimStyle
		colHL = cellDimStyle
	}

	// data rows
	editCell := m.mode == modeEdit && !m.colRename

	// edge-case: top border when cursor is first visible row
	if editCell && m.cy == m.scrollY {
		var bdr strings.Builder
		bdr.WriteString(strings.Repeat(" ", gutterW))
		for ci := visStart; ci < visEnd; ci++ {
			w := colWidths[ci]
			if ci == m.cx {
				bdr.WriteString(editBorderStyle.Render("╭" + strings.Repeat("─", w) + "╮"))
			} else {
				bdr.WriteString(strings.Repeat(" ", w+2))
			}
			if ci < visEnd-1 {
				bdr.WriteString(" ")
			}
		}
		lines = append(lines, bdr.String())
		dataHeight--
	}

	endRow := min(m.scrollY+dataHeight, len(m.rows))
	for ri := m.scrollY; ri < endRow; ri++ {
		row := m.rows[ri]

		// gutter
		origIdx := 0
		if ri < len(m.rowOrigIdx) {
			origIdx = m.rowOrigIdx[ri]
		}
		var gutter string
		if m.selected[origIdx] {
			gutter = gutterSelStyle.Render(fmt.Sprintf("◆ %*d ", digits, origIdx))
		} else {
			gutter = gutterStyle.Render(fmt.Sprintf("  %*d ", digits, origIdx))
		}

		// row cells
		var rowBuf strings.Builder
		rowBuf.WriteString(gutter)
		for ci := visStart; ci < visEnd; ci++ {
			w := colWidths[ci]
			c := m.cols[ci]
			val := row[c.key]

			// edit border overlays: top/bottom of cursor in edit column
			isBorderAbove := editCell && ci == m.cx && ri == m.cy-1
			isBorderBelow := editCell && ci == m.cx && ri == m.cy+1
			isVisSel := inVisual && ri >= vy1 && ri <= vy2 && ci >= vx1 && ci <= vx2

			if isBorderAbove {
				rowBuf.WriteString(editBorderStyle.Render("╭" + strings.Repeat("─", w) + "╮"))
			} else if isBorderBelow {
				rowBuf.WriteString(editBorderStyle.Render("╰" + strings.Repeat("─", w) + "╯"))
			} else if editCell && ri == m.cy && ci == m.cx {
				display := m.editBuf + "_"
				aligned := alignCell(display, c.typ, w)
				rowBuf.WriteString(editBorderStyle.Render("│"))
				rowBuf.WriteString(editCurStyle.Render(aligned))
				rowBuf.WriteString(editBorderStyle.Render("│"))
			} else {
				display := formatCell(val, c.typ)
				aligned := alignCell(display, c.typ, w)
				cell := " " + aligned + " "
				if ri == m.cy && ci == m.cx {
					rowBuf.WriteString(cellCur.Render(cell))
				} else if isVisSel {
					rowBuf.WriteString(visualSelStyle.Render(cell))
				} else if !inVisual && ri == m.cy {
					rowBuf.WriteString(rowHL.Render(cell))
				} else if !inVisual && ci == m.cx {
					rowBuf.WriteString(colHL.Render(cell))
				} else {
					rowBuf.WriteString(cellDimStyle.Render(cell))
				}
			}
			if ci < visEnd-1 {
				rowBuf.WriteString(cellDimStyle.Render("│"))
			}
		}

		rowStr := rowBuf.String()
		rowVisLen := lipgloss.Width(rowStr)
		if rowVisLen < m.width {
			pad := strings.Repeat(" ", m.width-rowVisLen)
			if !inVisual && ri == m.cy {
				rowStr += rowHL.Render(pad)
			} else {
				rowStr += pad
			}
		}
		lines = append(lines, rowStr)
	}

	// edge-case: bottom border when cursor is last visible row
	if editCell && m.cy >= endRow-1 {
		var bdr strings.Builder
		bdr.WriteString(strings.Repeat(" ", gutterW))
		for ci := visStart; ci < visEnd; ci++ {
			w := colWidths[ci]
			if ci == m.cx {
				bdr.WriteString(editBorderStyle.Render("╰" + strings.Repeat("─", w) + "╯"))
			} else {
				bdr.WriteString(strings.Repeat(" ", w+2))
			}
			if ci < visEnd-1 {
				bdr.WriteString(" ")
			}
		}
		lines = append(lines, bdr.String())
	}

	for len(lines) < m.height-3 {
		lines = append(lines, "")
	}

	// info line: column name, type, full cell value, stats
	if len(m.cols) > 0 && m.cx < len(m.cols) {
		c := m.cols[m.cx]
		fullVal := formatCell(m.cellValue(), c.typ)
		if fullVal == "" {
			fullVal = "(empty)"
		}
		info := fmt.Sprintf(" %s (%s) = %s", c.name, c.typ, fullVal)
		stats := m.colStats(m.cx)
		if stats != "" {
			combined := info + "  │  " + stats
			if len(combined) <= m.width {
				info = combined
			} else if len(info)+7 < m.width {
				info = combined[:m.width-1] + "."
			}
		}
		if len(info) > m.width {
			info = info[:m.width-1] + "."
		}
		lines = append(lines, dimStyle.Render(info))
	} else {
		lines = append(lines, "")
	}

	// status bar
	modeStr := "NORMAL"
	if m.isQuery && m.mode == modeNormal {
		if m.queryDirty {
			modeStr = "QUERY*"
		} else {
			modeStr = "QUERY"
		}
	}
	stStyle := statusStyle
	switch m.mode {
	case modeEdit:
		if m.colRename {
			modeStr = "RENAME"
		} else {
			modeStr = "EDIT"
		}
		stStyle = editStatStyle
	case modeVisual:
		vx1, vy1, vx2, vy2 := m.visualRect()
		modeStr = fmt.Sprintf("VISUAL %dx%d", vx2-vx1+1, vy2-vy1+1)
		stStyle = visualStatStyle
	}
	if m.pending != "" {
		modeStr += " " + m.pending
	}
	yankInd := ""
	if m.yankBuf != nil {
		yankInd = " [y]"
	}
	selInd := ""
	if len(m.selected) > 0 {
		selInd = fmt.Sprintf(" sel:%d", len(m.selected))
	}
	undoInd := ""
	if len(m.undoStack) > 0 {
		undoInd = fmt.Sprintf(" u:%d", len(m.undoStack))
	}
	status := fmt.Sprintf(" [%d,%d] %s%s%s%s", m.cx, m.cy, modeStr, yankInd, selInd, undoInd)
	lines = append(lines, stStyle.Render(status))

	var help string
	if m.isQuery {
		help = " hjkl move  v visual  y yank  </> sort  e edit  E execute  q back"
	} else {
		help = " hjkl move  JKHL shift  v visual  i edit  r rename  dd del  o row  A/X col  </> sort  u undo  ^R redo"
	}
	lines = append(lines, dimStyle.Render(help))
	return strings.Join(lines, "\n")
}

var refStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("14"))

func renderQueryLine(line string) string {
	indices := sheetRefRe.FindAllStringIndex(line, -1)
	if len(indices) == 0 {
		return dimStyle.Render(line)
	}
	var buf strings.Builder
	prev := 0
	for _, loc := range indices {
		if loc[0] > prev {
			buf.WriteString(dimStyle.Render(line[prev:loc[0]]))
		}
		buf.WriteString(refStyle.Render(line[loc[0]:loc[1]]))
		prev = loc[1]
	}
	if prev < len(line) {
		buf.WriteString(dimStyle.Render(line[prev:]))
	}
	return buf.String()
}

func (m model) computeColWidths() []int {
	widths := make([]int, len(m.cols))
	for i, c := range m.cols {
		widths[i] = max(len(c.name), 3)
	}
	sampleEnd := min(len(m.rows), 100)
	for _, row := range m.rows[:sampleEnd] {
		for i, c := range m.cols {
			s := formatCell(row[c.key], c.typ)
			if len(s) > widths[i] {
				widths[i] = len(s)
			}
		}
	}
	for i := range widths {
		widths[i] = min(widths[i], 40)
	}
	return widths
}

func (m model) expandColWidths(widths []int, visStart, visEnd int) []int {
	out := make([]int, len(widths))
	copy(out, widths)

	nVis := visEnd - visStart
	if nVis <= 0 {
		return out
	}

	used := 0
	for ci := visStart; ci < visEnd; ci++ {
		used += out[ci] + 2
		if ci < visEnd-1 {
			used++
		}
	}
	slack := m.width - used
	if slack <= 0 {
		return out
	}

	per := slack / nVis
	rem := slack % nVis
	for ci := visStart; ci < visEnd; ci++ {
		out[ci] += per
		if ci-visStart < rem {
			out[ci]++
		}
	}
	return out
}

func (m model) visibleColRange(widths []int) (int, int) {
	avail := m.width
	start := m.scrollX
	if start >= len(widths) {
		start = 0
	}
	used := 0
	end := start
	for end < len(widths) {
		w := widths[end] + 3
		if used+w > avail && end > start {
			break
		}
		used += w
		end++
	}
	if m.cx >= end {
		end = m.cx + 1
		used = 0
		for i := end - 1; i >= 0; i-- {
			used += widths[i] + 3
			if used > avail {
				start = i + 1
				break
			}
			start = i
		}
	}
	if m.cx < start {
		start = m.cx
	}
	return start, end
}

// --- Cell formatting ---

func formatCell(val any, typ string) string {
	if val == nil {
		return ""
	}
	switch typ {
	case "usd":
		switch v := val.(type) {
		case float64:
			return fmt.Sprintf("$%.2f", v)
		case int64:
			return fmt.Sprintf("$%d.00", v)
		default:
			return fmt.Sprintf("%v", val)
		}
	case "bool":
		switch v := val.(type) {
		case bool:
			if v {
				return "[x]"
			}
			return "[ ]"
		default:
			return fmt.Sprintf("%v", val)
		}
	case "percentage":
		switch v := val.(type) {
		case float64:
			return fmt.Sprintf("%.0f%%", v*100)
		default:
			return fmt.Sprintf("%v", val)
		}
	case "int":
		switch v := val.(type) {
		case float64:
			return strconv.Itoa(int(v))
		case int64:
			return strconv.FormatInt(v, 10)
		default:
			return fmt.Sprintf("%v", val)
		}
	case "num", "float":
		switch v := val.(type) {
		case float64:
			if v == math.Trunc(v) {
				return strconv.Itoa(int(v))
			}
			return strconv.FormatFloat(v, 'f', -1, 64)
		default:
			return fmt.Sprintf("%v", val)
		}
	default:
		return fmt.Sprintf("%v", val)
	}
}

func alignCell(s string, typ string, width int) string {
	if len(s) > width {
		return s[:width-1] + "."
	}
	switch typ {
	case "usd", "num", "int", "float", "percentage":
		return fmt.Sprintf("%*s", width, s)
	default:
		return fmt.Sprintf("%-*s", width, s)
	}
}

func parseCell(s string, typ string) any {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil
	}
	switch typ {
	case "int":
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			return v
		}
		return s
	case "num", "float", "usd":
		cleaned := strings.TrimPrefix(s, "$")
		cleaned = strings.ReplaceAll(cleaned, ",", "")
		if v, err := strconv.ParseFloat(cleaned, 64); err == nil {
			return v
		}
		return s
	case "bool":
		lower := strings.ToLower(s)
		return lower == "true" || lower == "1" || lower == "yes" || lower == "x"
	default:
		return s
	}
}

func main() {
	dataDir := "data/automerge"
	if len(os.Args) > 1 {
		dataDir = os.Args[1]
	}

	p := tea.NewProgram(initialModel(dataDir), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
