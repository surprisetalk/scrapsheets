package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"

	"github.com/automerge/automerge-go"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var (
	titleStyle   = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("12"))
	headerStyle  = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("15")).Background(lipgloss.Color("8"))
	cursorStyle  = lipgloss.NewStyle().Background(lipgloss.Color("240")).Foreground(lipgloss.Color("15")).Bold(true)
	editCurStyle = lipgloss.NewStyle().Background(lipgloss.Color("29")).Foreground(lipgloss.Color("15")).Bold(true)
	dimStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color("8"))
	statusStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color("14"))
	editStatStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("29"))
	errorStyle   = lipgloss.NewStyle().Foreground(lipgloss.Color("9"))
	rowCurStyle  = lipgloss.NewStyle().Background(lipgloss.Color("236"))
	editRowStyle = lipgloss.NewStyle().Background(lipgloss.Color("236"))
	libHdrStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color("8")).Bold(true)
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
	isMetadata bool

	// vim state
	pending  string
	yank     any
	yankType string
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
	case tea.KeyMsg:
		if m.view == viewLibrary {
			return m.updateLibrary(msg)
		}
		if m.mode == modeEdit {
			return m.updateEdit(msg)
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
	cols, rows, err := readTable(doc)
	if err != nil {
		m.err = err
		return m, nil
	}

	// detect metadata format for correct mutation paths
	m.isMetadata = false
	dataVal, err := doc.Path("data").Get()
	if err == nil && dataVal.Kind() == automerge.KindList {
		if first, err := dataVal.List().Get(0); err == nil && first.Kind() == automerge.KindMap {
			keys, _ := first.Map().Keys()
			if hasKey(keys, "data") && hasKey(keys, "type") {
				m.isMetadata = true
			}
		}
	}

	m.view = viewTable
	m.doc = doc
	m.docPath = info.path
	m.docID = info.id
	m.cols = cols
	m.rows = rows
	m.cx, m.cy = 0, 0
	m.scrollX, m.scrollY = 0, 0
	m.mode = modeNormal
	m.pending = ""
	m.err = nil
	return m, nil
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
	case "0", "home", "^":
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
	case "H":
		m.cy = m.scrollY
	case "M":
		dataH := max(m.height-6, 1)
		m.cy = min(m.scrollY+dataH/2, lastRow)
	case "L":
		dataH := max(m.height-6, 1)
		m.cy = min(m.scrollY+dataH-1, lastRow)
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
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = formatCell(m.cellValue(), m.cols[m.cx].typ)
		}
	case "a":
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = formatCell(m.cellValue(), m.cols[m.cx].typ)
		}
	case "c":
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = ""
		}
	case "x":
		m.setCellValue(nil)

	// -- yank / paste --
	case "y":
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.yank = m.cellValue()
			m.yankType = m.cols[m.cx].typ
		}
	case "p":
		if m.yank != nil && len(m.rows) > 0 && m.cx < len(m.cols) {
			m.setCellValue(m.yank)
		}

	// -- row operations --
	case "o":
		return m.insertRowAt(m.cy + 1)
	case "O":
		return m.insertRowAt(m.cy)
	case "d":
		m.pending = "d"

	// -- column operations --
	case "A":
		return m.appendCol()
	}
	return m, nil
}

// --- Edit mode ---

func (m model) updateEdit(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "enter":
		m.commitEdit()
		m.mode = modeNormal
		if m.cy < len(m.rows)-1 {
			m.cy++
		}
	case "esc":
		m.mode = modeNormal
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

// --- Mutations ---

func (m *model) commitEdit() {
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
	m.doc.Commit("tui edit")
	if err := saveDoc(m.doc, m.docPath); err != nil {
		m.err = err
	}
}

func (m model) cellValue() any {
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return nil
	}
	return m.rows[m.cy][m.cols[m.cx].key]
}

// --- Row/Column ops ---

func (m model) deleteRow() (tea.Model, tea.Cmd) {
	if len(m.rows) == 0 {
		return m, nil
	}
	dataIdx := m.cy + 1
	dataList := resolveDataList(m.doc)
	if dataList != nil {
		dataList.Delete(dataIdx)
	}
	m.rows = append(m.rows[:m.cy], m.rows[m.cy+1:]...)
	if m.cy >= len(m.rows) && m.cy > 0 {
		m.cy--
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
	// splice into local rows
	m.rows = append(m.rows, nil)
	copy(m.rows[idx+1:], m.rows[idx:])
	m.rows[idx] = row

	dataIdx := idx + 1
	dataList := resolveDataList(m.doc)
	if dataList != nil {
		dataList.Insert(dataIdx, automerge.NewMap())
	}
	m.cy = idx
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
			lines = append(lines, line)
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
	titleLine := titleStyle.Render(" "+title) + dimStyle.Render(fmt.Sprintf("  %dx%d", len(m.cols), len(m.rows)))
	lines = append(lines, titleLine)

	if m.err != nil {
		lines = append(lines, errorStyle.Render(" error: "+m.err.Error()))
	}

	if len(m.cols) == 0 {
		lines = append(lines, dimStyle.Render(" (empty table)"))
		for len(lines) < m.height {
			lines = append(lines, "")
		}
		return strings.Join(lines, "\n")
	}

	colWidths := m.computeColWidths()

	dataHeight := max(m.height-len(lines)-4, 1)

	if m.cy < m.scrollY {
		m.scrollY = m.cy
	}
	if m.cy >= m.scrollY+dataHeight {
		m.scrollY = m.cy - dataHeight + 1
	}

	visStart, visEnd := m.visibleColRange(colWidths)
	colWidths = m.expandColWidths(colWidths, visStart, visEnd)

	// header
	var hdr strings.Builder
	for ci := visStart; ci < visEnd; ci++ {
		w := colWidths[ci]
		name := m.cols[ci].name
		if len(name) > w {
			name = name[:w-1] + "."
		}
		cell := fmt.Sprintf(" %-*s ", w, name)
		hdr.WriteString(headerStyle.Render(cell))
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
	if m.mode == modeEdit {
		cellCur = editCurStyle
		rowHL = editRowStyle
	}

	// data rows
	endRow := min(m.scrollY+dataHeight, len(m.rows))
	for ri := m.scrollY; ri < endRow; ri++ {
		row := m.rows[ri]
		var rowBuf strings.Builder
		for ci := visStart; ci < visEnd; ci++ {
			w := colWidths[ci]
			c := m.cols[ci]
			val := row[c.key]

			var display string
			if m.mode == modeEdit && ri == m.cy && ci == m.cx {
				display = m.editBuf + "_"
			} else {
				display = formatCell(val, c.typ)
			}

			aligned := alignCell(display, c.typ, w)
			cell := " " + aligned + " "

			if ri == m.cy && ci == m.cx {
				rowBuf.WriteString(cellCur.Render(cell))
			} else if ri == m.cy {
				rowBuf.WriteString(rowHL.Render(cell))
			} else {
				rowBuf.WriteString(cell)
			}
			if ci < visEnd-1 {
				if ri == m.cy {
					rowBuf.WriteString(rowHL.Render("│"))
				} else {
					rowBuf.WriteString(dimStyle.Render("│"))
				}
			}
		}

		rowStr := rowBuf.String()
		rowVisLen := lipgloss.Width(rowStr)
		if rowVisLen < m.width {
			pad := strings.Repeat(" ", m.width-rowVisLen)
			if ri == m.cy {
				rowStr += rowHL.Render(pad)
			} else {
				rowStr += pad
			}
		}
		lines = append(lines, rowStr)
	}

	for len(lines) < m.height-2 {
		lines = append(lines, "")
	}

	// status bar
	modeStr := "NORMAL"
	stStyle := statusStyle
	if m.mode == modeEdit {
		modeStr = "EDIT"
		stStyle = editStatStyle
	}
	if m.pending != "" {
		modeStr += " " + m.pending
	}
	yankInd := ""
	if m.yank != nil {
		yankInd = " [y]"
	}
	status := fmt.Sprintf(" [%d,%d] %s%s", m.cx, m.cy, modeStr, yankInd)
	lines = append(lines, stStyle.Render(status))

	help := " hjkl/wb move  0/$ ends  gg/G top/bot  i edit  c clear  dd del  o/O row  y/p yank  esc back"
	lines = append(lines, dimStyle.Render(help))
	return strings.Join(lines, "\n")
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
