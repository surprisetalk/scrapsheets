package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"

	"github.com/automerge/automerge-go"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// styles
var (
	titleStyle  = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("12"))
	headerStyle = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("15")).Background(lipgloss.Color("8"))
	cursorStyle = lipgloss.NewStyle().Background(lipgloss.Color("4")).Foreground(lipgloss.Color("15"))
	dimStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color("8"))
	statusStyle = lipgloss.NewStyle().Foreground(lipgloss.Color("14"))
	errorStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color("9"))
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

	// table
	doc     *automerge.Doc
	docPath string
	docID   string
	cols    []col
	rows    []map[string]any
	cx, cy  int // cursor x, y (y=0 is first data row)
	scrollX int
	scrollY int
	mode    mode
	editBuf string
	dirty   bool
}

func initialModel(dataDir string) model {
	m := model{dataDir: dataDir, view: viewLibrary}
	allDocs, err := discoverDocs(dataDir)
	if err != nil {
		m.err = err
	}
	// filter to interesting docs (skip empty/unknown)
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

func (m model) updateLibrary(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit
	case "up", "k":
		if m.libCursor > 0 {
			m.libCursor--
		}
	case "down", "j":
		if m.libCursor < len(m.docs)-1 {
			m.libCursor++
		}
	case "enter":
		if m.libCursor < len(m.docs) {
			return m.openDoc(m.docs[m.libCursor])
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
	m.view = viewTable
	m.doc = doc
	m.docPath = info.path
	m.docID = info.id
	m.cols = cols
	m.rows = rows
	m.cx, m.cy = 0, 0
	m.scrollX, m.scrollY = 0, 0
	m.mode = modeNormal
	m.dirty = false
	m.err = nil
	return m, nil
}

// --- Table (normal) ---

func (m model) updateTable(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "esc":
		m.view = viewLibrary
		m.doc = nil
		return m, nil
	case "ctrl+c":
		return m, tea.Quit
	case "left", "h":
		if m.cx > 0 {
			m.cx--
		}
	case "right", "l":
		if m.cx < len(m.cols)-1 {
			m.cx++
		}
	case "up", "k":
		if m.cy > 0 {
			m.cy--
		}
	case "down", "j":
		if m.cy < len(m.rows)-1 {
			m.cy++
		}
	case "home":
		m.cx = 0
	case "end":
		m.cx = len(m.cols) - 1
	case "ctrl+home":
		m.cx, m.cy = 0, 0
	case "ctrl+end":
		m.cx = len(m.cols) - 1
		m.cy = len(m.rows) - 1
	case "tab":
		m.cx++
		if m.cx >= len(m.cols) {
			m.cx = 0
			m.cy++
			if m.cy >= len(m.rows) {
				m.cy = len(m.rows) - 1
			}
		}
	case "shift+tab":
		m.cx--
		if m.cx < 0 {
			m.cx = len(m.cols) - 1
			m.cy--
			if m.cy < 0 {
				m.cy = 0
			}
		}
	case "enter":
		if len(m.rows) > 0 && m.cx < len(m.cols) {
			m.mode = modeEdit
			m.editBuf = formatCell(m.cellValue(), m.cols[m.cx].typ)
		}
	case "a":
		return m.appendRow()
	case "A":
		return m.appendCol()
	case "d":
		// TODO: dd for delete row
	case "ctrl+s":
		return m.save()
	}
	return m, nil
}

// --- Edit mode ---

func (m model) updateEdit(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "enter":
		m.commitEdit()
		m.mode = modeNormal
		// move down after confirm
		if m.cy < len(m.rows)-1 {
			m.cy++
		}
	case "esc":
		m.mode = modeNormal
	case "backspace":
		if len(m.editBuf) > 0 {
			m.editBuf = m.editBuf[:len(m.editBuf)-1]
		}
	case "tab":
		m.commitEdit()
		m.mode = modeNormal
		m.cx++
		if m.cx >= len(m.cols) {
			m.cx = 0
			m.cy++
			if m.cy >= len(m.rows) {
				m.cy = len(m.rows) - 1
			}
		}
	default:
		if len(msg.String()) == 1 || msg.String() == " " {
			m.editBuf += msg.String()
		}
	}
	return m, nil
}

func (m *model) commitEdit() {
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return
	}
	c := m.cols[m.cx]
	val := parseCell(m.editBuf, c.typ)
	m.rows[m.cy][c.key] = val

	// update automerge doc
	rowIdx := m.cy + 1 // data rows start at index 1
	m.doc.Path("data", rowIdx, c.key).Set(val)
	m.dirty = true
}

func (m model) cellValue() any {
	if m.cy >= len(m.rows) || m.cx >= len(m.cols) {
		return nil
	}
	return m.rows[m.cy][m.cols[m.cx].key]
}

// --- Row/Column ops ---

func (m model) appendRow() (tea.Model, tea.Cmd) {
	row := make(map[string]any)
	for _, c := range m.cols {
		row[c.key] = nil
	}
	m.rows = append(m.rows, row)

	newIdx := len(m.rows) // 1-indexed in automerge data list
	dataList := m.doc.Path("data").List()
	if dataList != nil {
		newMap := automerge.NewMap()
		dataList.Append(newMap)
		_ = newIdx // row is appended as empty map
	}
	m.cy = len(m.rows) - 1
	m.dirty = true
	return m, nil
}

func (m model) appendCol() (tea.Model, tea.Cmd) {
	newKey := strconv.Itoa(len(m.cols))
	c := col{key: newKey, name: "col" + newKey, typ: "text"}
	m.cols = append(m.cols, c)

	// update automerge row 0
	colDef := automerge.NewMap()
	m.doc.Path("data", 0, newKey).Set(colDef)
	m.doc.Path("data", 0, newKey, "name").Set(c.name)
	m.doc.Path("data", 0, newKey, "type").Set(c.typ)
	m.doc.Path("data", 0, newKey, "key").Set(newKey)

	for i, row := range m.rows {
		row[newKey] = nil
		_ = i
	}
	m.dirty = true
	return m, nil
}

func (m model) save() (tea.Model, tea.Cmd) {
	if !m.dirty {
		return m, nil
	}
	m.doc.Commit("tui edit")
	if err := saveDoc(m.doc, m.docPath); err != nil {
		m.err = err
		return m, nil
	}
	m.dirty = false
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
	var b strings.Builder
	b.WriteString(titleStyle.Render(" Scrapsheets TUI"))
	b.WriteString("\n")

	if m.err != nil {
		b.WriteString(errorStyle.Render(" error: "+m.err.Error()) + "\n")
	}

	if len(m.docs) == 0 {
		b.WriteString(dimStyle.Render(" no documents found\n"))
		b.WriteString(dimStyle.Render(fmt.Sprintf(" (searched %s)\n", m.dataDir)))
	}

	visibleRows := m.height - 4
	if visibleRows < 1 {
		visibleRows = 1
	}

	// adjust scroll to keep cursor visible
	if m.libCursor < m.libScroll {
		m.libScroll = m.libCursor
	}
	if m.libCursor >= m.libScroll+visibleRows {
		m.libScroll = m.libCursor - visibleRows + 1
	}

	for i := m.libScroll; i < len(m.docs) && i < m.libScroll+visibleRows; i++ {
		d := m.docs[i]
		cursor := "  "
		if i == m.libCursor {
			cursor = "> "
		}

		id := d.id
		if len(id) > 12 {
			id = id[:12] + ".."
		}

		line := fmt.Sprintf("%s%-8s %-14s %3dx%-3d %s",
			cursor, d.docType, id, d.nCols, d.nRows, d.modTime.Format("Jan 02"))

		if i == m.libCursor {
			b.WriteString(cursorStyle.Render(line))
		} else {
			b.WriteString(line)
		}
		b.WriteString("\n")
	}

	b.WriteString("\n")
	b.WriteString(dimStyle.Render(" j/k navigate  enter open  q quit"))
	return b.String()
}

func (m model) viewTable() string {
	var b strings.Builder

	// title
	title := m.docID
	if len(title) > 20 {
		title = title[:20] + ".."
	}
	b.WriteString(titleStyle.Render(" " + title))
	if m.dirty {
		b.WriteString(" *")
	}
	b.WriteString("\n")

	if m.err != nil {
		b.WriteString(errorStyle.Render(" error: "+m.err.Error()) + "\n")
	}

	if len(m.cols) == 0 {
		b.WriteString(dimStyle.Render(" (empty table)\n"))
		return b.String()
	}

	// compute column widths
	colWidths := m.computeColWidths()

	// available height for data rows
	dataHeight := m.height - 5 // title + header + status + help + border

	// adjust scroll
	if m.cy < m.scrollY {
		m.scrollY = m.cy
	}
	if m.cy >= m.scrollY+dataHeight {
		m.scrollY = m.cy - dataHeight + 1
	}

	// visible column range
	visStart, visEnd := m.visibleColRange(colWidths)

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
			hdr.WriteString(dimStyle.Render("|"))
		}
	}
	b.WriteString(hdr.String())
	b.WriteString("\n")

	// separator
	var sep strings.Builder
	for ci := visStart; ci < visEnd; ci++ {
		w := colWidths[ci]
		sep.WriteString(dimStyle.Render(strings.Repeat("─", w+2)))
		if ci < visEnd-1 {
			sep.WriteString(dimStyle.Render("┼"))
		}
	}
	b.WriteString(sep.String())
	b.WriteString("\n")

	// data rows
	endRow := m.scrollY + dataHeight
	if endRow > len(m.rows) {
		endRow = len(m.rows)
	}
	for ri := m.scrollY; ri < endRow; ri++ {
		row := m.rows[ri]
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
			cell := fmt.Sprintf(" %s ", aligned)

			if ri == m.cy && ci == m.cx {
				b.WriteString(cursorStyle.Render(cell))
			} else {
				b.WriteString(cell)
			}
			if ci < visEnd-1 {
				b.WriteString(dimStyle.Render("│"))
			}
		}
		b.WriteString("\n")
	}

	// status bar
	modeStr := "NORMAL"
	if m.mode == modeEdit {
		modeStr = "EDIT"
	}
	status := fmt.Sprintf(" [%d,%d] %s  %dx%d", m.cx, m.cy, modeStr, len(m.cols), len(m.rows))
	b.WriteString(statusStyle.Render(status))
	b.WriteString("\n")

	help := " hjkl move  enter edit  a row  A col  ctrl+s save  esc back"
	b.WriteString(dimStyle.Render(help))
	return b.String()
}

func (m model) computeColWidths() []int {
	widths := make([]int, len(m.cols))
	for i, c := range m.cols {
		widths[i] = len(c.name)
		if widths[i] < 4 {
			widths[i] = 4
		}
	}
	// sample rows for width
	sampleEnd := len(m.rows)
	if sampleEnd > 100 {
		sampleEnd = 100
	}
	for _, row := range m.rows[:sampleEnd] {
		for i, c := range m.cols {
			s := formatCell(row[c.key], c.typ)
			if len(s) > widths[i] {
				widths[i] = len(s)
			}
		}
	}
	// cap at reasonable max
	for i := range widths {
		if widths[i] > 30 {
			widths[i] = 30
		}
	}
	return widths
}

func (m model) visibleColRange(widths []int) (int, int) {
	avail := m.width - 2
	start := m.scrollX
	if start >= len(widths) {
		start = 0
	}
	used := 0
	end := start
	for end < len(widths) {
		w := widths[end] + 3 // padding + separator
		if used+w > avail && end > start {
			break
		}
		used += w
		end++
	}
	// ensure cursor column is visible
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
		// right-align numbers
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
