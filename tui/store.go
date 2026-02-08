package main

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/automerge/automerge-go"
	_ "modernc.org/sqlite"
)

type docInfo struct {
	id      string
	docType string
	title   string
	path    string // directory path containing snapshot/incremental
	modTime time.Time
	size    int64
	nCols   int
	nRows   int
}

type col struct {
	key  string
	name string
	typ  string
}

func discoverDocs(baseDir string) ([]docInfo, error) {
	var docs []docInfo

	prefixes, err := os.ReadDir(baseDir)
	if err != nil {
		return nil, fmt.Errorf("read automerge dir %s: %w", baseDir, err)
	}

	for _, prefix := range prefixes {
		if !prefix.IsDir() {
			continue
		}
		prefixPath := filepath.Join(baseDir, prefix.Name())
		entries, err := os.ReadDir(prefixPath)
		if err != nil {
			continue
		}
		for _, entry := range entries {
			if !entry.IsDir() {
				continue
			}
			docID := prefix.Name() + entry.Name()
			docPath := filepath.Join(prefixPath, entry.Name())

			info, err := entry.Info()
			if err != nil {
				continue
			}

			doc, size, err := loadDoc(docPath)
			if err != nil {
				continue
			}

			docType, nCols, nRows, title := inspectDoc(doc)

			docs = append(docs, docInfo{
				id:      docID,
				docType: docType,
				title:   title,
				path:    docPath,
				modTime: info.ModTime(),
				size:    size,
				nCols:   nCols,
				nRows:   nRows,
			})
		}
	}

	sort.Slice(docs, func(i, j int) bool {
		return docs[i].modTime.After(docs[j].modTime)
	})
	return docs, nil
}

func loadDoc(docPath string) (*automerge.Doc, int64, error) {
	var doc *automerge.Doc
	var totalSize int64

	// try snapshot first
	snapDir := filepath.Join(docPath, "snapshot")
	if entries, err := os.ReadDir(snapDir); err == nil {
		for _, e := range entries {
			if e.IsDir() {
				continue
			}
			data, err := os.ReadFile(filepath.Join(snapDir, e.Name()))
			if err != nil {
				continue
			}
			totalSize += int64(len(data))
			doc, err = automerge.Load(data)
			if err != nil {
				return nil, 0, fmt.Errorf("load snapshot: %w", err)
			}
			break // only one snapshot
		}
	}

	// apply incrementals
	incDir := filepath.Join(docPath, "incremental")
	if entries, err := os.ReadDir(incDir); err == nil {
		for _, e := range entries {
			if e.IsDir() {
				continue
			}
			data, err := os.ReadFile(filepath.Join(incDir, e.Name()))
			if err != nil {
				continue
			}
			totalSize += int64(len(data))
			if doc == nil {
				// no snapshot — try loading incremental as a full doc
				doc, err = automerge.Load(data)
				if err != nil {
					return nil, 0, fmt.Errorf("load incremental as doc: %w", err)
				}
			} else {
				doc.LoadIncremental(data)
			}
		}
	}

	if doc == nil {
		return nil, 0, fmt.Errorf("no data found in %s", docPath)
	}
	return doc, totalSize, nil
}

func inspectDoc(doc *automerge.Doc) (docType string, nCols int, nRows int, title string) {
	docType = "unknown"

	// check root "type" (set by CSV import path)
	if v, err := doc.Path("type").Get(); err == nil && v.Kind() == automerge.KindStr {
		docType = v.Str()
	}

	dataVal, err := doc.Path("data").Get()
	if err != nil {
		return
	}

	switch dataVal.Kind() {
	case automerge.KindMap:
		if dataVal.Map().Len() == 0 {
			docType = "empty"
		}
		return
	case automerge.KindList:
		// continue below
	default:
		return
	}

	dataList := dataVal.List()
	total := dataList.Len()
	if total == 0 {
		docType = "empty"
		return
	}

	row0, err := dataList.Get(0)
	if err != nil || row0.Kind() != automerge.KindMap {
		return
	}
	row0Map := row0.Map()
	keys, _ := row0Map.Keys()

	if len(keys) == 0 {
		return
	}

	// "data+type" metadata doc: data[0] has {data, type} keys
	if hasKey(keys, "data") && hasKey(keys, "type") {
		if tv, err := row0Map.Get("type"); err == nil && tv.Kind() == automerge.KindText {
			txt := tv.Text()
			if s, err := txt.Get(); err == nil {
				docType = s
			}
		}
		// the nested data[0].data list contains the actual table rows
		if dv, err := row0Map.Get("data"); err == nil && dv.Kind() == automerge.KindList {
			inner := dv.List()
			innerLen := inner.Len()
			if innerLen > 0 {
				nRows = innerLen - 1
				if firstRow, err := inner.Get(0); err == nil && firstRow.Kind() == automerge.KindMap {
					fm := firstRow.Map()
					nCols = fm.Len()
					title = colNamesTitle(fm)
				}
			}
		}
		return
	}

	// direct table: data[0] has numeric keys with {name, type, key} sub-maps
	firstVal, err := row0Map.Get(keys[0])
	if err == nil && firstVal.Kind() == automerge.KindMap {
		fm := firstVal.Map()
		fkeys, _ := fm.Keys()
		if hasKey(fkeys, "name") {
			if docType == "unknown" {
				docType = "table"
			}
			nCols = len(keys)
			nRows = total - 1
			title = colNamesTitle(row0Map)
			return
		}
	}

	// infer by keys
	if hasKey(keys, "code") && hasKey(keys, "lang") {
		docType = "query"
		title = getStr(row0Map, "code")
		if i := strings.Index(title, "\n"); i >= 0 {
			title = title[:i]
		}
	} else if hasKey(keys, "url") {
		if hasKey(keys, "interval") {
			docType = "net-http"
		} else {
			docType = "net-socket"
		}
		title = getStr(row0Map, "url")
	}
	return
}

func colNamesTitle(colDefsMap *automerge.Map) string {
	keys, _ := colDefsMap.Keys()
	sortedKeys := make([]string, len(keys))
	copy(sortedKeys, keys)
	sort.Slice(sortedKeys, func(i, j int) bool {
		a, _ := strconv.Atoi(sortedKeys[i])
		b, _ := strconv.Atoi(sortedKeys[j])
		return a < b
	})
	var names []string
	for _, k := range sortedKeys {
		v, err := colDefsMap.Get(k)
		if err != nil || v.Kind() != automerge.KindMap {
			continue
		}
		name := getStr(v.Map(), "name")
		if name != "" {
			names = append(names, name)
		}
		if len(names) >= 5 {
			break
		}
	}
	return strings.Join(names, ", ")
}

func getStr(m *automerge.Map, key string) string {
	v, err := m.Get(key)
	if err != nil {
		return ""
	}
	switch v.Kind() {
	case automerge.KindStr:
		return v.Str()
	case automerge.KindText:
		s, _ := v.Text().Get()
		return s
	default:
		return fmt.Sprintf("%v", v.Interface())
	}
}

func hasKey(keys []string, key string) bool {
	for _, k := range keys {
		if k == key {
			return true
		}
	}
	return false
}

func readQueryDoc(doc *automerge.Doc) (code, lang string, cols []col, rows []map[string]any, err error) {
	dataList := resolveDataList(doc)
	if dataList == nil || dataList.Len() == 0 {
		return "", "", nil, nil, fmt.Errorf("doc has no data")
	}

	row0, e := dataList.Get(0)
	if e != nil || row0.Kind() != automerge.KindMap {
		return "", "", nil, nil, fmt.Errorf("query row 0: not a map")
	}
	row0Map := row0.Map()
	code = getStr(row0Map, "code")
	lang = getStr(row0Map, "lang")

	// check for cached results: data[1] should be column defs if present
	if dataList.Len() < 2 {
		return code, lang, nil, nil, nil
	}
	row1, e := dataList.Get(1)
	if e != nil || row1.Kind() != automerge.KindMap {
		return code, lang, nil, nil, nil
	}
	row1Map := row1.Map()
	keys, _ := row1Map.Keys()
	if len(keys) == 0 {
		return code, lang, nil, nil, nil
	}
	firstVal, e := row1Map.Get(keys[0])
	if e != nil || firstVal.Kind() != automerge.KindMap {
		return code, lang, nil, nil, nil
	}
	fkeys, _ := firstVal.Map().Keys()
	if !hasKey(fkeys, "name") {
		return code, lang, nil, nil, nil
	}

	// data[1] is column defs, data[2..] are result rows — read as offset table
	cols, rows, _ = readTableFromListOffset(dataList, 1)
	return code, lang, cols, rows, nil
}

func readTable(doc *automerge.Doc) ([]col, []map[string]any, error) {
	dataList := resolveDataList(doc)
	if dataList == nil {
		return nil, nil, fmt.Errorf("doc has no table data")
	}
	return readTableFromList(dataList)
}

// resolveDataList handles both direct tables (data = [cols, ...rows])
// and metadata docs (data = [{data: [cols, ...rows], type: "table"}])
func resolveDataList(doc *automerge.Doc) *automerge.List {
	dataVal, err := doc.Path("data").Get()
	if err != nil {
		return nil
	}
	if dataVal.Kind() != automerge.KindList {
		return nil
	}
	outerList := dataVal.List()
	if outerList.Len() == 0 {
		return nil
	}

	// check if this is a metadata doc with nested data
	first, err := outerList.Get(0)
	if err != nil || first.Kind() != automerge.KindMap {
		return outerList
	}
	firstMap := first.Map()
	keys, _ := firstMap.Keys()
	if hasKey(keys, "data") && hasKey(keys, "type") {
		if inner, err := firstMap.Get("data"); err == nil && inner.Kind() == automerge.KindList {
			return inner.List()
		}
	}
	return outerList
}

func readTableFromList(dataList *automerge.List) ([]col, []map[string]any, error) {
	return readTableFromListOffset(dataList, 0)
}

func readTableFromListOffset(dataList *automerge.List, colDefIdx int) ([]col, []map[string]any, error) {
	total := dataList.Len()
	if total <= colDefIdx {
		return nil, nil, fmt.Errorf("data list is empty")
	}

	row0Val, err := dataList.Get(colDefIdx)
	if err != nil {
		return nil, nil, fmt.Errorf("get row %d: %w", colDefIdx, err)
	}
	if row0Val.Kind() != automerge.KindMap {
		return nil, nil, fmt.Errorf("row %d is %s, expected map", colDefIdx, row0Val.Kind())
	}
	row0 := row0Val.Map()
	keys, err := row0.Keys()
	if err != nil {
		return nil, nil, fmt.Errorf("row %d keys: %w", colDefIdx, err)
	}

	var cols []col
	for _, k := range keys {
		colVal, err := row0.Get(k)
		if err != nil || colVal.Kind() != automerge.KindMap {
			continue
		}
		colMap := colVal.Map()
		c := col{key: k}
		c.name = getStr(colMap, "name")
		c.typ = getStr(colMap, "type")
		if c.name == "" {
			c.name = "col" + k
		}
		cols = append(cols, c)
	}

	sort.Slice(cols, func(i, j int) bool {
		a, _ := strconv.Atoi(cols[i].key)
		b, _ := strconv.Atoi(cols[j].key)
		return a < b
	})

	var rows []map[string]any
	for i := colDefIdx + 1; i < total; i++ {
		rowVal, err := dataList.Get(i)
		if err != nil || rowVal.Kind() != automerge.KindMap {
			continue
		}
		rowMap := rowVal.Map()
		row := make(map[string]any)
		for _, c := range cols {
			v, err := rowMap.Get(c.key)
			if err != nil || v.Kind() == automerge.KindVoid {
				row[c.key] = nil
				continue
			}
			switch v.Kind() {
			case automerge.KindStr:
				row[c.key] = v.Str()
			case automerge.KindFloat64:
				row[c.key] = v.Float64()
			case automerge.KindInt64:
				row[c.key] = v.Int64()
			case automerge.KindUint64:
				row[c.key] = v.Uint64()
			case automerge.KindBool:
				row[c.key] = v.Bool()
			case automerge.KindNull:
				row[c.key] = nil
			default:
				row[c.key] = v.Interface()
			}
		}
		rows = append(rows, row)
	}

	return cols, rows, nil
}

var sheetRefRe = regexp.MustCompile(`@[a-zA-Z0-9_:.-]+`)

func executeQuery(code string, dataDir string) ([]col, []map[string]any, error) {
	refs := sheetRefRe.FindAllString(code, -1)

	type sheet struct {
		cols      []col
		rows      []map[string]any
		tableName string
	}
	loaded := map[string]*sheet{}

	for _, ref := range refs {
		id := ref[1:] // strip @
		if _, ok := loaded[id]; ok {
			continue
		}

		// sheet_id = type:doc_id — we need the doc_id for filesystem lookup
		docID := id
		if i := strings.Index(id, ":"); i >= 0 {
			docID = id[i+1:]
		}

		docPath := docPathFromID(dataDir, docID)
		doc, _, err := loadDoc(docPath)
		if err != nil {
			return nil, nil, fmt.Errorf("load @%s (%s): %w", id, docPath, err)
		}
		cols, rows, err := readTable(doc)
		if err != nil {
			return nil, nil, fmt.Errorf("read @%s: %w", id, err)
		}

		tableName := sanitizeIdent(id)
		loaded[id] = &sheet{cols: cols, rows: rows, tableName: tableName}
	}

	// rewrite SQL: @type:doc_id → "type_doc_id"
	rewritten := code
	for id, s := range loaded {
		rewritten = strings.ReplaceAll(rewritten, "@"+id, `"`+s.tableName+`"`)
	}

	db, err := sql.Open("sqlite", ":memory:")
	if err != nil {
		return nil, nil, fmt.Errorf("sqlite: %w", err)
	}
	defer db.Close()

	for _, s := range loaded {
		if err := loadIntoSQLite(db, s.tableName, s.cols, s.rows); err != nil {
			return nil, nil, err
		}
	}

	sqlRows, err := db.Query(rewritten)
	if err != nil {
		return nil, nil, err
	}
	defer sqlRows.Close()
	return scanQueryResults(sqlRows)
}

func docPathFromID(dataDir, docID string) string {
	if len(docID) < 2 {
		return filepath.Join(dataDir, docID)
	}
	return filepath.Join(dataDir, docID[:2], docID[2:])
}

func sanitizeIdent(s string) string {
	var b strings.Builder
	for _, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' {
			b.WriteRune(c)
		} else {
			b.WriteRune('_')
		}
	}
	return b.String()
}

func loadIntoSQLite(db *sql.DB, tableName string, cols []col, rows []map[string]any) error {
	// CREATE TABLE with column names
	colDefs := make([]string, len(cols))
	for i, c := range cols {
		sqlType := "TEXT"
		switch c.typ {
		case "num", "int", "float", "usd", "percentage":
			sqlType = "REAL"
		case "bool":
			sqlType = "INTEGER"
		}
		colDefs[i] = fmt.Sprintf(`"%s" %s`, c.name, sqlType)
	}
	_, err := db.Exec(fmt.Sprintf(`CREATE TABLE "%s" (%s)`, tableName, strings.Join(colDefs, ", ")))
	if err != nil {
		return fmt.Errorf("create table %s: %w", tableName, err)
	}

	if len(rows) == 0 {
		return nil
	}

	// batch INSERT
	placeholders := make([]string, len(cols))
	for i := range placeholders {
		placeholders[i] = "?"
	}
	insertSQL := fmt.Sprintf(`INSERT INTO "%s" VALUES (%s)`, tableName, strings.Join(placeholders, ","))

	tx, err := db.Begin()
	if err != nil {
		return err
	}
	stmt, err := tx.Prepare(insertSQL)
	if err != nil {
		tx.Rollback()
		return err
	}
	for _, row := range rows {
		vals := make([]any, len(cols))
		for i, c := range cols {
			vals[i] = row[c.key]
		}
		if _, err := stmt.Exec(vals...); err != nil {
			tx.Rollback()
			return fmt.Errorf("insert into %s: %w", tableName, err)
		}
	}
	stmt.Close()
	return tx.Commit()
}

func scanQueryResults(sqlRows *sql.Rows) ([]col, []map[string]any, error) {
	colNames, err := sqlRows.Columns()
	if err != nil {
		return nil, nil, err
	}

	resultCols := make([]col, len(colNames))
	for i, name := range colNames {
		resultCols[i] = col{key: strconv.Itoa(i), name: name, typ: "text"}
	}

	var resultRows []map[string]any
	for sqlRows.Next() {
		ptrs := make([]any, len(colNames))
		for i := range ptrs {
			ptrs[i] = new(any)
		}
		if err := sqlRows.Scan(ptrs...); err != nil {
			return nil, nil, err
		}
		row := make(map[string]any)
		for i := range colNames {
			v := *(ptrs[i].(*any))
			// sqlite driver returns int64/float64/string/[]byte/nil
			if b, ok := v.([]byte); ok {
				v = string(b)
			}
			row[strconv.Itoa(i)] = v
		}
		resultRows = append(resultRows, row)
	}
	return resultCols, resultRows, sqlRows.Err()
}

func saveDoc(doc *automerge.Doc, docPath string) error {
	data := doc.Save()
	snapDir := filepath.Join(docPath, "snapshot")
	os.MkdirAll(snapDir, 0755)

	// clear existing snapshots
	if entries, err := os.ReadDir(snapDir); err == nil {
		for _, e := range entries {
			os.Remove(filepath.Join(snapDir, e.Name()))
		}
	}
	// clear incrementals (snapshot supersedes them)
	incDir := filepath.Join(docPath, "incremental")
	if entries, err := os.ReadDir(incDir); err == nil {
		for _, e := range entries {
			os.Remove(filepath.Join(incDir, e.Name()))
		}
	}
	os.Remove(incDir)

	// write new snapshot with a simple name
	return os.WriteFile(filepath.Join(snapDir, "tui-save"), data, 0644)
}
