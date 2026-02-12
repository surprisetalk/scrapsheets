package main

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestDiscoverAndReadDocs(t *testing.T) {
	dataDir := filepath.Join("..", "data", "automerge")
	if _, err := os.Stat(dataDir); os.IsNotExist(err) {
		t.Skip("data/automerge not found, skipping")
	}

	docs, err := discoverDocs(dataDir)
	if err != nil {
		t.Fatalf("discoverDocs: %v", err)
	}
	if len(docs) == 0 {
		t.Fatal("expected at least 1 document")
	}

	// count types
	types := map[string]int{}
	for _, d := range docs {
		types[d.docType]++
	}
	fmt.Printf("%d docs:", len(docs))
	for k, v := range types {
		fmt.Printf(" %s=%d", k, v)
	}
	fmt.Println()

	if types["table"] == 0 {
		t.Fatal("expected at least 1 table document")
	}

	// try reading a table
	for _, d := range docs {
		if d.docType != "table" || d.nCols == 0 {
			continue
		}
		doc, _, err := loadDoc(d.path)
		if err != nil {
			t.Fatalf("loadDoc %s: %v", d.id, err)
		}
		cols, rows, err := readTable(doc)
		if err != nil {
			t.Fatalf("readTable %s: %v", d.id, err)
		}
		fmt.Printf("table %s: %d cols, %d rows\n", d.id, len(cols), len(rows))
		for _, c := range cols {
			fmt.Printf("  col %s: name=%q type=%q\n", c.key, c.name, c.typ)
		}
		if len(cols) == 0 {
			t.Fatalf("expected columns in table %s", d.id)
		}
		break
	}

	// try reading a query doc
	for _, d := range docs {
		if d.docType != "query" {
			continue
		}
		doc, _, err := loadDoc(d.path)
		if err != nil {
			t.Fatalf("loadDoc query %s: %v", d.id, err)
		}
		code, lang, cols, rows, err := readQueryDoc(doc)
		if err != nil {
			t.Fatalf("readQueryDoc %s: %v", d.id, err)
		}
		fmt.Printf("query %s: lang=%q code=%q cols=%d rows=%d\n", d.id, lang, truncate(code, 60), len(cols), len(rows))
		if code == "" {
			t.Fatalf("expected non-empty code in query %s", d.id)
		}
		break
	}
}

func TestExecuteQueryBasic(t *testing.T) {
	// pure in-memory test — no automerge docs needed
	cols, rows, err := executeQuery("SELECT 1 as a, 'hello' as b", ".")
	if err != nil {
		t.Fatalf("executeQuery: %v", err)
	}
	if len(cols) != 2 {
		t.Fatalf("expected 2 cols, got %d", len(cols))
	}
	if len(rows) != 1 {
		t.Fatalf("expected 1 row, got %d", len(rows))
	}
	fmt.Printf("basic query: cols=%v row=%v\n", cols, rows[0])
}

func TestExecuteQueryWithSheetRef(t *testing.T) {
	dataDir := filepath.Join("..", "data", "automerge")
	if _, err := os.Stat(dataDir); os.IsNotExist(err) {
		t.Skip("data/automerge not found, skipping")
	}

	// find a table doc to reference
	docs, err := discoverDocs(dataDir)
	if err != nil {
		t.Fatalf("discoverDocs: %v", err)
	}
	var target docInfo
	for _, d := range docs {
		if d.docType == "table" && d.nCols > 0 && d.nRows > 0 {
			target = d
			break
		}
	}
	if target.id == "" {
		t.Skip("no non-empty table doc found")
	}

	// execute a query referencing this sheet
	query := fmt.Sprintf("SELECT count(*) as cnt FROM @table:%s", target.id)
	cols, rows, err := executeQuery(query, dataDir)
	if err != nil {
		t.Fatalf("executeQuery(%q): %v", query, err)
	}
	if len(cols) != 1 || cols[0].name != "cnt" {
		t.Fatalf("expected 1 col named 'cnt', got %v", cols)
	}
	if len(rows) != 1 {
		t.Fatalf("expected 1 row, got %d", len(rows))
	}
	fmt.Printf("sheet ref query on @table:%s: result=%v\n", target.id, rows[0])
}

func TestCreateAndDeleteDoc(t *testing.T) {
	tmpDir := t.TempDir()

	info, err := createDoc(tmpDir, "table")
	if err != nil {
		t.Fatalf("createDoc table: %v", err)
	}
	if info.docType != "table" || info.nCols != 1 {
		t.Fatalf("unexpected table info: type=%s cols=%d", info.docType, info.nCols)
	}

	doc, _, err := loadDoc(info.path)
	if err != nil {
		t.Fatalf("loadDoc: %v", err)
	}
	cols, rows, err := readTable(doc)
	if err != nil {
		t.Fatalf("readTable: %v", err)
	}
	if len(cols) != 1 || cols[0].name != "a" {
		t.Fatalf("expected 1 col named 'a', got %v", cols)
	}
	if len(rows) != 0 {
		t.Fatalf("expected 0 rows, got %d", len(rows))
	}

	// create query
	qInfo, err := createDoc(tmpDir, "query")
	if err != nil {
		t.Fatalf("createDoc query: %v", err)
	}
	qDoc, _, err := loadDoc(qInfo.path)
	if err != nil {
		t.Fatalf("loadDoc query: %v", err)
	}
	code, lang, _, _, err := readQueryDoc(qDoc)
	if err != nil {
		t.Fatalf("readQueryDoc: %v", err)
	}
	if code != "select 1" || lang != "sql" {
		t.Fatalf("expected code='select 1' lang='sql', got code=%q lang=%q", code, lang)
	}

	// delete
	if err := deleteDocDir(info.path); err != nil {
		t.Fatalf("deleteDocDir: %v", err)
	}
	if _, err := os.Stat(info.path); !os.IsNotExist(err) {
		t.Fatal("expected path to not exist after delete")
	}
}

func TestCreateDemoTable(t *testing.T) {
	tmpDir := t.TempDir()

	info, err := createDemoTable(tmpDir)
	if err != nil {
		t.Fatalf("createDemoTable: %v", err)
	}
	if info.nCols != 3 || info.nRows != 9 {
		t.Fatalf("unexpected demo: %d cols, %d rows", info.nCols, info.nRows)
	}

	doc, _, err := loadDoc(info.path)
	if err != nil {
		t.Fatalf("loadDoc: %v", err)
	}
	cols, rows, err := readTable(doc)
	if err != nil {
		t.Fatalf("readTable: %v", err)
	}
	if len(cols) != 3 {
		t.Fatalf("expected 3 cols, got %d", len(cols))
	}
	if cols[0].name != "key" || cols[1].name != "value" || cols[2].name != "notes" {
		t.Fatalf("unexpected col names: %v", cols)
	}
	if len(rows) != 9 {
		t.Fatalf("expected 9 rows, got %d", len(rows))
	}
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
