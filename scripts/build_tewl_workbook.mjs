import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = process.env.WORKSPACE_ROOT
  ? path.resolve(process.env.WORKSPACE_ROOT)
  : path.resolve(__dirname, "..");
const outputDir = path.join(root, "Processed data", "04 - TEWAMETER");
const workbookPath = path.join(outputDir, "TEWL_processed_raw_window.xlsx");
const dataJsonPath = process.env.TEWL_DATA_JSON || "";

function parseCsv(text) {
  const rows = [];
  let row = [];
  let cell = "";
  let inQuotes = false;

  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    const next = text[i + 1];
    if (inQuotes) {
      if (char === '"' && next === '"') {
        cell += '"';
        i += 1;
      } else if (char === '"') {
        inQuotes = false;
      } else {
        cell += char;
      }
    } else if (char === '"') {
      inQuotes = true;
    } else if (char === ",") {
      row.push(cell);
      cell = "";
    } else if (char === "\n") {
      row.push(cell);
      rows.push(row);
      row = [];
      cell = "";
    } else if (char !== "\r") {
      cell += char;
    }
  }
  if (cell.length || row.length) {
    row.push(cell);
    rows.push(row);
  }
  return rows;
}

function coerce(value) {
  if (value === "") return "";
  if (/^-?\d+(\.\d+)?$/.test(value)) return Number(value);
  return value;
}

async function readCsvRows(filename) {
  const text = await fs.readFile(path.join(outputDir, filename), "utf8");
  return parseCsv(text).map((row) => row.map(coerce));
}

function rowsFromRecords(columns, records) {
  return [
    columns,
    ...records.map((record) => columns.map((column) => record[column] ?? "")),
  ];
}

function columnName(index) {
  let n = index + 1;
  let name = "";
  while (n > 0) {
    const mod = (n - 1) % 26;
    name = String.fromCharCode(65 + mod) + name;
    n = Math.floor((n - mod) / 26);
  }
  return name;
}

function writeSheet(workbook, sheetName, rows) {
  const sheet = workbook.worksheets.add(sheetName);
  if (!rows.length) return;
  const width = Math.max(...rows.map((row) => row.length));
  const paddedRows = rows.map((row) => {
    const padded = row.slice();
    while (padded.length < width) padded.push("");
    return padded;
  });
  const endCell = `${columnName(width - 1)}${paddedRows.length}`;
  sheet.getRange(`A1:${endCell}`).values = paddedRows;
}

async function main() {
  const workbook = Workbook.create();

  let qc;
  if (dataJsonPath) {
    const payload = JSON.parse(await fs.readFile(dataJsonPath, "utf8"));
    for (const [sheetName, sheet] of Object.entries(payload.sheets)) {
      writeSheet(workbook, sheetName, rowsFromRecords(sheet.columns, sheet.rows));
    }
    qc = payload.qc;
  } else {
    writeSheet(
      workbook,
      "RawWindowSummaryClean",
      await readCsvRows("tewl_raw_window_summary_clean.csv"),
    );
    writeSheet(
      workbook,
      "RawWindowMeasurements",
      await readCsvRows("tewl_raw_window_measurements_clean.csv"),
    );
    writeSheet(
      workbook,
      "SummaryAll",
      await readCsvRows("tewl_summary_long.csv"),
    );
    qc = JSON.parse(await fs.readFile(path.join(outputDir, "tewl_qc_report.json"), "utf8"));
  }
  const qcRows = [
    ["metric", "value"],
    ["raw_data_window_rule", qc.raw_data_window_rule],
    ["parsed_files", qc.parsed_files],
    ["clean_summary_rows", qc.clean_summary_rows],
    ["clean_raw_window_measurement_rows", qc.clean_raw_window_measurement_rows],
    ["parse_errors", qc.parse_errors],
    ["duplicate_subject_visit_rows", JSON.stringify(qc.duplicate_subject_visit_rows)],
    ["missing_visits_in_clean_data_assuming_0_to_14", JSON.stringify(qc.missing_visits_in_clean_data_assuming_0_to_14)],
    ["cleaning_rules", qc.cleaning_rules.join(" | ")],
  ];
  writeSheet(workbook, "QC", qcRows);

  await fs.mkdir(outputDir, { recursive: true });
  const output = await SpreadsheetFile.exportXlsx(workbook);
  await output.save(workbookPath);
  console.log(workbookPath);
}

await main();
