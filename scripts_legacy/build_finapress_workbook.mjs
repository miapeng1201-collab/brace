import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, "..");
const inputJson = path.join(root, "Generated outputs", "finapress", "finapress_processed_payload.json");
const outputDir = path.join(root, "processed_data", "03_finapress");
const outputPath = path.join(outputDir, "Finapress_processed_clean.xlsx");

const payload = JSON.parse(await fs.readFile(inputJson, "utf8"));

function columnName(indexZeroBased) {
  let n = indexZeroBased + 1;
  let out = "";
  while (n > 0) {
    const rem = (n - 1) % 26;
    out = String.fromCharCode(65 + rem) + out;
    n = Math.floor((n - 1) / 26);
  }
  return out;
}

function sheetMatrix(records) {
  if (!records.length) return [[]];
  const headers = Object.keys(records[0]);
  const rows = records.map((row) =>
    headers.map((header) => {
      const value = row[header] ?? null;
      if (header === "Date" && typeof value === "string") {
        const [year, month, day] = value.split("-").map(Number);
        return new Date(Date.UTC(year, month - 1, day, 12, 0, 0));
      }
      return value;
    }),
  );
  return [headers, ...rows];
}

function styleSheet(sheet, rowCount, colCount) {
  if (!rowCount || !colCount) return;
  const lastCol = columnName(colCount - 1);
  const used = sheet.getRange(`A1:${lastCol}${rowCount}`);
  const header = sheet.getRange(`A1:${lastCol}1`);
  header.format.fill = { color: "#1F4E5F" };
  header.format.font = { color: "#FFFFFF", bold: true };
  header.format.wrapText = true;
  used.format.font = { name: "Aptos", size: 10 };
  used.format.borders = { preset: "insideHorizontal", style: "thin", color: "#E6EAF0" };
  sheet.freezePanes.freezeRows(1);
  const sizingRange = rowCount > 2000 ? sheet.getRange(`A1:${lastCol}200`) : used;
  sizingRange.format.autofitColumns();
  sizingRange.format.autofitRows();
}

function setNumberFormats(sheet, headers, rowCount) {
  if (rowCount < 2) return;
  headers.forEach((header, index) => {
    const col = columnName(index);
    const range = sheet.getRange(`${col}2:${col}${rowCount}`);
    if (header === "Date") range.setNumberFormat("yyyy-mm-dd");
    if (["Nr", "Day2", "Stage_day", "Records", "Subjects", "N", "Missing"].includes(header)) {
      range.setNumberFormat("#,##0");
    }
    if (!["Date", "Subject", "ID", "Group", "Stage", "Phase", "Campaign", "Metric", "Field", "Value"].includes(header)) {
      range.setNumberFormat("0.00");
    }
  });
}

function addSheet(workbook, name, records) {
  const sheet = workbook.worksheets.add(name);
  const matrix = sheetMatrix(records);
  sheet.getRangeByIndexes(0, 0, matrix.length, matrix[0].length).values = matrix;
  styleSheet(sheet, matrix.length, matrix[0].length);
  setNumberFormats(sheet, matrix[0], matrix.length);
  return { sheet, rowCount: matrix.length, colCount: matrix[0].length };
}

const workbook = Workbook.create();
const order = [
  "README",
  "Fina_Avg_Clean",
  "Fina_Detail_Clean",
  "Metric_Summary",
  "Coverage",
  "Stage_Counts",
];

for (const sheetName of order) {
  addSheet(workbook, sheetName, payload.sheets[sheetName]);
}

const readme = workbook.worksheets.getItem("README");
readme.getRange("A1:B1").format.fill = { color: "#284B63" };
readme.getRange("A:B").format.columnWidth = 38;
readme.getRange("B:B").format.columnWidth = 110;
readme.getRange("A1:B5").format.wrapText = true;

await fs.mkdir(outputDir, { recursive: true });

const readmePreview = await workbook.render({ sheetName: "README", autoCrop: "all", scale: 1, format: "png" });
await fs.writeFile(
  path.join(root, "Generated outputs", "finapress", "finapress_readme_preview.png"),
  new Uint8Array(await readmePreview.arrayBuffer()),
);

const inspect = await workbook.inspect({
  kind: "workbook,sheet,table",
  maxChars: 4000,
  tableMaxRows: 4,
  tableMaxCols: 8,
});
console.log(inspect.ndjson);

const errors = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 50 },
  summary: "formula error scan",
});
console.log(errors.ndjson);

const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(outputPath);
console.log(outputPath);
