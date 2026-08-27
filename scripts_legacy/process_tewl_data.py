from __future__ import annotations

import csv
import json
import os
import re
import statistics
import zipfile
from collections import Counter, defaultdict
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any
from xml.etree import ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
SOURCE_DIR = ROOT / "Original_data" / "04 - TEWAMETER"
OUTPUT_DIR = ROOT / "processed_data" / "04_tewameter"
STAGE_DAY_LOOKUP = ROOT / "Generated outputs" / "BRACE_subject_stage_day_dates.xlsx"
RIK_SCORE_WORKBOOK = Path("/Users/ree/Desktop/TEWL_merged_data.xlsx")
RIK_SCORE_SHEET = "TEWL_rik_check (2)"

NS = {
    "m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
}

LOOKUP_COLUMNS = [
    "group",
    "stage",
    "day_number",
    "stage_date",
    "Rik_score",
]

SUMMARY_COLUMNS = [
    "cohort",
    "subject",
    *LOOKUP_COLUMNS,
    "visit",
    "source_file",
    "file_datetime",
    "session_datetime",
    "take_datetime",
    "probe",
    "probe_sn",
    "operator",
    "study",
    "take_label",
    "order_of_take",
    "number_of_measurements",
    "ambient_temperature_take_c",
    "ambient_relative_humidity_take_pct",
    "ambient_air_pressure_take_hpa",
    "stop_event",
    "stop_time_threshold",
    "avg_tewl_robust_g_m2_h",
    "avg_tewl_spread_g_m2_h",
    "avg_tewl_uncertainty_g_m2_h",
    "avg_hl_hd_robust_w_m2",
    "avg_hl_ec_robust_w_m2",
    "avg_hl_total_robust_w_m2",
    "avg_ch2o_ambience_robust_g_m3",
    "avg_ch2o_skin_robust_g_m3",
    "avg_temperature_ambience_robust_c",
    "avg_temperature_skin_robust_c",
    "avg_rh_ambience_robust_pct",
    "avg_rh_skin_robust_pct",
]

MEASUREMENT_COLUMNS = [
    "cohort",
    "subject",
    *LOOKUP_COLUMNS,
    "visit",
    "measurement_order",
    "source_file",
    "measurement_datetime",
    "ambient_temperature_measurement_c",
    "ambient_relative_humidity_measurement_pct",
    "ambient_air_pressure_measurement_hpa",
    "duration_s",
    "tewl_robust_g_m2_h",
    "tewl_spread_g_m2_h",
    "tewl_uncertainty_g_m2_h",
    "hl_hd_robust_w_m2",
    "hl_ec_robust_w_m2",
    "hl_total_robust_w_m2",
    "ch2o_ambience_robust_g_m3",
    "ch2o_skin_robust_g_m3",
    "temperature_ambience_robust_c",
    "temperature_skin_last_c",
    "rh_ambience_robust_pct",
    "rh_skin_robust_pct",
]

RAW_WINDOW_MEASUREMENT_COLUMNS = [
    "cohort",
    "subject",
    *LOOKUP_COLUMNS,
    "visit",
    "measurement_number",
    "measurement_label",
    "source_file",
    "window_start_s",
    "window_end_s",
    "window_n_points",
    "window_tewl_mean_g_m2_h",
]

RAW_WINDOW_SUMMARY_COLUMNS = SUMMARY_COLUMNS + [
    "raw_window_start_s",
    "raw_window_end_s",
    "raw_window_measurement_count",
    "raw_window_total_points",
    "raw_window_tewl_mean_of_measurements_g_m2_h",
    "measurement_1",
    "measurement_2",
    "measurement_3",
    "measurement_4",
    "measurement_5",
    "measurement_6",
    "measurement_7",
    "measurement_8",
    "measurement_9",
    "measurement_10",
]

SUMMARY_MAP = {
    "Time & Date Session": "session_datetime",
    "Time & Date Take": "take_datetime",
    "Probe": "probe",
    "Probe SN": "probe_sn",
    "Operator": "operator",
    "Study": "study",
    "Subject": "subject",
    "T": "take_label",
    "Order of Take": "order_of_take",
    "Number of Measurements": "number_of_measurements",
    "Ambient Temperature Take [C°]": "ambient_temperature_take_c",
    "Ambient Relative Humidity Take [%]": "ambient_relative_humidity_take_pct",
    "Ambient Air Pressure Take [hPa]": "ambient_air_pressure_take_hpa",
    "Stop Event": "stop_event",
    "Stop Time Threshold": "stop_time_threshold",
    "Avg TEWL Robust [g/m²/h]": "avg_tewl_robust_g_m2_h",
    "Avg TEWL Spread [g/m²/h]": "avg_tewl_spread_g_m2_h",
    "Avg TEWL Uncertainty [g/m²/h]": "avg_tewl_uncertainty_g_m2_h",
    "Avg HL HD Robust [W/m²]": "avg_hl_hd_robust_w_m2",
    "Avg HL EC Robust [W/m²]": "avg_hl_ec_robust_w_m2",
    "Avg HL Total Robust [W/m²]": "avg_hl_total_robust_w_m2",
    "Avg cH₂O Ambience Robust [g/m³]": "avg_ch2o_ambience_robust_g_m3",
    "Avg cH₂O Skin Robust [g/m³]": "avg_ch2o_skin_robust_g_m3",
    "Avg Temperature Ambience Robust [C°]": "avg_temperature_ambience_robust_c",
    "Avg Temperature Skin Robust [C°]": "avg_temperature_skin_robust_c",
    "Avg RH Ambience Robust [%]": "avg_rh_ambience_robust_pct",
    "Avg RH Skin Robust [%]": "avg_rh_skin_robust_pct",
}

MEASUREMENT_MAP = {
    "Order of Measurement": "measurement_order",
    "Time & Date Measurement": "measurement_datetime",
    "Ambient Temperature Measurement [C°]": "ambient_temperature_measurement_c",
    "Ambient Relative Humidity Measurement [%]": "ambient_relative_humidity_measurement_pct",
    "Ambient Air Pressure Measurement [hPa]": "ambient_air_pressure_measurement_hpa",
    "Duration [s]": "duration_s",
    "TEWL Robust [g/m²/h]": "tewl_robust_g_m2_h",
    "TEWL Spread [g/m²/h]": "tewl_spread_g_m2_h",
    "TEWL Uncertainty [g/m²/h]": "tewl_uncertainty_g_m2_h",
    "HL HD Robust [W/m²]": "hl_hd_robust_w_m2",
    "HL EC Robust [W/m²]": "hl_ec_robust_w_m2",
    "HL Total Robust [W/m²]": "hl_total_robust_w_m2",
    "cH₂O Ambience Robust [g/m³]": "ch2o_ambience_robust_g_m3",
    "cH₂O Skin Robust [g/m³]": "ch2o_skin_robust_g_m3",
    "Temperature Ambience Robust [C°]": "temperature_ambience_robust_c",
    "Temperature Skin Last [C°]": "temperature_skin_last_c",
    "RH Ambience Robust [%]": "rh_ambience_robust_pct",
    "RH Skin Robust [%]": "rh_skin_robust_pct",
}


def col_number(ref: str) -> int:
    letters = "".join(ch for ch in ref if ch.isalpha())
    number = 0
    for char in letters:
        number = number * 26 + ord(char.upper()) - 64
    return number


def excel_datetime(value: Any) -> str:
    if value in (None, ""):
        return ""
    try:
        serial = float(value)
    except (TypeError, ValueError):
        return str(value)
    base = datetime(1899, 12, 30)
    dt = base + timedelta(days=serial)
    return dt.replace(microsecond=0).isoformat(sep=" ")


def number_or_text(value: Any) -> Any:
    if value in (None, ""):
        return ""
    if isinstance(value, (int, float)):
        return value
    text = str(value)
    try:
        num = float(text)
    except ValueError:
        return text
    if num.is_integer():
        return int(num)
    return num


def iso_date(value: Any) -> str:
    if value in (None, ""):
        return ""
    if isinstance(value, datetime):
        return value.date().isoformat()
    text = str(value)
    if len(text) >= 10 and re.fullmatch(r"\d{4}-\d{2}-\d{2}.*", text):
        return text[:10]
    return text


def load_stage_day_lookup() -> dict[tuple[str, str, str], dict[str, Any]]:
    from openpyxl import load_workbook

    wb = load_workbook(STAGE_DAY_LOOKUP, read_only=True, data_only=True)
    ws = wb["Subject stage dates"]
    rows = ws.iter_rows(values_only=True)
    headers = next(rows)
    header_index = {header: index for index, header in enumerate(headers)}
    lookup = {}
    for row in rows:
        cohort = row[header_index["cohort"]]
        subject = row[header_index["subject"]]
        check_date = iso_date(row[header_index["check_date"]])
        if not cohort or not subject or not check_date:
            continue
        key = (str(cohort), str(subject), check_date)
        lookup[key] = {
            "group": row[header_index["group_lookup"]] or "",
            "stage": row[header_index["stage"]] or "",
            "day_number": row[header_index["day_number_lookup"]] or "",
            "stage_date": check_date,
        }
    return lookup


def load_rik_score_lookup() -> dict[str, Any]:
    from openpyxl import load_workbook

    wb = load_workbook(RIK_SCORE_WORKBOOK, read_only=True, data_only=True)
    ws = wb[RIK_SCORE_SHEET]
    rows = ws.iter_rows(values_only=True)
    headers = next(rows)
    header_index = {header: index for index, header in enumerate(headers)}
    file_idx = header_index["File"]
    score_idx = header_index["Rik_score"]
    lookup = {}
    for row in rows:
        file_name = row[file_idx]
        if not file_name:
            continue
        lookup[str(file_name)] = row[score_idx] if row[score_idx] is not None else ""
    return lookup


def row_date(row: dict[str, Any]) -> str:
    for key in ("take_datetime", "file_datetime", "session_datetime"):
        value = row.get(key)
        if value:
            return iso_date(value)
    return ""


def attach_stage_day_lookup(
    summary_rows: list[dict[str, Any]],
    measurement_rows: list[dict[str, Any]],
    raw_window_measurement_rows: list[dict[str, Any]],
) -> dict[str, Any]:
    lookup = load_stage_day_lookup()
    rik_lookup = load_rik_score_lookup()
    by_file = {}
    unmatched = []
    rik_unmatched = []
    for row in summary_rows:
        key = (str(row["cohort"]), str(row["subject"]), row_date(row))
        match = lookup.get(key, {})
        for column in ("group", "stage", "day_number", "stage_date"):
            row[column] = match.get(column, "")
        file_name = Path(row["source_file"]).name
        row["Rik_score"] = rik_lookup.get(file_name, "")
        by_file[row["source_file"]] = {column: row.get(column, "") for column in LOOKUP_COLUMNS}
        if not match and row.get("subject") != "test001":
            unmatched.append(
                {
                    "cohort": row["cohort"],
                    "subject": row["subject"],
                    "visit": row["visit"],
                    "date": key[2],
                    "source_file": row["source_file"],
                }
            )
        if file_name not in rik_lookup and row.get("subject") != "test001":
            rik_unmatched.append(
                {
                    "cohort": row["cohort"],
                    "subject": row["subject"],
                    "visit": row["visit"],
                    "file": file_name,
                    "source_file": row["source_file"],
                }
            )

    for rows in (measurement_rows, raw_window_measurement_rows):
        for row in rows:
            match = by_file.get(row["source_file"], {})
            for column in LOOKUP_COLUMNS:
                row[column] = match.get(column, "")

    return {
        "lookup_file": str(STAGE_DAY_LOOKUP.relative_to(ROOT)),
        "lookup_keys": len(lookup),
        "unmatched_summary_rows_excluding_test001": unmatched,
        "rik_score_file": str(RIK_SCORE_WORKBOOK),
        "rik_score_sheet": RIK_SCORE_SHEET,
        "rik_score_keys": len(rik_lookup),
        "rik_score_unmatched_summary_rows_excluding_test001": rik_unmatched,
    }


def read_shared_strings(zf: zipfile.ZipFile) -> list[str]:
    try:
        root = ET.fromstring(zf.read("xl/sharedStrings.xml"))
    except KeyError:
        return []
    strings = []
    for si in root.findall("m:si", NS):
        strings.append("".join(t.text or "" for t in si.findall(".//m:t", NS)))
    return strings


def sheet_paths(zf: zipfile.ZipFile) -> dict[str, str]:
    workbook = ET.fromstring(zf.read("xl/workbook.xml"))
    rels = ET.fromstring(zf.read("xl/_rels/workbook.xml.rels"))
    targets = {rel.attrib["Id"]: rel.attrib["Target"] for rel in rels}
    paths = {}
    for sheet in workbook.findall("m:sheets/m:sheet", NS):
        rid = sheet.attrib[f"{{{NS['r']}}}id"]
        target = targets[rid].lstrip("/")
        paths[sheet.attrib["name"]] = f"xl/{target}"
    return paths


def read_sheet(zf: zipfile.ZipFile, path: str, shared_strings: list[str]) -> list[list[Any]]:
    root = ET.fromstring(zf.read(path))
    table = []
    for row in root.findall(".//m:sheetData/m:row", NS):
        values: dict[int, Any] = {}
        for cell in row.findall("m:c", NS):
            ref = cell.attrib["r"]
            idx = col_number(ref)
            value = cell.find("m:v", NS)
            text: Any = "" if value is None else value.text
            if cell.attrib.get("t") == "s" and text != "":
                text = shared_strings[int(text)]
            values[idx] = text
        width = max(values) if values else 0
        table.append([values.get(i, "") for i in range(1, width + 1)])
    return table


def parse_raw_window_measurements(
    raw_rows: list[list[Any]],
    base: dict[str, Any],
    window_start: float = 35.0,
    window_end: float = 55.0,
) -> list[dict[str, Any]]:
    if len(raw_rows) < 3:
        return []

    label_row = raw_rows[0]
    header_row = raw_rows[1]
    starts = []
    for index, value in enumerate(label_row):
        if isinstance(value, str) and re.fullmatch(r"Measurement_\d+", value):
            starts.append((index, value))

    results = []
    for pos, (start_col, label) in enumerate(starts):
        end_col = starts[pos + 1][0] if pos + 1 < len(starts) else max(len(row) for row in raw_rows)
        headers = header_row[start_col:end_col]
        try:
            elapsed_idx = start_col + headers.index("Elapsed Time [s]")
            tewl_idx = start_col + headers.index("TEWL [g/m²/h]")
        except ValueError:
            continue

        values = []
        for row in raw_rows[2:]:
            elapsed = row[elapsed_idx] if elapsed_idx < len(row) else ""
            tewl = row[tewl_idx] if tewl_idx < len(row) else ""
            try:
                elapsed_value = float(elapsed)
                tewl_value = float(tewl)
            except (TypeError, ValueError):
                continue
            if window_start <= elapsed_value <= window_end:
                values.append(tewl_value)

        measurement_number_match = re.search(r"(\d+)$", label)
        measurement_number = int(measurement_number_match.group(1)) if measurement_number_match else ""
        results.append(
            {
                "cohort": base["cohort"],
                "subject": base["subject"],
                "visit": base["visit"],
                "measurement_number": measurement_number,
                "measurement_label": label,
                "source_file": base["source_file"],
                "window_start_s": window_start,
                "window_end_s": window_end,
                "window_n_points": len(values),
                "window_tewl_mean_g_m2_h": statistics.fmean(values) if values else "",
            }
        )
    return results


def parse_file(path: Path) -> tuple[dict[str, Any], list[dict[str, Any]], list[dict[str, Any]]]:
    rel = path.relative_to(ROOT)
    parts = path.parts
    cohort = next(part for part in parts if part in {"C1", "C2"})
    idx = parts.index("Tewameter")
    subject = parts[idx + 1]
    visit = parts[idx + 2]
    file_datetime = datetime.strptime(path.name[:19], "%Y_%m_%d_%H_%M_%S").isoformat(sep=" ")

    with zipfile.ZipFile(path) as zf:
        shared_strings = read_shared_strings(zf)
        paths = sheet_paths(zf)
        summary_rows = read_sheet(zf, paths["Summary"], shared_strings)
        measurement_rows = read_sheet(zf, paths["Measurements"], shared_strings)
        raw_rows = read_sheet(zf, paths["Raw Data"], shared_strings)

    summary = {
        "cohort": cohort,
        "subject": subject,
        "visit": int(visit) if str(visit).isdigit() else visit,
        "source_file": str(rel),
        "file_datetime": file_datetime,
    }
    headers = summary_rows[0]
    values = summary_rows[1] if len(summary_rows) > 1 else []
    for header, value in zip(headers, values):
        out_key = SUMMARY_MAP.get(header)
        if not out_key:
            continue
        if out_key.endswith("_datetime"):
            summary[out_key] = excel_datetime(value)
        else:
            summary[out_key] = number_or_text(value)
    summary["subject"] = summary.get("subject") or subject
    raw_window_measurements = parse_raw_window_measurements(raw_rows, summary)

    measurements = []
    headers = measurement_rows[0] if measurement_rows else []
    for row in measurement_rows[1:]:
        if not any(cell not in ("", None) for cell in row):
            continue
        item = {
            "cohort": cohort,
            "subject": summary["subject"],
            "visit": summary["visit"],
            "source_file": str(rel),
        }
        for header, value in zip(headers, row):
            out_key = MEASUREMENT_MAP.get(header)
            if not out_key:
                continue
            if out_key.endswith("_datetime"):
                item[out_key] = excel_datetime(value)
            else:
                item[out_key] = number_or_text(value)
        measurements.append(item)
    return summary, measurements, raw_window_measurements


def write_csv(path: Path, rows: list[dict[str, Any]], columns: list[str]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=columns, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def build_wide(summary_rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    by_key: dict[tuple[str, str], dict[str, Any]] = defaultdict(dict)
    visits = sorted({int(row["visit"]) for row in summary_rows if isinstance(row.get("visit"), int)})
    for row in summary_rows:
        key = (row["cohort"], row["subject"])
        by_key[key]["cohort"] = row["cohort"]
        by_key[key]["subject"] = row["subject"]
        visit = row["visit"]
        by_key[key][f"visit_{visit}_tewl"] = row.get("avg_tewl_robust_g_m2_h", "")
        by_key[key][f"visit_{visit}_take_datetime"] = row.get("take_datetime", "")
    columns = ["cohort", "subject"]
    for visit in visits:
        columns.extend([f"visit_{visit}_tewl", f"visit_{visit}_take_datetime"])
    return [{col: row.get(col, "") for col in columns} for row in sorted(by_key.values(), key=lambda r: (r["cohort"], r["subject"]))], columns


def attach_raw_window_summary(
    summary_rows: list[dict[str, Any]],
    raw_window_rows: list[dict[str, Any]],
) -> list[dict[str, Any]]:
    by_file = defaultdict(list)
    for row in raw_window_rows:
        by_file[row["source_file"]].append(row)

    out = []
    for row in summary_rows:
        measurements = sorted(by_file.get(row["source_file"], []), key=lambda item: item["measurement_number"])
        means = [float(item["window_tewl_mean_g_m2_h"]) for item in measurements if item["window_tewl_mean_g_m2_h"] != ""]
        combined = dict(row)
        measurement_mean_columns = {
            f"measurement_{idx}": ""
            for idx in range(1, 11)
        }
        for item in measurements:
            number = item.get("measurement_number")
            if isinstance(number, int) and 1 <= number <= 10:
                measurement_mean_columns[f"measurement_{number}"] = item["window_tewl_mean_g_m2_h"]
        combined.update(
            {
                "raw_window_start_s": 35,
                "raw_window_end_s": 55,
                "raw_window_measurement_count": len(measurements),
                "raw_window_total_points": sum(int(item["window_n_points"]) for item in measurements),
                "raw_window_tewl_mean_of_measurements_g_m2_h": statistics.fmean(means) if means else "",
                **measurement_mean_columns,
            }
        )
        out.append(combined)
    return out


def is_clean_summary_row(row: dict[str, Any]) -> bool:
    if row.get("subject") == "test001":
        return False
    if row.get("avg_tewl_robust_g_m2_h") in ("", None):
        return False
    return True


def main() -> None:
    files = sorted(
        p
        for cohort in ("C1", "C2")
        for p in (SOURCE_DIR / cohort / "Tewameter").glob("*/*/*.xlsx")
        if not p.name.startswith(".~") and not p.name.startswith("Overview")
    )

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    summary_rows: list[dict[str, Any]] = []
    measurement_rows: list[dict[str, Any]] = []
    raw_window_measurement_rows: list[dict[str, Any]] = []
    errors = []

    for path in files:
        try:
            summary, measurements, raw_window_measurements = parse_file(path)
        except Exception as exc:  # noqa: BLE001 - report all parse failures in QC.
            errors.append({"source_file": str(path.relative_to(ROOT)), "error": repr(exc)})
            continue
        summary_rows.append({column: summary.get(column, "") for column in SUMMARY_COLUMNS})
        for item in measurements:
            measurement_rows.append({column: item.get(column, "") for column in MEASUREMENT_COLUMNS})
        for item in raw_window_measurements:
            raw_window_measurement_rows.append(
                {column: item.get(column, "") for column in RAW_WINDOW_MEASUREMENT_COLUMNS}
            )

    summary_rows.sort(key=lambda r: (r["cohort"], r["subject"], int(r["visit"]) if isinstance(r["visit"], int) else 999))
    measurement_rows.sort(
        key=lambda r: (
            r["cohort"],
            r["subject"],
            int(r["visit"]) if isinstance(r["visit"], int) else 999,
            int(r["measurement_order"]) if isinstance(r.get("measurement_order"), int) else 999,
        )
    )
    stage_day_qc = attach_stage_day_lookup(summary_rows, measurement_rows, raw_window_measurement_rows)

    wide_rows, wide_columns = build_wide(summary_rows)
    raw_window_summary_rows = attach_raw_window_summary(summary_rows, raw_window_measurement_rows)
    clean_summary_rows = [row for row in summary_rows if is_clean_summary_row(row)]
    clean_files = {row["source_file"] for row in clean_summary_rows}
    clean_measurement_rows = [row for row in measurement_rows if row["source_file"] in clean_files]
    clean_raw_window_measurement_rows = [
        row for row in raw_window_measurement_rows if row["source_file"] in clean_files
    ]
    clean_raw_window_summary_rows = [
        row for row in raw_window_summary_rows if row["source_file"] in clean_files
    ]
    clean_wide_rows, clean_wide_columns = build_wide(clean_summary_rows)

    expected_measurements = sum(int(r["number_of_measurements"] or 0) for r in summary_rows)
    tewl_values = [float(r["avg_tewl_robust_g_m2_h"]) for r in clean_summary_rows if r["avg_tewl_robust_g_m2_h"] != ""]
    duplicate_keys = [
        {"cohort": cohort, "subject": subject, "visit": visit, "count": count}
        for (cohort, subject, visit), count in Counter((r["cohort"], r["subject"], r["visit"]) for r in summary_rows).items()
        if count > 1
    ]
    subjects_by_cohort = defaultdict(set)
    visits_by_cohort = defaultdict(set)
    for row in summary_rows:
        subjects_by_cohort[row["cohort"]].add(row["subject"])
        visits_by_cohort[row["cohort"]].add(row["visit"])
    clean_subjects_by_cohort = defaultdict(set)
    clean_visits_by_cohort_subject = defaultdict(set)
    for row in clean_summary_rows:
        clean_subjects_by_cohort[row["cohort"]].add(row["subject"])
        clean_visits_by_cohort_subject[(row["cohort"], row["subject"])].add(row["visit"])
    missing_visits = []
    expected_visits = set(range(15))
    for (cohort, subject), visits in sorted(clean_visits_by_cohort_subject.items()):
        missing = sorted(expected_visits - {int(visit) for visit in visits})
        if missing:
            missing_visits.append({"cohort": cohort, "subject": subject, "missing_visits": missing})

    qc = {
        "source_dir": str(SOURCE_DIR.relative_to(ROOT)),
        "output_dir": str(OUTPUT_DIR.relative_to(ROOT)),
        "parsed_files": len(summary_rows),
        "clean_summary_rows": len(clean_summary_rows),
        "parse_errors": len(errors),
        "measurement_rows": len(measurement_rows),
        "clean_measurement_rows": len(clean_measurement_rows),
        "raw_window_measurement_rows": len(raw_window_measurement_rows),
        "clean_raw_window_measurement_rows": len(clean_raw_window_measurement_rows),
        "raw_window_summary_rows": len(raw_window_summary_rows),
        "clean_raw_window_summary_rows": len(clean_raw_window_summary_rows),
        "expected_measurement_rows_from_summary": expected_measurements,
        "duplicate_subject_visit_rows": duplicate_keys,
        "subjects_by_cohort": {key: sorted(value) for key, value in subjects_by_cohort.items()},
        "clean_subjects_by_cohort": {key: sorted(value) for key, value in clean_subjects_by_cohort.items()},
        "visits_by_cohort": {key: sorted(value) for key, value in visits_by_cohort.items()},
        "missing_visits_in_clean_data_assuming_0_to_14": missing_visits,
        "cleaning_rules": [
            "Excluded subject test001 from clean outputs.",
            "Excluded rows with missing avg_tewl_robust_g_m2_h from clean outputs.",
        ],
        "raw_data_window_rule": "For each Measurement_N block in Raw Data, average TEWL [g/m²/h] where Elapsed Time [s] is between 35 and 55 inclusive.",
        "stage_day_lookup": stage_day_qc,
        "tewl_summary_stats": {
            "n": len(tewl_values),
            "min": min(tewl_values) if tewl_values else None,
            "max": max(tewl_values) if tewl_values else None,
            "mean": statistics.fmean(tewl_values) if tewl_values else None,
        },
        "errors": errors,
    }

    data_payload = {
        "sheets": {
            "RawWindowSummaryClean": {
                "columns": RAW_WINDOW_SUMMARY_COLUMNS,
                "rows": clean_raw_window_summary_rows,
            },
            "RawWindowMeasurements": {
                "columns": RAW_WINDOW_MEASUREMENT_COLUMNS,
                "rows": clean_raw_window_measurement_rows,
            },
            "SummaryAll": {
                "columns": SUMMARY_COLUMNS,
                "rows": summary_rows,
            },
        },
        "qc": qc,
    }

    if os.environ.get("TEWL_XLSX_ONLY") == "1":
        payload_path = Path(os.environ["TEWL_DATA_JSON"])
        payload_path.parent.mkdir(parents=True, exist_ok=True)
        payload_path.write_text(json.dumps(data_payload, ensure_ascii=False), encoding="utf-8")
        print(json.dumps(qc, indent=2, ensure_ascii=False))
        return

    write_csv(OUTPUT_DIR / "tewl_summary_long.csv", summary_rows, SUMMARY_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_measurements_long.csv", measurement_rows, MEASUREMENT_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_summary_wide.csv", wide_rows, wide_columns)
    write_csv(OUTPUT_DIR / "tewl_raw_window_measurements.csv", raw_window_measurement_rows, RAW_WINDOW_MEASUREMENT_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_raw_window_summary.csv", raw_window_summary_rows, RAW_WINDOW_SUMMARY_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_summary_clean.csv", clean_summary_rows, SUMMARY_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_measurements_clean.csv", clean_measurement_rows, MEASUREMENT_COLUMNS)
    write_csv(OUTPUT_DIR / "tewl_summary_wide_clean.csv", clean_wide_rows, clean_wide_columns)
    write_csv(
        OUTPUT_DIR / "tewl_raw_window_measurements_clean.csv",
        clean_raw_window_measurement_rows,
        RAW_WINDOW_MEASUREMENT_COLUMNS,
    )
    write_csv(
        OUTPUT_DIR / "tewl_raw_window_summary_clean.csv",
        clean_raw_window_summary_rows,
        RAW_WINDOW_SUMMARY_COLUMNS,
    )
    (OUTPUT_DIR / "tewl_qc_report.json").write_text(json.dumps(qc, indent=2, ensure_ascii=False), encoding="utf-8")

    print(json.dumps(qc, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
