from __future__ import annotations

import json
import re
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
SOURCE = Path("/Users/ree/Documents/New project/data_clean/Fina_cleaned.csv")
OUT_DIR = ROOT / "Generated outputs" / "finapress"
OUT_JSON = OUT_DIR / "fina_cleaned_csv_payload.json"

ID_COLS = {"subject", "group", "day", "stage", "date", "day_real", "stage_real"}


def excel_serial_to_iso(value: object) -> str | None:
    if pd.isna(value):
        return None
    date = pd.to_datetime(value, unit="D", origin="1899-12-30", errors="coerce")
    if pd.isna(date):
        return None
    return date.strftime("%Y-%m-%d")


def parse_stage(value: object) -> tuple[str | None, float | None]:
    if pd.isna(value):
        return None, None
    text = str(value).strip()
    if not text:
        return None, None
    match = re.match(r"^([A-Za-z]+)([-+]?\d+)?$", text)
    if not match:
        return text, None
    phase = match.group(1).upper()
    stage_day = float(match.group(2)) if match.group(2) is not None else None
    return phase, stage_day


def records_for_json(df: pd.DataFrame) -> list[dict[str, object]]:
    safe = df.astype(object).where(pd.notna(df), None)
    return safe.to_dict(orient="records")


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    raw = pd.read_csv(SOURCE)
    df = raw.copy()

    empty_cols = [col for col in df.columns if df[col].isna().all()]
    df = df.drop(columns=empty_cols)

    df["date_iso"] = df["date"].map(excel_serial_to_iso)
    df["stage_clean"] = df["stage_real"].where(df["stage_real"].notna(), df["stage"])

    parsed = df["stage_clean"].map(parse_stage)
    df["phase"] = [item[0] for item in parsed]
    df["stage_day"] = [item[1] for item in parsed]

    metric_cols = [col for col in df.columns if col not in ID_COLS | {"date_iso", "stage_clean", "phase", "stage_day"}]
    df["metric_present_n"] = df[metric_cols].notna().sum(axis=1)
    df["measurement_present"] = df["metric_present_n"] > 0
    df["missing_metric_n"] = df[metric_cols].isna().sum(axis=1)

    ordered_cols = [
        "subject",
        "group",
        "day",
        "stage",
        "stage_clean",
        "phase",
        "stage_day",
        "date_iso",
        "day_real",
        "measurement_present",
        "metric_present_n",
        "missing_metric_n",
    ] + metric_cols
    df = df[ordered_cols]

    complete_cases = df[df["measurement_present"]].copy()
    missing_rows = df[~df["measurement_present"]].copy()

    summary_rows = []
    for metric in metric_cols:
        values = pd.to_numeric(df[metric], errors="coerce")
        summary_rows.append(
            {
                "metric": metric,
                "n": int(values.notna().sum()),
                "missing": int(values.isna().sum()),
                "mean": float(values.mean()) if values.notna().any() else None,
                "sd": float(values.std()) if values.notna().sum() > 1 else None,
                "min": float(values.min()) if values.notna().any() else None,
                "max": float(values.max()) if values.notna().any() else None,
            }
        )
    metric_summary = pd.DataFrame(summary_rows)

    coverage = (
        df.groupby(["group", "phase"], dropna=False)
        .agg(
            rows=("subject", "count"),
            subjects=("subject", "nunique"),
            measurements_present=("measurement_present", "sum"),
        )
        .reset_index()
    )
    coverage["measurements_missing"] = coverage["rows"] - coverage["measurements_present"]
    coverage["present_percent"] = coverage["measurements_present"] / coverage["rows"]

    qa = {
        "source_file": str(SOURCE),
        "raw_rows": int(len(raw)),
        "raw_columns": int(raw.shape[1]),
        "dropped_empty_columns": ", ".join(empty_cols) if empty_cols else "",
        "clean_rows": int(len(df)),
        "clean_columns": int(df.shape[1]),
        "subjects": int(df["subject"].nunique()),
        "measurements_present_rows": int(df["measurement_present"].sum()),
        "measurements_missing_rows": int((~df["measurement_present"]).sum()),
    }

    payload = {
        "qa": qa,
        "sheets": {
            "README": [
                {"field": "Source", "value": qa["source_file"]},
                {"field": "Rows", "value": qa["raw_rows"]},
                {"field": "Dropped empty columns", "value": qa["dropped_empty_columns"]},
                {"field": "Date handling", "value": "Converted Excel serial date to date_iso; source date serial retained only in source CSV."},
                {"field": "Stage handling", "value": "stage_clean uses stage_real when present, otherwise stage; phase and stage_day parsed from stage_clean."},
                {"field": "Missing handling", "value": "Clean_All keeps all rows; Complete_Cases keeps rows with at least one metric present; Missing_Rows lists rows with all metrics missing."},
            ],
            "Clean_All": records_for_json(df),
            "Complete_Cases": records_for_json(complete_cases),
            "Missing_Rows": records_for_json(missing_rows),
            "Metric_Summary": records_for_json(metric_summary),
            "Coverage": records_for_json(coverage),
        },
    }

    OUT_JSON.write_text(json.dumps(payload, ensure_ascii=False, allow_nan=False), encoding="utf-8")
    print(OUT_JSON)
    print(json.dumps(qa, indent=2))


if __name__ == "__main__":
    main()
