from __future__ import annotations

import json
import re
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "Original data" / "03 - FINAPRESS" / "Finapress.xlsx"
OUT_DIR = ROOT / "Generated outputs" / "finapress"
OUT_JSON = OUT_DIR / "finapress_processed_payload.json"


METRIC_RE = re.compile(
    r"^(SV|CO|SVI|CI|dP/dt|SPTI|RPP|DPTI|DPTI/SPTI|LVET|ZAo|Cwk|Rp|SVR|BSA|SVRI)"
)


def clean_label(value: object) -> str | None:
    if pd.isna(value):
        return None
    text = str(value).strip()
    return text if text else None


def parse_stage(stage: object) -> tuple[str | None, float | None]:
    text = clean_label(stage)
    if text is None:
        return None, None
    match = re.match(r"^([A-Za-z]+)(-?\d+)?$", text)
    if not match:
        return text, None
    phase = match.group(1).upper()
    day = float(match.group(2)) if match.group(2) is not None else None
    return phase, day


def normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    renamed = {}
    for col in df.columns:
        new_col = str(col).strip()
        new_col = new_col.replace("Rp(mmHg.s/ml)", "Rp(mmHg,s/ml)")
        new_col = new_col.replace("SVRI(mmHg.s/ml.m^2)", "SVRI(mmHg,s/ml,m^2)")
        renamed[col] = new_col
    return df.rename(columns=renamed)


def tidy_values(df: pd.DataFrame) -> pd.DataFrame:
    df = normalize_columns(df)
    for col in df.columns:
        if col in {"Subject", "ID", "Group", "Stage", "Phase", "Campaign"}:
            df[col] = df[col].map(clean_label)
    if "Date" in df.columns:
        df["Date"] = pd.to_datetime(df["Date"], errors="coerce").dt.strftime("%Y-%m-%d")
    for col in df.columns:
        if METRIC_RE.match(str(col)) or col in {"Nr", "Day2", "Time", "Stage_day"}:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    return df


def records_for_json(df: pd.DataFrame) -> list[dict[str, object]]:
    safe = df.astype(object).where(pd.notna(df), None)
    return safe.to_dict(orient="records")


def add_stage_fields(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    parsed = df["Stage"].map(parse_stage)
    df["Phase"] = [item[0] for item in parsed]
    df["Stage_day"] = [item[1] for item in parsed]
    return df


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    avg = pd.read_excel(SOURCE, sheet_name="S_FINA_AVG")
    c1 = pd.read_excel(SOURCE, sheet_name="S_FINA_C1")
    c2 = pd.read_excel(SOURCE, sheet_name="S_FINA_C2")

    avg = tidy_values(add_stage_fields(avg))
    avg["Campaign"] = avg["Subject"].map(lambda s: "C1" if s and s <= "M" else "C2")

    lookup = avg[["ID", "Group", "Stage", "Phase", "Stage_day", "Campaign"]].drop_duplicates("ID")

    clean_detail_sheets: dict[str, pd.DataFrame] = {}
    for campaign, detail in {"C1": c1, "C2": c2}.items():
        detail = tidy_values(detail)
        detail = detail.drop(columns=[col for col in detail.columns if str(col).startswith("Column")], errors="ignore")
        detail = detail.merge(lookup, on="ID", how="left")
        detail["Campaign"] = detail["Campaign"].fillna(campaign)
        metric_cols = [col for col in detail.columns if METRIC_RE.match(str(col))]
        detail_clean = detail.loc[~detail[metric_cols].isna().all(axis=1)].copy()
        clean_detail_sheets[campaign] = detail_clean

    detailed_clean = pd.concat(clean_detail_sheets.values(), ignore_index=True)

    metric_cols = [col for col in avg.columns if METRIC_RE.match(str(col))]
    summary_rows = []
    for metric in metric_cols:
        values = pd.to_numeric(avg[metric], errors="coerce")
        summary_rows.append(
            {
                "Metric": metric,
                "N": int(values.notna().sum()),
                "Missing": int(values.isna().sum()),
                "Mean": float(values.mean()) if values.notna().any() else None,
                "SD": float(values.std()) if values.notna().sum() > 1 else None,
                "Min": float(values.min()) if values.notna().any() else None,
                "Max": float(values.max()) if values.notna().any() else None,
            }
        )
    metric_summary = pd.DataFrame(summary_rows)

    coverage = (
        avg.groupby(["Campaign", "Group", "Phase"], dropna=False)
        .agg(Records=("ID", "count"), Subjects=("Subject", "nunique"))
        .reset_index()
        .sort_values(["Campaign", "Group", "Phase"])
    )

    stage_counts = (
        avg.groupby(["Subject", "Group", "Stage", "Phase"], dropna=False)
        .agg(Records=("ID", "count"))
        .reset_index()
        .sort_values(["Subject", "Stage"])
    )

    qa = {
        "source_file": str(SOURCE.relative_to(ROOT)),
        "avg_rows": int(len(avg)),
        "avg_subjects": int(avg["Subject"].nunique()),
        "avg_ids": int(avg["ID"].nunique()),
        "detail_clean_rows": int(len(detailed_clean)),
        "detail_clean_ids": int(detailed_clean["ID"].nunique()),
        "removed_blank_separator_rows": int(len(c1) + len(c2) - len(detailed_clean)),
        "missing_group_stage_in_detail": int(detailed_clean["Group"].isna().sum() + detailed_clean["Stage"].isna().sum()),
    }

    payload = {
        "qa": qa,
        "sheets": {
            "README": [
                {"Field": "Source", "Value": qa["source_file"]},
                {"Field": "Processing", "Value": "S_FINA_AVG retained as one row per subject-stage; S_FINA_C1/C2 separator rows with all metric columns blank removed."},
                {"Field": "Added fields", "Value": "Group, Stage, Phase, Stage_day, Campaign added to detailed rows from S_FINA_AVG lookup by ID."},
                {"Field": "Rows removed", "Value": qa["removed_blank_separator_rows"]},
            ],
            "Fina_Avg_Clean": records_for_json(avg),
            "Fina_Detail_Clean": records_for_json(detailed_clean),
            "Metric_Summary": records_for_json(metric_summary),
            "Coverage": records_for_json(coverage),
            "Stage_Counts": records_for_json(stage_counts),
        },
    }

    OUT_JSON.write_text(json.dumps(payload, ensure_ascii=False, allow_nan=False), encoding="utf-8")
    print(OUT_JSON)
    print(json.dumps(qa, indent=2))


if __name__ == "__main__":
    main()
