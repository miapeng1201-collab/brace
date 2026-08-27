from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
INPUT = ROOT / "processed_data" / "04_tewameter" / "TEWL_processed_raw_window.xlsx"
OUTPUT = ROOT / "processed_data" / "04_tewameter" / "TEWL_final_missing_heatmap.png"


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont | ImageFont.ImageFont:
    candidates = [
        "/System/Library/Fonts/Supplemental/Arial Bold.ttf" if bold else "/System/Library/Fonts/Supplemental/Arial.ttf",
        "/System/Library/Fonts/Supplemental/Helvetica Bold.ttf" if bold else "/System/Library/Fonts/Supplemental/Helvetica.ttf",
    ]
    for candidate in candidates:
        try:
            return ImageFont.truetype(candidate, size)
        except OSError:
            pass
    return ImageFont.load_default()


def clean_blank(value):
    if isinstance(value, str) and value.strip().lower() in {"", "na", "n/a"}:
        return np.nan
    return value


def draw_rotated_label(base: Image.Image, xy: tuple[int, int], text: str, text_font, fill=(45, 45, 45)) -> None:
    label = Image.new("RGBA", (130, 32), (255, 255, 255, 0))
    draw = ImageDraw.Draw(label)
    draw.text((0, 0), text, font=text_font, fill=fill)
    rotated = label.rotate(90, expand=True)
    base.alpha_composite(rotated, xy)


def main() -> None:
    df = pd.read_excel(INPUT, sheet_name="RawWindowSummaryClean")
    df["final_clean_tewl"] = df["final_clean_tewl"].map(clean_blank)
    df["real day"] = pd.to_numeric(df["real day"], errors="coerce")
    df["subject"] = df["subject"].astype(str)

    stage_order = (
        df[["real stage", "real day"]]
        .drop_duplicates()
        .sort_values(["real day", "real stage"], kind="stable")
        .reset_index(drop=True)
    )
    stage_keys = list(stage_order.itertuples(index=False, name=None))
    stage_labels = [str(stage) for stage, _day in stage_keys]

    groups = ["Control", "Ex", "ExAG"]
    group_subjects = {
        group: sorted(df.loc[df["group"] == group, "subject"].dropna().unique())
        for group in groups
    }

    cell = 26
    left = 110
    group_gap = 34
    label_h = 18
    bottom_label_h = 165
    legend_h = 70
    title_h = 54
    cols = len(stage_keys)
    rows_total = sum(len(group_subjects[group]) for group in groups) + group_gap // cell * (len(groups) - 1)
    width = left + cols * cell + 40
    height = (
        title_h
        + label_h
        + sum(len(group_subjects[group]) * cell for group in groups)
        + group_gap * (len(groups) - 1)
        + bottom_label_h
        + legend_h
    )

    img = Image.new("RGBA", (width, height), "white")
    draw = ImageDraw.Draw(img)
    title_font = font(19, bold=True)
    label_font = font(10)
    small_font = font(9)
    group_font = font(12, bold=True)

    draw.text((18, 16), "TEWL final_clean_tewl Missingness by Subject, Group, Stage", font=title_font, fill=(20, 20, 20))

    present_color = (232, 243, 255, 255)
    missing_color = (201, 52, 47, 255)
    grid_color = (255, 255, 255, 255)
    text_color = (40, 40, 40)

    total_missing = 0
    total_cells = 0
    y = title_h + label_h
    for group in groups:
        subjects = group_subjects[group]
        matrix = np.ones((len(subjects), cols), dtype=int)
        for i, subject in enumerate(subjects):
            sub = df[(df["group"] == group) & (df["subject"] == subject)]
            for j, (stage, day) in enumerate(stage_keys):
                row = sub[(sub["real stage"] == stage) & (sub["real day"] == day)]
                if not row.empty and row["final_clean_tewl"].notna().any():
                    matrix[i, j] = 0

        missing_count = int(matrix.sum())
        total_missing += missing_count
        total_cells += matrix.size

        draw.text((18, y + max(0, len(subjects) * cell // 2 - 10)), group, font=group_font, fill=text_color)
        draw.text((18, y + max(0, len(subjects) * cell // 2 + 8)), f"{missing_count}/{matrix.size}", font=small_font, fill=(90, 90, 90))
        for i, subject in enumerate(subjects):
            row_y = y + i * cell
            draw.text((72, row_y + 6), subject, font=label_font, fill=text_color)
            for j in range(cols):
                x = left + j * cell
                color = missing_color if matrix[i, j] else present_color
                draw.rectangle((x, row_y, x + cell - 1, row_y + cell - 1), fill=color)
                draw.rectangle((x, row_y, x + cell - 1, row_y + cell - 1), outline=grid_color)
        y += len(subjects) * cell + group_gap

    stage_label_y = y - group_gap + 12
    for j, label in enumerate(stage_labels):
        draw_rotated_label(img, (left + j * cell - 2, stage_label_y), label, small_font, text_color)

    legend_y = stage_label_y + 138
    draw.rectangle((left, legend_y, left + 18, legend_y + 18), fill=present_color, outline=(180, 180, 180))
    draw.text((left + 24, legend_y + 2), "Present", font=label_font, fill=text_color)
    draw.rectangle((left + 110, legend_y, left + 128, legend_y + 18), fill=missing_color, outline=(180, 180, 180))
    draw.text((left + 134, legend_y + 2), "Missing or no record", font=label_font, fill=text_color)
    draw.text(
        (18, height - 20),
        f"Source: {INPUT.name} / RawWindowSummaryClean. Total missing/no-record cells: {total_missing}/{total_cells}.",
        font=small_font,
        fill=(80, 80, 80),
    )

    img.convert("RGB").save(OUTPUT, quality=95)
    print(OUTPUT)


if __name__ == "__main__":
    main()
