#!/usr/bin/env python3
"""
Report CodeVersion from lab trial CSVs and write code_versions.csv.

Run from this directory (exp/data_from_lab):
    python compute_code_versions.py
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path

from compute_pay import (
    DATA_DIR,
    TrialFile,
    discover_trial_files,
    report_unusual_naming,
)

VERSION_COL = "CodeVersion"
OUT_CSV = "code_versions.csv"


def code_version(path: Path) -> str | None:
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            value = row.get(VERSION_COL, "").strip()
            if value:
                return value
    return None


def build_version_table(
    files: list[TrialFile],
) -> tuple[list[str], list[list[str]]]:
    subjects = sorted({f.subject_id for f in files})
    col_names = [f"subj{s}" for s in subjects]

    # (session, variant) -> {subject_id: version}
    versions: dict[tuple[int, str], dict[int, str]] = {}
    for entry in files:
        value = code_version(entry.path)
        if value is None:
            print(
                f"Warning: no {VERSION_COL} in {entry.path.name}",
                file=sys.stderr,
            )
            continue
        key = (entry.session, entry.variant)
        bucket = versions.setdefault(key, {})
        if entry.subject_id in bucket and bucket[entry.subject_id] != value:
            print(
                f"Warning: conflicting {VERSION_COL} for subj{entry.subject_id} "
                f"ses{entry.session} variant={entry.variant!r}; "
                f"keeping {value!r} from {entry.path.name}",
                file=sys.stderr,
            )
        bucket[entry.subject_id] = value

    def row_for_session(session: int, variant: str) -> list[str]:
        label = str(session) if not variant else f"{session}_{variant}"
        cells = [label]
        bucket = versions.get((session, variant), {})
        for subject_id in subjects:
            cells.append(bucket.get(subject_id, ""))
        return cells

    table_rows: list[list[str]] = []

    for session in sorted(session for session, variant in versions if variant == ""):
        table_rows.append(row_for_session(session, ""))

    for session, variant in sorted(
        (session, variant)
        for session, variant in versions
        if variant != ""
    ):
        table_rows.append(row_for_session(session, variant))

    return ["session", *col_names], table_rows


def write_csv(data_dir: Path, header: list[str], rows: list[list[str]]) -> Path:
    out_path = data_dir / OUT_CSV
    with out_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(header)
        writer.writerows(rows)
    return out_path


def main() -> int:
    data_dir = Path.cwd()
    if data_dir.resolve() != DATA_DIR.resolve():
        print(
            f"Note: cwd is {data_dir}; script lives in {DATA_DIR}. "
            "Using cwd for input/output.",
        )

    trial_files = discover_trial_files(data_dir)
    if not trial_files:
        print(f"No trial CSV files found in {data_dir}", file=sys.stderr)
        return 1

    report_unusual_naming(trial_files)
    header, rows = build_version_table(trial_files)
    out_path = write_csv(data_dir, header, rows)

    print()
    print(f"Wrote {out_path}")
    print(f"{VERSION_COL} by subject and session:")
    for entry in sorted(trial_files, key=lambda f: (f.subject_id, f.session, f.variant)):
        value = code_version(entry.path)
        if value is None:
            continue
        label = (
            f"ses{entry.session}"
            if not entry.variant
            else f"ses{entry.session}_{entry.variant}"
        )
        print(f"  subj{entry.subject_id} {label}: {value}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
