#!/usr/bin/env python3
"""
Report CodeVersion and session date from lab trial CSVs and write code_versions.csv.

Run from this directory (exp/data_from_lab):
    python compute_code_versions.py
"""

from __future__ import annotations

import csv
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from compute_pay import (
    DATA_DIR,
    TrialFile,
    discover_trial_files,
    report_unusual_naming,
)

VERSION_COL = "CodeVersion"
WALL_CLOCK_COL = "TrialWallClockTime"
OUT_CSV = "code_versions.csv"
WALL_CLOCK_FMT = "%Y-%m-%d %H:%M:%S.%f"


@dataclass(frozen=True)
class SessionInfo:
    code_version: str
    session_date: str  # YYYY-MM-DD


def parse_wall_clock_date(text: str) -> str | None:
    value = text.strip()
    if not value:
        return None
    try:
        return datetime.strptime(value, WALL_CLOCK_FMT).date().isoformat()
    except ValueError:
        date_part = value[:10]
        try:
            return datetime.strptime(date_part, "%Y-%m-%d").date().isoformat()
        except ValueError:
            return None


def session_info(path: Path) -> SessionInfo | None:
    code_version: str | None = None
    session_date: str | None = None
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            if code_version is None:
                value = row.get(VERSION_COL, "").strip()
                if value:
                    code_version = value
            if session_date is None:
                session_date = parse_wall_clock_date(row.get(WALL_CLOCK_COL, ""))
            if code_version is not None and session_date is not None:
                break
    if code_version is None:
        return None
    return SessionInfo(code_version=code_version, session_date=session_date or "")


def build_version_table(
    files: list[TrialFile],
) -> tuple[list[str], list[list[str]]]:
    subjects = sorted({f.subject_id for f in files})
    col_names: list[str] = []
    for subject_id in subjects:
        col_names.append(f"subj{subject_id}-codev")
        col_names.append(f"subj{subject_id}-date")

    # (session, variant) -> {subject_id: SessionInfo}
    sessions: dict[tuple[int, str], dict[int, SessionInfo]] = {}
    for entry in files:
        info = session_info(entry.path)
        if info is None:
            print(
                f"Warning: no {VERSION_COL} in {entry.path.name}",
                file=sys.stderr,
            )
            continue
        if not info.session_date:
            print(
                f"Warning: no {WALL_CLOCK_COL} date in {entry.path.name}",
                file=sys.stderr,
            )
        key = (entry.session, entry.variant)
        bucket = sessions.setdefault(key, {})
        if entry.subject_id in bucket:
            prev = bucket[entry.subject_id]
            if (
                prev.code_version != info.code_version
                or prev.session_date != info.session_date
            ):
                print(
                    f"Warning: conflicting session info for subj{entry.subject_id} "
                    f"ses{entry.session} variant={entry.variant!r}; "
                    f"keeping values from {entry.path.name}",
                    file=sys.stderr,
                )
        bucket[entry.subject_id] = info

    def row_for_session(session: int, variant: str) -> list[str]:
        label = str(session) if not variant else f"{session}_{variant}"
        cells = [label]
        bucket = sessions.get((session, variant), {})
        for subject_id in subjects:
            info = bucket.get(subject_id)
            if info is None:
                cells.extend(["", ""])
            else:
                cells.extend([info.code_version, info.session_date])
        return cells

    table_rows: list[list[str]] = []

    for session in sorted(session for session, variant in sessions if variant == ""):
        table_rows.append(row_for_session(session, ""))

    for session, variant in sorted(
        (session, variant)
        for session, variant in sessions
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
    print(f"{VERSION_COL} and session date by subject and session:")
    for entry in sorted(trial_files, key=lambda f: (f.subject_id, f.session, f.variant)):
        info = session_info(entry.path)
        if info is None:
            continue
        label = (
            f"ses{entry.session}"
            if not entry.variant
            else f"ses{entry.session}_{entry.variant}"
        )
        date_label = info.session_date or "?"
        print(f"  subj{entry.subject_id} {label}: {info.code_version} ({date_label})")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
