#!/usr/bin/env python3
"""
Compute per-subject session duration and inter-block break time (sessions 1-6).

Session duration: elapsed time from the first to the last trial in a session.
Break time: sum of gaps between consecutive blocks (time between the last trial
of one block and the first trial of the next).

Run from this directory (exp/data_from_lab):
    python compute_session_times.py
"""

from __future__ import annotations

import csv
import re
import sys
from dataclasses import dataclass
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent
RE_STD = re.compile(r"^CCRP_subj(\d+)_ses(\d+)_trials\.csv$", re.IGNORECASE)
SESSIONS = tuple(range(1, 7))
OUT_CSV = "session_times_minutes.csv"


@dataclass(frozen=True)
class TrialFile:
    path: Path
    subject_id: int
    session: int


@dataclass(frozen=True)
class SessionTiming:
    duration_sec: float
    break_sec: float


def parse_standard_trial_file(path: Path) -> TrialFile | None:
    match = RE_STD.match(path.name)
    if not match:
        return None
    subject_id, session = int(match.group(1)), int(match.group(2))
    if subject_id >= 100 or session not in SESSIONS:
        return None
    return TrialFile(path=path, subject_id=subject_id, session=session)


def discover_trial_files(data_dir: Path) -> list[TrialFile]:
    files: list[TrialFile] = []
    seen: set[Path] = set()
    for pattern in ("sub*/CCRP_subj*_ses*_trials.csv", "CCRP_subj*_ses*_trials.csv"):
        for path in sorted(data_dir.glob(pattern)):
            resolved = path.resolve()
            if resolved in seen:
                continue
            seen.add(resolved)
            parsed = parse_standard_trial_file(path)
            if parsed is not None:
                files.append(parsed)
    return files


def _trial_elapsed_sec(row: dict[str, str]) -> float:
    return float(row["SessionElapsedSec"])


def compute_session_timing(path: Path) -> SessionTiming | None:
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    if not rows:
        return None

    elapsed_values = [_trial_elapsed_sec(row) for row in rows]
    duration_sec = max(elapsed_values) - min(elapsed_values)

    break_sec = 0.0
    for prev_row, next_row in zip(rows, rows[1:]):
        prev_block = int(prev_row["Block"])
        next_block = int(next_row["Block"])
        if next_block > prev_block:
            break_sec += _trial_elapsed_sec(next_row) - _trial_elapsed_sec(prev_row)

    return SessionTiming(duration_sec=duration_sec, break_sec=break_sec)


def sec_to_minutes(seconds: float) -> float:
    return round(seconds / 60.0, 2)


def build_timing_table(
    files: list[TrialFile],
) -> tuple[list[str], list[list[str]]]:
    subjects = sorted({f.subject_id for f in files})
    timing: dict[tuple[int, int], SessionTiming] = {}

    for entry in files:
        result = compute_session_timing(entry.path)
        if result is None:
            print(
                f"Warning: no trials in {entry.path.name}",
                file=sys.stderr,
            )
            continue
        key = (entry.subject_id, entry.session)
        if key in timing:
            print(
                f"Warning: duplicate subj{entry.subject_id} ses{entry.session}; "
                f"keeping {entry.path.name}",
                file=sys.stderr,
            )
        timing[key] = result

    header = (
        ["Subject"]
        + [f"Session{s}" for s in SESSIONS]
        + [f"Break{s}" for s in SESSIONS]
    )
    table_rows: list[list[str]] = []

    for subject_id in subjects:
        row: list[str] = [f"subj{subject_id}"]
        for session in SESSIONS:
            result = timing.get((subject_id, session))
            if result is None:
                row.append("")
            else:
                row.append(f"{sec_to_minutes(result.duration_sec):g}")
        for session in SESSIONS:
            result = timing.get((subject_id, session))
            if result is None:
                row.append("")
            else:
                row.append(f"{sec_to_minutes(result.break_sec):g}")
        table_rows.append(row)

    return header, table_rows


def write_csv(
    data_dir: Path,
    filename: str,
    header: list[str],
    rows: list[list[str]],
) -> Path:
    out_path = data_dir / filename
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
        print(f"No standard session 1-6 trial CSV files found in {data_dir}", file=sys.stderr)
        return 1

    header, rows = build_timing_table(trial_files)
    out_path = write_csv(data_dir, OUT_CSV, header, rows)

    print(f"Wrote {out_path}")
    print("Session durations and per-session break times (minutes):")
    for row in rows:
        print("  " + ", ".join(row))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
