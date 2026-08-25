#!/usr/bin/env python3
"""
Compute per-session QC rates from CCRP trial CSVs and write pay-style summary tables.

Reads trial files from sub*/ folders under exp/data_from_lab.
Main trials only (WarmUpTrial == 0). Warmup trials are excluded.

Rates (proportions 0–1):
  - timeout: no keypress logged (empty Response or "timeout")
  - too_fast: RT below TOO_FAST_RT_MS (default 100 ms; implausible manual response)
  - error: ACC != 1 (any non-maximum reward, including timeouts and suboptimal choices)

Separate rows are kept for session variants, matching pay.csv labels:
  - ses1(2)           -> row 1_(2)
  - ses6 from comp 3  -> row 6_a3
  - ses15 discard     -> row 15_discard

Outputs (same directory as this script):
  qc_timeout_rate.csv
  qc_too_fast_rate.csv
  qc_error_rate.csv
  qc_trial_counts.csv   (main-trial N per cell, for reference)

Run from exp/data_from_lab:
    python compute_session_qc.py
"""

from __future__ import annotations

import csv
import sys
from dataclasses import dataclass
from pathlib import Path

from compute_pay import (
    DATA_DIR,
    discover_trial_files,
    pay_row_variant,
    resolve_session_retries,
)

TOO_FAST_RT_MS = 100.0

OUT_TIMEOUT = "qc_timeout_rate.csv"
OUT_TOO_FAST = "qc_too_fast_rate.csv"
OUT_ERROR = "qc_error_rate.csv"
OUT_COUNTS = "qc_trial_counts.csv"


@dataclass(frozen=True)
class SessionRates:
    n_main: int
    n_timeout: int
    n_too_fast: int
    n_error: int

    @property
    def timeout_rate(self) -> float | None:
        return None if self.n_main == 0 else self.n_timeout / self.n_main

    @property
    def too_fast_rate(self) -> float | None:
        return None if self.n_main == 0 else self.n_too_fast / self.n_main

    @property
    def error_rate(self) -> float | None:
        return None if self.n_main == 0 else self.n_error / self.n_main


def is_timeout(row: dict[str, str]) -> bool:
    response = (row.get("Response") or "").strip().lower()
    return response in ("", "none", "timeout")


def parse_rt(row: dict[str, str]) -> float | None:
    raw = (row.get("RT") or "").strip()
    if not raw:
        return None
    try:
        return float(raw)
    except ValueError:
        return None


def is_too_fast(row: dict[str, str]) -> bool:
    rt = parse_rt(row)
    return rt is not None and rt < TOO_FAST_RT_MS


def is_error(row: dict[str, str]) -> bool:
    return (row.get("ACC") or "").strip() != "1"


def analyze_session(path: Path) -> SessionRates:
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))

    main_rows = [row for row in rows if (row.get("WarmUpTrial") or "").strip() == "0"]
    return SessionRates(
        n_main=len(main_rows),
        n_timeout=sum(1 for row in main_rows if is_timeout(row)),
        n_too_fast=sum(1 for row in main_rows if is_too_fast(row)),
        n_error=sum(1 for row in main_rows if is_error(row)),
    )


def session_row_label(session: int, variant: str) -> str:
    return str(session) if not variant else f"{session}_{variant}"


def build_rate_table(
    session_stats: dict[tuple[int, str], dict[int, SessionRates]],
    subjects: list[int],
    rate_attr: str,
) -> tuple[list[str], list[list[str]]]:
    col_names = [f"subj{s}" for s in subjects]

    def cell_value(subject_id: int, stats: SessionRates) -> str:
        rate = getattr(stats, rate_attr)
        if rate is None:
            return ""
        return f"{rate:.4f}"

    row_labels: list[str] = []
    table_rows: list[list[str]] = []

    standard_sessions = sorted(session for session, variant in session_stats if variant == "")
    for session in standard_sessions:
        row_labels.append(str(session))
        bucket = session_stats[(session, "")]
        table_rows.append(
            [str(session)]
            + [cell_value(subject_id, bucket[subject_id]) if subject_id in bucket else ""
               for subject_id in subjects]
        )

    extra_keys = sorted(
        (session, variant)
        for session, variant in session_stats
        if variant != ""
    )
    for session, variant in extra_keys:
        label = session_row_label(session, variant)
        row_labels.append(label)
        bucket = session_stats[(session, variant)]
        table_rows.append(
            [label]
            + [cell_value(subject_id, bucket[subject_id]) if subject_id in bucket else ""
               for subject_id in subjects]
        )

    totals: dict[int, SessionRates] = {s: SessionRates(0, 0, 0, 0) for s in subjects}
    for bucket in session_stats.values():
        for subject_id, stats in bucket.items():
            total = totals[subject_id]
            totals[subject_id] = SessionRates(
                n_main=total.n_main + stats.n_main,
                n_timeout=total.n_timeout + stats.n_timeout,
                n_too_fast=total.n_too_fast + stats.n_too_fast,
                n_error=total.n_error + stats.n_error,
            )

    total_row = ["total"] + [cell_value(s, totals[s]) for s in subjects]
    row_labels.append("total")
    table_rows.append(total_row)

    return ["session", *col_names], table_rows


def build_count_table(
    session_stats: dict[tuple[int, str], dict[int, SessionRates]],
    subjects: list[int],
) -> tuple[list[str], list[list[str]]]:
    col_names = [f"subj{s}" for s in subjects]

    def row_for(session: int, variant: str) -> list[str]:
        label = session_row_label(session, variant)
        bucket = session_stats[(session, variant)]
        return [label] + [
            str(bucket[subject_id].n_main) if subject_id in bucket else ""
            for subject_id in subjects
        ]

    table_rows: list[list[str]] = []
    for session in sorted(session for session, variant in session_stats if variant == ""):
        table_rows.append(row_for(session, ""))
    for session, variant in sorted(
        (session, variant)
        for session, variant in session_stats
        if variant != ""
    ):
        table_rows.append(row_for(session, variant))

    totals = {s: 0 for s in subjects}
    for bucket in session_stats.values():
        for subject_id, stats in bucket.items():
            totals[subject_id] += stats.n_main
    table_rows.append(["total"] + [str(totals[s]) for s in subjects])

    return ["session", *col_names], table_rows


def write_csv(path: Path, header: list[str], rows: list[list[str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(header)
        writer.writerows(rows)


def main() -> int:
    data_dir = Path.cwd()
    if data_dir.resolve() != DATA_DIR.resolve():
        print(
            f"Note: cwd is {data_dir}; script lives in {DATA_DIR}. "
            "Using cwd for input/output.",
            file=sys.stderr,
        )

    trial_files = resolve_session_retries(discover_trial_files(data_dir))
    if not trial_files:
        print(f"No trial CSV files found in {data_dir}", file=sys.stderr)
        return 1

    session_stats: dict[tuple[int, str], dict[int, SessionRates]] = {}
    for entry in trial_files:
        stats = analyze_session(entry.path)
        row_variant = pay_row_variant(entry)
        key = (entry.session, row_variant)
        bucket = session_stats.setdefault(key, {})
        if entry.subject_id in bucket:
            print(
                f"Warning: duplicate QC key subj{entry.subject_id} "
                f"session row {session_row_label(entry.session, row_variant)!r}; "
                f"keeping {entry.path.name}",
                file=sys.stderr,
            )
        bucket[entry.subject_id] = stats

    subjects = sorted({entry.subject_id for entry in trial_files})

    outputs = [
        (OUT_TIMEOUT, "timeout_rate"),
        (OUT_TOO_FAST, "too_fast_rate"),
        (OUT_ERROR, "error_rate"),
    ]
    for filename, attr in outputs:
        header, rows = build_rate_table(session_stats, subjects, attr)
        out_path = data_dir / filename
        write_csv(out_path, header, rows)
        print(f"Wrote {out_path}")

    count_header, count_rows = build_count_table(session_stats, subjects)
    count_path = data_dir / OUT_COUNTS
    write_csv(count_path, count_header, count_rows)
    print(f"Wrote {count_path}")

    print()
    print(f"Too-fast threshold: RT < {TOO_FAST_RT_MS:g} ms on main trials")
    print("Rates are proportions (0–1). Rows match pay.csv session labels.")
    print("Special sessions ((2), _aN, discard, star, etc.) appear on their own rows.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
