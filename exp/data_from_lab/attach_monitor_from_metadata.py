#!/usr/bin/env python3
"""
Attach MonitorName from each session metadata.json to the matching trials CSV.

Scans only subject folders (sub1, sub2, ...) under exp/data_from_lab — not raw_data
or other top-level folders. For every *_metadata.json with a paired *_trials.csv,
reads run_and_display_metadata.monitor_name and writes it as the last CSV column
(MonitorName). Rows/files that already have a non-empty MonitorName are left unchanged.

Run from exp/data_from_lab:
    python attach_monitor_from_metadata.py --dry-run
    python attach_monitor_from_metadata.py --apply
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from datetime import datetime
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent
SUBJECT_DIR_RE = re.compile(r"^sub\d+$")
MONITOR_NAME_COL = "MonitorName"
PATCH_LOG = DATA_DIR / "device_monitor_patch_log_2026-06-18.md"


def subject_dirs(data_dir: Path) -> list[Path]:
    return sorted(
        path
        for path in data_dir.iterdir()
        if path.is_dir() and SUBJECT_DIR_RE.match(path.name)
    )


def read_monitor_name(metadata_path: Path) -> tuple[str | None, str | None]:
    """Return (monitor_name, error_message)."""
    try:
        text = metadata_path.read_text(encoding="utf-8").strip()
    except OSError as exc:
        return None, f"cannot read metadata: {exc}"
    if not text:
        return None, "metadata file is empty"
    try:
        meta = json.loads(text).get("run_and_display_metadata", {})
    except json.JSONDecodeError as exc:
        return None, f"invalid metadata JSON: {exc.msg}"
    value = str(meta.get("monitor_name", "")).strip()
    if not value:
        return None, "no monitor_name in metadata"
    return value, None


def read_trial_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        fieldnames = list(reader.fieldnames or [])
        rows = list(reader)
    return fieldnames, rows


def write_trial_csv(
    path: Path,
    fieldnames: list[str],
    rows: list[dict[str, str]],
) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def monitor_already_present(rows: list[dict[str, str]]) -> bool:
    return any((row.get(MONITOR_NAME_COL) or "").strip() for row in rows)


def attach_monitor(
    trials_path: Path,
    monitor_name: str,
    apply: bool,
) -> str:
    """Return action: 'patched', 'skipped_existing', or 'empty'."""
    fieldnames, rows = read_trial_csv(trials_path)
    if not rows:
        return "empty"

    if MONITOR_NAME_COL not in fieldnames:
        fieldnames = [*fieldnames, MONITOR_NAME_COL]
    elif monitor_already_present(rows):
        return "skipped_existing"

    if not apply:
        return "patched"

    for row in rows:
        row[MONITOR_NAME_COL] = monitor_name
    write_trial_csv(trials_path, fieldnames, rows)
    return "patched"


def append_log(
    patched_subjects: list[str],
    skipped_subjects: list[str],
    warnings: list[str],
    apply: bool,
) -> None:
    run_time = datetime.now().isoformat(timespec="seconds")
    lines = [
        "",
        f"# MonitorName from metadata ({run_time})",
        "",
        f"Mode: {'apply' if apply else 'dry-run'} | script: attach_monitor_from_metadata.py",
        "",
    ]

    if patched_subjects:
        lines.append(
            "Patched MonitorName from metadata.json for all session data in: "
            + ";".join(f"/{name}" for name in patched_subjects)
        )
    else:
        lines.append("Patched MonitorName from metadata.json for: (none)")

    if skipped_subjects:
        lines.append(
            "MonitorName already present (not overwritten) in: "
            + ";".join(f"/{name}" for name in skipped_subjects)
        )

    for warning in warnings:
        lines.append(f"WARN {warning}")

    with PATCH_LOG.open("a", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")


def run(apply: bool) -> int:
    patched_by_subject: dict[str, int] = {}
    skipped_by_subject: dict[str, int] = {}
    warnings: list[str] = []

    for subject_dir in subject_dirs(DATA_DIR):
        for metadata_path in sorted(subject_dir.glob("*_metadata.json")):
            trials_path = metadata_path.with_name(
                metadata_path.name.replace("_metadata.json", "_trials.csv")
            )
            if not trials_path.exists():
                warnings.append(
                    f"{subject_dir.name}: missing trials CSV for {metadata_path.name}"
                )
                continue

            monitor_name, meta_error = read_monitor_name(metadata_path)
            if meta_error:
                warnings.append(
                    f"{subject_dir.name}/{metadata_path.name}: {meta_error}"
                )
                continue

            action = attach_monitor(trials_path, monitor_name, apply)
            if action == "patched":
                patched_by_subject[subject_dir.name] = (
                    patched_by_subject.get(subject_dir.name, 0) + 1
                )
            elif action == "skipped_existing":
                skipped_by_subject[subject_dir.name] = (
                    skipped_by_subject.get(subject_dir.name, 0) + 1
                )

    patched_subjects = sorted(patched_by_subject)
    skipped_subjects = sorted(skipped_by_subject)

    append_log(patched_subjects, skipped_subjects, warnings, apply)

    mode = "Applied" if apply else "Dry-run"
    print(f"{mode}: patched {sum(patched_by_subject.values())} file(s) "
          f"across {len(patched_subjects)} subject folder(s)")
    if skipped_subjects:
        print(
            f"Skipped {sum(skipped_by_subject.values())} file(s) with existing MonitorName "
            f"across {len(skipped_subjects)} subject folder(s)"
        )
    if warnings:
        print(f"Warnings: {len(warnings)}", file=sys.stderr)
        for warning in warnings:
            print(f"  {warning}", file=sys.stderr)
    print(f"Appended summary to {PATCH_LOG}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--dry-run",
        action="store_true",
        help="Report planned changes without writing CSV files",
    )
    group.add_argument(
        "--apply",
        action="store_true",
        help="Write MonitorName column to trials CSV files",
    )
    args = parser.parse_args()
    return run(apply=args.apply)


if __name__ == "__main__":
    raise SystemExit(main())
