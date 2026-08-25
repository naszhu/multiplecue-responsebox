#!/usr/bin/env python3
"""
Copy session data from raw_data/ into each participant subfolder (subN/).

Files are matched by subject id in the filename (e.g. CCRP_subj31_ses12_trials.csv
-> sub31/). Existing destination files are never overwritten.

Run from exp/data_from_lab:
    python copy_raw_data.py
"""

from __future__ import annotations

import re
import shutil
import sys
from datetime import datetime
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent
RAW_DATA_DIR = DATA_DIR / "raw_data"
COPY_LOG = DATA_DIR / "log_copy_from_raw.md"

RE_CCRP = re.compile(
    r"^CCRP_subj(.+?)_ses\d+(?:\([^)]+\))?_(?:trials|metadata)(?:_(?:a\d+))?\.(?:csv|json|dat)$",
    re.IGNORECASE,
)


def subject_target(filename: str) -> tuple[str, str] | None:
    match = RE_CCRP.match(filename)
    if not match:
        return None
    subj_token = match.group(1)
    return f"sub{subj_token}", filename


def append_copy_log(lines: list[str]) -> None:
    with COPY_LOG.open("a", encoding="utf-8") as log_file:
        log_file.write("\n".join(lines))
        log_file.write("\n")


def distribute(raw_dir: Path, lab_dir: Path) -> int:
    if not raw_dir.is_dir():
        print(f"raw_data not found: {raw_dir}", file=sys.stderr)
        return 1

    copied = 0
    skipped = 0
    ignored = 0
    log_lines: list[str] = [
        f"## Run {datetime.now().isoformat(timespec='seconds')}",
        "",
    ]
    copy_lines: list[str] = []

    for source_file in sorted(raw_dir.iterdir()):
        if not source_file.is_file():
            continue

        parsed = subject_target(source_file.name)
        if parsed is None:
            ignored += 1
            continue

        sub_dir_name, dest_name = parsed
        dest_dir = lab_dir / sub_dir_name
        dest_path = dest_dir / dest_name
        rel_dest = f"{sub_dir_name}/{dest_name}"

        if dest_path.exists():
            skipped += 1
            continue

        dest_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source_file, dest_path)
        line = f"COPY   {source_file.name} -> {rel_dest}"
        print(line)
        copy_lines.append(line)
        copied += 1

    summary = f"Done: {copied} copied | {skipped} skipped | {ignored} ignored (not session data)"
    print()
    print(summary)

    if copied > 0:
        log_lines.extend(copy_lines)
        log_lines.append("")
        log_lines.append(summary)
        append_copy_log(log_lines)

    return 0


def main() -> int:
    return distribute(RAW_DATA_DIR.resolve(), DATA_DIR.resolve())


if __name__ == "__main__":
    raise SystemExit(main())
