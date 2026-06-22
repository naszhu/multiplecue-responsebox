#!/usr/bin/env python3
"""
Copy lab data from the Ventoy drive into raw_data, then distribute to subject folders.

Step 1 — copy from /media/lea/Ventoy/data into raw_data/ (flat, original filenames).
Step 2 — copy each CCRP session file from raw_data into data_from_lab/sub{N}/.

If a destination file with the same name already exists, skip it. Never overwrite.

Run from this directory (raw_data):
    python copy_ventoy_data.py
"""

from __future__ import annotations

import re
import shutil
import sys
from pathlib import Path

DEFAULT_SOURCE = Path("/media/lea/Ventoy/data")
RAW_DATA_DIR = Path(__file__).resolve().parent
LAB_DATA_DIR = RAW_DATA_DIR.parent

RE_CCRP = re.compile(
    r"^CCRP_subj(.+?)_ses\d+_(?:trials|metadata)\.(?:csv|json)$",
    re.IGNORECASE,
)


def collect_files(source_root: Path) -> list[Path]:
    if not source_root.is_dir():
        raise FileNotFoundError(f"Source not found: {source_root}")
    return sorted(p for p in source_root.rglob("*") if p.is_file())


def run_copy(source_root: Path, dest_dir: Path) -> int:
    files = collect_files(source_root)
    if not files:
        print(f"No files found under {source_root}", file=sys.stderr)
        return 1

    dest_dir.mkdir(parents=True, exist_ok=True)
    copied = 0
    skipped = 0

    print("=== Copy Ventoy -> raw_data ===")
    print(f"Source: {source_root}")
    print(f"Destination: {dest_dir}")
    print(f"Files found: {len(files)}")
    print()

    for source_file in files:
        dest_path = dest_dir / source_file.name

        if dest_path.exists():
            print(f"SKIP   {source_file.relative_to(source_root)} -> {dest_path.name} (exists)")
            skipped += 1
            continue

        print(f"COPY   {source_file.relative_to(source_root)} -> {dest_path.name}")
        shutil.copy2(source_file, dest_path)
        copied += 1

    print()
    print(f"Ventoy copy: {copied} copied | {skipped} skipped")
    return 0


def ccrp_target(name: str) -> tuple[str, str] | None:
    match = RE_CCRP.match(name)
    if not match:
        return None
    subj_token = match.group(1)
    return f"sub{subj_token}", name


def run_distribute(raw_dir: Path, lab_dir: Path) -> int:
    copied = 0
    skipped = 0
    ignored = 0

    print()
    print("=== Distribute raw_data -> data_from_lab/subN/ ===")
    print(f"Source: {raw_dir}")
    print(f"Destination root: {lab_dir}")
    print()

    for source_file in sorted(raw_dir.iterdir()):
        if not source_file.is_file():
            continue

        parsed = ccrp_target(source_file.name)
        if parsed is None:
            ignored += 1
            continue

        sub_dir_name, dest_name = parsed
        dest_dir = lab_dir / sub_dir_name
        dest_path = dest_dir / dest_name
        rel_dest = f"{sub_dir_name}/{dest_name}"

        if dest_path.exists():
            print(f"SKIP   {source_file.name} -> {rel_dest} (exists)")
            skipped += 1
            continue

        dest_dir.mkdir(parents=True, exist_ok=True)
        print(f"COPY   {source_file.name} -> {rel_dest}")
        shutil.copy2(source_file, dest_path)
        copied += 1

    print()
    print(f"Distribute: {copied} copied | {skipped} skipped | {ignored} ignored (not CCRP)")
    return 0


def main() -> int:
    code = run_copy(DEFAULT_SOURCE.resolve(), RAW_DATA_DIR.resolve())
    if code != 0:
        return code
    return run_distribute(RAW_DATA_DIR.resolve(), LAB_DATA_DIR.resolve())


if __name__ == "__main__":
    raise SystemExit(main())
