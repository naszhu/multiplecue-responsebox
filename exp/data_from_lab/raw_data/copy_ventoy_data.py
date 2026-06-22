#!/usr/bin/env python3
"""
Copy all files from the Ventoy lab-data drive into this raw_data folder (flat).

Source layout example:
    /media/lea/Ventoy/data/computer1/CCRP_subj9_ses1_trials.csv
    /media/lea/Ventoy/data/computer3/past/CCRP_subjtest1_ses1.dat

All files are copied into raw_data/ by basename. If the same filename appears
in more than one source folder, later copies are saved as:
    computer3__CCRP_subj7_ses1_trials.csv

Existing destination files are never overwritten (skipped).

Run from this directory (raw_data):
    python copy_ventoy_data.py
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path

DEFAULT_SOURCE = Path("/media/lea/Ventoy/data")
DATA_DIR = Path(__file__).resolve().parent


def dest_name_for(source_root: Path, source_file: Path, used_names: set[str]) -> str:
    base = source_file.name
    if base not in used_names:
        return base

    rel = source_file.relative_to(source_root)
    parent = rel.parent.as_posix().replace("/", "__")
    prefixed = f"{parent}__{base}" if parent != "." else base
    if prefixed not in used_names:
        return prefixed

    stem = source_file.stem
    suffix = source_file.suffix
    counter = 2
    while True:
        candidate = f"{parent}__{stem}_{counter}{suffix}"
        if candidate not in used_names:
            return candidate
        counter += 1


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
    used_names = {p.name for p in dest_dir.iterdir() if p.is_file()}
    copied = 0
    skipped = 0
    renamed = 0

    print(f"Source: {source_root}")
    print(f"Destination: {dest_dir}")
    print(f"Files found: {len(files)}")
    print()

    for source_file in files:
        dest_name = dest_name_for(source_root, source_file, used_names)
        dest_path = dest_dir / dest_name

        if dest_path.exists():
            print(f"SKIP   {source_file.relative_to(source_root)} -> {dest_name} (exists)")
            skipped += 1
            used_names.add(dest_name)
            continue

        if dest_name != source_file.name:
            renamed += 1
            print(f"COPY   {source_file.relative_to(source_root)} -> {dest_name}")
        else:
            print(f"COPY   {source_file.relative_to(source_root)} -> {dest_name}")

        shutil.copy2(source_file, dest_path)
        used_names.add(dest_name)
        copied += 1

    print()
    print(f"Copied: {copied} | Skipped (already exist): {skipped}")
    if renamed:
        print(f"Renamed on collision: {renamed}")
    return 0


def main() -> int:
    return run_copy(DEFAULT_SOURCE.resolve(), DATA_DIR.resolve())


if __name__ == "__main__":
    raise SystemExit(main())
