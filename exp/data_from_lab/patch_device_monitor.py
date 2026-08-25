#!/usr/bin/env python3
"""
Patch DeviceName and MonitorName in v3/v4 session files using Sheet6 scheduling notes.

Run from exp/data_from_lab:
    python patch_device_monitor.py --dry-run
    python patch_device_monitor.py --apply
    python patch_device_monitor.py --rebuild-log
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from dataclasses import dataclass, field
from datetime import date, datetime
from pathlib import Path

from compute_code_versions import parse_wall_clock_date

DATA_DIR = Path(__file__).resolve().parent
SHEET6_CSV = DATA_DIR / "timechart - Sheet6.csv"
CODE_VERSIONS_CSV = DATA_DIR / "code_versions.csv"
SNAPSHOT_JSON = DATA_DIR / "device_monitor_patch_before.json"
TEMP_DIR = DATA_DIR / "temp"
MONITOR_NAME_COL = "MonitorName"
DEVICE_NAME_COL = "DeviceName"
WALL_CLOCK_COL = "TrialWallClockTime"

SHEET6_DATE_RE = re.compile(
    r"^(?:CANCEL\s+)?"
    r"(?P<weekday>[A-Za-z]+),\s+"
    r"(?P<day>\d{1,2})\s+"
    r"(?P<month>[A-Za-z]+)\s+"
    r"(?P<year>\d{4})",
    re.IGNORECASE,
)


def log_path_label(path: Path) -> str:
    try:
        return path.relative_to(DATA_DIR).as_posix()
    except ValueError:
        return path.name


@dataclass
class Sheet6Entry:
    device: str | None
    monitor: str | None


@dataclass
class TargetSession:
    subject_id: int
    session: int
    code_version: str
    date_iso: str
    trials_path: Path
    metadata_path: Path


@dataclass
class PatchLog:
    lines: list[str] = field(default_factory=list)

    def change(self, path: Path, column: str, old: str, new: str) -> None:
        if old == new:
            return
        self.lines.append(
            f"{log_path_label(path)} -- {column} -- from '{old}' -- to '{new}'"
        )

    def mismatch(
        self,
        path: Path,
        field_name: str,
        data_value: str,
        sheet6_value: str,
    ) -> None:
        self.lines.append(
            f"MISMATCH {log_path_label(path)} -- {field_name} in data '{data_value}' "
            f"-- Sheet6 says '{sheet6_value}' (applied Sheet6)"
        )

    def unmatched(self, target: TargetSession, date_iso: str) -> None:
        self.lines.append(
            f"UNMATCHED {log_path_label(target.trials_path)} date {date_iso} "
            f"-- no Sheet6 entry"
        )

    def warn(self, message: str) -> None:
        self.lines.append(f"WARN {message}")


def normalize_device(raw: str) -> str | None:
    value = raw.strip()
    if not value or value.lower() == "cancel":
        return None
    upper = value.upper()
    if re.fullmatch(r"SRB\d+", upper):
        return upper
    if upper in {"RB", "KB", "SRB"}:
        return upper
    if re.fullmatch(r"SRB\d+", upper.replace(" ", "")):
        return upper.replace(" ", "")
    match = re.fullmatch(r"srb(\d+)", value, re.IGNORECASE)
    if match:
        return f"SRB{match.group(1)}"
    return upper


def normalize_computer(raw: str) -> str | None:
    value = raw.strip()
    if not value or value.lower() == "cancel":
        return None
    suffix = value.lower()
    if suffix.startswith("room1_"):
        suffix = suffix[len("room1_") :]
    elif suffix.startswith("room2_"):
        suffix = suffix[len("room2_") :]
    return f"room1_{suffix}"


def monitor_suffix(monitor: str) -> str:
    value = monitor.strip().lower()
    for prefix in ("room1_", "room2_"):
        if value.startswith(prefix):
            return value[len(prefix) :]
    return value


def parse_sheet6_date(text: str) -> date | None:
    value = text.strip()
    if not value or value.lower().startswith("cancel"):
        return None
    match = SHEET6_DATE_RE.match(value)
    if not match:
        return None
    month_day = f"{match.group('day')} {match.group('month')} {match.group('year')}"
    try:
        return datetime.strptime(month_day, "%d %B %Y").date()
    except ValueError:
        return None


def load_sheet6_lookup(path: Path) -> dict[tuple[int, date], Sheet6Entry]:
    lookup: dict[tuple[int, date], Sheet6Entry] = {}
    conflicts: list[str] = []

    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            subject_raw = row.get("Subject Number", "").strip()
            if not subject_raw.isdigit():
                continue
            subject_id = int(subject_raw)

            for part in range(1, 5):
                time_col = f"Part {part} Time"
                device_col = f"Device ID day {part}"
                computer_col = (
                    f"Computer ID Day {part}"
                    if part == 1
                    else f"Computer ID Day {part}"
                )
                parsed_date = parse_sheet6_date(row.get(time_col, ""))
                if parsed_date is None:
                    continue

                device = normalize_device(row.get(device_col, ""))
                monitor = normalize_computer(row.get(computer_col, ""))
                if device is None and monitor is None:
                    continue

                key = (subject_id, parsed_date)
                new_entry = Sheet6Entry(device=device, monitor=monitor)
                if key in lookup:
                    prev = lookup[key]
                    if (
                        (new_entry.device and prev.device and new_entry.device != prev.device)
                        or (
                            new_entry.monitor
                            and prev.monitor
                            and new_entry.monitor != prev.monitor
                        )
                    ):
                        conflicts.append(
                            f"subj{subject_id} {parsed_date.isoformat()} part {part}: "
                            f"device {prev.device}->{new_entry.device}, "
                            f"monitor {prev.monitor}->{new_entry.monitor}"
                        )
                    lookup[key] = Sheet6Entry(
                        device=new_entry.device or prev.device,
                        monitor=new_entry.monitor or prev.monitor,
                    )
                else:
                    lookup[key] = new_entry

    if conflicts:
        for message in conflicts:
            print(f"Sheet6 conflict: {message}", file=sys.stderr)

    return lookup


def is_v3_or_v4(code_version: str) -> bool:
    return code_version.startswith("v3") or code_version.startswith("v4")


def parse_session_label(label: str) -> int | None:
    label = label.strip()
    if label.isdigit():
        return int(label)
    return None


def load_targets(code_versions_path: Path) -> list[TargetSession]:
    targets: list[TargetSession] = []

    with code_versions_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        subject_cols = [
            (header, int(header.removeprefix("subj").removesuffix("-codev")))
            for header in reader.fieldnames or []
            if header.endswith("-codev")
        ]

        for row in reader:
            session = parse_session_label(row.get("session", ""))
            if session is None:
                continue

            for col_name, subject_id in subject_cols:
                code_version = row.get(col_name, "").strip()
                date_col = f"subj{subject_id}-date"
                date_iso = row.get(date_col, "").strip()
                if not code_version or not date_iso:
                    continue
                if not is_v3_or_v4(code_version):
                    continue

                subdir = DATA_DIR / f"sub{subject_id}"
                stem = f"CCRP_subj{subject_id}_ses{session}"
                trials_path = subdir / f"{stem}_trials.csv"
                metadata_path = subdir / f"{stem}_metadata.json"
                if not trials_path.exists():
                    print(
                        f"Warning: missing {trials_path}",
                        file=sys.stderr,
                    )
                    continue

                targets.append(
                    TargetSession(
                        subject_id=subject_id,
                        session=session,
                        code_version=code_version,
                        date_iso=date_iso,
                        trials_path=trials_path,
                        metadata_path=metadata_path,
                    )
                )

    return targets


def trial_csv_date(path: Path) -> str | None:
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            parsed = parse_wall_clock_date(row.get(WALL_CLOCK_COL, ""))
            if parsed:
                return parsed
    return None


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


def read_trial_first_values(path: Path) -> tuple[str, str]:
    if not path.exists():
        return "", ""
    _, rows = read_trial_csv(path)
    if not rows:
        return "", ""
    return (
        rows[0].get(DEVICE_NAME_COL, ""),
        rows[0].get(MONITOR_NAME_COL, ""),
    )


def read_metadata_values(path: Path) -> tuple[str, str, bool]:
    if not path.exists():
        return "", "", False
    with path.open(encoding="utf-8") as handle:
        meta = json.load(handle).get("run_and_display_metadata", {})
    return (
        str(meta.get("device_name", "")),
        str(meta.get("monitor_name", "")),
        bool(meta.get("device_name_overridden", False)),
    )


def before_backup_path(path: Path) -> Path:
    return TEMP_DIR / path.name


def save_before_snapshot(targets: list[TargetSession]) -> None:
    snapshot: dict[str, dict[str, str | bool]] = {}
    for target in targets:
        trial_backup = before_backup_path(target.trials_path)
        meta_backup = before_backup_path(target.metadata_path)
        trial_source = trial_backup if trial_backup.exists() else target.trials_path
        meta_source = meta_backup if meta_backup.exists() else target.metadata_path
        device, monitor = read_trial_first_values(trial_source)
        meta_device, meta_monitor, overridden = read_metadata_values(meta_source)
        snapshot[target.trials_path.name] = {
            "device": device,
            "monitor": monitor,
            "meta_device": meta_device,
            "meta_monitor": meta_monitor,
            "overridden": overridden,
        }
    SNAPSHOT_JSON.write_text(
        json.dumps(snapshot, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def load_before_snapshot() -> dict[str, dict[str, str | bool]]:
    if not SNAPSHOT_JSON.exists():
        return {}
    return json.loads(SNAPSHOT_JSON.read_text(encoding="utf-8"))


def infer_before_values(
    target: TargetSession,
    snapshot: dict[str, dict[str, str | bool]],
    entry: Sheet6Entry | None = None,
) -> tuple[str, str, str, str]:
    key = target.trials_path.name
    if key in snapshot:
        entry_data = snapshot[key]
        return (
            str(entry_data.get("device", "")),
            str(entry_data.get("monitor", "")),
            str(entry_data.get("meta_device", "")),
            str(entry_data.get("meta_monitor", "")),
        )

    trial_backup = before_backup_path(target.trials_path)
    meta_backup = before_backup_path(target.metadata_path)
    if trial_backup.exists():
        device, monitor = read_trial_first_values(trial_backup)
    else:
        device, monitor = read_trial_first_values(target.trials_path)
        monitor = ""

    if meta_backup.exists():
        meta_device, meta_monitor, overridden = read_metadata_values(meta_backup)
    else:
        meta_device, meta_monitor, overridden = read_metadata_values(target.metadata_path)
        if overridden and entry and entry.device and meta_device == entry.device:
            meta_device = "KB"
            device = "KB"
        if target.subject_id in {10, 12} and entry and entry.monitor:
            if meta_monitor == entry.monitor and monitor_suffix(entry.monitor) in {
                "a3",
                "a6",
            }:
                meta_monitor = "room1_a4"

    return device, monitor, meta_device, meta_monitor


def rebuild_change_log() -> PatchLog:
    lookup = load_sheet6_lookup(SHEET6_CSV)
    targets = load_targets(CODE_VERSIONS_CSV)
    snapshot = load_before_snapshot()
    log = PatchLog()
    matched = 0
    unmatched = 0

    for target in targets:
        session_date = date.fromisoformat(target.date_iso)
        verified = trial_csv_date(target.trials_path)
        if verified and verified != target.date_iso:
            session_date = date.fromisoformat(verified)

        entry = lookup.get((target.subject_id, session_date))
        if entry is None:
            log.unmatched(target, session_date.isoformat())
            unmatched += 1
            continue

        matched += 1
        old_device, old_monitor, old_meta_device, old_meta_monitor = infer_before_values(
            target, snapshot, entry
        )
        cur_device, cur_monitor = read_trial_first_values(target.trials_path)
        meta_device, meta_monitor, _ = read_metadata_values(target.metadata_path)

        if entry.device is not None:
            log.change(target.trials_path, DEVICE_NAME_COL, old_device, entry.device)
            log.change(target.metadata_path, "device_name", old_meta_device, entry.device)
        if entry.monitor is not None:
            log.change(target.trials_path, MONITOR_NAME_COL, old_monitor, entry.monitor)
            if old_meta_monitor and monitor_suffix(old_meta_monitor) != monitor_suffix(
                entry.monitor
            ):
                log.mismatch(
                    target.metadata_path,
                    "monitor_name",
                    old_meta_monitor,
                    monitor_suffix(entry.monitor),
                )
            log.change(target.metadata_path, "monitor_name", old_meta_monitor, entry.monitor)

        if cur_device != (entry.device or cur_device):
            log.warn(
                f"{log_path_label(target.trials_path)}: current DeviceName {cur_device!r} "
                f"!= Sheet6 {entry.device!r}"
            )
        if cur_monitor != (entry.monitor or cur_monitor):
            log.warn(
                f"{log_path_label(target.trials_path)}: current MonitorName {cur_monitor!r} "
                f"!= Sheet6 {entry.monitor!r}"
            )
        if meta_device != (entry.device or meta_device):
            log.warn(
                f"{log_path_label(target.metadata_path)}: metadata device_name {meta_device!r} "
                f"!= Sheet6 {entry.device!r}"
            )

    run_time = datetime.now().isoformat(timespec="seconds")
    header = [
        f"# Device/Monitor patch log ({run_time})",
        "",
        "Mode: rebuild-log (applied changes reconstructed)",
        f"Targets: {len(targets)} | matched: {matched} | unmatched: {unmatched}",
        "",
    ]
    log.lines = header + log.lines
    return log


def patch_trial_csv(
    path: Path,
    device: str | None,
    monitor: str | None,
    log: PatchLog,
    apply: bool,
) -> None:
    fieldnames, rows = read_trial_csv(path)
    if not rows:
        log.warn(f"{path.name} has no data rows")
        return

    if MONITOR_NAME_COL in fieldnames:
        pass
    else:
        fieldnames = [*fieldnames, MONITOR_NAME_COL]

    old_device = rows[0].get(DEVICE_NAME_COL, "")
    old_monitor = rows[0].get(MONITOR_NAME_COL, "")

    if device is not None:
        log.change(path, DEVICE_NAME_COL, old_device, device)
    if monitor is not None:
        log.change(path, MONITOR_NAME_COL, old_monitor, monitor)

    if not apply:
        return

    for row in rows:
        if device is not None:
            row[DEVICE_NAME_COL] = device
        if monitor is not None:
            row[MONITOR_NAME_COL] = monitor

    write_trial_csv(path, fieldnames, rows)


def patch_metadata_json(
    path: Path,
    device: str | None,
    monitor: str | None,
    log: PatchLog,
    apply: bool,
) -> None:
    if not path.exists():
        log.warn(f"missing metadata {path.name}")
        return

    with path.open(encoding="utf-8") as handle:
        data = json.load(handle)

    meta = data.setdefault("run_and_display_metadata", {})
    old_device = str(meta.get("device_name", ""))
    old_monitor = str(meta.get("monitor_name", ""))

    if monitor is not None and old_monitor:
        if monitor_suffix(old_monitor) != monitor_suffix(monitor):
            log.mismatch(
                path,
                "monitor_name",
                old_monitor,
                monitor_suffix(monitor),
            )

    if device is not None and device != old_device:
        log.change(path, "device_name", old_device, device)
    if monitor is not None and monitor != old_monitor:
        log.change(path, "monitor_name", old_monitor, monitor)

    if not apply:
        return

    if device is not None:
        meta["device_name"] = device
        if "device_name_assigned" in meta:
            meta["device_name_assigned"] = device
        if device != old_device:
            meta["device_name_overridden"] = True
    if monitor is not None:
        meta["monitor_name"] = monitor

    column_order = data.get("column_order")
    if isinstance(column_order, list) and MONITOR_NAME_COL not in column_order:
        column_order.append(MONITOR_NAME_COL)

    column_definitions = data.get("column_definitions")
    if isinstance(column_definitions, dict):
        column_definitions.setdefault(
            MONITOR_NAME_COL,
            "Monitor/computer name for this session (room1_* from experimenter schedule).",
        )

    with path.open("w", encoding="utf-8") as handle:
        json.dump(data, handle, indent=2, ensure_ascii=False)
        handle.write("\n")


def run_patch(apply: bool) -> PatchLog:
    if not SHEET6_CSV.exists():
        raise SystemExit(f"Missing {SHEET6_CSV}")
    if not CODE_VERSIONS_CSV.exists():
        raise SystemExit(f"Missing {CODE_VERSIONS_CSV}")

    lookup = load_sheet6_lookup(SHEET6_CSV)
    targets = load_targets(CODE_VERSIONS_CSV)
    if apply:
        save_before_snapshot(targets)
    log = PatchLog()

    matched = 0
    unmatched = 0

    for target in targets:
        session_date = date.fromisoformat(target.date_iso)
        verified = trial_csv_date(target.trials_path)
        if verified and verified != target.date_iso:
            log.warn(
                f"{log_path_label(target.trials_path)}: code_versions date "
                f"{target.date_iso} != TrialWallClockTime {verified} "
                f"(using {verified})"
            )
            session_date = date.fromisoformat(verified)

        entry = lookup.get((target.subject_id, session_date))
        if entry is None:
            log.unmatched(target, session_date.isoformat())
            unmatched += 1
            continue

        matched += 1
        if entry.device is None and entry.monitor is None:
            log.warn(
                f"subj{target.subject_id} ses{target.session} "
                f"{session_date.isoformat()}: Sheet6 match has no device or monitor"
            )
            continue
        if entry.device is None:
            log.warn(
                f"subj{target.subject_id} ses{target.session} "
                f"{session_date.isoformat()}: Sheet6 device empty, updating monitor only"
            )

        patch_trial_csv(
            target.trials_path,
            entry.device,
            entry.monitor,
            log,
            apply,
        )
        patch_metadata_json(
            target.metadata_path,
            entry.device,
            entry.monitor,
            log,
            apply,
        )

    run_time = datetime.now().isoformat(timespec="seconds")
    header = [
        f"# Device/Monitor patch log ({run_time})",
        "",
        f"Mode: {'apply' if apply else 'dry-run'}",
        f"Targets: {len(targets)} | matched: {matched} | unmatched: {unmatched}",
        "",
    ]
    log.lines = header + log.lines
    return log


def write_log(log: PatchLog, mode: str) -> Path:
    stamp = datetime.now().strftime("%Y-%m-%d_%H%M%S")
    log_name = f"device_monitor_patch_log_{stamp}_{mode}.md"
    out_path = DATA_DIR / log_name
    out_path.write_text("\n".join(log.lines) + "\n", encoding="utf-8")
    if mode in {"apply", "rebuild-log"}:
        dated = DATA_DIR / f"device_monitor_patch_log_{date.today().isoformat()}.md"
        dated.write_text("\n".join(log.lines) + "\n", encoding="utf-8")
    return out_path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--dry-run",
        action="store_true",
        help="Report planned changes without writing files",
    )
    group.add_argument(
        "--apply",
        action="store_true",
        help="Apply changes to subfolder trial CSVs and metadata JSON",
    )
    group.add_argument(
        "--rebuild-log",
        action="store_true",
        help="Reconstruct apply change log from backups/snapshot",
    )
    args = parser.parse_args()

    if args.rebuild_log:
        log = rebuild_change_log()
        mode = "rebuild-log"
    else:
        log = run_patch(apply=args.apply)
        mode = "apply" if args.apply else "dry-run"

    out_path = write_log(log, mode)
    print(f"Wrote {out_path}")
    print(f"Change/mismatch/unmatched lines: {len(log.lines) - 5}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
