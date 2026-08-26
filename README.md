# multiplecue-responsebox (CCRP)

Lab experiment and behavioral analysis for the **Cued Color Response Paradigm**: reward-cued color→key selection under time pressure (single- and dual-cue competition).

Participants see colored circles and press the matching color key (keyboard, Cedrus, or self-made Arduino response box). Digit cues mark which colors are rewarded and by how much; dual-cue trials create competing options. Points convert to money.

## Design (short)

- Sessions **1–5**: practice
- Sessions **6–17**: experimental (~10 cue conditions per session)
- Trial counts and timing: `exp/trial_number_design.md`

## Run experiment

```bash
cd exp
python testmain.py
```

Main entry point: **`exp/testmain.py`**. Choose subject, session, and response device in the startup dialog.

Hardware smoke tests: `exp/test_self_made_response_box.py`, `exp/test_cedrus_box.py`. Older Arduino sync demo: `exp/test.py`.

## Analysis

R scripts under `analysis/` (RT, accuracy, SAT, session QC). Typical cohort in recent scripts: subjects ~1–32, sessions 6–17. Lab CSVs live under `exp/data_from_lab/` (mostly gitignored).

Example:

```bash
Rscript analysis/aug_12_rt_by_condition_sub1-32.R
```

## Layout

- `exp/` — PsychoPy experiment + lab data helpers
- `analysis/` — R analysis and figures
- `3d print/` — response-box hardware files
- `ethics/`, `forms/` — ethics / consent

## Related repos

- [multiplecue-prospectiveM](https://github.com/naszhu/multiplecue-prospectiveM) — PM extension of this paradigm
- [multiplecue-RTfit](https://github.com/naszhu/multiplecue-RTfit) — MIS+LBA fits on this project's lab data
