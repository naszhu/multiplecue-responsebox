# CCRP (response-box) — trial number design

Source: `testmain.py` (`SESSION_CONFIG`, `_build_trials`, warm-up and burn-in rules).

Participants complete **17 sessions** (5 practice + 12 experimental). Session 6+ all use the same experimental configuration.

## Per-session trial counts

| Session | Phase | Cue conditions | Blocks | Main trials/block | Warm-up trials | **Total trials** |
|--------:|-------|----------------|-------:|----------------:|---------------:|-----------------:|
| 1 | Practice | 4 single cues | 10 | 20 | 22 | **222** |
| 2 | Practice | 4 single cues | 10 | 20 | 22 | **222** |
| 3 | Practice | 10 single+dual | 4 | 50 | 10 | **210** |
| 4 | Practice | 10 single+dual | 4 | 30 | 10 | **130** |
| 5 | Practice | 10 single+dual | 4 | 50 | 10 | **210** |
| 6–17 | Experimental | 10 single+dual | 8 | 50 | 18 | **418** |

### How totals are computed

```
warm-up  = 4 + 2 × (n_blocks − 1)
main     = n_blocks × n_per_block
total    = warm-up + main
```

Within each block, trials are balanced across cue conditions (`n_per_block ÷ n_conditions` repetitions per condition). Warm-up trials are the first 4 trials of block 1 and the first 2 trials of every later block (drawn from the same balanced pool).

### Optional burn-in block (sessions 6+ only)

Added when **session ≥ 6**, **first session of the day**, and **participation day > 1**:

| Component | Trials |
|-----------|-------:|
| Burn-in block (block 0) | **+20** |
| **Total with burn-in** | **438** |

Burn-in is **not** included in the standard counts above. In the current lab data (subj2–7), session files contain **418 trials** (no burn-in block recorded).

## Session configuration details

| Session | Display | Color-key legend |
|--------:|---------|------------------|
| 1 | Single cue + target at center | On (training) |
| 2 | Four peripheral cues/targets | On (training) |
| 3 | Four peripheral; full cue set | On (training) |
| 4 | Four peripheral; full cue set | Off (warm-ups only) |
| 5 | Four peripheral; full cue set | Off (warm-ups only) |
| 6–17 | Four peripheral; full cue set | Off (warm-ups only) |

## Grand totals (17 sessions)

| | Trials |
|---|------:|
| Sessions 1–5 (practice) | 994 |
| Sessions 6–17 (experimental, ×12) | 5,016 |
| **Standard total (no burn-in)** | **6,010** |
| If burn-in on every experimental day-start (up to 12×) | +240 max |

## Observed session duration (lab data)

From `data_from_lab/session_times_minutes.csv` (subj2–7; minutes). Duration = first-to-last trial `SessionElapsedSec`. Break = sum of inter-block gaps (self-paced).

| Session | Trials | Mean duration (min) | Mean break (min) | Mean sec/trial |
|--------:|-------:|--------------------:|-----------------:|---------------:|
| 1 | 222 | 13.9 | 0.9 | 3.75 |
| 2 | 222 | 14.6 | 0.9 | 3.93 |
| 3 | 210 | 14.7 | 0.6 | 4.21 |
| 4 | 130 | 9.5 | 0.4 | 4.37 |
| 5 | 210 | 13.8 | 0.5 | 3.94 |
| 6 | 418 | 30.3 | 3.3 | 4.36 |
| 7–17 | 418 | 29.5–33.1 | ~1.5–3.3 | ~4.2–4.7 |

### Estimated total time (17 sessions)

| Scope | Estimate |
|-------|----------|
| Practice only (sessions 1–5) | ~67 min (~1.1 h) |
| Experimental only (sessions 6–17) | ~363 min (~6.0 h) |
| **All 17 sessions** | **~430 min (~7.2 h)** |

*Excludes instruction screens, optional burn-in, and day-start setup. Subj6 took longer breaks in some sessions (up to 40 min for session 6).*

## Timing parameters (per trial)

| Event | Duration |
|-------|----------|
| Pre-trial jitter | min(1.0 + Exp(0.5), 5.0) s — mean ~1.5 s |
| Stimulus on screen | Until response |
| Response deadline | 2.0 s |
| Feedback | 1.5 s |
| Inter-block break | Self-paced (SPACE); empirical mean ~28 s/gap (session 6) |
