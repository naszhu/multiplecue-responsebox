# CCRP001 (Cued Color Response Paradigm) - Complete Experimental Settings

## Experiment Identification
- **Experiment Name**: CCRP001
- **Experiment Number**: 2001
- **Experiment Type**: 3 (Cued Color Response Paradigm)
- **Cue Target Association Type**: 8 (4 complex associations in a cross, small cues and small distances)
- **Mask Cue Present**: False
- **Calibration Flag**: False

---

## SESSIONS

### Number of Practice Sessions
- **NoPracSessions**: 5 (sessions 1-5 are practice, session 6+ are experimental)

### Practice Sessions (Sessions 1-5)

#### Session 1:
- **Cue Set**: `[[1], [2], [3], [4]]` (single cues only)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `5`
- **Repetitions per Block**: `5`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`
- **Show All Targets**: `0` (only single cue and rewarded stimulus centrally)
- **Cue-Arrow Response Associations**: `1` (enabled)

#### Session 2:
- **Cue Set**: `[[1], [2], [3], [4]]` (single cues only)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `5`
- **Repetitions per Block**: `5`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`
- **Show All Targets**: `1` (show all targets)
- **Cue-Arrow Response Associations**: `1` (enabled)

#### Session 3:
- **Cue Set**: `[[1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]` (single and dual cues)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `2`
- **Repetitions per Block**: `3`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`
- **Show All Targets**: `1` (show all targets)
- **Cue-Arrow Response Associations**: `1` (enabled)

#### Session 4:
- **Cue Set**: `[[1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]` (single and dual cues)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `2`
- **Repetitions per Block**: `3`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`
- **Show All Targets**: `1` (show all targets)
- **Cue-Arrow Response Associations**: `0` (disabled)

#### Session 5:
- **Cue Set**: `[[1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]` (single and dual cues)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `2`
- **Repetitions per Block**: `5`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`
- **Show All Targets**: `1` (show all targets)
- **Cue-Arrow Response Associations**: `0` (disabled)

### Experimental Sessions (Session 6+)
- **Cue Set**: `[[1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]` (single and dual cues)
- **Cue SOA Conditions**: `[200]` ms
- **Stimulus Exposure Duration (ED)**: `200` ms
- **Number of Blocks**: `4`
- **Repetitions per Block**: `5`
- **Warm-up Trials (First Block)**: `4`
- **Warm-up Trials (Other Blocks)**: `2`

---

## TIMING PARAMETERS

### SOA (Stimulus Onset Asynchrony)
- **Experimental SOA Conditions**: `[200]` ms (single condition)
- **Practice SOA Conditions**: 
  - Session 1: `[200]` ms
  - Session 2: `[200]` ms
  - Session 3: `[200]` ms
  - Session 4: `[200]` ms
  - Session 5: `[200]` ms

### Exposure Durations
- **Stimulus Exposure Duration (StimED)**:
  - Practice Sessions: `200` ms (all sessions)
  - Experimental Sessions: `200` ms
- **Mask Exposure Duration (MaskED)**: `50` ms (fixed for all experiments)

### Trial Timing
- **Trial Start Jitter**:
  - **Offset Time**: `1.0` seconds
  - **Mean Time**: `0.5` seconds
  - **Max Time**: `5.0` seconds
  - Jitter is calculated as: `min(OffsetTime + exponential(MeanTime), MaxTime)`

### Response Deadline
- **Response Deadline**: `2.0` seconds (from cue onset)

---

## TRIAL STRUCTURE

### Number of Conditions
- **Nconditions** = `len(CueSet) × len(cueSOAconds)`
- For experimental sessions: `10 cue conditions × 1 SOA = 10 conditions per repetition`

### Trial Calculation
- Trials are generated per block using `blockFunction()`
- Each block contains:
  - Warm-up trials (if enabled): `FirstWarmUpTrials` for first block, `OtherWarmUpTrials` for others
  - Experimental trials: All combinations of `CueSet × cueSOAconds × Repetitions`
- Trials within each block are shuffled randomly

### Total Trials per Session
**Practice Sessions:**
- Session 1: `(4 warm-up + 4 cues × 1 SOA × 5 reps) × 5 blocks = (4 + 20) × 5 = 120 trials`
- Session 2: `(4 warm-up + 4 cues × 1 SOA × 5 reps) × 5 blocks = (4 + 20) × 5 = 120 trials`
- Session 3: `(4 warm-up + 10 cues × 1 SOA × 3 reps) × 2 blocks = (4 + 30) × 2 = 68 trials`
- Session 4: `(4 warm-up + 10 cues × 1 SOA × 3 reps) × 2 blocks = (4 + 30) × 2 = 68 trials`
- Session 5: `(4 warm-up + 10 cues × 1 SOA × 5 reps) × 2 blocks = (4 + 50) × 2 = 108 trials`

**Experimental Sessions:**
- `(4 warm-up + 10 cues × 1 SOA × 5 reps) × 4 blocks = (4 + 50) × 4 = 216 trials`

---

## CUE PARAMETERS

### Cue Values
- **CueValue**: `[1, 2, 3, 4]` (4 different cue types)
- **Cue Symbols**: `[1, 2, 3, 4]` (numeric symbols)

### Cue Sets
- **Experimental Cue Set**: `[[1], [2], [3], [4], [1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]`
  - 4 single cue conditions
  - 6 dual cue conditions
  - Total: 10 cue conditions

### Cue Configuration
- **Number of Cue Locations**: `4`
- **Cue Association List**: `[1, 2, 3, 4]` (simple 1-to-1 mapping)
- **Cue Location Rotation**: `π/4` radians (45 degrees)
- **Cue Distance**: `50 × StimFactor` (degrees of visual angle)
- **Cue Scale Factor**: `0.7` (cues are 70% of standard size)
- **Cue Box Shape**: `'circle'` (circular cue boxes)
- **Cue Box Size**: `25 × StimFactor × 0.7` (degrees)
- **Cue Text Size**: `20 × StimFactor × 0.7` (degrees)

---

## STIMULUS PARAMETERS

### Color Targets
- **Number of Targets**: `4`
- **Target Location Rotation**: `π/4` radians (45 degrees)
- **Color Target Size**: `20 × StimFactor` (degrees of visual angle)
- **Color Target Edge Color**: `(-1, -1, -1)` (black edge)

### Stimulus Colors
- **Stimulus Target Colors**: `"1234"` (4 colors: 1, 2, 3, 4)
- **Stimulus Target Colors RGB**: 
  - Color 1: `(1, -1, -1)` (red)
  - Color 2: `(-1, 1, -1)` (green)
  - Color 3: `(-1, -1, 1)` (blue)
  - Color 4: `(1, 1, -1)` (yellow)
- **Color Counterbalancing**: Colors are rotated across participants based on subject number

### Response Keys
- **Response Keys**: `["Z", "X", ".", "-"]` (4 keys mapped to 4 colors)
  - Key "Z" → Color 1
  - Key "X" → Color 2
  - Key "." → Color 3
  - Key "-" → Color 4

---

## REWARD PARAMETERS

- **Max Reward Flag**: `True` (maximum reward system enabled)
- **Variable Reward Feedback Flag**: `False` (fixed reward feedback)
- **Reward Money Factor**: `0.1` (reward scaling factor)

---

## DISPLAY PARAMETERS

### Refresh Rate
- **Fixed Refresh Rate**: User-specified (default: 100 Hz, options: 60, 75, 100 Hz)
- **Test Refresh Rate**: User option ("Y" or "N")
- **Measured Refresh Rate**: Calculated during experiment

### Timing Precision
- **Delta_t**: `(1/RefreshRate)/2` (half a frame period, used for timing adjustments)

---

## EYE TRACKER PARAMETERS

- **Eye Tracker**: User-specified (can be 'none')
- **Maximum Eye Recordings**: `0` (no eye movement recordings for CCRP001)
- **Drift Correction**: 
  - **Drift Correction No Blocks**: `1` (drift correction every 1 block)
  - Drift correction is performed at the start of each block (when Block % DriftCorrectionNoBlocks == 0)

---

## ADDITIONAL PARAMETERS

### Calibration Parameters
- **Contrast Step Size**: `1.0`
- **No Trial Cal**: `10` (trials per calibration check)
- **Contrast Calibration Level**: `0.7`

### Fixation Parameters
- **Fixation Size**: Standard size (defined by StimFactor)
- **Fixation Area Size**: Standard size (defined by StimFactor)

### Stimulus Scale Factor
- **StimFactor**: User-configurable scaling factor for all stimulus sizes

---

## TRIAL STRUCTURE DETAILS

### Trial Sequence
1. **Trial Start Jitter**: Random delay (1.0 + exponential(0.5) seconds, max 5.0s)
2. **Cue Presentation**: Cues displayed for `cueSOA` duration (200 ms)
3. **Stimulus Presentation**: Colored targets displayed for `StimED` duration (200 ms)
4. **Mask Presentation**: Mask displayed for `MaskED` duration (50 ms)
5. **Response Period**: Participant responds with keypress (deadline: 2.0s from cue onset)
6. **Feedback**: Visual feedback provided

### Trial Data Recorded
- Session number
- Block number
- Trial number
- Warm-up trial flag
- Cue condition
- Condition number
- Cue SOA
- Stimulus exposure duration (ED)
- Response time (RT)
- Accuracy
- Color target time
- Trial start jitter time
- Expected reward
- Refresh rate
- Measured refresh rate
- And many more parameters...

---

## NOTES

- **Experiment Type 3**: This is the Cued Color Response Paradigm (CCRP) where:
  - Cues indicate which colored targets to attend
  - Participants respond by pressing keys associated with colors
  - Response mapping: cue → color → keypress

- **Cue-Target Association Type 8**: 
  - 4 cue locations arranged in a cross pattern
  - Small cues (70% scale)
  - Cue distance: 50 degrees
  - Circular cue boxes

- **Practice Sessions**: 
  - Sessions 1-2: Single cues only, with cue-arrow associations
  - Sessions 3-5: Full cue set, transitioning from associations to no associations
  - Session 1: Central presentation only
  - Sessions 2-5: All targets visible

- **Experimental Sessions**: 
  - Full cue set (10 conditions)
  - No cue-arrow associations
  - All targets visible
  - 4 blocks × 5 repetitions = 20 repetitions per condition

---

## FILE OUTPUT

Data files are saved with format:
- Practice: `CCRP001-Prac-subj-XXX-ses-XXX-YYYYMMDD.dat`
- Experimental: `CCRP001-subj-XXX-ses-XXX-YYYYMMDD.dat`

All experimental parameters are logged in the data file header.
