"""
CCRP (Cued Color Response Paradigm): Color-to-key response mapping.
Each color (Red, Green, Blue, Yellow) is associated with a key (D, C, K, M).
Participant's task: press the key for the color of the circle with the highest reward.

Display flow (win.flip locations):
  FLIP 1: Instructions → wait for Space
  FLIP A: Fixation only (per trial)
  FLIP B: Cues/stimuli → wait for key response
  FLIP C: Feedback (per trial)
  FLIP 4: End message → wait for any key
"""
import math
import random
from itertools import combinations, permutations
from pathlib import Path

import pandas as pd
from psychopy import gui, logging, visual, core, event, monitors

logging.console.setLevel(logging.DEBUG)
is_debug = False
DEBUG_DURATION = 0.001  # 1ms when is_debug: short presentation + auto-response, stop at end screen
# Constants 
EXPERIMENT_NAME = "CCP"
EXPERIMENT_NUMBER = 1001
MAX_SESSION = 6  # Session 6+ uses experimental config (4 blocks × 50 trials)
# Per-session config (index = session - 1). Session 6+ uses config index 5.
# reward_value_set: which reward values (1–4) appear in trials. [1]=one pos has reward 1; [1,2]=two pos have rewards 1,2.
# Color is always independent from reward value (each position gets a random color assignment).
SESSION_CONFIG = [
    {"reward_value_set": [[1], [2], [3], [4]], "n_blocks": 5, "n_per_block": 20, "center": True, "color_map": True},
    {"reward_value_set": [[1], [2], [3], [4]], "n_blocks": 5, "n_per_block": 20, "center": False, "color_map": True},
    {"reward_value_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 30, "center": False, "color_map": True},
    {"reward_value_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 30, "center": False, "color_map": False},
    {"reward_value_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 50, "center": False, "color_map": False},
    {"reward_value_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 4, "n_per_block": 50, "center": False, "color_map": False},
]
MAX_WAIT_TIME = 2.0
# Trial start jitter (match paradigm: TrialStartJitterOffsetTime, MeanTime, MaxTime)
TRIAL_START_JITTER_OFFSET = 1.0
TRIAL_START_JITTER_MEAN = 0.5
TRIAL_START_JITTER_MAX = 5.0
# Warm-up trials per block (match paradigm: FirstWarmUpTrials, OtherWarmUpTrials)
FIRST_WARMUP_TRIALS = 4
OTHER_WARMUP_TRIALS = 2
FEEDBACK_WAIT_TIME = 1.5
REWARD_MONEY_FACTOR = 0.1
RESPONSE_DEADLINE = 2.0  # seconds, match paradigm ResponseDeadline

# Stimulus colors RGB (-1 to 1): Red, Green, Blue, Yellow
STIMULUS_TARGET_COLORS_RGB = [(1, -1, -1), (-1, 1, -1), (-1, -1, 1), (1, 1, -1)]
CUE_TEXT_COLOR = (-1, -1, -1)
CUE_BG_COLOR = (1, 1, 1)
BG_COLOR = (0, 0, 0)

# Cue positions (x, y) in deg: 4 locations at 2 deg, rotation 45° (TargetDistance=2, TargetLocRotation=pi/4)
TARGET_DISTANCE_DEG = 2.0
TARGET_LOC_ROTATION_RAD = math.pi / 4
POSITIONS_DEG = [
    (
        TARGET_DISTANCE_DEG * math.cos(i * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD),
        TARGET_DISTANCE_DEG * math.sin(i * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD)
    )
    for i in range(4)
]

# Index	    Angle	cos	sin	(x, y)	Screen location
# 0	45°	    +0.707	+0.707	(1.414, 1.414)	top-right
# 1	135°	-0.707	+0.707	(-1.414, 1.414)	top-left
# 2	225°	-0.707	-0.707	(-1.414, -1.414)	bottom-left
# 3	315°	+0.707	-0.707	(1.414, -1.414)	bottom-right

# Stimulus sizes (deg): ColorTargetSize=0.8 radius, CueBoxSize/2=0.35, CueTextSize=0.56 (StimFactor=0.04, CueScaleFactor=0.7)
CUE_OUTER_RADIUS_DEG = 0.8   # ColorTargetSize
CUE_INNER_RADIUS_DEG = 0.35  # CueBoxSize/2
CUE_TEXT_HEIGHT_DEG = 0.56   # CueTextSize
STIMULUS_OPACITY = 1.0       # Paradigm StimulusOpacity

# Cue number text (match paradigm TextStim: ori=0, no font/bold, antialias, center anchor)
CUE_TEXT_ORI = 0
CUE_TEXT_ANTIALIAS = True
FIXATION_SIZE_DEG = 0.16     # FixationSize = 4*StimFactor
FIXATION_POINT_COLOR = (-1, -1, -1)  # Black for CCRP (ExperimentType 3)

# Feedback (match paradigm: Feedback1-4, positions, sizes, colors)
STIM_FACTOR = 0.04
FEEDBACK_LETTER_SIZE_DEG = 50 * STIM_FACTOR   # FeedbackLetterSize = 50*StimFactor
FEEDBACK1_POS_DEG = (0, 70 * STIM_FACTOR)    # Feedback1 pos for Exp type 3
FEEDBACK2_POS_DEG = (0, -70 * STIM_FACTOR)
FEEDBACK3_POS_DEG = (0, -140 * STIM_FACTOR)
FEEDBACK4_POS_DEG = (0, -200 * STIM_FACTOR)
FEEDBACK_ZERO_REWARD_COLOR = (1, -1, -1)     # Red when reward=0
FEEDBACK_POS_REWARD_COLOR = (-1, 0.7, -1)    # Green when reward>0

INSTRUCTION_LETTER_SIZE_DEG = 0.6  # InstructionLetterSize = 15*StimFactor

# Color-response instruction (paradigm: ResponseColorSize, ResponseColorDistance, -150*StimFactor)
# Paradigm: ResponseColorSize=30*StimFactor, ResponseColorDistance=50*StimFactor, y=-150*StimFactor
# x = ResponseColorDistance * (-StimulusColorNoResponses/2 + 0.5 + i)
RESPONSE_COLOR_SIZE_DEG = 30 * STIM_FACTOR      # 1.2 deg
RESPONSE_COLOR_DISTANCE_DEG = 50 * STIM_FACTOR  # 2 deg
COLOR_MAP_Y_DEG = -150 * STIM_FACTOR            # -6 deg

# Display / monitor (match paradigm exactly)
WIN_SIZE_PIX = (1920, 1080)
MONITOR_NAME = "OF2A_03_5_513_lab5"
MONITOR_WIDTH_CM = 52
MONITOR_DISTANCE_CM = 60
USE_UNITS = "deg"
USE_COLOR_SPACE = "rgb" #same as the defult value
MULTI_SAMPLE = True   # same as defult value, Anti-aliasing for smooth edges (paradigm has smooth circles)
NUM_SAMPLES = 4      # Samples per pixel when multiSample enabled
CIRCLE_EDGES = 200   # Paradigm uses edges=200 for smooth circles (default ~32 is jagged)

# Possible reward values (points 1–4). 
REWARD_VALUES = [1, 2, 3, 4]

# Color-to-key mapping: Red→D, Green→C, Blue→K, Yellow→M
# Position i has fixed color: 0=Red, 1=Green, 2=Blue, 3=Yellow (from STIMULUS_TARGET_COLORS_RGB)
COLOR_KEYS = ['d', 'c', 'k', 'm']  # key for position/color 0,1,2,3

NUM_POSITIONS = 4


# Session dialog: run one session per launch (6+ uses experimental config: 4 blocks × 50 trials)
session_dlg = gui.Dlg(title="CCRP Session")
session_dlg.addField("Participant", initial="1")
session_dlg.addField("Session", initial=1, choices=[1, 2, 3, 4, 5, 6])
session_dlg.addField(
    "Color map layout",
    initial="horizontal",
    choices=["horizontal", "keyboard"],
    tip="horizontal: 4 boxes in a row; keyboard: 2x2 grid matching D/C/K/M (Sessions 1–3)",
)
session_dlg.show()
if not session_dlg.OK:
    raise SystemExit("Session dialog cancelled")
PARTICIPANT = str(session_dlg.data[0]).strip()
SESSION = int(session_dlg.data[1])  # 1-based session number
COLOR_MAP_LAYOUT = session_dlg.data[2]  # "horizontal" or "keyboard"

_out_dir = (Path(__file__).resolve().parent / "data_written").resolve()
_out_path = (_out_dir / f"CCRP_subj{PARTICIPANT}_ses{SESSION}.csv").resolve()
if _out_path.exists():
    raise SystemExit(f"Data file already exists for participant {PARTICIPANT} session {SESSION}. Exiting.")


def _session_config_idx(session: int) -> int:
    """Return config index for session (1-based). Session 6+ uses same config as session 6."""
    return min(session - 1, 5)


def _build_trial_row(
    *,
    position_to_color_id,
    position_to_reward,
    colors_shown,
    selected_position,
    selected_color,
    pressed_key,
    keys,
    cue_time,
    response_time,
    end_trial_time,
    actual_reward,
    max_reward,
    cum_reward,
    session,
    block,
    trial_index,
    warm_up=0,
    trial_start_jitter_time_ms=0,
):
    """Build a trial data row """
    # Color layout: 4-digit strings (position 0..3). Rewards from position_to_reward.
    colors = [position_to_color_id[i] or 0 for i in range(NUM_POSITIONS)]
    reward_vals = [position_to_reward[i] or 0 for i in range(NUM_POSITIONS)]
    sorted_colors = sorted(colors)
    color_ranks = [NUM_POSITIONS - sorted_colors.index(colors[i]) for i in range(NUM_POSITIONS)]

    # Response
    sel = selected_position
    has_response = sel is not None
    resp_loc = "".join("1" if i == sel else "0" for i in range(NUM_POSITIONS)) if has_response else "0000"
    point_target = (sel + 1) if has_response else 0
    sel_color = position_to_color_id[sel] if has_response else None

    rt_sec = (response_time - cue_time) if (keys and pressed_key != "escape") else (MAX_WAIT_TIME if not keys else 0.0)
    late = rt_sec > RESPONSE_DEADLINE
    intr = 1 if (has_response and sel_color is None) else 0
    # ACC: correct = selected the circle with highest reward (color-to-key rule)
    acc = 1 if actual_reward == max_reward and max_reward > 0 else 0
    color_exp = sel_color if (has_response and sel_color) else 0
    color_rank_resp = color_ranks[sel] if (has_response and sel_color) else 0
    pos_with_selected_color = next((p for p in range(NUM_POSITIONS) if position_to_color_id.get(p) == selected_color), None) if selected_color is not None else None
    exp_reward = (position_to_reward[pos_with_selected_color] or 0) if (has_response and pos_with_selected_color is not None) else 0

    cond = f"{len(colors_shown)}cue"

    return {
        "ExperimentName": EXPERIMENT_NAME,
        "ExperimentNumber": EXPERIMENT_NUMBER,
        "ColorMapLayout": COLOR_MAP_LAYOUT,
        "Subject": PARTICIPANT,
        "Session": session + 1,
        "Block": block,
        "Trial": trial_index + 1,
        "WarmUpTrial": warm_up,
        "CueCondition": cond,
        "NumCues": len(colors_shown),
        "TrialStartJitterTime": round(trial_start_jitter_time_ms, 2),  # ms
        "CueSOA": 0,
        "Cues": "".join(str(c) for c in colors),
        "CueValues": "".join(str(v) for v in reward_vals),
        "CueRanks": "".join(str(r) for r in color_ranks),
        "Response": pressed_key or "timeout",

        # RespLoc: 4-digit one-hot of selected color (CCRP: pos0=Red, pos1=Green, pos2=Blue, pos3=Yellow)
        "RespLoc": resp_loc,
        # PointTargetResponse: 1=Red, 2=Green, 3=Blue, 4=Yellow, 0=timeout
        "PointTargetResponse": point_target, 
        
        "RT": round(rt_sec * 1000, 2),  # ms
        "LateResponse": late, # 1 - late response, 0 - on time response
        "ACC": acc, # 1 - correct response, 0 - incorrect response
        "INTR": intr, # 1 - intrusion error, 0 - no intrusion error, if respond to the non-cued position
        "CueResponseValue": actual_reward, # the value of the cue at the selected position
        "CueResponseExpValue": color_exp, # the color at the selected position
        "CueRankResponse": color_rank_resp, # the rank of the color at the selected position
        "ExpectedReward": exp_reward, # the expected reward for the trial
        "Reward": actual_reward, # the actual reward for the trial
        "MaxReward": max_reward, # the maximum reward for the trial
        "CumReward": cum_reward, # the cumulative reward for the session
        "CueTime": round(cue_time * 1000, 2),  # ms
        "PointTargetTime": round((cue_time + rt_sec) * 1000, 2) if rt_sec else round(cue_time * 1000, 2),  # ms
        "ColorTargetTime": round(cue_time * 1000, 2),  # ms
        "EndTrialTime": round(end_trial_time * 1000, 2),  # ms
        "Note": "",
    }


# Create window matching paradigm display settings
mon = monitors.Monitor(MONITOR_NAME)
mon.setSizePix(WIN_SIZE_PIX)
mon.setWidth(MONITOR_WIDTH_CM)
mon.setDistance(MONITOR_DISTANCE_CM)
mon.saveMon()
win = visual.Window(
    size=WIN_SIZE_PIX,
    fullscr=True,
    allowGUI=True,
    units=USE_UNITS,
    colorSpace=USE_COLOR_SPACE,
    monitor=MONITOR_NAME,
    color=BG_COLOR,
    multiSample=MULTI_SAMPLE,
    numSamples=NUM_SAMPLES,
)

# Define cue positions (4 locations, from POSITIONS_DEG)
positions = POSITIONS_DEG

# Response keys: color-to-key mapping (Red=D, Green=C, Blue=K, Yellow=M)
# Position 0=Red, 1=Green, 2=Blue, 3=Yellow
response_keys = COLOR_KEYS

# Colors for each cue (from paradigm StimulusTargetColorsRGB)
cue_colors = STIMULUS_TARGET_COLORS_RGB

# cue_stimuli: list of (outer, inner, text) per color
#   Index 0=Red, 1=Green, 2=Blue, 3=Yellow
#   outer = colored circle, inner = white circle, text = reward digit (1–4)
cue_stimuli = []
for i, pos in enumerate(positions):
    outer = visual.Circle(win, radius=CUE_OUTER_RADIUS_DEG, fillColor=cue_colors[i], lineColor=None, pos=pos, edges=CIRCLE_EDGES)
    inner = visual.Circle(win, radius=CUE_INNER_RADIUS_DEG, fillColor=CUE_BG_COLOR, lineColor=None, pos=pos, edges=CIRCLE_EDGES)
    text = visual.TextStim(
        win,
        text="",
        ori=CUE_TEXT_ORI,
        pos=pos,
        height=CUE_TEXT_HEIGHT_DEG,
        units=USE_UNITS,
        colorSpace=USE_COLOR_SPACE,
        opacity=STIMULUS_OPACITY,
        color=CUE_TEXT_COLOR,
        antialias=CUE_TEXT_ANTIALIAS,
        alignText="center",
        anchorHoriz="center",
        anchorVert="center",
    )
    cue_stimuli.append((outer, inner, text))  # Store as tuple

# Example cue_stimuli after loop:
#   cue_stimuli[0] = (outer_red, inner_red, text_red)   # Red circle at pos 0
#   cue_stimuli[1] = (outer_green, inner_green, text_green)  # Green at pos 1
#   cue_stimuli[2] = (outer_blue, inner_blue, text_blue)     # Blue at pos 2
#   cue_stimuli[3] = (outer_yellow, inner_yellow, text_yellow)  # Yellow at pos 3

# color_response_squares: 4 colored Rects at bottom (Session 1 only)
#   Index 0=Red, 1=Green, 2=Blue, 3=Yellow. Maps colors to key positions.
#   Layout "horizontal": 4 boxes in a row (Red, Green, Blue, Yellow left→right).
#   Layout "keyboard": relative positions like D/C/K/M - D above C (left), K above M (right), staggered.
def _color_map_positions(layout: str):
    """Return (x, y) for each color index 0..3 based on layout."""
    d = RESPONSE_COLOR_DISTANCE_DEG
    base_y = COLOR_MAP_Y_DEG
    if layout == "horizontal":
        return [
            (d * (-NUM_POSITIONS / 2 + 0.5 + i), base_y)
            for i in range(NUM_POSITIONS)
        ]
    # keyboard: D above-left of C, K above-right of M. Each position written explicitly, no overlap.
    # col_w: horizontal gap between left pair (D,C) and right pair (K,M) - larger = more space in middle.
    row_h = d * 0.6       # vertical spacing between top and bottom row
    col_w = d * 1.4       # horizontal spacing: left pair | gap | right pair (wider = more middle gap)
    stagger = d * 0.35    # bottom row indent (C right of D, M left of K)
    return [
        (-col_w, base_y + row_h),              # 0 Red   (D) top-left
        (-col_w + stagger, base_y - row_h),    # 1 Green (C) bottom-left, below D, shifted right
        (col_w, base_y + row_h),               # 2 Blue  (K) top-right
        (col_w - stagger, base_y - row_h),     # 3 Yellow(M) bottom-right, below K, shifted left
    ]

color_map_positions = _color_map_positions(COLOR_MAP_LAYOUT)
color_response_squares = []
for i in range(NUM_POSITIONS):
    rect = visual.Rect(
        win,
        width=RESPONSE_COLOR_SIZE_DEG,
        height=RESPONSE_COLOR_SIZE_DEG,
        fillColor=STIMULUS_TARGET_COLORS_RGB[i],
        lineColor=None,
        pos=color_map_positions[i],
        units=USE_UNITS,
    )
    color_response_squares.append(rect)

# -----------------------------------------------------------------------------
# Trial generation: cue-balanced; color/reward sampled randomly. Same logic all sessions.
# All conditions enumerated BEFORE experiment.
# -----------------------------------------------------------------------------
session_idx = SESSION - 1
config_idx = _session_config_idx(SESSION)
cfg = SESSION_CONFIG[config_idx]
n_blocks = cfg["n_blocks"]
n_trials_per_block = cfg["n_per_block"]
# Total trials = warm-up (4 + 2*(n_blocks-1)) + main (n_blocks * n_trials_per_block)
n_warmup_first = FIRST_WARMUP_TRIALS
n_warmup_other = OTHER_WARMUP_TRIALS
total_warmup = n_warmup_first + (n_blocks - 1) * n_warmup_other
total_trials = total_warmup + n_blocks * n_trials_per_block


def _pool_for_reward_values(reward_values: list, cfg: dict) -> list:
    """
    Enumerate all trial variants for a reward-value condition.

    Output format:
      {"position_to_color_id": {0..3: color_id 1–4 or None},
       "position_to_reward": {0..3: reward (1–4) or None}}

    Color is always independent from reward value. Each position gets a color (1–4);
    reward values are fixed by the condition.
    """
    def make_trial(position_to_color_id: dict, position_to_reward: dict) -> dict:
        return {"position_to_color_id": position_to_color_id, "position_to_reward": position_to_reward}

    n_positions = NUM_POSITIONS
    all_color_ids = list(range(1, n_positions + 1))

    # -------------------------------------------------------------------------
    # SESSION 1: one stimulus at center. position 0 = center; others unused.
    # -------------------------------------------------------------------------
    if cfg["center"]:
        reward_value = reward_values[0]
        out = []
        for color_id in all_color_ids:
            position_to_color_id = {i: color_id if i == 0 else None for i in range(n_positions)} #there is only one position so take i = 0
            position_to_reward = {i: reward_value if i == 0 else None for i in range(n_positions)}
            out.append(make_trial(position_to_color_id, position_to_reward))
        return out

    # -------------------------------------------------------------------------
    # SESSIONS 2–6: n_positions color circles at n_positions positions. 1 or 2 positions show reward.
    # Single reward [r]: one pos has reward r; colors permuted across all positions.
    # Dual reward [r1,r2]: two pos have rewards r1,r2; colors permuted across all positions.
    # This code builds the full combinatorial set of trial variants for each reward condition.
    # -------------------------------------------------------------------------
    if len(reward_values) == 1:
        reward_value = reward_values[0]
        out = []
        for reward_pos in range(n_positions):
            for color_id_i in permutations(all_color_ids):
                position_to_color_id = {i: color_id_i[i] for i in range(n_positions)}
                position_to_reward = {i: None for i in range(n_positions)}
                position_to_reward[reward_pos] = reward_value
                out.append(make_trial(position_to_color_id, position_to_reward))
        return out

    reward_a, reward_b = reward_values[0], reward_values[1]
    out = []
    for position_a, position_b in permutations(range(n_positions), 2):
        for reward_at_a, reward_at_b in [(reward_a, reward_b), (reward_b, reward_a)]:
            for color_id_i in permutations(all_color_ids):
                position_to_color_id = {i: color_id_i[i] for i in range(n_positions)}
                position_to_reward = {i: None for i in range(n_positions)}
                position_to_reward[position_a], position_to_reward[position_b] = reward_at_a, reward_at_b
                out.append(make_trial(position_to_color_id, position_to_reward))
    return out


def _build_trials(cfg: dict) -> list:
    """Reward-value-balanced: each condition reps per block. Warm-up trials prepended per block (match paradigm)."""
    reward_value_set, n_blocks, n_per_block = cfg["reward_value_set"], cfg["n_blocks"], cfg["n_per_block"]
    reps_per_condition = n_per_block // len(reward_value_set) #how many trials each condition repeats
    trial_pools = [_pool_for_reward_values(reward_values, cfg) for reward_values in reward_value_set]

    def _make_block():
        block = [random.choice(pool) for pool in trial_pools for _ in range(reps_per_condition)]
        random.shuffle(block)
        return block

    trials = []
    for block_idx in range(n_blocks):
        block_number = block_idx + 1
        n_warmup = FIRST_WARMUP_TRIALS if block_idx == 0 else OTHER_WARMUP_TRIALS

        # Warm-up: full block, take first N (match paradigm blockFunction + del WarmUpBlockData[N:])
        warmup_block = _make_block()
        warmup_trials = warmup_block[:n_warmup]

        # Main block
        main_block = _make_block()

        for i, t in enumerate(warmup_trials):
            trials.append({**t, "warm_up": 1, "block": block_number, "trial_in_block": i + 1})
        for i, t in enumerate(main_block):
            trials.append({**t, "warm_up": 0, "block": block_number, "trial_in_block": n_warmup + i + 1})
    return trials

trial_data_list = _build_trials(cfg)

# Fixation point (match paradigm: visual.Circle, size=FixationSize, FixationPointColor)
fixation = visual.Circle(
    win,
    size=FIXATION_SIZE_DEG,
    units=USE_UNITS,
    fillColor=FIXATION_POINT_COLOR,
    lineColor=None,
    pos=(0, 0),
    edges=CIRCLE_EDGES,
)

# Feedback texts (match paradigm Feedback1-4: positions, sizes, ori, opacity)
# feedback1: main trial outcome (actual/max reward), largest text, color-coded (red=0, green>0)
feedback1 = visual.TextStim(win, text="", pos=FEEDBACK1_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
# feedback2: cumulative reward so far
feedback2 = visual.TextStim(win, text="", pos=FEEDBACK2_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.6, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
# feedback3: response time in ms
feedback3 = visual.TextStim(win, text="", pos=FEEDBACK3_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.6, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
# feedback4: block and trial number (e.g. "Block 1  Trial 3 / 20")
feedback4 = visual.TextStim(win, text="", pos=FEEDBACK4_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.2, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)

end_text = visual.TextStim(win, text="End of session!\n\n\nContact the Experimenter", color="white", height=INSTRUCTION_LETTER_SIZE_DEG)
esc_confirm_text = visual.TextStim(win, text="Press ESC again to exit\n\nPress SPACE to continue", color="white", height=INSTRUCTION_LETTER_SIZE_DEG)

# Instructions (match paradigm: InstructionLetterSize=15*StimFactor, wrapWidth=800*StimFactor)
INSTRUCTION_DIR = Path(__file__).resolve().parent / "Instructions"
INSTRUCTION_WRAP_WIDTH_DEG = 800 * STIM_FACTOR  # 32 deg, match paradigm Instruction
instructions = visual.TextStim(
    win,
    text="Press the key for the COLOR of the circle with the highest reward:\n"
         "Red = D, Green = C, Blue = K, Yellow = M\n\n"
         "Press SPACE to start",
    color="white",
    height=INSTRUCTION_LETTER_SIZE_DEG,
    wrapWidth=INSTRUCTION_WRAP_WIDTH_DEG,
    units=USE_UNITS,
)

def _load_session_instruction(session: int) -> str:
    """Load instruction text for session (1-based). Fallback to default if file missing."""
    if session in (4, 5):
        path = INSTRUCTION_DIR / "CCRP instruction ses 4-5.txt"
    elif session >= 6:
        path = INSTRUCTION_DIR / "CCRP instruction exp ses.txt"
    else:
        path = INSTRUCTION_DIR / f"CCRP instruction ses {session}.txt"
    if path.exists():
        return path.read_text(encoding="utf-8", errors="replace").strip()
    return instructions.text  # fallback

# =============================================================================
# FLIP 1: INSTRUCTIONS SCREEN
# =============================================================================
# Presented: Session-specific instruction text + "Press SPACE to begin"
# Waits for: Space key before continuing
instructions.setText(_load_session_instruction(SESSION) + "\n\nPress SPACE to begin.")
instructions.draw()
win.flip()
if is_debug:
    core.wait(DEBUG_DURATION)
else:
    event.waitKeys(keyList=['space'])

# Create clock for response time measurement
clock = core.Clock()

# Initialize cumulative reward and trial data log
cum_reward = 0.0
trial_index = 0
out_dir = _out_dir
out_dir.mkdir(parents=True, exist_ok=True)
out_path = _out_path
print(f"Data will be saved to: {out_path}")
first_trial_save = True  # Write header on first trial
completed_normally = True

# =============================================================================
# TRIAL LOOP
# =============================================================================
# Each trial has 3 display phases (3 win.flip() calls):
#   FLIP A: Fixation only
#   FLIP B: Cues (stimuli) + wait for key response
#   FLIP C: Feedback (reward, RT, etc.)

for trial_in_session in range(total_trials):
    trial_data = trial_data_list[trial_index]
    position_to_color_id = trial_data["position_to_color_id"]
    position_to_reward = trial_data["position_to_reward"]

    # colors_shown: list of color IDs displayed this trial
    colors_shown = [c for c in position_to_color_id.values() if c is not None]

    # =========================================================================
    # FLIP A: FIXATION SCREEN
    # =========================================================================
    # Presented: Black fixation dot at center (nothing else)
    # Duration: jittered (offset + exponential(mean), capped at max) - match paradigm
    if is_debug:
        trial_start_jitter_time = DEBUG_DURATION
    else:
        trial_start_jitter_time = min(
            TRIAL_START_JITTER_OFFSET + random.expovariate(1.0 / TRIAL_START_JITTER_MEAN),
            TRIAL_START_JITTER_MAX,
        )
    trial_start_jitter_time_ms = trial_start_jitter_time * 1000
    fixation.draw()
    win.flip()
    core.wait(trial_start_jitter_time)

    clock.reset()

    show_color_map = SESSION in (1, 2, 3)

    # =========================================================================
    # FLIP B: CUE/STIMULUS SCREEN (stays until keypress or timeout)
    # =========================================================================
    fixation.draw()
    if SESSION == 1:
        # Session 1 only: draw single stimulus at center
        color_id = position_to_color_id[0]
        outer, inner, text = cue_stimuli[color_id - 1]
        text.setText(str(position_to_reward[0]))
        orig_pos = outer.pos
        for stim in (outer, inner, text):
            stim.setPos((0, 0))
        outer.draw()
        inner.draw()
        text.draw()
        for stim in (outer, inner, text):
            stim.setPos(orig_pos)
    else:
        # Sessions 2+: 4 color circles at 4 positions. Reward from position_to_reward.
        for pos_idx in range(4):
            color_id = position_to_color_id[pos_idx]
            outer, inner, text = cue_stimuli[color_id - 1]
            target_pos = positions[pos_idx]
            r = position_to_reward.get(pos_idx)
            text.setText(str(r) if r is not None else "")
            orig_positions = [outer.pos, inner.pos, text.pos]
            for stim in (outer, inner, text):
                stim.setPos(target_pos)
            outer.draw()
            inner.draw()
            text.draw()
            for stim, orig in zip((outer, inner, text), orig_positions):
                stim.setPos(orig)
    if show_color_map:
        for rect in color_response_squares:
            rect.draw()
    win.flip()
    cue_time = clock.getTime()

    # -------------------------------------------------------------------------
    # Wait for key response (D/C/K/M or escape). Screen stays at FLIP B until then.
    # -------------------------------------------------------------------------
    rewards_at_positions = [r for r in position_to_reward.values() if r is not None]
    max_reward = max(rewards_at_positions) if rewards_at_positions else 0
    actual_reward = 0
    pressed_key = ""
    rt = None
    selected_position = None
    selected_color = None
    response_time = cue_time

    if is_debug:
        core.wait(DEBUG_DURATION)
        best_pos = max(position_to_reward, key=lambda p: position_to_reward.get(p) or 0)
        correct_color_id = position_to_color_id.get(best_pos) or 1
        pressed_key = response_keys[correct_color_id - 1]
        response_time = cue_time + DEBUG_DURATION
        keys = [(pressed_key, response_time)]
        rt = DEBUG_DURATION * 1000
        selected_position = correct_color_id - 1
        selected_color = correct_color_id
        actual_reward = position_to_reward.get(best_pos) or 0
    else:
        event.clearEvents()
        keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)

    if keys and not is_debug:
        pressed_key = keys[0][0]
        response_time = keys[0][1]
        if pressed_key == 'escape':
            esc_confirm_text.draw()
            win.flip()
            k = event.waitKeys(keyList=['escape', 'space'])
            if k[0] == 'escape':
                completed_normally = False
                break
            trial_index += 1
            continue
        rt = (response_time - cue_time) * 1000
        selected_position = response_keys.index(pressed_key)
        selected_color = selected_position + 1  # color pressed (1–4)
        # Find position with that color; reward = position_to_reward[that_pos] (0 if None)
        pos_with_color = next((p for p in range(4) if position_to_color_id.get(p) == selected_color), None)
        actual_reward = (position_to_reward.get(pos_with_color) or 0) if pos_with_color is not None else 0
    elif not is_debug:
        rt = MAX_WAIT_TIME * 1000

    cum_reward += actual_reward * REWARD_MONEY_FACTOR
    cum_reward = round(cum_reward, 2)

    # =========================================================================
    # FLIP C: FEEDBACK SCREEN
    # =========================================================================
    # Presented: actual/max reward, cumulative reward, RT, block/trial info, fixation
    # Duration: FEEDBACK_WAIT_TIME (1.5 s)
    # -------------------------------------------------------------------------
    feedback1.setText(str(actual_reward) + " / " + str(max_reward))  # e.g. "2 / 4"
    feedback1.setColor(FEEDBACK_ZERO_REWARD_COLOR if actual_reward == 0 else FEEDBACK_POS_REWARD_COLOR)
    feedback2.setText("%.2f" % cum_reward)  # cumulative reward
    feedback3.setText(("%5.0f" % rt + " ms") if rt is not None else "")  # RT in ms
    current_block = trial_data["block"]
    trial_in_block = trial_data["trial_in_block"]
    n_in_block = (n_warmup_first + n_trials_per_block) if current_block == 1 else (n_warmup_other + n_trials_per_block)
    feedback4.setText("Block  " + str(current_block) + " / " + str(n_blocks) + "     Trial  " + str(trial_in_block) + " / " + str(n_in_block))  # block & trial info

    feedback1.draw()
    feedback2.draw()
    feedback3.draw()
    feedback4.draw()
    fixation.draw()
    win.flip()
    core.wait(DEBUG_DURATION if is_debug else FEEDBACK_WAIT_TIME)
    end_trial_time = clock.getTime()

    # Build paradigm-style trial row
    row = _build_trial_row(
        position_to_color_id=position_to_color_id,
        position_to_reward=position_to_reward,
        colors_shown=colors_shown,
        selected_position=selected_position,
        selected_color=selected_color,
        pressed_key=pressed_key,
        keys=keys,
        cue_time=cue_time,
        response_time=response_time,
        end_trial_time=end_trial_time,
        actual_reward=actual_reward,
        max_reward=max_reward,
        cum_reward=cum_reward,
        session=session_idx,
        block=trial_data["block"],
        trial_index=trial_index,
        warm_up=trial_data["warm_up"],
        trial_start_jitter_time_ms=trial_start_jitter_time_ms,
    )

    df = pd.DataFrame([row])
    df.to_csv(str(out_path), mode="w" if first_trial_save else "a", header=first_trial_save, index=False)
    first_trial_save = False

    trial_index += 1

# =============================================================================
# FLIP 4: END MESSAGE (only when experiment completes normally)
# =============================================================================
if completed_normally:
    end_text.draw()
    win.flip()
    event.waitKeys()

win.close()
core.quit()

