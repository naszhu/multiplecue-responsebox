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

# Constants 
EXPERIMENT_NAME = "CCP"
EXPERIMENT_NUMBER = 1001
MAX_SESSION = 6  # Session 6+ uses experimental config (4 blocks × 50 trials)
# Per-session config (index = session - 1). Session 6+ uses config index 5.
# S1: center only, color map. S2–6: 4 circles, 1–2 rewards. reward_from_cue: True = reward = CUE_REWARD_VALUES[color]; False = reward random 1–4.
SESSION_CONFIG = [
    {"cue_set": [[1], [2], [3], [4]], "n_blocks": 5, "n_per_block": 20, "center": True, "color_map": True, "reward_from_cue": True},
    {"cue_set": [[1], [2], [3], [4]], "n_blocks": 5, "n_per_block": 20, "center": False, "color_map": True, "reward_from_cue": False},
    {"cue_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 30, "center": False, "color_map": True, "reward_from_cue": True},
    {"cue_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 30, "center": False, "color_map": False, "reward_from_cue": True},
    {"cue_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 2, "n_per_block": 50, "center": False, "color_map": False, "reward_from_cue": True},
    {"cue_set": [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]], "n_blocks": 4, "n_per_block": 50, "center": False, "color_map": False, "reward_from_cue": True},
]
MAX_WAIT_TIME = 2.0
FIXATION_WAIT_TIME = 1.0
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

# Reward values (points) per cue: cue 1→1pt, cue 2→2pt, cue 3→3pt, cue 4→4pt
CUE_REWARD_VALUES = [1, 2, 3, 4]

# Color-to-key mapping: Red→D, Green→C, Blue→K, Yellow→M
# Position i has fixed color: 0=Red, 1=Green, 2=Blue, 3=Yellow (from STIMULUS_TARGET_COLORS_RGB)
COLOR_KEYS = ['d', 'c', 'k', 'm']  # key for position/color 0,1,2,3

NUM_POSITIONS = 4


# Session dialog: run one session per launch (6+ uses experimental config: 4 blocks × 50 trials)
session_dlg = gui.Dlg(title="CCRP Session")
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
SESSION = int(session_dlg.data[0])  # 1-based session number
COLOR_MAP_LAYOUT = session_dlg.data[1]  # "horizontal" or "keyboard"


def _session_config_idx(session: int) -> int:
    """Return config index for session (1-based). Session 6+ uses same config as session 6."""
    return min(session - 1, 5)


def _build_trial_row(
    *,
    position_to_cueid,
    position_to_reward,
    cues_shown,
    selected_position,
    selected_cue,
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
):
    """Build a trial data row """
    # Cue layout: 4-digit strings (position 0..3). Rewards from position_to_reward.
    cues = [position_to_cueid[i] or 0 for i in range(NUM_POSITIONS)]
    cue_vals = [position_to_reward[i] or 0 for i in range(NUM_POSITIONS)]
    sorted_cues = sorted(cues)
    cue_ranks = [NUM_POSITIONS - sorted_cues.index(cues[i]) for i in range(NUM_POSITIONS)]

    # Response
    sel = selected_position
    has_response = sel is not None
    resp_loc = "".join("1" if i == sel else "0" for i in range(NUM_POSITIONS)) if has_response else "0000"
    point_target = (sel + 1) if has_response else 0
    sel_cue = position_to_cueid[sel] if has_response else None

    rt_sec = (response_time - cue_time) if (keys and pressed_key != "escape") else (MAX_WAIT_TIME if not keys else 0.0)
    late = rt_sec > RESPONSE_DEADLINE
    intr = 1 if (has_response and sel_cue is None) else 0
    # ACC: correct = selected the circle with highest reward (color-to-key rule)
    acc = 1 if actual_reward == max_reward and max_reward > 0 else 0
    cue_exp = sel_cue if (has_response and sel_cue) else 0
    cue_rank_resp = cue_ranks[sel] if (has_response and sel_cue) else 0
    pos_with_selected_color = next((p for p in range(NUM_POSITIONS) if position_to_cueid.get(p) == (sel + 1)), None)
    exp_reward = (position_to_reward[pos_with_selected_color] or 0) if (has_response and pos_with_selected_color is not None) else 0

    cond = f"{len(cues_shown)}cue"

    return {
        "ExperimentName": EXPERIMENT_NAME,
        "ExperimentNumber": EXPERIMENT_NUMBER,
        "ColorMapLayout": COLOR_MAP_LAYOUT,
        "Subject": "",
        "Session": session + 1,
        "Block": block,
        "Trial": trial_index + 1,
        "WarmUpTrial": 0,
        "CueCondition": cond,
        "NumCues": len(cues_shown),
        "TrialStartJitterTime": FIXATION_WAIT_TIME,
        "CueSOA": 0,
        "Cues": "".join(str(c) for c in cues),
        "CueValues": "".join(str(v) for v in cue_vals),
        "CueRanks": "".join(str(r) for r in cue_ranks),
        "Response": pressed_key or "timeout",

        # RespLoc: 4-digit one-hot of selected color (CCRP: pos0=Red, pos1=Green, pos2=Blue, pos3=Yellow)
        "RespLoc": resp_loc,
        # PointTargetResponse: 1=Red, 2=Green, 3=Blue, 4=Yellow, 0=timeout
        "PointTargetResponse": point_target, 
        
        "RT": round(rt_sec, 4),
        "LateResponse": late, # 1 - late response, 0 - on time response
        "ACC": acc, # 1 - correct response, 0 - incorrect response
        "INTR": intr, # 1 - intrusion error, 0 - no intrusion error, if respond to the non-cued position
        "CueResponseValue": actual_reward, # the value of the cue at the selected position
        "CueResponseExpValue": cue_exp, # the expected value of the cue at the selected position
        "CueRankResponse": cue_rank_resp, # the rank of the cue at the selected position
        "ExpectedReward": exp_reward, # the expected reward for the trial
        "Reward": actual_reward, # the actual reward for the trial
        "MaxReward": max_reward, # the maximum reward for the trial
        "CumReward": cum_reward, # the cumulative reward for the session
        "CueTime": round(cue_time, 4),
        "PointTargetTime": round(cue_time + rt_sec, 4) if rt_sec else round(cue_time, 4),
        "ColorTargetTime": round(cue_time, 4),
        "EndTrialTime": round(end_trial_time, 4),
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
total_trials = n_blocks * n_trials_per_block


def _pool_for_cue(cue, cfg: dict) -> list:
    """
    Enumerate all trial variants for a cue condition.

    All sessions use the same output format:
      {"position_to_cueid": {0..3: cue_id or None},
       "position_to_reward": {0..3: reward (1–4) or None}}

    - position_to_cueid: which color at each screen position
    - position_to_reward: reward displayed at each position (None = no reward shown)
    - reward_from_cue: True = reward = CUE_REWARD_VALUES[color]; False = reward random 1–4
    """
    def make_trial(cueid: dict, reward: dict) -> dict:
        return {"position_to_cueid": cueid, "position_to_reward": reward}

    # -------------------------------------------------------------------------
    # SESSION 1: one cue at center. position 0 = center; others unused.
    # -------------------------------------------------------------------------
    if cfg["center"]:
        cid = cue[0]
        cueid = {0: cid, 1: None, 2: None, 3: None}
        reward = {0: CUE_REWARD_VALUES[cid - 1], 1: None, 2: None, 3: None}
        return [make_trial(cueid, reward)]

    # -------------------------------------------------------------------------
    # SESSIONS 2–6: 4 color circles at 4 positions (shuffled). 1 or 2 show reward.
    # Single cue: color cue[0] at one pos gets reward; other 3 colors, no reward.
    # Two cues: colors cue[0], cue[1] at two pos get rewards; other 2 colors, no reward.
    # reward_from_cue: True = reward = CUE_REWARD_VALUES[color]; False = reward random 1–4.
    # -------------------------------------------------------------------------
    reward_from_cue = cfg.get("reward_from_cue", True)
    if len(cue) == 1:
        cid = cue[0]
        reward_values = [CUE_REWARD_VALUES[cid - 1]] if reward_from_cue else list(range(1, 5))
        other_colors = [c for c in [1, 2, 3, 4] if c != cid]
        out = []
        for reward_pos in range(4):
            for perm in permutations(other_colors):
                for r in reward_values:
                    position_to_cueid = {0: None, 1: None, 2: None, 3: None}
                    position_to_cueid[reward_pos] = cid
                    other_positions = [p for p in range(4) if p != reward_pos]
                    for i, pos in enumerate(other_positions):
                        position_to_cueid[pos] = perm[i]
                    position_to_reward = {0: None, 1: None, 2: None, 3: None}
                    position_to_reward[reward_pos] = r
                    out.append(make_trial(position_to_cueid, position_to_reward))
        return out
    a, b = cue[0], cue[1]
    ra, rb = CUE_REWARD_VALUES[a - 1], CUE_REWARD_VALUES[b - 1]
    other_colors = [c for c in [1, 2, 3, 4] if c not in (a, b)]
    out = []
    for pa, pb in permutations(range(4), 2):
        for perm in permutations(other_colors):
            position_to_cueid = {0: None, 1: None, 2: None, 3: None}
            position_to_cueid[pa], position_to_cueid[pb] = a, b
            other_positions = [p for p in range(4) if p not in (pa, pb)]
            for i, pos in enumerate(other_positions):
                position_to_cueid[pos] = perm[i]
            position_to_reward = {0: None, 1: None, 2: None, 3: None}
            position_to_reward[pa], position_to_reward[pb] = ra, rb
            out.append(make_trial(position_to_cueid, position_to_reward))
    return out


def _build_trials(cfg: dict) -> list:
    """Cue-balanced: each condition reps per block. Sample color/reward randomly."""
    cue_set, n_blocks, n_per_block = cfg["cue_set"], cfg["n_blocks"], cfg["n_per_block"]
    reps = n_per_block // len(cue_set)
    pools = [_pool_for_cue(c, cfg) for c in cue_set]
    trials = []
    for _ in range(n_blocks):
        block = [random.choice(p) for p in pools for _ in range(reps)]
        random.shuffle(block)
        trials.extend(block)
    return trials


trial_cue_positions = _build_trials(cfg)

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

# Instructions (InstructionLetterSize = 15*StimFactor)
INSTRUCTION_DIR = Path(__file__).resolve().parent.parent / "others" / "To Lea 251127" / "Instructions"
instructions = visual.TextStim(
    win,
    text="Press the key for the COLOR of the circle with the highest reward:\n"
         "Red = D, Green = C, Blue = K, Yellow = M\n\n"
         "Press SPACE to start",
    color="white",
    height=INSTRUCTION_LETTER_SIZE_DEG,
    wrapWidth=20,
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
event.waitKeys(keyList=['space'])

# Create clock for response time measurement
clock = core.Clock()

# Initialize cumulative reward and trial data log
cum_reward = 0.0
trial_index = 0
out_dir = Path(__file__).resolve().parent / "data_written"
out_dir.mkdir(parents=True, exist_ok=True)
out_path = out_dir / "trial_data.csv"
first_trial_save = True  # Write header on first trial

# =============================================================================
# TRIAL LOOP
# =============================================================================
# Each trial has 3 display phases (3 win.flip() calls):
#   FLIP A: Fixation only
#   FLIP B: Cues (stimuli) + wait for key response
#   FLIP C: Feedback (reward, RT, etc.)

for trial_in_session in range(total_trials):
    trial_data = trial_cue_positions[trial_index]
    position_to_cueid = trial_data["position_to_cueid"]
    position_to_reward = trial_data["position_to_reward"]

    # cues_shown: list of cue_ids (colors) displayed this trial
    cues_shown = [c for c in position_to_cueid.values() if c is not None]

    # =========================================================================
    # FLIP A: FIXATION SCREEN
    # =========================================================================
    # Presented: Black fixation dot at center (nothing else)
    # Duration: FIXATION_WAIT_TIME (1.0 s)
    fixation.draw()
    win.flip()
    core.wait(FIXATION_WAIT_TIME)

    clock.reset()

    show_color_map = SESSION in (1, 2, 3)

    # =========================================================================
    # FLIP B: CUE/STIMULUS SCREEN (stays until keypress or timeout)
    # =========================================================================
    fixation.draw()
    if SESSION == 1:
        # Session 1 only: draw single cue at center
        cue_id = position_to_cueid[0]
        outer, inner, text = cue_stimuli[cue_id - 1]
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
            cue_id = position_to_cueid[pos_idx]
            outer, inner, text = cue_stimuli[cue_id - 1]
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
    event.clearEvents()
    keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)

    rewards_at_positions = [r for r in position_to_reward.values() if r is not None]
    max_reward = max(rewards_at_positions) if rewards_at_positions else 0
    actual_reward = 0
    pressed_key = ""
    rt = None
    selected_position = None
    selected_cue = None
    response_time = cue_time

    if keys:
        pressed_key = keys[0][0]
        response_time = keys[0][1]
        if pressed_key == 'escape':
            break
        rt = (response_time - cue_time) * 1000
        selected_position = response_keys.index(pressed_key)
        selected_cue = selected_position + 1  # color pressed (1–4)
        # Find position with that color; reward = position_to_reward[that_pos] (0 if None)
        pos_with_color = next((p for p in range(4) if position_to_cueid.get(p) == selected_cue), None)
        actual_reward = (position_to_reward.get(pos_with_color) or 0) if pos_with_color is not None else 0
    else:
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
    current_block = (trial_index // n_trials_per_block) + 1
    trial_in_block = (trial_index % n_trials_per_block) + 1
    feedback4.setText("Block  " + str(current_block) + " / " + str(n_blocks) + "     Trial  " + str(trial_in_block) + " / " + str(n_trials_per_block))  # block & trial info

    feedback1.draw()
    feedback2.draw()
    feedback3.draw()
    feedback4.draw()
    fixation.draw()
    win.flip()
    core.wait(FEEDBACK_WAIT_TIME)
    end_trial_time = clock.getTime()

    # Build paradigm-style trial row
    current_block = (trial_index // n_trials_per_block) + 1
    row = _build_trial_row(
        position_to_cueid=position_to_cueid,
        position_to_reward=position_to_reward,
        cues_shown=cues_shown,
        selected_position=selected_position,
        selected_cue=selected_cue,
        pressed_key=pressed_key,
        keys=keys,
        cue_time=cue_time,
        response_time=response_time,
        end_trial_time=end_trial_time,
        actual_reward=actual_reward,
        max_reward=max_reward,
        cum_reward=cum_reward,
        session=session_idx,
        block=current_block,
        trial_index=trial_index,
    )

    df = pd.DataFrame([row])
    df.to_csv(out_path, mode="w" if first_trial_save else "a", header=first_trial_save, index=False)
    first_trial_save = False

    trial_index += 1

# =============================================================================
# FLIP 4: END MESSAGE
# =============================================================================
# Presented: "Demo complete! Press any key to exit"
# Waits for: Any key before closing
end_text = visual.TextStim(win, text="Demo complete!\n\nPress any key to exit", color="white", height=FEEDBACK_LETTER_SIZE_DEG)
end_text.draw()
win.flip()
event.waitKeys()

win.close()
core.quit()

