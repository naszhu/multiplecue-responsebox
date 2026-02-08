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
from pathlib import Path

import pandas as pd
from psychopy import gui, logging, visual, core, event, monitors

logging.console.setLevel(logging.DEBUG)

# Constants 
EXPERIMENT_NAME = "CCP"
EXPERIMENT_NUMBER = 1001
MAX_SESSION = 5  # Session 1 = practice (single cue center), Session 2+ = full
# Per-session config (index = session - 1). Session 1 = practice, 2+ = full.
NUM_TRIALS_PER_SESSION = [20, 5, 5, 5, 5]  # Session 1: 20 single-cue, 2-5: multi-cue
PRAC_SHOW_ALL_TARGETS = [False, True, True, True, True]  # Session 1: single cue center
PRAC_SHOW_COLOR_RESPONSE_MAP = [True, False, False, False, False]  # Session 1: color map at bottom
MAX_WAIT_TIME = 5.0
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


# Session dialog: run one session per launch
session_dlg = gui.Dlg(title="CCRP Session")
session_dlg.addField("Session", initial=1, choices=[1, 2, 3, 4, 5])
session_dlg.show()
if not session_dlg.OK:
    raise SystemExit("Session dialog cancelled")
SESSION = int(session_dlg.data[0])  # 1-based session number


def _build_trial_row(
    *,
    position_to_cueid,
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
    trial_index,
):
    """Build a trial data row """
    # Cue layout: 4-digit strings (position 0..3)
    cues = [position_to_cueid[i] or 0 for i in range(NUM_POSITIONS)]
    cue_vals = [CUE_REWARD_VALUES[c - 1] if c else 0 for c in cues]
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
    exp_reward = CUE_REWARD_VALUES[cue_exp - 1] if cue_exp > 0 else 0

    cond = f"{len(cues_shown)}cue"

    return {
        "ExperimentName": EXPERIMENT_NAME,
        "ExperimentNumber": EXPERIMENT_NUMBER,
        "Subject": "",
        "Session": session + 1,
        "Block": session + 1,
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
color_response_squares = []
for i in range(NUM_POSITIONS):
    x = RESPONSE_COLOR_DISTANCE_DEG * (-NUM_POSITIONS / 2 + 0.5 + i)
    rect = visual.Rect(
        win,
        width=RESPONSE_COLOR_SIZE_DEG,
        height=RESPONSE_COLOR_SIZE_DEG,
        fillColor=STIMULUS_TARGET_COLORS_RGB[i],
        lineColor=None,
        pos=(x, COLOR_MAP_Y_DEG),
        units=USE_UNITS,
    )
    color_response_squares.append(rect)

# -----------------------------------------------------------------------------
# trial_cue_positions: list of dicts, one per trial
#   Each dict = position_to_cueid: {0: cue_or_None, 1: ..., 2: ..., 3: ...}
#   Session 1: each trial has one cue at position 0 (drawn at center)
#   Session 2+: each trial has 1–2 cues at random positions
# -----------------------------------------------------------------------------
session_idx = SESSION - 1
n_trials = NUM_TRIALS_PER_SESSION[session_idx]
total_trials = n_trials
trial_cue_positions = []
for _ in range(n_trials):
    if PRAC_SHOW_ALL_TARGETS[session_idx]:
        num_cues = random.randint(1, 2)
        cues = random.sample([1, 2, 3, 4], num_cues)
        available_positions = random.sample([0, 1, 2, 3], num_cues)
    else:
        cue = random.choice([1, 2, 3, 4])
        cues = [cue]
        available_positions = [0]
    position_to_cueid = {}
    for pos_idx in range(4):
        if pos_idx in available_positions:
            cue_idx = available_positions.index(pos_idx)
            position_to_cueid[pos_idx] = cues[cue_idx]
        else:
            position_to_cueid[pos_idx] = None
    trial_cue_positions.append(position_to_cueid)

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
feedback1 = visual.TextStim(win, text="", pos=FEEDBACK1_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
feedback2 = visual.TextStim(win, text="", pos=FEEDBACK2_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.6, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
feedback3 = visual.TextStim(win, text="", pos=FEEDBACK3_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.6, color=(1, 1, 1), units=USE_UNITS, opacity=STIMULUS_OPACITY)
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

for trial_in_session in range(n_trials):
    # -------------------------------------------------------------------------
    # position_to_cueid: dict mapping position_index -> cue_id (or None)
    #   Keys: 0,1,2,3 = screen positions (0=top-right, 1=top-left, 2=bottom-left, 3=bottom-right)
    #   Values: 1,2,3,4 = cue/color (1=Red, 2=Green, 3=Blue, 4=Yellow) or None if no cue there
    #   Example Session 1: {0: 3, 1: None, 2: None, 3: None}  → only Blue circle at center
    #   Example Session 2: {0: 1, 1: 4, 2: None, 3: None}     → Red top-right, Yellow top-left
    # -------------------------------------------------------------------------
    position_to_cueid = trial_cue_positions[trial_index]

    # cues_shown: list of cue_ids actually displayed this trial
    #   Example: [3] for Session 1 single cue; [1, 4] for two cues
    cues_shown = [cue for cue in position_to_cueid.values() if cue is not None]

    # =========================================================================
    # FLIP A: FIXATION SCREEN
    # =========================================================================
    # Presented: Black fixation dot at center (nothing else)
    # Duration: FIXATION_WAIT_TIME (1.0 s)
    fixation.draw()
    win.flip()
    core.wait(FIXATION_WAIT_TIME)

    clock.reset()

    # Session 1: single cue at center + color map at bottom
    # Session 2+: 1–2 cues at fixed positions (no color map)
    is_session1 = not PRAC_SHOW_ALL_TARGETS[session_idx]
    show_color_map = PRAC_SHOW_COLOR_RESPONSE_MAP[session_idx]

    # Set reward digit (1–4) in each circle's text stim. Only positions with a cue get text.
    for pos_idx, (outer, inner, text) in enumerate(cue_stimuli):
        cue_id = position_to_cueid[pos_idx]  # e.g. 3 for Blue, or None
        if cue_id is not None:
            text.setText(str(CUE_REWARD_VALUES[cue_id - 1]))  # e.g. cue 3 → "3" (CUE_REWARD_VALUES[2])
        else:
            text.setText("")

    # =========================================================================
    # FLIP B: CUE/STIMULUS SCREEN (stays until keypress or timeout)
    # =========================================================================
    # Presented:
    #   Session 1: One colored circle at center + digit (1–4) + 4 color squares at bottom
    #   Session 2+: 1–2 colored circles at fixed positions + digits
    #   Always: Fixation dot in center
    # -------------------------------------------------------------------------
    fixation.draw()
    if is_session1:
        # cue_id: which cue/color (1–4). position_to_cueid[0] = the single cue (Session 1 uses pos 0 only)
        cue_id = position_to_cueid[0]
        # cue_stimuli[cue_id-1]: the (outer, inner, text) for that color
        #   cue_id 1→Red, 2→Green, 3→Blue, 4→Yellow
        outer, inner, text = cue_stimuli[cue_id - 1]
        text.setText(str(CUE_REWARD_VALUES[cue_id - 1]))  # reward digit in center circle
        orig_pos = outer.pos
        for stim in (outer, inner, text):
            stim.setPos((0, 0))  # move to center
        outer.draw()
        inner.draw()
        text.draw()
        for stim in (outer, inner, text):
            stim.setPos(orig_pos)  # restore for next trial
        if show_color_map:
            for rect in color_response_squares:
                rect.draw()
    else:
        for outer, inner, text in cue_stimuli:
            outer.draw()
            inner.draw()
            text.draw()
    win.flip()
    cue_time = clock.getTime()

    # -------------------------------------------------------------------------
    # Wait for key response (D/C/K/M or escape). Screen stays at FLIP B until then.
    # -------------------------------------------------------------------------
    event.clearEvents()
    keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)

    max_reward = max(CUE_REWARD_VALUES[c - 1] for c in cues_shown)
    actual_reward = 0
    pressed_key = ""
    rt = None
    selected_position = None  # 0–3 = color index (Red, Green, Blue, Yellow)
    selected_cue = None      # 1–4 = cue_id of selected cue, or None
    response_time = cue_time

    if keys:
        pressed_key = keys[0][0]
        response_time = keys[0][1]
        if pressed_key == 'escape':
            break

        rt = (response_time - cue_time) * 1000
        # selected_position: which color key was pressed (0=Red/D, 1=Green/C, 2=Blue/K, 3=Yellow/M)
        selected_position = response_keys.index(pressed_key)
        if is_session1:
            # Session 1: key = color choice directly. selected_position 0→cue 1, 1→cue 2, etc.
            selected_cue = selected_position + 1
        else:
            # Session 2+: key = position. selected_cue = cue at that position (or None)
            selected_cue = position_to_cueid[selected_position]

        if selected_cue is not None and selected_cue in cues_shown:
            actual_reward = CUE_REWARD_VALUES[selected_cue - 1]
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
    feedback1.setText(str(actual_reward) + " / " + str(max_reward))
    feedback1.setColor(FEEDBACK_ZERO_REWARD_COLOR if actual_reward == 0 else FEEDBACK_POS_REWARD_COLOR)
    feedback2.setText("%.2f" % cum_reward)
    feedback3.setText(("%5.0f" % rt + " ms") if rt is not None else "")
    feedback4.setText("Block  " + str(SESSION) + "               Trial  " + str(trial_index + 1) + " / " + str(total_trials))

    feedback1.draw()
    feedback2.draw()
    feedback3.draw()
    feedback4.draw()
    fixation.draw()
    win.flip()
    core.wait(FEEDBACK_WAIT_TIME)
    end_trial_time = clock.getTime()

    # Build paradigm-style trial row
    row = _build_trial_row(
        position_to_cueid=position_to_cueid,
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

