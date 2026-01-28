"""
Simple Multiple Cue Paradigm Demo with Reward System
Multiple colored circles (cues) appear on screen at different locations.
Press corresponding key to respond.
"""
import math
import random
from psychopy import logging, visual, core, event, monitors

logging.console.setLevel(logging.DEBUG)

# Constants (defaults from IntentionSelectionParadigmPyExp251118a colored-cue paradigm)
NUM_SESSIONS = 1
NUM_TRIALS_PER_SESSION = 5
MAX_WAIT_TIME = 5.0
FIXATION_WAIT_TIME = 1.0
FEEDBACK_WAIT_TIME = 1.5
REWARD_MONEY_FACTOR = 0.1

# Stimulus colors RGB (-1 to 1): Red, Green, Blue, Yellow
STIMULUS_TARGET_COLORS_RGB = [(1, -1, -1), (-1, 1, -1), (-1, -1, 1), (1, 1, -1)]
CUE_TEXT_COLOR = (-1, -1, -1)
CUE_BG_COLOR = (1, 1, 1)
BG_COLOR = (0, 0, 0)

# Cue positions (x, y) in deg: 4 locations at 2 deg, rotation 45° (TargetDistance=2, TargetLocRotation=pi/4)
TARGET_DISTANCE_DEG = 2.0
TARGET_LOC_ROTATION_RAD = math.pi / 4
POSITIONS_DEG = [
    (TARGET_DISTANCE_DEG * math.cos(0 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD),
     TARGET_DISTANCE_DEG * math.sin(0 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD)),
    (TARGET_DISTANCE_DEG * math.cos(1 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD),
     TARGET_DISTANCE_DEG * math.sin(1 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD)),
    (TARGET_DISTANCE_DEG * math.cos(2 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD),
     TARGET_DISTANCE_DEG * math.sin(2 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD)),
    (TARGET_DISTANCE_DEG * math.cos(3 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD),
     TARGET_DISTANCE_DEG * math.sin(3 * 2 * math.pi / 4 + TARGET_LOC_ROTATION_RAD)),
]

# Stimulus sizes (deg): ColorTargetSize=0.8 radius, CueBoxSize/2=0.35, CueTextSize=0.56 (StimFactor=0.04, CueScaleFactor=0.7)
CUE_OUTER_RADIUS_DEG = 0.8   # ColorTargetSize
CUE_INNER_RADIUS_DEG = 0.35  # CueBoxSize/2
CUE_TEXT_HEIGHT_DEG = 0.56   # CueTextSize
STIMULUS_OPACITY = 1.0       # Paradigm StimulusOpacity

# Cue number text (match paradigm TextStim: ori=0, no font/bold, antialias, center anchor)
CUE_TEXT_ORI = 0
CUE_TEXT_ANTIALIAS = True
FIXATION_SIZE_DEG = 0.16     # FixationSize = 4*StimFactor
FEEDBACK_LETTER_SIZE_DEG = 2.0   # FeedbackLetterSize = 50*StimFactor
FEEDBACK2_POS_DEG = (0, -2.8)    # pos=(0, -70*StimFactor)
INSTRUCTION_LETTER_SIZE_DEG = 0.6  # InstructionLetterSize = 15*StimFactor

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

# Cue values: Cue 1=1pt, Cue 2=2pt, Cue 3=3pt, Cue 4=4pt
CUE_VALUE = [1, 2, 3, 4]

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

# Response keys corresponding to each position
response_keys = ['1', '2', '3', '4']

# Colors for each cue (from paradigm StimulusTargetColorsRGB)
cue_colors = STIMULUS_TARGET_COLORS_RGB

# Create donut-shaped cues (colored outer circle with white inner circle)
cue_stimuli = []  # list of tuples (outer, inner, text)
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

# Pre-assign all reward values for all trials (all sessions)
# For each trial, assign which cues appear at which positions
total_trials = NUM_SESSIONS * NUM_TRIALS_PER_SESSION
trial_cue_positions = []  # For each trial: dict mapping position_index -> cue_number (or None)
for _ in range(total_trials):
    # Randomly select 1-2 cues to show per trial
    num_cues = random.randint(1, 2)
    cues = random.sample([1, 2, 3, 4], num_cues)
    # Randomly assign these cues to positions (0-3)
    available_positions = random.sample([0, 1, 2, 3], num_cues)
    # Create mapping: position -> cue_number
    position_to_cue = {}
    for pos_idx in range(4):
        if pos_idx in available_positions:
            cue_idx = available_positions.index(pos_idx)
            position_to_cue[pos_idx] = cues[cue_idx]
        else:
            position_to_cue[pos_idx] = None  # No cue at this position
    trial_cue_positions.append(position_to_cue)

# Fixation point
fixation = visual.TextStim(win, text="+", color="white", height=FIXATION_SIZE_DEG)

# Feedback texts (match paradigm: Feedback1 at (0,0), Feedback2 at (0,-70*StimFactor))
feedback1 = visual.TextStim(win, text="", color="white", pos=(0, 0), height=FEEDBACK_LETTER_SIZE_DEG)
feedback2 = visual.TextStim(win, text="", color="white", pos=FEEDBACK2_POS_DEG, height=FEEDBACK_LETTER_SIZE_DEG * 0.6)

# Instructions (InstructionLetterSize = 15*StimFactor)
instructions = visual.TextStim(
    win,
    text="Press 1, 2, 3, or 4 to respond to the cue at that location\n\nPress SPACE to start",
    color="white",
    height=INSTRUCTION_LETTER_SIZE_DEG,
)

# Show instructions
instructions.draw()
win.flip()
event.waitKeys(keyList=['space'])

# Create clock for response time measurement
clock = core.Clock()

# Initialize cumulative reward
cum_reward = 0.0
trial_index = 0

# Run sessions
for session in range(NUM_SESSIONS):
    # Reset cumulative reward at start of new session
    if session > 0:
        cum_reward = 0.0
    
    # Run trials in this session
    for trial_in_session in range(NUM_TRIALS_PER_SESSION):
        # Get pre-assigned position-to-cue mapping for this trial
        position_to_cue = trial_cue_positions[trial_index]
        
        # Get list of cues shown in this trial (for reward calculation)
        cues_shown = [cue for cue in position_to_cue.values() if cue is not None]
        
        # Show fixation
        fixation.draw()
        win.flip()
        core.wait(FIXATION_WAIT_TIME)
        
        # Reset clock before showing cues
        clock.reset()
        
        # Set reward numbers in circles (only show numbers at positions where cues are assigned)
        for pos_idx, (outer, inner, text) in enumerate(cue_stimuli):
            cue_num = position_to_cue[pos_idx]  # Get cue number at this position (or None)
            if cue_num is not None:
                text.setText(str(CUE_VALUE[cue_num - 1]))  # Show reward value for this cue
            else:
                text.setText("")  # No cue at this position, no number
        
        # Show all cues and record presentation time
        for outer, inner, text in cue_stimuli:
            outer.draw()  # Draw colored outer circle
            inner.draw()   # Draw white inner circle
            text.draw()    # Draw reward number (only if cue is shown at this position)
        win.flip()
        cue_time = clock.getTime()  # Record when cues appear
        
        # Wait for response
        event.clearEvents()
        keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)
        
        # Calculate max reward (highest cue value among cues shown in this trial)
        max_reward = max(CUE_VALUE[c - 1] for c in cues_shown)
        actual_reward = 0
        
        if keys:
            pressed_key = keys[0][0]  # Key name
            response_time = keys[0][1]  # Response time timestamp
            
            # Check for escape key
            if pressed_key == 'escape':
                break
            
            rt = (response_time - cue_time) * 1000  # RT in milliseconds
            selected_position = response_keys.index(pressed_key)  # Position index (0-3)
            selected_cue = position_to_cue[selected_position]  # Get cue number at selected position
            
            # Calculate actual reward: value of the cue at the selected position
            # Only give reward if the selected position has a cue (was shown in this trial)
            if selected_cue is not None and selected_cue in cues_shown:
                actual_reward = CUE_VALUE[selected_cue - 1]
        else:
            rt = MAX_WAIT_TIME * 1000  # Timeout
        
        # Update cumulative reward
        cum_reward += actual_reward * REWARD_MONEY_FACTOR
        cum_reward = round(cum_reward, 2)
        
        # Show feedback: ActualReward / MaxReward
        feedback1.text = f"{actual_reward} / {max_reward}"
        feedback1.color = "red" if actual_reward == 0 else "green"
        feedback2.text = f"{cum_reward:.2f}"
        
        feedback1.draw()
        feedback2.draw()
        win.flip()
        core.wait(FEEDBACK_WAIT_TIME)
        
        trial_index += 1

# End message
end_text = visual.TextStim(win, text="Demo complete!\n\nPress any key to exit", color="white", height=FEEDBACK_LETTER_SIZE_DEG)
end_text.draw()
win.flip()
event.waitKeys()

win.close()
core.quit()

