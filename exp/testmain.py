"""
Simple Multiple Cue Paradigm Demo with Reward System
Multiple colored circles (cues) appear on screen at different locations.
Press corresponding key to respond.
"""
from psychopy import logging

logging.console.setLevel(logging.DEBUG)

# Constants
NUM_SESSIONS = 1
NUM_TRIALS_PER_SESSION = 5
MAX_WAIT_TIME = 5.0
FIXATION_WAIT_TIME = 1.0
FEEDBACK_WAIT_TIME = 1.5
REWARD_MONEY_FACTOR = 0.1

# Cue values: Cue 1=1pt, Cue 2=2pt, Cue 3=3pt, Cue 4=4pt
CUE_VALUE = [1, 2, 3, 4]

from psychopy import visual, core, event
import random

# Create window
win = visual.Window(size=[800, 600], fullscr=False, color="black", units="pix")

# Define cue positions (4 locations)
positions = [
    (-200, 200),   # Top-left
    (200, 200),    # Top-right
    (-200, -200),  # Bottom-left
    (200, -200)    # Bottom-right
]

# Response keys corresponding to each position
response_keys = ['1', '2', '3', '4']

# Colors for each cue (RGB values from -1 to 1)
cue_colors = [
    (1, 0, 0),    # Red
    (0, 1, 0),    # Green
    (0, 0, 1),    # Blue
    (1, 1, 0)     # Yellow
]

# Create donut-shaped cues (colored outer circle with white inner circle)
cue_stimuli = []  # list of tuples (outer, inner, text)
for i, pos in enumerate(positions):
    outer = visual.Circle(win, radius=30, fillColor=cue_colors[i], lineColor=None, pos=pos)
    inner = visual.Circle(win, radius=15, fillColor=(1, 1, 1), lineColor=None, pos=pos)  # White inner circle
    text = visual.TextStim(win, text="", color=(-1, -1, -1), pos=pos, height=20, font='Arial Bold', bold=True)
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
fixation = visual.TextStim(win, text="+", color="white", height=30)

# Feedback texts
feedback1 = visual.TextStim(win, text="", color="white", pos=(0, 70), height=30)  # Expected/Max reward
feedback2 = visual.TextStim(win, text="", color="white", pos=(0, -70), height=30)  # Cumulative reward

# Instructions
instructions = visual.TextStim(
    win, 
    text="Press 1, 2, 3, or 4 to respond to the cue at that location\n\nPress SPACE to start",
    color="white",
    height=25
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
end_text = visual.TextStim(win, text="Demo complete!\n\nPress any key to exit", color="white", height=30)
end_text.draw()
win.flip()
event.waitKeys()

win.close()
core.quit()

