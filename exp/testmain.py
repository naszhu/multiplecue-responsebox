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
# For each trial, assign which cues are shown (1-4) and their values
total_trials = NUM_SESSIONS * NUM_TRIALS_PER_SESSION
trial_cues = []  # For each trial: list of which cues are shown (e.g., [1, 3] means cues 1 and 3)
for _ in range(total_trials):
    # Randomly select 1-2 cues to show per trial
    num_cues = random.randint(1, 2)
    cues = random.sample([1, 2, 3, 4], num_cues)
    trial_cues.append(sorted(cues))

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
        # Get pre-assigned cues for this trial
        cues_shown = trial_cues[trial_index]
        
        # Show fixation
        fixation.draw()
        win.flip()
        core.wait(FIXATION_WAIT_TIME)
        
        # Reset clock before showing cues
        clock.reset()
        
        # Set reward numbers in circles (always show 1, 2, 3, 4 in each circle)
        for i, (outer, inner, text) in enumerate(cue_stimuli):
            text.setText(str(CUE_VALUE[i]))  # Show reward value (1, 2, 3, or 4)
        
        # Show all cues and record presentation time
        for outer, inner, text in cue_stimuli:
            outer.draw()  # Draw colored outer circle
            inner.draw()   # Draw white inner circle
            text.draw()    # Draw reward number
        win.flip()
        cue_time = clock.getTime()  # Record when cues appear
        
        # Wait for response
        event.clearEvents()
        keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)
        
        # Calculate rewards
        expected_reward = sum(CUE_VALUE[c - 1] for c in cues_shown)
        max_reward = max(CUE_VALUE[c - 1] for c in cues_shown)
        actual_reward = 0
        
        if keys:
            pressed_key = keys[0][0]  # Key name
            response_time = keys[0][1]  # Response time timestamp
            
            # Check for escape key
            if pressed_key == 'escape':
                break
            
            rt = (response_time - cue_time) * 1000  # RT in milliseconds
            selected_cue = response_keys.index(pressed_key) + 1  # Cue number (1-4)
            
            # Check if selected cue was shown
            if selected_cue in cues_shown:
                actual_reward = CUE_VALUE[selected_cue - 1]
        else:
            rt = MAX_WAIT_TIME * 1000  # Timeout
        
        # Update cumulative reward
        cum_reward += actual_reward * REWARD_MONEY_FACTOR
        cum_reward = round(cum_reward, 2)
        
        # Show feedback
        feedback1.text = f"{expected_reward} / {max_reward}"
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

