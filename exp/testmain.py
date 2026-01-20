"""
Simple Multiple Cue Paradigm Demo
Multiple colored circles (cues) appear on screen at different locations.
Press corresponding key to respond.
"""
from psychopy import logging

logging.console.setLevel(logging.DEBUG)

MAX_WAIT_TIME = 5.0
FIXATION_WAIT_TIME = 1.0
FEEDBACK_WAIT_TIME = 1.5
NUM_TRIALS = 5

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

# Create donut-shaped cues (colored outer circle with white inner circle and random number)
cue_stimuli = []
for i, pos in enumerate(positions):
    outer = visual.Circle(win, radius=30, fillColor=cue_colors[i], lineColor=None, pos=pos)
    inner = visual.Circle(win, radius=15, fillColor=(1, 1, 1), lineColor=None, pos=pos)  # White inner circle
    text = visual.TextStim(win, text=str(random.randint(1, 9)), color=(-1, -1, -1), pos=pos, height=20, font='Arial Bold', bold=True)  # Bold black random number
    cue_stimuli.append((outer, inner, text))  # Store as tuple

# Fixation point
fixation = visual.TextStim(win, text="+", color="white", height=30)

# Feedback text
feedback = visual.TextStim(win, text="", color="white", pos=(0, -300), height=30)

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

# Run trials
for trial in range(NUM_TRIALS):
    # Show fixation
    fixation.draw()
    win.flip()
    core.wait(FIXATION_WAIT_TIME)
    
    # Reset clock before showing cues
    clock.reset()
    
    # Generate new random numbers for each cue
    for outer, inner, text in cue_stimuli:
        text.setText(str(random.randint(1, 9)))  # Update with new random number
    
    # Show all cues and record presentation time
    for outer, inner, text in cue_stimuli:
        outer.draw()  # Draw colored outer circle
        inner.draw()   # Draw white inner circle
        text.draw()    # Draw random number text
    win.flip()
    cue_time = clock.getTime()  # Record when cues appear (should be ~0)
    
    # Wait for response
    event.clearEvents()
    keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=MAX_WAIT_TIME, timeStamped=clock)
    
    # print(keys)
    if keys:
        pressed_key = keys[0][0]  # Key name
        response_time = keys[0][1]  # Response time timestamp
        
        # Check for escape key
        if pressed_key == 'escape':
            break
        
        rt = (response_time - cue_time) * 1000  # RT in milliseconds
        cue_index = response_keys.index(pressed_key)
        feedback.text = f"Trial {trial+1}: Position {cue_index+1} (key: {pressed_key})\nRT: {rt:.1f} ms"
    else:
        feedback.text = f"Trial {trial+1}: Timeout - no response"
    
    # Show feedback
    feedback.pos = (0, 0)  # Ensure feedback is centered
    feedback.draw()
    win.flip()
    core.wait(FEEDBACK_WAIT_TIME)

# End message
end_text = visual.TextStim(win, text="Demo complete!\n\nPress any key to exit", color="white", height=30)
end_text.draw()
win.flip()
event.waitKeys()

win.close()
core.quit()

