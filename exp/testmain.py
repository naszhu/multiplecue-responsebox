"""
Simple Multiple Q Paradigm Demo
Multiple Q's appear on screen at different locations.
Press corresponding key to respond.
"""
from psychopy import visual, core, event

# Create window
win = visual.Window(size=[800, 600], fullscr=False, color="black", units="pix")

# Define Q positions (4 locations)
positions = [
    (-200, 200),   # Top-left
    (200, 200),    # Top-right
    (-200, -200),  # Bottom-left
    (200, -200)    # Bottom-right
]

# Response keys corresponding to each position
response_keys = ['1', '2', '3', '4']

# Create Q stimuli at each position
q_stimuli = []
for pos in positions:
    q = visual.TextStim(win, text="Q", color="white", pos=pos, height=50)
    q_stimuli.append(q)

# Fixation point
fixation = visual.TextStim(win, text="+", color="white", height=30)

# Feedback text
feedback = visual.TextStim(win, text="", color="white", pos=(0, -300), height=30)

# Instructions
instructions = visual.TextStim(
    win, 
    text="Press 1, 2, 3, or 4 to respond to the Q at that location\n\nPress SPACE to start",
    color="white",
    height=25
)

# Show instructions
instructions.draw()
win.flip()
event.waitKeys(keyList=['space'])

# Run 5 trials
for trial in range(5):
    # Show fixation
    fixation.draw()
    win.flip()
    core.wait(1.0)
    
    # Show all Q's
    for q in q_stimuli:
        q.draw()
    win.flip()
    
    # Wait for response
    event.clearEvents()
    keys = event.waitKeys(keyList=response_keys + ['escape'], maxWait=5.0)
    
    if keys:
        if 'escape' in keys:
            break
        
        # Find which Q was selected
        pressed_key = keys[0]
        if pressed_key in response_keys:
            q_index = response_keys.index(pressed_key)
            feedback.text = f"Trial {trial+1}: Responded to Q at position {q_index+1} (key: {pressed_key})"
        else:
            feedback.text = f"Trial {trial+1}: No valid response"
    else:
        feedback.text = f"Trial {trial+1}: Timeout - no response"
    
    # Show feedback
    feedback.draw()
    win.flip()
    core.wait(1.5)

# End message
end_text = visual.TextStim(win, text="Demo complete!\n\nPress any key to exit", color="white", height=30)
end_text.draw()
win.flip()
event.waitKeys()

win.close()
core.quit()

