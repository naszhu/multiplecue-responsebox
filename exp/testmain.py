from psychopy import visual, core, event
import random

win = visual.Window([800,600], color='black')

# Define cue positions (4 locations as an example)
cue_positions = [(-200, 0), (200, 0), (0, 200), (0, -200)]
cue_keys = ['a', 's', 'k', 'l']  # keys for each location
cue_colors = ['red', 'green', 'blue', 'yellow']

# Create colored circle cues at each position
cues = [visual.Circle(win, pos=pos, radius=40, fillColor=color, lineColor='white', lineWidth=4) 
        for pos, color in zip(cue_positions, cue_colors)]

instr = visual.TextStim(
    win, 
    text='Press the key matching the highlighted cue color location!\na=left, s=right, k=top, l=bottom\nPress space to start.', 
    color='white'
)
instr.draw()
win.flip()
event.waitKeys(keyList=['space'])

n_trials = 5
for trial in range(n_trials):
    highlight_idx = random.randint(0, 3)
    # Draw all cues
    for idx, cue in enumerate(cues):
        if idx == highlight_idx:
            cue.lineColor = 'orange'
            cue.lineWidth = 8
        else:
            cue.lineColor = 'white'
            cue.lineWidth = 4
        cue.draw()
    win.flip()
    rt_clock = core.Clock()
    keys = event.waitKeys(keyList=cue_keys + ['escape'], timeStamped=rt_clock)
    key, rt = keys[0]
    if key == 'escape':
        break
    # Give brief feedback
    feedback = visual.TextStim(win, text="Correct!" if key == cue_keys[highlight_idx] else "Wrong!", color='white')
    feedback.draw()
    win.flip()
    core.wait(0.5)

win.close()
core.quit()


