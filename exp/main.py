"""
Main experiment script for Cued Color Response Paradigm (CCRP)
"""
from psychopy import core, visual, event
from psychopy.core import StaticPeriod
import sys
import os
import random as rnd
from numpy import random

# Add src directory to path - allows importing modules from src folder
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))  # String: add "exp/src" to Python path

from config import EXPERIMENT_CONFIG, WINDOW_CONFIG  # Import: configuration settings
from experiment_params import (
    ExpCueSet, PracCueSet, ExpCueSetVal, PracCueSetVal,  # Lists: cue combinations
    ExpCueSOAconds, PracCueSOAconds, ExpStimED, PracStimED,  # Integers/lists: timing parameters
    ExpNoBlocks, PracNoBlocks, ExpRepetitions, PracRepetitions,  # Integers/tuples: block configuration
    NoCueLocations, CueSymbols, CueValue, NoTargets, CueAssocList,  # Integers/lists: stimulus parameters
    RewardMoneyFactor, StimulusTargetColors, StimulusTargetColorsRGB, StimulusColorNoResponses,  # Floats/lists: reward and colors
    ResponseKeys, TrialStartJitterOffsetTime, TrialStartJitterMeanTime, TrialStartJitterMaxTime,  # Lists/floats: response and timing
    PracShowAllTargets, PracCueArrowResponseAssociations, CueBgColor  # Lists/floats: practice session display settings and colors
)
from display import setup_monitor, create_window  # Functions: window setup
from stimuli import create_cue_locations, create_target_locations, create_stimuli  # Functions: stimulus creation
from trial import generate_block_trials  # Function: trial generation
from data_handler import create_data_file, save_trial_data  # Functions: data file management

# Initialize clock - monotonic clock for precise timing
trial_clock = core.monotonicClock  # Clock: system clock that always increases

# Setup monitor and window
mon = setup_monitor()  # Monitor: configured monitor object
win = create_window(mon)  # Window: display window object

# Get refresh rate and timing parameters
refresh_rate = 100  # Integer: screen refresh rate in Hz (100 Hz = 10ms per frame)
delta_t = (1 / refresh_rate) / 2  # Float: timing adjustment in seconds (0.005 = 5ms)
isi = StaticPeriod(None)  # StaticPeriod: object for precise frame timing

# Generate stimulus locations
cue_locations = create_cue_locations()  # List: [[x1,y1], [x2,y2], [x3,y3], [x4,y4]] cue positions
target_locations = create_target_locations()  # List: [[x1,y1], [x2,y2], [x3,y3], [x4,y4]] target positions

# Create stimuli objects
stimuli = create_stimuli(win, cue_locations, target_locations)  # Dict: all stimulus objects

# Determine session type - practice or experimental
session = EXPERIMENT_CONFIG['session']  # Integer: session number (1-5 = practice, 6+ = experimental)
no_prac_sessions = len(PracCueSet)  # Integer: number of practice sessions (5)

if session <= no_prac_sessions:  # Check: practice session?
    practice = True  # Boolean: True = practice session
    cue_soa_conds = PracCueSOAconds[session - 1]  # List: SOA durations for this session [200]
    stim_ed = PracStimED[session - 1]  # Integer: exposure duration for this session (200ms)
    cue_set = PracCueSet[session - 1]  # List: cue combinations for this session
    cue_set_val = PracCueSetVal[session - 1]  # List: cue values for this session
    no_blocks = PracNoBlocks[session - 1]  # Integer: number of blocks (5, 5, 2, 2, 2)
    repetitions = PracRepetitions[session - 1]  # Integer: repetitions per condition (5, 5, 3, 3, 5)
else:  # Experimental session
    practice = False  # Boolean: False = experimental session
    cue_soa_conds = ExpCueSOAconds  # List: SOA durations [200]
    stim_ed = ExpStimED  # Integer: exposure duration (200ms)
    cue_set = ExpCueSet  # List: cue combinations
    cue_set_val = ExpCueSetVal  # List: cue values
    no_blocks = ExpNoBlocks  # Integer: number of blocks (4)
    repetitions = ExpRepetitions  # Integer: repetitions per condition (5)

# Generate all trials
all_trials = []  # List: will contain all Trial objects
start_block = EXPERIMENT_CONFIG['start_block']  # Integer: which block to start from (1)

for block in range(start_block - 1, no_blocks):  # Loop: through blocks
    block_num = block + 1  # Integer: block number (1, 2, 3, ...)
    
    # Generate experimental trials for this block
    block_trials = generate_block_trials(
        session, block_num, repetitions, 0, cue_soa_conds,  # Integers/lists: session, block, reps, warmup, SOAs
        NoCueLocations, cue_set, cue_set_val,  # Integer/lists: cue parameters
        StimulusTargetColors, NoTargets, stim_ed  # String/integer: color and timing parameters
    )
    all_trials.extend(block_trials)  # Add: trials to main list

# Create data file
data_file, data_filename = create_data_file(
    "CCRP", EXPERIMENT_CONFIG['subject'], session, practice  # String/integers/boolean: experiment name, subject, session, practice flag
)

# Create fixation display buffer - pre-rendered fixation point
fixation_display = visual.BufferImageStim(win, stim=[stimuli['fixation']])  # BufferImageStim: cached fixation point

# Main experiment loop
for trial_num, trial in enumerate(all_trials):  # Loop: through each trial
    
    # Set cue text - display cue numbers in boxes
    for i in range(NoCueLocations):  # Loop: through 4 cue positions
        if trial.cues[i] > 0:  # Check: cue present at this position?
            stimuli['cue_texts'][i].setText(str(CueSymbols[trial.cues[i] - 1]))  # String: set text to cue number ("1", "2", "3", or "4")
        else:  # No cue
            stimuli['cue_texts'][i].setText("")  # String: empty text (no cue shown)
    
    # Set color targets - assign colors to target positions
    # Handle first practice session: show single cue+target in center if PracShowAllTargets[0] == 0
    show_all_targets = True  # Boolean: whether to show all targets or just one in center
    if session <= no_prac_sessions and PracShowAllTargets[session - 1] == 0:  # Check: first practice session?
        show_all_targets = False  # Boolean: show only single cue+target in center
        for i in range(NoTargets):  # Loop: through 4 target positions
            if trial.cues[i] == 0:  # Check: no cue at this position?
                # Hide this target - move off screen
                stimuli['cue_boxes'][i].setColor(WINDOW_CONFIG['bg_color'])  # Tuple: set to background color (black)
                stimuli['color_targets'][i].setColor(WINDOW_CONFIG['bg_color'])  # Tuple: hide target
                stimuli['cue_texts'][i].setPos((0, -400 * 0.04))  # Tuple: move off screen
                stimuli['cue_boxes'][i].setPos((0, -400 * 0.04))  # Tuple: move off screen
                stimuli['color_targets'][i].setPos((0, -400 * 0.04))  # Tuple: move off screen
            else:  # Cue present - show in center
                color_idx = int(trial.target_colors[i]) - 1  # Integer: convert "1"->0, "2"->1, etc.
                stimuli['cue_boxes'][i].setColor(CueBgColor)  # Tuple: show cue box (white)
                stimuli['color_targets'][i].setColor(StimulusTargetColorsRGB[color_idx])  # Tuple: set color
                stimuli['cue_texts'][i].setPos((0, 0))  # Tuple: center position
                stimuli['cue_boxes'][i].setPos((0, 0))  # Tuple: center position
                stimuli['color_targets'][i].setPos((0, 0))  # Tuple: center position
    else:  # Show all targets at their positions
        for i in range(NoTargets):  # Loop: through 4 target positions
            color_idx = int(trial.target_colors[i]) - 1  # Integer: convert "1"->0, "2"->1, etc. (0-based index)
            stimuli['color_targets'][i].setColor(StimulusTargetColorsRGB[color_idx])  # Tuple: set RGB color (red, green, blue, or yellow)
            stimuli['cue_boxes'][i].setColor(CueBgColor)  # Tuple: ensure cue box is visible
            stimuli['cue_texts'][i].setPos(cue_locations[i])  # Tuple: reset to normal position
            stimuli['cue_boxes'][i].setPos(cue_locations[i])  # Tuple: reset to normal position
            stimuli['color_targets'][i].setPos(target_locations[i])  # Tuple: reset to normal position
    
    # Set color-response instruction display - show which keys map to which colors
    show_color_response = False  # Boolean: whether to show color-response mapping
    if session <= no_prac_sessions:  # Check: practice session?
        if PracCueArrowResponseAssociations[session - 1] == 1:  # Check: show mapping for this session?
            show_color_response = True  # Boolean: show the mapping
    for i in range(StimulusColorNoResponses):  # Loop: through 4 colors
        if show_color_response:  # Check: show instruction?
            stimuli['color_response_instruction'][i].setColor(StimulusTargetColorsRGB[i])  # Tuple: set to color (red, green, blue, yellow)
        else:  # Hide instruction
            stimuli['color_response_instruction'][i].setColor(WINDOW_CONFIG['bg_color'])  # Tuple: set to background (invisible)
    
    # Create display buffer - cues AND colors appear together simultaneously
    cue_color_stim_list = [stimuli['fixation']] + stimuli['cue_arrows'] + stimuli['color_targets'] + stimuli['cue_boxes'] + stimuli['cue_texts'] + stimuli['color_response_instruction']  # List: all stimuli together
    cue_color_display = visual.BufferImageStim(win, stim=cue_color_stim_list)  # BufferImageStim: cached display with cues and colors
    
    # Present fixation
    fixation_display.draw()  # Draw: fixation point
    win.flip()  # Flip: show on screen
    
    # Wait for keypress to start trial
    event.clearEvents()  # Clear: remove any pending keypresses
    event.waitKeys()  # Wait: for any keypress to continue
    
    # Calculate trial start jitter - random delay before display
    jitter_time = min([TrialStartJitterOffsetTime + random.exponential(TrialStartJitterMeanTime), TrialStartJitterMaxTime])  # Float: jitter duration in seconds (1.0-5.0s)
    core.wait(jitter_time)  # Wait: jitter duration
    
    # Present cues AND colors together - they appear simultaneously
    cue_color_display.draw()  # Draw: cues + colors + arrows + instructions
    win.flip()  # Flip: show everything on screen
    trial.cue_time = trial_clock.getTime()  # Float: record timestamp (seconds)
    trial.color_target_time = trial_clock.getTime()  # Float: same time (colors appear with cues)
    
    # Collect response - wait for keypress
    event.clearEvents()  # Clear: remove any pending keypresses
    this_key = event.waitKeys()  # List: wait for keypress (e.g., ['z'])
    
    # Process keypress - convert to standard format
    temp = this_key[0]  # String: key name (e.g., "z", "comma", "period")
    if temp == "comma":  # Check: comma key?
        temp = ","  # String: convert to comma character
    elif temp == "period":  # Check: period key?
        temp = "."  # String: convert to period character
    elif temp == "minus":  # Check: minus key?
        temp = "-"  # String: convert to minus character
    else:  # Letter key
        temp = temp.upper()  # String: convert to uppercase ("z" -> "Z")
    
    trial.response = temp  # String: store response ("Z", "X", ".", "-")
    trial.end_trial_time = trial_clock.getTime()  # Float: record timestamp (seconds)
    trial.rt = trial.color_target_time - trial.cue_time  # Float: reaction time in seconds
    
    # Calculate accuracy and reward - check if response matches cued color
    temp_resp = ""  # String: will contain response index as string
    try:  # Try: find response key index
        temp_resp = str(ResponseKeys.index(trial.response) + 1)  # String: convert key to number ("1"->"1", "2"->"2", etc.)
    except:  # Key not in ResponseKeys
        temp_resp = ""  # String: invalid response (empty)
    
    # Find which target position has a cue (cues are at cue positions, need to map to target positions)
    for i in range(len(trial.target_colors)):  # Loop: through 4 target positions
        # Find cue position that maps to this target position using CueAssocList
        cue_pos = None  # Integer: will contain cue position index
        for j in range(NoCueLocations):  # Loop: through 4 cue positions
            if CueAssocList[j] - 1 == i:  # Check: this cue maps to target i? (CueAssocList is 1-based)
                cue_pos = j  # Integer: found matching cue position
                break
        
        if temp_resp == trial.target_colors[i]:  # Check: response matches this target's color?
            trial.response_loc += "1"  # String: mark as selected (e.g., "1000")
            
            if cue_pos is not None and trial.cues[cue_pos] > 0:  # Check: cue present at mapped position?
                trial.cue_response_rank = trial.cue_ranks[cue_pos]  # Integer: rank of selected cue (1-4)
                trial.expected_reward += CueValue[trial.cues[cue_pos] - 1]  # Integer: add cue value to expected reward
                
                if trial.cues_val[cue_pos] > 0:  # Check: reward value present?
                    trial.reward += CueValue[trial.cues_val[cue_pos] - 1]  # Integer: add reward value
                
                trial.cue_response_exp_value = trial.cues[cue_pos]  # Integer: expected cue value
                trial.cue_response_value = trial.cues_val[cue_pos]  # Integer: actual reward value
                trial.acc += 1  # Integer: correct response (1)
            else:  # No cue at this target position
                trial.intr += 1  # Integer: intrusion error (responded to non-cued target)
        else:  # Response doesn't match this target
            trial.response_loc += "0"  # String: mark as not selected
    
    trial.err = len(temp_resp) - trial.acc - trial.intr if temp_resp else 1  # Integer: errors (wrong key or invalid)
    
    # Calculate max reward - highest possible reward for this trial
    active_cues = [c for c in trial.cues if c > 0]  # List: filter out zeros (e.g., [1, 2])
    if len(active_cues) > 0:  # Check: any cues present?
        trial.max_reward = CueValue[max(active_cues) - 1]  # Integer: highest cue value (4)
    else:  # No cues
        trial.max_reward = 0  # Integer: zero reward
    
    # Calculate cumulative reward - running total across trials
    if trial_num > 0:  # Check: not first trial?
        trial.cum_reward = all_trials[trial_num - 1].cum_reward + (trial.reward * RewardMoneyFactor)  # Float: add to previous total
    else:  # First trial
        trial.cum_reward = trial.reward * RewardMoneyFactor  # Float: start cumulative reward
    trial.cum_reward = round(trial.cum_reward, 2)  # Float: round to 2 decimal places
    
    # Save trial data
    save_trial_data(data_file, trial, trial_num + 1)  # Write: trial data to file
    
    # Show feedback - display reward and cumulative total
    feedback_text = visual.TextStim(
        win, text=f"{trial.expected_reward} / {trial.max_reward}\n{trial.cum_reward:.2f}",  # String: "4 / 4\n0.40"
        height=0.5, units='deg', color=(1, 1, 1)  # TextStim: white text, 0.5 deg height
    )
    feedback_text.draw()  # Draw: feedback text
    win.flip()  # Flip: show feedback on screen
    
    event.clearEvents()  # Clear: remove any pending keypresses
    core.wait(1.0)  # Wait: 1 second before next trial

# Close data file
data_file.close()  # Close: file handle

# End screen
end_text = visual.TextStim(
    win, text="End of session!\n\nContact the Experimenter",  # String: end message
    height=0.5, units='deg', color=(1, 1, 1)  # TextStim: white text, 0.5 deg height
)
end_text.draw()  # Draw: end message
win.flip()  # Flip: show end message
event.waitKeys()  # Wait: for keypress before closing

core.quit()  # Quit: exit program
