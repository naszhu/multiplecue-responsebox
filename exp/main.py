"""
Main experiment script for Multiple Cue Paradigm
"""
from psychopy import core, visual, event
from psychopy.core import StaticPeriod
import sys
import os
import random as rnd

# Add src directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from config import EXPERIMENT_CONFIG, WINDOW_CONFIG
from experiment_params import (
    ExpCueSet, PracCueSet, ExpCueSetVal, PracCueSetVal,
    ExpCueSOAconds, PracCueSOAconds, ExpStimED, PracStimED,
    ExpNoBlocks, PracNoBlocks, ExpRepetitions, PracRepetitions,
    MaskED, NoCueLocations, CueSymbols, CueValue, NoTargets,
    MaxRewardFlag, RewardMoneyFactor, StimulusTargetLetters
)
from display import setup_monitor, create_window
from stimuli import create_cue_locations, create_target_locations, create_stimuli
from trial import generate_block_trials
from data_handler import create_data_file, save_trial_data

# Initialize clock
trial_clock = core.monotonicClock

# Setup monitor and window
mon = setup_monitor()
win = create_window(mon)

# Get refresh rate (assuming 100 Hz default)
refresh_rate = 100
delta_t = (1 / refresh_rate) / 2
isi = StaticPeriod(None)

# Generate stimulus locations
cue_locations = create_cue_locations()
target_locations = create_target_locations()

# Create stimuli
stimuli = create_stimuli(win, cue_locations, target_locations)

# Determine session type
session = EXPERIMENT_CONFIG['session']
no_prac_sessions = len(PracCueSet)

if session <= no_prac_sessions:
    practice = True
    cue_soa_conds = PracCueSOAconds[session - 1]
    stim_ed = PracStimED[session - 1]
    cue_set = PracCueSet[session - 1]
    cue_set_val = PracCueSetVal[session - 1]
    no_blocks = PracNoBlocks[session - 1]
    repetitions = PracRepetitions[session - 1]
else:
    practice = False
    cue_soa_conds = ExpCueSOAconds
    stim_ed = ExpStimED
    cue_set = ExpCueSet
    cue_set_val = ExpCueSetVal
    no_blocks = ExpNoBlocks
    repetitions = ExpRepetitions

# Generate all trials
all_trials = []
start_block = EXPERIMENT_CONFIG['start_block']

for block in range(start_block - 1, no_blocks):
    block_num = block + 1
    
    # Generate experimental trials
    block_trials = generate_block_trials(
        session, block_num, repetitions, 0, cue_soa_conds,
        NoCueLocations, cue_set, cue_set_val,
        StimulusTargetLetters, NoTargets, stim_ed
    )
    all_trials.extend(block_trials)

# Create data file
data_file, data_filename = create_data_file(
    "MCP", EXPERIMENT_CONFIG['subject'], session, practice
)

# Create fixation display buffer
fixation_display = visual.BufferImageStim(win, stim=[stimuli['fixation']])

# Main experiment loop
for trial_num, trial in enumerate(all_trials):
    
    # Set cue text
    for i in range(NoCueLocations):
        if trial.cues[i] > 0:
            stimuli['cue_texts'][i].setText(str(CueSymbols[trial.cues[i] - 1]))
        else:
            stimuli['cue_texts'][i].setText("")
    
    # Set target letters
    for i in range(NoTargets):
        stimuli['letter_stim'][i].setText(trial.target_letters[i])
    
    # Set mask properties (masks are Rect objects, so we can rotate them)
    for i in range(NoTargets):
        ori = rnd.randint(0, 3) * 90
        stimuli['masks'][i].setOri(ori)
    
    # Create display buffers
    cue_stim_list = [stimuli['fixation']] + stimuli['cue_arrows'] + \
                    stimuli['cue_boxes'] + stimuli['cue_texts']
    cues_display = visual.BufferImageStim(win, stim=cue_stim_list)
    
    letter_stim_list = [stimuli['fixation']] + stimuli['cue_arrows'] + \
                       stimuli['cue_boxes'] + stimuli['cue_texts'] + \
                       stimuli['letter_stim']
    letter_display = visual.BufferImageStim(win, stim=letter_stim_list)
    
    mask_stim_list = [stimuli['fixation']] + stimuli['cue_boxes'] + \
                     stimuli['cue_texts'] + stimuli['masks']
    mask_display = visual.BufferImageStim(win, stim=mask_stim_list)
    
    # Present fixation
    fixation_display.draw()
    win.flip()
    
    # Wait for keypress to start trial
    event.clearEvents()
    event.waitKeys()
    
    # Present cues
    cues_display.draw()
    win.flip()
    trial.cue_time = trial_clock.getTime()
    
    # Wait for SOA
    isi.start(trial.cue_soa / refresh_rate - delta_t)
    letter_display.draw()
    isi.complete()
    
    # Present letters
    win.flip()
    trial.stim_time = trial_clock.getTime()
    
    # Wait for exposure duration
    isi.start(trial.ed / refresh_rate - delta_t)
    mask_display.draw()
    isi.complete()
    
    # Present masks
    win.flip()
    trial.mask_time = trial_clock.getTime()
    
    # Wait for mask duration
    isi.start(MaskED / refresh_rate - delta_t)
    isi.complete()
    
    # Blank screen
    win.flip()
    trial.end_trial_time = trial_clock.getTime()
    
    # Collect response
    event.clearEvents()
    response_text = visual.TextStim(win, text="", height=0.5, units='deg', color=(1, 1, 1))
    temp_resp = ''
    stop_response = False
    
    while not stop_response:
        this_key = event.waitKeys()
        
        if len(this_key) > 0:
            if this_key[0] == 'escape':
                stop_response = True
            elif this_key[0] == 'space' and temp_resp != '':
                stop_response = True
            else:
                if this_key[0] == 'backspace' and len(temp_resp) > 0:
                    temp_resp = temp_resp[:-1]
                else:
                    if len(this_key[0]) == 1 and len(temp_resp) == 0:
                        temp_resp = temp_resp + this_key[0].upper()
        
        response_text.setText(temp_resp)
        response_text.draw()
        win.flip()
    
    if len(temp_resp) == 0:
        temp_resp = "-"
    
    trial.response = temp_resp
    trial.rt = trial.end_trial_time - trial.stim_time
    
    # Calculate accuracy and reward
    for i in range(len(trial.target_letters)):
        if temp_resp.find(trial.target_letters[i]) > -1:
            trial.response_loc += "1"
            
            trial.cue_response_rank = trial.cue_ranks[i]
            
            if trial.cues[i] > 0:
                trial.expected_reward += CueValue[trial.cues[i] - 1]
                
                if trial.cues_val[i] > 0:
                    trial.reward += CueValue[trial.cues_val[i] - 1]
                
                trial.cue_response_exp_value = trial.cues[i]
                trial.cue_response_value = trial.cues_val[i]
            
            if trial.cues[i] == 0:
                trial.intr += 1
            else:
                trial.acc += 1
        else:
            trial.response_loc += "0"
    
    trial.err = len(temp_resp) - trial.acc - trial.intr
    
    # Calculate max reward
    active_cues = [c for c in trial.cues if c > 0]
    if len(active_cues) > 0:
        trial.max_reward = CueValue[max(active_cues) - 1]
    else:
        trial.max_reward = 0
    
    # Calculate cumulative reward
    if trial_num > 0:
        trial.cum_reward = all_trials[trial_num - 1].cum_reward + \
                          (trial.reward * RewardMoneyFactor)
    else:
        trial.cum_reward = trial.reward * RewardMoneyFactor
    
    trial.cum_reward = round(trial.cum_reward, 2)
    
    # Save trial data
    save_trial_data(data_file, trial, trial_num + 1)
    
    # Show feedback
    feedback_text = visual.TextStim(
        win, text=f"{trial.expected_reward} / {trial.max_reward}\n{trial.cum_reward:.2f}",
        height=0.5, units='deg', color=(1, 1, 1)
    )
    feedback_text.draw()
    win.flip()
    
    event.clearEvents()
    event.waitKeys()

# Close data file
data_file.close()

# End screen
end_text = visual.TextStim(
    win, text="End of session!\n\nContact the Experimenter",
    height=0.5, units='deg', color=(1, 1, 1)
)
end_text.draw()
win.flip()
event.waitKeys()

core.quit()

