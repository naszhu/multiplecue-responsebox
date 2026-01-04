
#================================================================
# LOG
#================================================================
# TO DOs
# - Presentation of cue masks with recording of RT with .iohub
#----------------------------------------------------------------
# 2025-08-05
# - Centrally presented single color target in first practice session
# - Black fixation cross in Exp type 3
# - New instructions for Experiment type 3
#----------------------------------------------------------------
# 2025-08-04
# - Display color-response associations in practice trials
# - Show "Contact Experimenter" i end instruction screen
# - Permutation of response keys-color links
# - Header information about new variables for ExperimentType 3
# - 
#----------------------------------------------------------------
# 2025-08-01
# - Implementation of responses, RTs, and accuracy
# - Cues placed on top of color targets
# - Color target moved close to fixation
# - Response keys linked to colors
#----------------------------------------------------------------
# 2025-07-31
# - Further work with the new CCRP paradigm
# - Implementation of 3rd Experimental type based on 2nd Expwerimental Type and new variables for Color Targets
# - Got to implementation with display of colored targets
# - IntentionSelectionParadigmPyExp250731e.py 
#----------------------------------------------------------------
# 2025-07-17
# - New Type 3 experiment = Cued Color Response Paradigm (CCRP) with cue -> color of stimuli -> response to color
#================================================================



#------------------------------------------------
# Load libraries
#------------------------------------------------

# Eye-link
from __future__ import absolute_import, division, print_function
from psychopy import core, visual
from psychopy import iohub
from psychopy.iohub.client.eyetracker.validation import TargetStim
from psychopy.iohub.util import hideWindow, showWindow

# General
from operator import *
from psychopy import visual, event, core, data, misc, gui, monitors
import random as rnd
import sys, os
from datetime import datetime
from numpy import *
from psychopy.platform_specific import rush

from checkmonitor import * 

import psychopy.info

#-------------------------------------------------
# Start the clock
#-------------------------------------------------

#trialClock=core.Clock() # Standard way of initiating clock
trialClock = core.monotonicClock # instance of class psychopy.core.MonotonicClock that got created when PsychoPy.core was imported. That clock instance is deliberately designed always to return the time since the start of the study.


#-------------------------------------------------
# Get system and program file information
#-------------------------------------------------


PsychoPyVersion = psychopy.info.RunTimeInfo(win=False,refreshTest=False)['psychopyVersion']

ExperimentalProgramName = os.path.basename(__file__)
ExpStartTime = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")


print('\n\n\n')
print('----------------------------------------------------------------------------')
print('Starting Experiment')
print('{0} @ {1}'.format(ExperimentalProgramName, ExpStartTime))
print('----------------------------------------------------------------------------')
print('\n')


#-------------------------------------------------
# Load experimental parameters and run dialog box
#-------------------------------------------------



try: #try to get a previous parameters file
    expInfo = misc.fromFile('ExpConfig.psydat')
#    print('Successfully read parameters')
except: #if not there then use a default set
    expInfo = {'subject':0, 'age':0, 'gender':'NA', 'experiment':'?', 'session':0, 'startblock':1, 'fullscreen':'Y', 'fixrefreshrate':100, 'testrefreshrate':'Y', 'eyetracker':'none', 'eyetrackercheck':'none', 'testtiming':'N'}
#    print('Creating new parameter file')


expInfo['date'] = ExpStartTime

myDlg = gui.Dlg(title="Intention Selection Paradigms")
myDlg.addFixedField('Date', label='Date', initial=expInfo['date'])
myDlg.addField('Subject #', initial=str(expInfo['subject']))
myDlg.addField('Age', initial=str(expInfo['age']))
myDlg.addField('Gender', initial=expInfo['gender'], choices=["Female", "Male", "Other", "NA"])
myDlg.addField('Experiment #', initial=expInfo['experiment'], choices=[
    "MCP002 - 4 cues, reward 1234, line, complex", 
    "MCP003 - 4 cues, reward 1234, line, complex", 
    "MCP004 - 4 cues, reward 2389, line, complex", 
    "MCP005 - 4 cues, reward 1234, square, simple", 
    "MCP006 - 8 cues, reward 1234, circle, complex", 
    "MCP007 - 4 cues, reward 1234, line, complex", 
    "MCP008 - 8 cues, reward 1234, circle, simpel", 
    "MCP009 - 8 cues, reward 123456789, circle, simpel",
    "MCP010 - 8 cues, reward 1234, random reward, circle, simpel, implicit reward structure",
    "MCP011 - 8 cues, reward 1234, random reward, circle, simpel, explicit reward structure",
    "MCP012 - 8 cues, reward 1234, 1H3L reward, circle, simpel",
    "MCP500 - 8 cues, reward 1234, circle, simpel - CoInActWorkshop 221213", 
    "CPP001 - 4 cues, reward 1234, square, complex, cuemask", 
    "CPP002 - 4 cues, reward 1234, square, complex, nocuemask", 
    "CPP003 - 4 cues, reward 1234, square, complex, nocuemask, small cues and cue distances", 
    "CCRP001 - 4 cues, reward 1234, square, complex, nocuemask, small cues and cue distances" 
    ])
myDlg.addField('Session', str(expInfo['session']))
myDlg.addField('Block', str(1)) # Starting block as number 1 as default
myDlg.addField('Full screen', initial=expInfo['fullscreen'], choices=["Y","N"])
myDlg.addField('Fix refreshrate', initial=expInfo['fixrefreshrate'], choices=[60,75,100])
myDlg.addField('Test refreshrate', initial=expInfo['testrefreshrate'], choices=["Y","N"])
myDlg.addField('Eyetracker', initial=expInfo['eyetracker'], choices=["none","mouse","eyelink"])
myDlg.addField('Eyetracker check', initial=expInfo['eyetrackercheck'], choices=["none","driftcheck","driftcorrection"])
myDlg.addField('Test timing', initial=expInfo['testtiming'], choices=["Y","N"])

ok_data = myDlg.show()  # show dialog and wait for OK or Cancel

if myDlg.OK:  # or if ok_data is not None
    i = 0
    # expInfo['date'] = ok_data[i];i += 1
    i += 1
    expInfo['subject'] = int(ok_data[i]);i += 1
    expInfo['age'] = int(ok_data[i]);i += 1
    expInfo['gender'] = ok_data[i];i += 1
    expInfo['experiment'] = ok_data[i];i += 1
    expInfo['session'] = int(ok_data[i]);i += 1
    expInfo['startblock'] = int(ok_data[i]);i += 1
    expInfo['fullscreen'] = ok_data[i];i += 1
    expInfo['fixrefreshrate'] = ok_data[i];i += 1
    expInfo['testrefreshrate'] = ok_data[i];i += 1
    expInfo['eyetracker'] = ok_data[i];i += 1
    expInfo['eyetrackercheck'] = ok_data[i];i += 1
    expInfo['testtiming'] = ok_data[i];i += 1
else:
    print('User cancelled')
    core.quit()

misc.toFile('ExpConfig.psydat',expInfo)




#-------------------------------------------------
# Open log file
#-------------------------------------------------

# make directory if it doesn't exist already
if not os.path.exists("Log/"):
   os.makedirs("Log/")

ExperimentalLogFileName = "Log/" + ExperimentalProgramName + ".log"

# Define function that writes to the log file and closes it again
def write_log( log_string ):
    flog = open(ExperimentalLogFileName, "a")
    flog.write(log_string)
    flog.close()

# Write initial lines to the log file
write_log( '\n\n----------------------------------------------------------------------------\n' )
write_log( '{0} @ {1}\n'.format(ExperimentalProgramName, ExpStartTime) )


#------------------------------------------------
# Setup parameters for specific experiment type
#------------------------------------------------


ExperimentName = expInfo['experiment']

ExpCueSetValue = [ ]
PracCueSetValue = [ ]

CueCharacters = "1234567890%&@"

StimulusTargetLetters = "ABCDEFGHIJKLMNOPQRSTUVXYZW"	# full UK alphabet
# StimulusTargetLetters = "BCDFGHJKLMNPQRSTVXZW"	# only consonants

StimulusTargetColors = "1234"
StimulusTargetColorsRGB = [ (1,-1,-1), (-1,1,-1), (-1,-1,1), (1,1,-1) ]
StimulusColorNoResponses = len(StimulusTargetColorsRGB)


# ExperimentTypes
# 1 = Multiple Cued Paradigm (MCP; Cued Intention Paradigm / Intention Selection Paradigm)
# 2 = Cued Pointing Paradigm (CPP)
# 3 = Cued Color Response Paradigm (CCRP)

# Define association types from name of experiment
if "MCP002" in ExperimentName:
    ExperimentNumber = 2
    ExperimentType = 1
    CueTargetAssociationType = 1
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,100]    # !!! NOTE: Original set of SOA condtions, but ExpCueSOAconds = [1,15,30,50,80,100] may be run to include the 800 ms SOA. !!!
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ExpRepetitions = 1
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).

elif "MCP003" in ExperimentName:
    ExperimentNumber = 3
    ExperimentType = 1
    CueTargetAssociationType = 1
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4], [1,1],[2,2],[3,3],[4,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
    
elif "MCP004" in ExperimentName:
    ExperimentNumber = 4
    ExperimentType = 1
    CueTargetAssociationType = 1
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [2,3,8,9]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
    
elif "MCP005" in ExperimentName:
    ExperimentNumber = 5
    ExperimentType = 1
    CueTargetAssociationType = 2
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
    
elif "MCP006" in ExperimentName:
    ExperimentNumber = 6
    ExperimentType = 1
    CueTargetAssociationType = 4
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 
    
elif "MCP007" in ExperimentName:
    ExperimentNumber = 7
    ExperimentType = 1
    CueTargetAssociationType = 1
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4], [1,2,3],[1,2,4],[1,3,4],[2,3,4], [1,2,3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 
    
elif "MCP008" in ExperimentName:
    ExperimentNumber = 8
    ExperimentType = 1
    CueTargetAssociationType = 3
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 
    
elif "MCP009" in ExperimentName:
    ExperimentNumber = 9
    ExperimentType = 1
    CueTargetAssociationType = 3
    MaskCuePresent = True
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [5], [6], [7], [8], [9], [1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9] ]
    PracCueSet = [ [ [1], [2], [3], [4], [5], [6], [7], [8], [9] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4,5,6,7,8,9]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 
    
elif "MCP010" in ExperimentName or "MCP011" in ExperimentName:  # The difference between MCP010 and MCP011 is only in the instruction of the participants. In MCP010 the participants has to learn the reward contingencies them selves. In MCP011, they are explicitly informed about them.
    if "MCP010" in ExperimentName:
        ExperimentNumber = 10
    else:
        ExperimentNumber = 11
    ExperimentType = 1
    CueTargetAssociationType = 3
    MaskCuePresent = True
    CalibrationFlag = False
    ExpCueSet =               3 * [ [1],[2],[3],[1,2],[1,3],[2,3] ]
    ExpCueSetVal =                [ [1],[2],[3],[1,2],[1,3],[2,3] ] 
    ExpCueSetVal = ExpCueSetVal + [ [1],[2],[0],[1,2],[1,0],[2,0] ]
    ExpCueSetVal = ExpCueSetVal + [ [1],[0],[0],[1,0],[1,0],[0,0] ]
    PracCueSet =    [ 3 * [ [1],[2],[3] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    PracCueSetVal = [ [ [1],[2],[3], [1],[2],[0], [1],[0],[0] ], ExpCueSetVal, ExpCueSetVal, ExpCueSetVal ]
    CueValue = [1,2,3]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = False
    VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.5
    CueSymbolsSet = ["%","&","@"]
    CueSymbols = [ CueSymbolsSet[ (i+expInfo['subject']-1) % len(CueSymbolsSet) ] for i in range(len(CueSymbolsSet)) ]    # Rotate the symbols to counterbalance across participants
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    ExpCueSOAconds = [1,15,30,50,80]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    PracNoBlocks = (1,1,2,2)
    PracRepetitions = (7,7,1,1)
    ExpNoBlocks = 3
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0       # Response deadline in seconds
    MaxEyeRecordings = 0            # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0     # Drift correction after N blocks (0 = No drift correction).
 
    
    
elif "MCP012" in ExperimentName:
    ExperimentNumber = 12
    ExperimentType = 1
    CueTargetAssociationType = 3
    MaskCuePresent = True
    CalibrationFlag = False
#    ExpCueSet = [ [1,1,1,2],[1,1,2,2],[1,2,2,2],[2,2,2,2] ]
#    ExpCueSet = [ [1,2,1,1],[1,3,1,1],[1,4,1,1],[2,3,2,2],[2,4,2,2],[3,4,3,3] ]
#    ExpCueSet = [ [1], [2], [3], [4], [1,2,1,1],[1,3,1,1],[1,4,1,1],[2,3,2,2],[2,4,2,2],[3,4,3,3] ]
    ExpCueSet = [ [1,1,1,1],[1,1,1,2],[1,1,2,2],[1,2,2,2],[2,2,2,2] ]
#    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    PracCueSet = [ [ [1], [2] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
#    CueValue = [1,2,3,4]
    CueValue = [1,2]
    CueSymbols = CueValue
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    RewardMoneyFactor = 0.1
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
#    PracCueSOAconds = [ [100], [100], [1,15,30,50,80,100], [1,15,30,50,80,100] ]
    PracCueSOAconds = [ [100], [100], [30], [30] ]
    PracStimED = (100,30,15,5)
    ExpStimED = 5
    ExpCueSOAconds = [1,15,30,50,80,100]
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (7,7,1,5)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 

elif "MCP500" in ExperimentName:    # CoInAct workshop eksperiment
    ExperimentNumber = 500
    ExperimentType = 1
    CueTargetAssociationType = 3
    MaskCuePresent = True
    CalibrationFlag = False
    ExpCueSet = [ [1], [2], [3], [4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 0.0
    TrialStartJitterMeanTime = 0.0
    TrialStartJitterMaxTime = 0.0
    PracCueSOAconds = [ [100], [100], [1,30,50,100], [1,30,50,100] ]
    ExpCueSOAconds = [1,30,50,100]
    PracStimED = (100,30,15,8)
    ExpStimED = 8
    PracNoBlocks = (1,1,1,1)
    PracRepetitions = (8,4,1,1)
    ExpNoBlocks = 2
    ResponseKeys = []
    FirstWarmUpTrials = 0
    OtherWarmUpTrials = 0
    ExpRepetitions = 1
    ResponseDeadline = 0.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings
    DriftCorrectionNoBlocks = 0 # Drift correction after N blocks (0 = No drift correction).
 

elif "CPP001" in ExperimentName:
    ExperimentNumber = 1001
    ExperimentType = 2
#    CueTargetAssociationType = 2
#    CueTargetAssociationType = 5
    CueTargetAssociationType = 6
    MaskCuePresent = True
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    # ExpCueSetVal = [ [1],[0],[3],[0], [1,0],[1,3],[1,0],[0,3],[0,0],[3,0] ]
    # PracCueSetVal = [ [ [1], [0], [3], [0] ], ExpCueSetVal, ExpCueSetVal, ExpCueSetVal ]
    # CueValue = [2,4,6,8]
    # MaxRewardFlag = False
    # VariableRewardFeedbackFlag = True
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 1.0
    TrialStartJitterMeanTime = 0.5
    TrialStartJitterMaxTime = 5.0
    ExpCueSOAconds = [50]
    PracCueSOAconds = [ [100], [100], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (100,100,100,100)
    ExpStimED = 100
    PracNoBlocks = (5,2,2,2)
    PracRepetitions = (5,3,3,5)
    ExpNoBlocks = 4
    ResponseKeys = []
    FirstWarmUpTrials = 4
    OtherWarmUpTrials = 2
    ExpRepetitions = 5 
    ResponseDeadline = 2.0      # Response deadline in seconds
    MaxEyeRecordings = 2000  # Maximum number of eye movement recordings 
    # MaxEyeRecordings = 20  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 1 # Drift correction after N blocks (0 = No drift correction).

elif "CPP002" in ExperimentName:
    ExperimentNumber = 1002
    ExperimentType = 2
    CueTargetAssociationType = 6
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 1.0
    TrialStartJitterMeanTime = 0.5
    TrialStartJitterMaxTime = 5.0
    ExpCueSOAconds = [200]
    PracCueSOAconds = [ [200], [200], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (200,200,200,200)
    ExpStimED = 200
    PracNoBlocks = (5,2,2,2)
    PracRepetitions = (5,3,3,5)
    ExpNoBlocks = 4
    ResponseKeys = []
    FirstWarmUpTrials = 4
    OtherWarmUpTrials = 2
    ExpRepetitions = 5 
    ResponseDeadline = 2.0      # Response deadline in seconds
    MaxEyeRecordings = 2000  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 1 # Drift correction after N blocks (0 = No drift correction).

elif "CPP003" in ExperimentName:
    ExperimentNumber = 1003
    ExperimentType = 2
    CueTargetAssociationType = 7
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0]
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    TrialStartJitterOffsetTime = 1.0
    TrialStartJitterMeanTime = 0.5
    TrialStartJitterMaxTime = 5.0
    ExpCueSOAconds = [200]
    PracCueSOAconds = [ [200], [200], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (200,200,200,200)
    ExpStimED = 200
    PracNoBlocks = (5,2,2,2)
    PracRepetitions = (5,3,3,5)
    ExpNoBlocks = 4
    ResponseKeys = []
    FirstWarmUpTrials = 4
    OtherWarmUpTrials = 2
    ExpRepetitions = 5 
    ResponseDeadline = 2.0      # Response deadline in seconds
    MaxEyeRecordings = 2000  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 1 # Drift correction after N blocks (0 = No drift correction).

elif "CCRP001" in ExperimentName:
    ExperimentNumber = 2001
    ExperimentType = 3
    CueTargetAssociationType = 8
    MaskCuePresent = False
    CalibrationFlag = False
    ExpCueSet = [ [1],[2],[3],[4], [1,2],[1,3],[1,4],[2,3],[2,4],[3,4] ]
    PracCueSet = [ [ [1], [2], [3], [4] ], [ [1], [2], [3], [4] ], ExpCueSet, ExpCueSet, ExpCueSet ]
    CueValue = [1,2,3,4]
    PracCueArrowResponseAssociations = [1, 1, 1, 0, 0]
    PracShowAllTargets = [0, 1, 1, 1, 1] 
    ExpCueSetVal = ExpCueSet
    PracCueSetVal = PracCueSet
    MaxRewardFlag = True
    VariableRewardFeedbackFlag = False
    RewardMoneyFactor = 0.1
    CueSymbols = CueValue
    StimulusTargetColors = ''.join([ StimulusTargetColors[ (i+expInfo['subject']-1) % StimulusColorNoResponses ] for i in range(StimulusColorNoResponses) ])    # Rotate the colors to counterbalance across participants
    StimulusTargetColorsRGB = [ StimulusTargetColorsRGB[ (i+expInfo['subject']-1) % StimulusColorNoResponses ] for i in range(StimulusColorNoResponses) ]    # Rotate the RGB color values to counterbalance across participants
    TrialStartJitterOffsetTime = 1.0
    TrialStartJitterMeanTime = 0.5
    TrialStartJitterMaxTime = 5.0
    ExpCueSOAconds = [200]
    PracCueSOAconds = [ [200], [200], [200], ExpCueSOAconds, ExpCueSOAconds ]
    PracStimED = (200,200,200,200,200)
    ExpStimED = 200
    PracNoBlocks = (5,5,2,2,2)
    PracRepetitions = (5,5,3,3,5)
    ExpNoBlocks = 4
    ResponseKeys = ["Z","X",".","-"]
    FirstWarmUpTrials = 4
    OtherWarmUpTrials = 2
    ExpRepetitions = 5
    ResponseDeadline = 2.0      # Response deadline in seconds
    MaxEyeRecordings = 0  # Maximum number of eye movement recordings 
    DriftCorrectionNoBlocks = 1 # Drift correction after N blocks (0 = No drift correction).

else:
    print(ExperimentName)
    print("Not valid experiment name and number")
    core.quit()


if ExperimentType == 2 and expInfo['eyetracker'] == 'none':
    print("No valid eye tracker chosen.")
    core.quit()
    


#------------------------------------------------
# Set session variables
#------------------------------------------------

NoPracSessions = len(PracCueSet)

Session = int(expInfo['session'])

if Session <= NoPracSessions:
    expInfo['practice'] = 'Y'
    cueSOAconds = PracCueSOAconds[Session-1]
    StimED = PracStimED[Session-1]
    CueSet = PracCueSet[Session-1]
    CueSetVal = PracCueSetVal[Session-1]
    NoBlocks = PracNoBlocks[Session-1]
    Repetitions = PracRepetitions[Session-1]
else:
    expInfo['practice'] = 'N'
    cueSOAconds = ExpCueSOAconds
    StimED = ExpStimED
    CueSet = ExpCueSet
    CueSetVal = ExpCueSetVal
    NoBlocks = ExpNoBlocks
    Repetitions = ExpRepetitions
    
Nconditions = len(CueSet)*len(cueSOAconds)



#------------------------------------------------
# Select instruction text
#------------------------------------------------
InstructionFileName = ""

if ExperimentType == 1:
    if Session == 1:
        if ExperimentNumber in [2,3,5,7,12]:
            InstructionFileName = "MCP instruction ses 1a.txt"
        elif ExperimentNumber in [4]:
            InstructionFileName = "MCP instruction ses 1b.txt"
        elif ExperimentNumber in [9]:
            InstructionFileName = "MCP instruction ses 1c.txt"
        elif ExperimentNumber in [6,8,500]:
            InstructionFileName = "MCP instruction ses 1d.txt"
        elif ExperimentNumber in [10]:
            InstructionFileName = "MCP instruction ses 1e.txt"
        elif ExperimentNumber in [11]:
            InstructionFileName = "MCP instruction ses 1f.txt"
    elif Session == 2:
        InstructionFileName = "MCP instruction ses 2.txt"
    elif Session == 3:
        InstructionFileName = "MCP instruction ses 3.txt"
    elif Session == 4:
        InstructionFileName = "MCP instruction ses 4.txt"
    else:
        InstructionFileName = "MCP instruction exp ses.txt"

elif ExperimentType == 2:
    if Session == 1:
        if ExperimentNumber in [1001,1002]:
            InstructionFileName = "CPP instruction ses 1a.txt"
    elif Session == 2:
        InstructionFileName = "CPP instruction ses 2.txt"
    elif Session == 3:
        if ExperimentNumber in [1001]:
            InstructionFileName = "CPP instruction ses 3a.txt"
        if ExperimentNumber in [1002,1003]:
            InstructionFileName = "CPP instruction ses 3b.txt"
    elif Session == 4:
        InstructionFileName = "CPP instruction ses 4.txt"
    else:
        InstructionFileName = "CPP instruction exp ses.txt"

elif ExperimentType == 3:
    if Session == 1:
        if ExperimentNumber in [2001]:
            InstructionFileName = "CCRP instruction ses 1.txt"
    elif Session == 2:
        InstructionFileName = "CCRP instruction ses 2.txt"
    elif Session == 3:
        if ExperimentNumber in [2001]:
            InstructionFileName = "CCRP instruction ses 3.txt"
    elif Session in (4,5):
        InstructionFileName = "CCRP instruction ses 4-5.txt"
    else:
        InstructionFileName = "CCRP instruction exp ses.txt"

if InstructionFileName == "":
    print("Not valid experiment type")
    core.quit()

#------------------------------------------------
# Define mask exposure duration
#------------------------------------------------

MaskED = 50

#------------------------------------------------
# Load instructions
#------------------------------------------------

# Get the directory where this script is located
script_dir = os.path.dirname(os.path.abspath(__file__))
InstructionFileName = os.path.join(script_dir, "Instructions", InstructionFileName)

f = open(InstructionFileName, "r")
InstructionString = f.read()
f.close()

#------------------------------------------------
# Define global stimulus display variables
#------------------------------------------------

# Screen colors
#BgColor = (-1,-1,-1)
#BgColor = (-.5,-.5,-.5)
BgColor = (0,0,0)
EyeTrackerBgColor = BgColor

# Stimulus colors
CueTextColor = (-1,-1,-1)
CueBgColor = (1,1,1)

if ExperimentType == 1:
    CueArrowScaleLength = 0.8
elif ExperimentType == 2:
    CueArrowScaleLength = 1.0
elif ExperimentType == 3:
    CueArrowScaleLength = 1.0
else:
    print('Experiment type invalid')
    core.quit()

if Session >= NoPracSessions:
    CueArrowsColor = BgColor
else:
    if PracCueArrowResponseAssociations[Session-1] == 1:
        CueArrowsColor = (1,-1,-1)
    else:
        CueArrowsColor = BgColor


LetterColor = (1,1,1)

if VariableRewardFeedbackFlag:
    FeedbackZeroRewardColor = (1,-1,-1)
    FeedbackPosRewardColor = (-1,.7,-1)
else:
    FeedbackZeroRewardColor = LetterColor
    FeedbackPosRewardColor = LetterColor


# Stimulus contrasts
LetterContrast = 1.0
StimulusOpacity = 1.0


#StimFactor = 0.05
StimFactor = 0.04


# Stimulus sizes
FixationSize = 4 * StimFactor
CueBoxSize = 25 * StimFactor
CueTextSize = 20 * StimFactor
LetterSize = 60 * StimFactor

if ExperimentType in (1,2):
    TargetDistance = 200 * StimFactor
else:
    TargetDistance = 50 * StimFactor

MaskSize = [80 * StimFactor,80 * StimFactor] 
FeedbackLetterSize = 50 * StimFactor

# Response
ResponseLetterSize = 60 * StimFactor

ResponseColorSize = 30 * StimFactor
ResponseColorDistance = 50 * StimFactor

# Instruction color and sizes
InstructionColor = (1,1,1)
InstructionLetterSize = 15 * StimFactor

# Trial exit text, color, and size
if expInfo['eyetracker'] == 'none':
    ExitTrialText = "Choose\n\n\nS : exit\n\nSPACE : continue"
else:
    ExitTrialText = "Choose\n\n\nR : recalibrate\n\nS : exit\n\nSPACE : continue"

CalibrationAfterDriftCheckText = "Recalibration of eye tracker needed!\n\nPlease, call the experimenter\n\n\nR : recalibrate\n\nESC : continue without calibration"

ExitTrialColor = (.3,-7,-.7)
ExitTrialLetterSize = 50 * StimFactor

# Ready stimulus
ReadyColor = (-1,-1,-1)
ReadyLetterSize = 30 * StimFactor


# Fixation area color and sizes
if ExperimentNumber > 1000 and ExperimentNumber < 2000:
    FixationAreaSize = 50 * StimFactor
    DriftCorrectionAreaSize = FixationAreaSize * 1.5
    DriftCheckAreaSize = FixationAreaSize * 0.75
else:
    FixationAreaSize = 100 * StimFactor
    DriftCorrectionAreaSize = FixationAreaSize * .75
    DriftCheckAreaSize = FixationAreaSize * 0.75


# For stimulus displays
if ExperimentType in (1,2):
    FixationPointColor=(1,-1,-1)
elif ExperimentType == 3:
    FixationPointColor=(-1,-1,-1)
else:
    FixationPointColor=(-1,-1,-1)

# For eye tracker calibration
FixationAreaColor = (1,-1,-1)
FixationDotColor = (1,-1,-1)
FixationInstructionColor = (-1,-1,-1)

ResponseDeadlineColor = (1,.0,-.5)

# Point target area color and sizes
PointTargetSize = 10 * StimFactor
PointTargetEdgeColor = (-1,-1,-1)
PointTargetFillColor = (-1,1,-1)
PointTargetResponseFillColor = (-1,0,-1)

# PointTargetAreaSize = 50 * StimFactor
PointTargetAreaSize = 30 * StimFactor
PointTargetAreaColor = (-1,-1,-1)

PointTargetDotColor = (1,1,-1)

# Color target sizes
ColorTargetSize = 20 * StimFactor
ColorTargetEdgeColor = (-1,-1,-1)

# Calibration parameters
ContrastStepSize = 1.0
NoTrialCal = 10
ContrastCalibrationLevel = 0.7


# Drift check and correction coordinates
DriftCheckXY = [0,0]
DriftCorrectionXY = [0,0]





# Setup cue letter associations, no cues, cue locations, no letters, instruction arrows
# The CueAssocList indicates for each target/letter location the corresponding location of the associated cue location within the set of cue locations.

# standard settings
CueScaleFactor = 1.0
CueBoxShape = 'square'

if CueTargetAssociationType == 1:   # 4 complex associations on a line (as first standard experiments (1-4) in Paper # 1)
    NoCueLocations = 4
    CueAssocList = [1,2,4,3]
    CueLocRotation = 0
    CueDistance = 40 * StimFactor
    NoTargets = 4
    TargetLocRotation = pi/4

elif CueTargetAssociationType == 2: # 4 simple associations in a square
    NoCueLocations = 4
    CueAssocList = [1,2,3,4]
    CueDistance = 30 * StimFactor
    CueLocRotation = pi/4
    NoTargets = 4
    TargetLocRotation = pi/4

elif CueTargetAssociationType == 3: # 8 simple associations in a circle
    NoCueLocations = 8
    CueAssocList = [1,2,3,4,5,6,7,8]
    CueLocRotation = 0
    CueDistance = 50 * StimFactor
    NoTargets = 8
    TargetLocRotation = 0

elif CueTargetAssociationType == 4: # 8 complex associations in a circle with pair-wise clockwise/counter clockwise associations.
    NoCueLocations = 8
    CueAssocList = [2,1,4,3,6,5,8,7]
    CueLocRotation = 0
    CueDistance = 50 * StimFactor
    NoTargets = 8
    TargetLocRotation = 0

elif CueTargetAssociationType == 5: # 4 complex associations in a square with clockwise rotation of cue-target associations
    NoCueLocations = 4
    CueAssocList = [4,1,2,3]
    CueLocRotation = pi/4
    CueDistance = 30 * StimFactor
    NoTargets = 4
    TargetLocRotation = pi/4

elif CueTargetAssociationType == 6: # 4 complex associations in a cross
    NoCueLocations = 4
    CueAssocList = [1,2,3,4]
    CueLocRotation = 0
    CueDistance = 30 * StimFactor
    NoTargets = 4
    TargetLocRotation = pi/4

elif CueTargetAssociationType == 7: # 4 complex associations in a square small cues and small distances
    CueScaleFactor = 0.7
    NoCueLocations = 4
    CueAssocList = [1,2,3,4]
    CueLocRotation = pi/4   # 0
    CueDistance = 20 * StimFactor * CueScaleFactor
    NoTargets = 4
    TargetLocRotation = pi/4
    CueBoxSize = 25 * StimFactor * CueScaleFactor      # Overwrite size of cue boxes
    CueTextSize = 20 * StimFactor * CueScaleFactor     # Overwrite size of cue text font

elif CueTargetAssociationType == 8: # 4 complex associations in a cross small cues and small distances
    CueScaleFactor = 0.7
    NoCueLocations = 4
    CueAssocList = [1,2,3,4]
    CueLocRotation = pi/4   # 0
    CueDistance = 50 * StimFactor # * CueScaleFactor
    NoTargets = 4
    TargetLocRotation = pi/4
    CueBoxShape = 'circle'
    CueBoxSize = 25 * StimFactor * CueScaleFactor      # Overwrite size of cue boxes
    CueTextSize = 20 * StimFactor * CueScaleFactor     # Overwrite size of cue text font

else:
    print("No valid Cue Letter Association List indicated.")
    core.quit()


#------------------------------------------------
# Make data trial class
#------------------------------------------------

class trialClass:
    
    def __init__(self, Session, Block, WarmUp, CueCond, Cond, cueSOA, NoCueLocations, Cues, CuesVal, T, C):
        
        self.Session = Session
        self.Block = Block
#        self.Trial = 0
        self.CueCond = CueCond
        self.Cond = Cond
        self.cueSOA=cueSOA
        
        self.WarmUp = WarmUp
        
        # Assign empty Cue locations and shuffle cues
        self.NoCues = len(Cues)

        CueOrder = range(NoCueLocations)
        CueOrder = [*CueOrder]    # unpack range
        rnd.shuffle(CueOrder)
        
        self.Cues = NoCueLocations*[0]
        self.Cues[0:self.NoCues] = Cues

        self.CuesVal = NoCueLocations*[0]
        self.CuesVal[0:self.NoCues] = CuesVal
        
        temp1 = NoCueLocations*[0]
        temp2 = NoCueLocations*[0]
        for i in range(NoCueLocations):
            temp1[i] = self.Cues[CueOrder[i]]
            temp2[i] = self.CuesVal[CueOrder[i]]
            
        self.Cues = temp1 
        self.CuesVal = temp2
        
        
        self.CueRanks = [ sorted(self.Cues).index(x) for x in self.Cues ]
        self.CueRanks = [ NoCueLocations-x for x in self.CueRanks ]
        
        self.LetterContrast = -1
        
        self.T = T
        self.nT = len(T)

        self.C = C

        self.ED=StimED
        
        self.TrialStartJitterTime = 0.0
        
        self.CueTime = 0.0
        self.CueMaskTime = 0.0
        self.StimTime = 0.0
        self.MaskTime = 0.0
        self.EndTrialTime = 0.0
        self.mEDcues = 0.0
        self.mEDletters = 0.0
        self.mEDmasks = 0.0

        self.RT = 0.0
        self.LateResponse = False

        self.R = "-"
        self.RespLoc = ""
        self.PointTargetResponse = 0
        self.ACC = 0
        self.INTR = 0
        self.ERR = 0
        
        self.CueResponseValue = 0
        self.CueResponseExpValue = 0
        self.CueResponseRank = 0
        self.ExpectedReward = 0
        self.Reward = 0
        self.MaxReward = 0
        self.CumReward = 0.0
        
        self.Saccade = False
        
#        self.FixGazeX = 0.0
#        self.FixGazeY = 0.0
        self.CueGazeX = 0.0
        self.CueGazeY = 0.0
        self.LetterGazeX = 0.0
        self.LetterGazeY = 0.0
        self.MaskGazeX = 0.0
        self.MaskGazeY = 0.0
        
        self.PointTargetTime = 0.0
#        self.PointTarget = False
        self.TargetGazeX = 0.0
        self.TargetGazeY = 0.0
        
        self.ColorTargetTime = 0.0

        self.DriftCorrectX = 0.0
        self.DriftCorrectY = 0.0
        
        self.EyeOffsetTime = 0.0
        
        self.Note = ""

        self.NumberEyeSamples = 0
        self.EyeT = [0.0,] * MaxEyeRecordings
        self.EyeX = [0.0,] * MaxEyeRecordings
        self.EyeY = [0.0,] * MaxEyeRecordings
        self.EyeP = [0.0,] * MaxEyeRecordings   # Pupile size

#------------------------------------------------
# Make data block class
#------------------------------------------------

def blockFunction(ExperimentNumber, Session, Block, Repetitions, WarmUp, cueSOAconds, NoCueLocations, CueSet, CueSetVal, StimulusTargetLetters ):
        
    BlockData = []

    for rep in range(Repetitions):
        
        cuecond = 0
        cond = 0
        
        for i in range(len(CueSet)):
            
            Cues = CueSet[i]
            
            CuesVal = CueSetVal[i]
            cuecond += 1
            
            for cueSOA in cueSOAconds:
                
                    # Produce Target Letter Set 
                    temp = list(StimulusTargetLetters)
                    rnd.shuffle(temp)
                    LetterStimuli = ''.join(temp)

                    temp = list(StimulusTargetColors)
                    rnd.shuffle(temp)
                    ColorStimuli = ''.join(temp)

                    T = ""
                    C = ""
                   
                    if ExperimentType == 1:
                        for i in range(NoTargets):
                            T += LetterStimuli[i]
                        C = "-"
                    elif ExperimentType in (2,3):
                        T = "-"
                        for i in range(NoTargets):
                            C += ColorStimuli[i]
                    else:
                        print('Experiment type invalid')
                        core.quit()
                        

                    if (ExperimentNumber != 500) or (cueSOA==50 or len(Cues)==1 or Session <= 2): # only select a subset of the trial conditions for Experiment number 500
                        
                        cond += 1

                        tempData = trialClass(Session, Block, WarmUp, cuecond, cond, cueSOA, NoCueLocations, Cues, CuesVal, T, C)

                        BlockData.append( tempData )
                    
                        
        rnd.shuffle(BlockData)  # Shuffle the data within the block

    return(BlockData)
    
    
#------------------------------------------------
# Contruct trials
#------------------------------------------------

#FirstWarmUpTrials = 4
#OtherWarmUpTrials = 2
#Repetitions = 1
#NoBlocks = 4


Data = []

BlockRange = range(expInfo['startblock']-1, NoBlocks)    # Only generate the blocks from startblock

for Block in BlockRange:
    
    blocknumber = Block + 1

    WarmUpBlockData = blockFunction(ExperimentNumber, Session, blocknumber, Repetitions, 1, cueSOAconds, NoCueLocations, CueSet, CueSetVal, StimulusTargetLetters)
    
    BlockData = blockFunction(ExperimentNumber, Session, blocknumber, Repetitions, 0, cueSOAconds, NoCueLocations, CueSet, CueSetVal, StimulusTargetLetters)
    

    if FirstWarmUpTrials > 0:
        
        if blocknumber == expInfo['startblock']:
            del WarmUpBlockData[FirstWarmUpTrials:] # Get rid of the last trials
        else:
            del WarmUpBlockData[OtherWarmUpTrials:] # Get rid of the last trials
        
        Data.extend( WarmUpBlockData )    # Append the block of warm-up trials to the total set of trials


    Data.extend( BlockData )    # Append the block of experimental trials to the total set of trials

## Exclude trials with a lower block number than entered in input dialog
#Data[:] = [x for x in Data if not x.Block < expInfo['startblock'] ]


Ntrial = len(Data)


#for i in range(len(WarmUpBlockData)):
#    attrs = vars(WarmUpBlockData[i])
#    print(', '.join("%s: %s" % item for item in attrs.items()))
#    print("")


#------------------------------------------------
# Generate stimulus screen locations
#------------------------------------------------
CueLocationXY = []
LetterLocationXY = []
MaskLocationXY = []
PointTargetLocationXY = []
ColorTargetLocationXY = []

for i in range(NoCueLocations):
    
    if CueTargetAssociationType == 1:   # On a line
        x = CueDistance * (CueAssocList[i] - 1 - 1.5)
        y = 0

    elif CueTargetAssociationType in [2,3,4,5,6,7,8]:   # In a circle, square, or cross
        x = CueDistance * cos(2*pi/NoCueLocations * (CueAssocList[i]-1) + CueLocRotation)                
        y = CueDistance * sin(2*pi/NoCueLocations * (CueAssocList[i]-1) + CueLocRotation)

#    elif CueTargetAssociationType in [2,5]:   # In a square
#        x = CueDistance * cos(2*pi/NoCueLocations * (CueAssocList[i]-1) + pi/4)                
#        y = CueDistance * sin(2*pi/NoCueLocations * (CueAssocList[i]-1) + pi/4)
#        
#    elif CueTargetAssociationType in [3,4]:  # On a circle
#        x = CueDistance * cos(2*pi/NoCueLocations * (CueAssocList[i]-1))                
#        y = CueDistance * sin(2*pi/NoCueLocations * (CueAssocList[i]-1))
#
#    elif CueTargetAssociationType in [6]:   # In a cross
#        x = CueDistance * cos(2*pi/NoCueLocations * (CueAssocList[i]-1))                
#        y = CueDistance * sin(2*pi/NoCueLocations * (CueAssocList[i]-1))
        
    else:
        print("Unknown cue associaten type.")
        core.quit()
        
    CueLocationXY.append( [x,y] )


for i in range(NoTargets):
    
    x = TargetDistance * cos(2*pi/NoTargets*i + TargetLocRotation) # * StimFactor
    y = TargetDistance * sin(2*pi/NoTargets*i + TargetLocRotation) #* StimFactor
    
    LetterLocationXY.append( [x,y] )
    MaskLocationXY.append( [x,y] )
    
    PointTargetLocationXY.append( [x,y] )

    ColorTargetLocationXY.append( [x,y] )





#-----------------------------------------------------
# Setup monitor 
#-----------------------------------------------------

#use_unit_type = 'height'
use_unit_type = 'deg'
use_color_type = 'rgb'

MonitorName = "OF2A_03_5_513_lab5"
mon = monitors.Monitor(MonitorName)
mon.setSizePix((1920, 1080))
# mon.setSizePix((3440, 1440))
mon.setWidth(52)
mon.setDistance(60)
mon.saveMon()


#-----------------------------------------------------
# Setup and initiate experimental display
#-----------------------------------------------------

if sys.platform.find('win') != -1:
    #core.rush(3) # Enable in windows
    core.rush(True, realtime=True)
else:
    core.rush(True)

if expInfo['fullscreen'] == 'N':
    win = visual.Window((1920, 1080),
                        units=use_unit_type,
                        fullscr=False,
                        allowGUI=True,
                        colorSpace=use_color_type,
                        monitor="OF2A_03_5_513_lab5",
                        color=EyeTrackerBgColor )
    if expInfo['testrefreshrate'] == 'Y':
        fr = checkmonitor( win, set_resx=1920, set_resy=1080, set_framerate=expInfo['fixrefreshrate'], fr_tol = 10, maxnomesures = 10, nomesframes = 50, fullscreen = False )
else:
    win = visual.Window((1920, 1080),
#    win = visual.Window((2560, 1440),
#    win = visual.Window((3440, 1440),
                        units=use_unit_type,
                        fullscr=True,
                        allowGUI=False,
                        colorSpace=use_color_type,
                        monitor="OF2A_03_5_513_lab5",
                        color=EyeTrackerBgColor
                        )
    if expInfo['testrefreshrate'] == 'Y':
        fr = checkmonitor( win, set_resx=1920, set_resy=1080, set_framerate=expInfo['fixrefreshrate'], fr_tol = 10, maxnomesures = 10, nomesframes = 50, fullscreen = True )
#        fr = checkmonitor( win, set_resx=3440, set_resy=1440, set_framerate=expInfo['fixrefreshrate'], fr_tol = 4, maxnomesures = 50, nomesframes = 5 )
#        fr = checkmonitor( win, set_resx=1440, set_resy=900, set_framerate=expInfo['fixrefreshrate'], fr_tol = 4, maxnomesures = 50, nomesframes = 5 )


# Create Mouse object
MouseInstance = event.Mouse()

# Set refreshrate
RefreshRate = expInfo['fixrefreshrate']

try:
    MeasuredRefreshRate = win.getActualFrameRate()
    print("Refresh rate = %f" % MeasuredRefreshRate)
except:
    MeasuredRefreshRate = 0.0
    print("EXCEPTION: Not possible to measure refreshrate")


# Set delta value for reduction in waitning time before flipping pages. First the total time to next flip is calculated and then delta_t is deducted, e.g., if the ED is 500 ms at 100 Hz, delta will be 10 ms / 2 = 5 ms and the waiting time will be 500 ms - 5 ms = 495 ms before the flip is initiated.
delta_t = (1/RefreshRate)/2

#ISI = core.StaticPeriod(screenHz=RefreshRate)    # Setup Static Period Object to use for timing in between stimulus frames
ISI = core.StaticPeriod(None)    # Setup Static Period Object to use for timing in between stimulus frames




#------------------------------------------------------------------
# Setup instruction and feedback displays and display instructions
#------------------------------------------------------------------

EyeTrackerErrorText_stim = visual.TextStim(win, text="Fixation\nError!",
                            pos=[0*StimFactor, 0*StimFactor], height=InstructionLetterSize,
                            color=FixationAreaColor, units=use_unit_type,
                            wrapWidth=win.size[0] * .9)

EyeTrackerInitText_stim = visual.TextStim(win, text="Initializing Eyetracker",
                            pos=(0,0), height=InstructionLetterSize,
                            color=FixationInstructionColor, units=use_unit_type)

EyeTrackerDriftCorrectionText_stim = visual.TextStim(win, text="",
                            pos=(0,130*StimFactor), height=InstructionLetterSize,
                            color=FixationInstructionColor, units=use_unit_type)

ResponseDeadlineErrorText_stim = visual.TextStim(win, text="Too slow!",
                            pos=[0*StimFactor, 0*StimFactor], height=InstructionLetterSize,
                            color=ResponseDeadlineColor, units=use_unit_type,
                            wrapWidth=win.size[0] * .9)

ExitTrialText_stim = visual.TextStim(win, text=ExitTrialText,
                            pos=[0*StimFactor, 0*StimFactor], height=ExitTrialLetterSize,
                            color=ExitTrialColor, units=use_unit_type,
                            wrapWidth=win.size[0] * .9)

CalibrationAfterDriftCheckText_stim = visual.TextStim(win, text=CalibrationAfterDriftCheckText,
                            pos=[0*StimFactor, 0*StimFactor], height=ExitTrialLetterSize,
                            color=ExitTrialColor, units=use_unit_type,
                            wrapWidth=win.size[0] * .9)

Ready_stim = visual.TextStim(win, text="Press SPACE\n\nto continue",
                            pos=[0*StimFactor, 0*StimFactor], height=ReadyLetterSize,
                            color=ReadyColor, units=use_unit_type,
                            wrapWidth=win.size[0] * .9)


Instruction = visual.TextStim(win=win, ori=0,
    text=InstructionString,
    pos=[0, 0], height=InstructionLetterSize, wrapWidth=800*StimFactor,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)


Response = visual.TextStim(win=win, ori=0,
    text="",
    pos=[0, 0], height=ResponseLetterSize,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)

Feedback1 = visual.TextStim(win=win, ori=0,
    text="",
    pos=[0*StimFactor, 0*StimFactor], height=FeedbackLetterSize,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)
Feedback2 = visual.TextStim(win=win, ori=0,
    text="",
    pos=[0*StimFactor,-70*StimFactor], height=FeedbackLetterSize*0.6,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)
Feedback3 = visual.TextStim(win=win, ori=0,
    text="",
    pos=[0*StimFactor,-140*StimFactor], height=FeedbackLetterSize*0.6,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)
Feedback4 = visual.TextStim(win=win, ori=0,
    text="",
    pos=[0*StimFactor, -200*StimFactor], height=FeedbackLetterSize*0.2,
    color=[1,1,1],units=use_unit_type,opacity=StimulusOpacity)


# Set up color-response association instruction display
ColorResponseInstruction = []
for i in range(StimulusColorNoResponses):
    if Session >= NoPracSessions:
         ColorResponseInstruction.append( visual.Rect(win, width=0, height=0, lineColor=BgColor, fillColor=BgColor, units=use_unit_type, pos=(ResponseColorDistance*(-StimulusColorNoResponses/2+.5+i), -300*StimFactor)))
    else:
        if PracCueArrowResponseAssociations[Session-1] == 1:
            ColorResponseInstruction.append( visual.Rect(win, width=ResponseColorSize, height=ResponseColorSize, lineColor=StimulusTargetColorsRGB[i], fillColor=StimulusTargetColorsRGB[i], units=use_unit_type, pos=(ResponseColorDistance*(-StimulusColorNoResponses/2+.5+i), -150*StimFactor)))
        else:
            ColorResponseInstruction.append( visual.Rect(win, width=0, height=0, lineColor=BgColor, fillColor=BgColor, units=use_unit_type, pos=(ResponseColorDistance*(-StimulusColorNoResponses/2+.5+i), -300*StimFactor)))


#---------------------------------------------------------------------
# Setup eyemovement validation and drift correction fixation stimuli
#---------------------------------------------------------------------

# Create a target stim. iohub.client.eyetracker.validation.TargetStim provides a standard doughnut style
# target. Or use any stim that has `.setPos()`, `.radius`, `.innerRadius`, and `.draw()`.
Validation_Fixation_target_stim = TargetStim(win, radius=0.5, fillcolor=[.5, .5, .5], edgecolor=[-1, -1, -1], edgewidth=2,
                         dotcolor=[1, -1, -1], dotradius=0.20, units=use_unit_type, colorspace=use_color_type)

DriftCorrection_Fixation_target_stim = TargetStim(win, radius=0.5, fillcolor=[.5, .5, .5], edgecolor=[-1, -1, -1], edgewidth=2,
                         dotcolor=[-1, 1, -1], dotradius=0.20, units=use_unit_type, colorspace=use_color_type)

DriftCorrection_Fixation_target_stim_error = TargetStim(win, radius=0.5, fillcolor=[.5, .5, .5], edgecolor=[-1, -1, -1], edgewidth=2,
                         dotcolor=[1, -1, -1], dotradius=0.20, units=use_unit_type, colorspace=use_color_type)

#-------------------------------------------------
# Setup eyetracker
#-------------------------------------------------

EyeTrackerInitText_stim.draw()
win.flip()


# Eye tracker to use ('mouse', 'eyelink', 'gazepoint', or 'tobii')

TRACKER = expInfo['eyetracker'] 

if TRACKER == 'mouse':
    target_positions = 'THREE_POINTS'
elif TRACKER == 'eyelink':
    target_positions = 'FIVE_POINTS'
    #target_positions = 'NINE_POINTS',
    #target_positions = [(0.0, 0.0), (0.2, 0.2), (-0.2, 0.0), (0.2, 0.0), (0.2, -0.2), (-0.2, 0.2), (-0.2, -0.2), (0.0, 0.2), (0.0, -0.2)]
    #target_positions = [(0.0, -0.2), (0.2, 0.2), (-0.2, 0.2)]
else:
    target_positions = ''
    


eyetracker_config = dict(name='tracker')
#eyetracker_config = {'name': 'tracker', 'device_timer': {'interval': 0.005}}   # Set the sampling rate
#eyetracker_config = {'name': 'tracker', 'device_timer': {'interval': 0.100}}   # Set the sampling rate

devices_config = {}
#devices_config['experiment_code'] = "TestioHUbSave"    # If set, the ioHub will create a HDF5 file.
#devices_config['experiment_session'] = "TestioHUbSaveSession"

if TRACKER == 'mouse':
    win.setMouseVisible(True)
#    devices_config['eyetracker.hw.mouse.EyeTracker'] = eyetracker_config
    eyetracker_config['event_buffer_length'] = 2048
    eyetracker_config['runtime_settings'] = dict(sampling_rate=100, track_eyes='RIGHT')
    eyetracker_config['calibration'] = dict(auto_pace=False,
                                            target_duration=.1,
                                            target_delay=.5,
                                            screen_background_color=EyeTrackerBgColor,
                                            type=target_positions,
#                                            positions=target_positions,
                                            unit_type=None,
                                            color_type=None,
                                            target_attributes=dict(outer_diameter=1.0,
                                                                   inner_diameter=0.5,
                                                                   outer_fill_color=[-0.5, -0.5, -0.5],
                                                                   inner_fill_color=[-1, 1, -1],
                                                                   outer_line_color=[1, 1, 1],
                                                                   inner_line_color=[-1, -1, -1],
                                                                   animate=dict(enable=False,
                                                                                expansion_ratio=1.5,
                                                                                contract_only=False)
                                                                   )
                                            )
    devices_config['eyetracker.hw.mouse.EyeTracker'] = eyetracker_config

elif TRACKER == 'eyelink':
    win.setMouseVisible(False)
    eyetracker_config['model_name'] = 'EYELINK 1000 DESKTOP'
    eyetracker_config['simulation_mode'] = False
    eyetracker_config['event_buffer_length'] = 2048 # 3072
    eyetracker_config['runtime_settings'] = dict(sampling_rate=1000, track_eyes='RIGHT')
    eyetracker_config['calibration'] = dict(auto_pace=False,
                                            target_duration=0.5,
                                            target_delay=1.0,
                                            screen_background_color=EyeTrackerBgColor,
                                            type='NINE_POINTS',
                                            unit_type=None,
                                            color_type=None,
                                            target_attributes=dict(outer_diameter=1.0,
                                                                   inner_diameter=0.5,
                                                                   outer_fill_color=[-0.5, -0.5, -0.5],
                                                                   inner_fill_color=[-1, 1, -1],
                                                                   outer_line_color=[1, 1, 1],
                                                                   inner_line_color=[-1, -1, -1]
                                                                   )
                                            )
    devices_config['eyetracker.hw.sr_research.eyelink.EyeTracker'] = eyetracker_config 

elif TRACKER == 'none':
    win.setMouseVisible(False)
    eyetracker_config = dict(name='notracker')
else:
    print("{} is not a valid TRACKER name; please use 'mouse', 'eyelink', 'gazepoint', or 'tobii'.".format(TRACKER))
    core.quit()
    
print("\n")
print("\n")

# Since no experiment_code or session_code is given, no iohub hdf5 file
# will be saved, but device events are still available at runtime.


if TRACKER != 'none':

    io = iohub.launchHubServer(window=win, **devices_config)

    keyboard = io.getDevice('keyboard')

    # Get some iohub devices for future access.
    tracker = io.getDevice('tracker')

    
    if TRACKER != 'mouse':
        # Minimize the PsychoPy window if needed
        hideWindow(win)
#        win.setMouseVisible(False)
#        win.winHandle.set_fullscreen(False) # disable fullscreen
        
        if TRACKER != 'mouse' and expInfo['fullscreen'] == 'Y':
            MouseInstance.setPos((10000,-10000)) # move mouse to lower-right corner of screen
        
        # Display calibration gfx window and run calibration.
        result = tracker.runSetupProcedure()
        print("\nCalibration returned: \n", result, "\n")
        
        # Maximize the PsychoPy window if needed
        showWindow(win)
#        win.winHandle.maximize()
#        win.winHandle.set_fullscreen(True) 
#        win.winHandle.activate()
#        win.setMouseVisible(False)
#        win.flip()

    

# Use EyeLink internal validation procedure instead
#
#    #-------------------------------------------------
#    # Validation
#    #-------------------------------------------------
#
#    # Create a validation procedure, iohub must already be running with an
#    # eye tracker device, or errors will occur.
#    validation_proc = iohub.ValidationProcedure(win,
#                                                target=Validation_Fixation_target_stim,  # target stim
#                                                positions=target_positions,  # string constant or list of points
#                                                randomize_positions=True,  # boolean
#                                                expand_scale=1.0,  # float
#                                                target_duration=0.5,  # float
#                                                target_delay=0.1,  # float
#                                                enable_position_animation=False,
#                                                color_space=use_color_type,
#                                                unit_type=use_unit_type,
#                                                unit_type=None,
#                                                progress_on_key=" ",  # str or None
#                                                gaze_cursor=(-1.0, 1.0, -1.0),  # None or color value
#                                                show_results_screen=True,  # bool
#                                                save_results_screen=False,  # bool, only used if show_results_screen == True
#                                                )
#
#
#    # Run the validation procedure. run() does not return until the validation is complete.
#    validation_proc.run()
#    
#    if validation_proc.results:
#        results = validation_proc.results
#        print("\n++++ Validation Results ++++")
#        print("Passed:", results['passed'])
#        print("failed_pos_count:", results['positions_failed_processing'])
#        print("Units:", results['reporting_unit_type'])
#        print("min_error:", results['min_error'])
#        print("max_error:", results['max_error'])
#        print("mean_error:", results['mean_error'])
#    else:
#        print("\nValidation Aborted by User.")
#    print("\n\n")




#-------------------------------------------------
#-------------------------------------------------


#------------------------------------------------
# Setup data file
#------------------------------------------------

if expInfo['practice'] == 'N':
    DataFileName = ExperimentName+ '-subj-'+'{:03d}'.format(expInfo['subject'])+'-ses-'+'{:03d}'.format(expInfo['session'])+"-"+expInfo['date']+'.dat'
else:
    DataFileName = ExperimentName + '-Prac'+'-subj-'+'{:03d}'.format(expInfo['subject'])+'-ses-'+'{:03d}'.format(expInfo['session'])+"-"+expInfo['date']+'.dat'

print(DataFileName)

write_log(DataFileName+"\n")
write_log( 'Subject {0} session {1}\n'.format(expInfo['subject'], expInfo['session']) )


# Create directory if it doesn't already exist
if not os.path.exists("Data/"):
   os.makedirs("Data/")

f = open("Data/"+DataFileName, "a")


f.write( 'PsychoPy version: {0}\n'.format(PsychoPyVersion) )
f.write( 'Experimental Program: {0}\n'.format(ExperimentalProgramName) )
f.write( 'Experiment Name: {0}\n'.format(ExperimentName) )
f.write( 'Experiment Type: {0}\n'.format(ExperimentType) )
# f.write( 'Date_Time: {0}\n'.format(expInfo['date']) )
f.write( 'Exp start time: {0}\n'.format(ExpStartTime) )
# f.write( 'Exp end time: {0}\n'.format(ExpEndTime) )
f.write( '\n' )

f.write( 'Subject: {0}\n'.format(expInfo['subject']) )
f.write( 'Age: {0}\n'.format(expInfo['age']) )
f.write( 'Gender: {0}\n'.format(expInfo['gender']) )
f.write( '\n' )

f.write( 'Practice: {0}\n'.format(expInfo['practice']) )
f.write( 'Session: {0}\n'.format(Session) )
f.write( 'Number of Blocks: {0}\n'.format(NoBlocks) )
f.write( 'Repetitions: {0}\n'.format(Repetitions) )
f.write( 'Total number of trials: {0}\n'.format(Ntrial) )
f.write( 'Warmup trials 1st block: {0}\n'.format(FirstWarmUpTrials) )
f.write( 'Warmup trials other blocks: {0}\n'.format(OtherWarmUpTrials) )
f.write( '\n' )

f.write( 'Cue values: {0}\n'.format(CueValue) )
f.write( 'Cue Symbols: {0}\n'.format(CueSymbols) )
f.write( 'Number of cue conditions: {0}\n'.format(len(ExpCueSet)) )
f.write( 'Cue sets: {0}\n'.format(CueSet) )
f.write( 'CueValue sets: {0}\n'.format(CueSetVal) )
f.write( 'Reward Money Factor: {0}\n'.format(RewardMoneyFactor) )
f.write( 'Cue SOA set: {0}\n'.format(cueSOAconds) )
f.write( 'Letter stimuli ED: {0}\n'.format(StimED) )
f.write( 'Mask ED: {0}\n'.format(MaskED) )
f.write( '\n' )

f.write( 'Full screen: {0}\n'.format(expInfo['fullscreen']) )
f.write( 'Fixed refreshRate: {0}\n'.format(RefreshRate) )
f.write( 'Measured refreshRate: {0}\n'.format(MeasuredRefreshRate) )
f.write( 'Test refreshrate: {0}\n'.format(expInfo['testrefreshrate']) )
f.write( '\n' )
f.write( 'Eye tracker: {0}\n'.format(TRACKER) )
f.write( 'Number blocks between drift corrections (0 = no drift correction): {0}\n'.format(DriftCorrectionNoBlocks) )
f.write( 'Maximum number of eye movement samples: {0}\n'.format(MaxEyeRecordings) )
f.write( '\n' )
f.write( 'Offset trial start jitter time: {0}\n'.format(TrialStartJitterOffsetTime) )
f.write( 'Mean trial start jitter time: {0}\n'.format(TrialStartJitterMeanTime) )
f.write( 'Max trial start jitter time: {0}\n'.format(TrialStartJitterMaxTime) )
f.write( '\n' )

f.write( 'Response keys: {0}\n'.format(ResponseKeys) )
f.write( 'Response deadline: {0}\n'.format(ResponseDeadline) )
f.write( '\n' )

f.write( 'Monitor name: {0}\n'.format(MonitorName) )
f.write( 'Monitor set size: {0} pixels\n'.format(mon.getSizePix()) )
f.write( 'Monitor actual size: {0} pixels\n'.format(win.size) )
f.write( 'Monitor width: {0} cm\n'.format(mon.getWidth()) )
f.write( 'Monitor distance: {0} cm\n'.format(mon.getDistance()) )
f.write( '\n' )
f.write( 'Stimulus scale factor: {0}\n'.format(StimFactor) )
f.write( 'Fixation size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(FixationSize, misc.deg2cm(FixationSize,mon), misc.deg2pix(FixationSize,mon)) )
f.write( 'Fixation area size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(FixationAreaSize, misc.deg2cm(FixationAreaSize,mon), misc.deg2pix(FixationAreaSize,mon)) )
f.write( 'Cue size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(CueBoxSize, misc.deg2cm(CueBoxSize,mon), misc.deg2pix(CueBoxSize,mon)) )
f.write( 'Cue text size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(CueTextSize, misc.deg2cm(CueTextSize,mon), misc.deg2pix(CueTextSize,mon)) )
f.write( 'Cue distance: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(CueDistance, misc.deg2cm(CueDistance,mon), misc.deg2pix(CueDistance,mon)) )
f.write( 'Target distance: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(TargetDistance, misc.deg2cm(TargetDistance,mon), misc.deg2pix(TargetDistance,mon)) )
f.write( 'Letter size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(LetterSize, misc.deg2cm(LetterSize,mon), misc.deg2pix(LetterSize,mon)) )
f.write( 'Point target size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(PointTargetSize, misc.deg2cm(PointTargetSize,mon), misc.deg2pix(PointTargetSize,mon)) )
f.write( 'Point target area size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(PointTargetAreaSize, misc.deg2cm(PointTargetAreaSize,mon), misc.deg2pix(PointTargetAreaSize,mon)) )
f.write( 'Color target size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(ColorTargetSize, misc.deg2cm(ColorTargetSize,mon), misc.deg2pix(ColorTargetSize,mon)) )
f.write( 'Mask size X: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(MaskSize[0], misc.deg2cm(MaskSize[0],mon), misc.deg2pix(MaskSize[0],mon)) )
f.write( 'Mask size Y: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(MaskSize[1], misc.deg2cm(MaskSize[1],mon), misc.deg2pix(MaskSize[1],mon)) )
f.write( 'Feedback letter size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(FeedbackLetterSize, misc.deg2cm(FeedbackLetterSize,mon), misc.deg2pix(FeedbackLetterSize,mon)) )
f.write( 'Response letter size: {0} deg, {1:.2f} mm, {2:.2f} pixels\n'.format(ResponseLetterSize, misc.deg2cm(ResponseLetterSize,mon), misc.deg2pix(ResponseLetterSize,mon)) )


f.write( 'Background color: {0}\n'.format(BgColor) )
f.write( 'Eye tracker calibration/validation background color: {0}\n'.format(EyeTrackerBgColor) )
f.write( 'Cue background color: {0}\n'.format(CueBgColor) )
f.write( 'Cue text color: {0}\n'.format(CueTextColor) )
f.write( 'Cue box shape: {0}\n'.format(CueBoxShape) )
f.write( 'Cue arrow color: {0}\n'.format(CueArrowsColor) )
f.write( 'Letter color: {0}\n'.format(LetterColor) )
f.write( 'Letter contrast: {0}\n'.format(LetterContrast) )
f.write( 'Stimulus opacity: {0}\n'.format(StimulusOpacity) )
f.write( 'Stimulus target colors: {0}\n'.format(StimulusTargetColors) )
f.write( 'Stimulus target RGB color values: {0}\n'.format(StimulusTargetColorsRGB) )
f.write( 'Color target edgecolor: {0}\n'.format(ColorTargetEdgeColor) )

PointTargetEdgeColor = (-1,-1,-1)
PointTargetFillColor = (-1,1,-1)
PointTargetResponseFillColor = (-1,0,-1)

f.write( '\n\n' )


# Using internal EyeLink validation instead
#
#if TRACKER != 'none':
#    if validation_proc.results:
#        results = validation_proc.results
#        f.write("\n++++ Eye Movement Validation Results ++++\n")
#        f.write("Passed: {0}\n".format(results['passed']) )
#        f.write("failed_pos_count: {0}\n".format(results['positions_failed_processing']) )
#        f.write("Units: {0}\n".format(results['reporting_unit_type']) )
#        f.write("min_error: {0}\n".format(results['min_error']) )
#        f.write("max_error: {0}\n".format(results['max_error']) )
#        f.write("mean_error: {0}\n".format(results['mean_error']) )
#    
#f.write( '\n' )

f.write('ExperimentName\t')
f.write('ExperimentNumber\t')
f.write('Subject\t')
f.write('Session\t')
f.write('Block\t')
f.write('Trial\t')
f.write('WarmUpTrial\t')
f.write('CueCondition\t')
f.write('Condition\t')
f.write('NoCues\t')
f.write('TrialStartJitterTime\t')
f.write('CueSOA\t')
f.write('Cues\t')
f.write('CueValues\t')
f.write('CueRanks\t')
f.write('LetterContrast\t')
f.write('EDletters\t')
f.write('T\t')
f.write('C\t')
f.write('R\t')
f.write('RespLoc\t')
f.write('PointTargetResponse\t')
f.write('RT\t')
f.write('LateResponse\t')
f.write('ACC\t')
f.write('INTR\t')
f.write('ERR\t')
f.write('CueResponseValue\t')
f.write('CueResponseExpValue\t')
f.write('CueRankResponse\t')
f.write('ExpectedReward\t')
f.write('Reward\t')
f.write('MaxReward\t')
f.write('CumReward\t')
f.write('Saccade\t')
f.write('CueGazeX\t')
f.write('CueGazeY\t')
f.write('LetterGazeX\t')
f.write('LetterGazeY\t')
f.write('MaskGazeX\t')
f.write('MaskGazeY\t')
f.write('TargetGazeX\t')
f.write('TargetGazeY\t')
f.write('DriftCorrectX\t')
f.write('DriftCorrectY\t')
f.write('RefreshRate\t')
f.write('MeasuredRefreshRate\t')
f.write('mEDcues\t')
f.write('mEDletters\t')
f.write('mEDmasks\t')
f.write('CueTime\t')
f.write('CueMaskTime\t')
f.write('StimTime\t')
f.write('MaskTime\t')
f.write('PointTargetTime\t')
f.write('ColorTargetTime\t')
f.write('EndTrialTime\t')
f.write('EyeOffsetTime\t')

f.write('Note\t')

f.write('NumberEyeSamples\t')

for i in range(0,MaxEyeRecordings):
    f.write( 'EyeT{0}\t'.format(i+1) )
    f.write( 'EyeX{0}\t'.format(i+1) )
    f.write( 'EyeY{0}\t'.format(i+1) )
    f.write( 'EyeP{0}\t'.format(i+1) )

f.write( '\n' )

f.close()


#------------------------------------------------
# Clear display
#------------------------------------------------

win.setColor(BgColor)
win.flip()
win.flip()

#------------------------------------------------
# Displays Instructions
#------------------------------------------------

#Instruction.draw()
#win.flip()
#event.clearEvents()
#thisKey = event.waitKeys()
#event.clearEvents()

# Make sure that instruction screen is redrawn if eye tracker does not return gracefully
thisKey = []
while len(thisKey) == 0:
    Instruction.draw()
    win.flip()
    core.wait(.1)
    thisKey = event.getKeys()
    if len(thisKey) > 0:
        print(thisKey)


#------------------------------------------------
# Setup trial displays
#------------------------------------------------
#Fixation = visual.PatchStim(win, tex=None, mask='circle',sf=0, size=FixationSize, units=use_unit_type)
Fixation = visual.Circle(win, size=FixationSize, units=use_unit_type)
Fixation.setColor(FixationPointColor)

StimList = [Fixation]
FixationDisplay = visual.BufferImageStim(win, stim=StimList)


# Set op cues (reward digits/symbols), cue boxes (squares for the cues digits/symbols), cue arrows (lines from cues to targets), cue masks (#), mask cue box (squares for the cue mask).
Cue = []
CueBox = []
CueArrows = []
MaskCue = []
MaskCueBox = []
 
# Create the cue screen objects
for i in range(NoCueLocations):
    Cue.append( visual.TextStim(win=win, ori=0, text="", height=CueTextSize, units=use_unit_type, opacity=StimulusOpacity, color=CueTextColor ) )
    if CueBoxShape == 'circle':
        CueBox.append( visual.Circle(win=win, radius=CueBoxSize/2, units=use_unit_type,fillColor=CueBgColor) )
        MaskCueBox.append( visual.Circle(win=win, radius=CueBoxSize/2, units=use_unit_type,fillColor=CueBgColor) )
    else: # square
        CueBox.append( visual.Rect(win=win, width=CueBoxSize, height=CueBoxSize, units=use_unit_type,fillColor=CueBgColor) )
        MaskCueBox.append( visual.Rect(win=win, width=CueBoxSize, height=CueBoxSize, units=use_unit_type,fillColor=CueBgColor) )
    CueArrows.append( visual.Line(win=win, lineColor=CueArrowsColor,units=use_unit_type) )
    MaskCue.append( visual.TextStim(win=win, ori=0, text="", height=CueTextSize, units=use_unit_type, opacity=StimulusOpacity, color=CueTextColor ) )

# Set up letter target stimuli, masks, and pointing targets
LetterStim = []
Mask = []
PointTarget = []
PointTargetArea = []
ColorTarget = []
for i in range(NoTargets):
    LetterStim.append( visual.TextStim(win=win,text="", height=LetterSize, units=use_unit_type, opacity=StimulusOpacity, color=LetterColor) )
    Mask.append( visual.ImageStim(win=win, size=MaskSize, units=use_unit_type, opacity=StimulusOpacity, pos=MaskLocationXY[i] ) )

    PointTarget.append( visual.Circle(win, lineColor=PointTargetEdgeColor, fillColor=PointTargetFillColor, radius=PointTargetSize, units=use_unit_type, edges = 200, lineWidth=3, pos=PointTargetLocationXY[i]  ) )
    PointTargetArea.append( visual.Circle(win, lineColor=PointTargetAreaColor, fillColor=BgColor, radius=PointTargetAreaSize, units=use_unit_type, edges = 200, lineWidth=1, pos=PointTargetLocationXY[i]  ) )

    ColorTarget.append( visual.Circle(win, lineColor=ColorTargetEdgeColor, fillColor=(0,0,0), radius=ColorTargetSize, units=use_unit_type, edges = 200, lineWidth=3, pos=ColorTargetLocationXY[i]  ) )

PointTargetResponse = visual.Circle(win, lineColor=PointTargetEdgeColor, fillColor=PointTargetResponseFillColor, radius=PointTargetSize, units=use_unit_type, edges = 200, lineWidth=3 )


# Setup the locations of cues and cue masks
for i in range(NoCueLocations):
    Cue[i].setPos( CueLocationXY[i] )
    CueBox[i].setPos( CueLocationXY[i] )
    if ExperimentType == 1:
        CueArrows[i].setVertices( [ CueLocationXY[i], [LetterLocationXY[i][0]*CueArrowScaleLength, LetterLocationXY[i][1]*CueArrowScaleLength] ] )
    elif ExperimentType == 2:
        CueArrows[i].setVertices( [ CueLocationXY[i], [PointTargetLocationXY[i][0]*CueArrowScaleLength, PointTargetLocationXY[i][1]*CueArrowScaleLength] ] )
    elif ExperimentType == 3:
        CueArrows[i].setVertices( [ CueLocationXY[i], [ColorTargetLocationXY[i][0]*CueArrowScaleLength, ColorTargetLocationXY[i][1]*CueArrowScaleLength] ] )
    else:
        print('Experiment type invalid')
        core.quit()

    MaskCue[i].setPos( CueLocationXY[i] )
    MaskCueBox[i].setPos( CueLocationXY[i] )

# Setup the location of the targets and masks
for i in range(NoTargets):
    LetterStim[i].setPos( LetterLocationXY[i] )
    Mask[i].setPos( LetterLocationXY[i] )

# Define fixation region and fixation dot
FixationRegion = visual.Circle(win, lineColor=FixationAreaColor, fillColor=BgColor, radius=FixationAreaSize, units=use_unit_type, edges = 200, lineWidth=5 )
# FixationDot = visual.GratingStim(win, tex=None, mask='gauss', pos=(0, 0), size=(0.5, 0.5), color=FixationDotColor, units=use_unit_type)
# FixationDot = visual.PatchStim(win, tex=None, mask='circle',sf=0, pos=(0, 0), size=FixationSize, color=FixationDotColor, units=use_unit_type)
FixationDot = visual.Circle(win, pos=(0,0), size=FixationSize, color=FixationDotColor, units=use_unit_type)

# Define pointing target dot
PointTargetDot = visual.GratingStim(win, tex=None, mask='gauss', pos=(0, 0), size=(0.5, 0.5), color=PointTargetDotColor, units=use_unit_type)

# Define drift check region
DriftCheckRegion = visual.Circle(win, lineColor=FixationAreaColor, fillColor=BgColor, radius=DriftCheckAreaSize, units=use_unit_type, edges = 200, lineWidth=5 )

# Define drift correction region
DriftCorrectionRegion = visual.Circle(win, lineColor=FixationAreaColor, fillColor=BgColor, radius=DriftCorrectionAreaSize, units=use_unit_type, edges = 200, lineWidth=5 )

#------------------------------------------------
# Initiate experimental task timers
#------------------------------------------------

# trialClock=core.Clock()
# trialClock.reset()

# t = 0
# m = 0


if TRACKER == 'mouse':
    win.setMouseVisible(True)
else:
    win.setMouseVisible(False)


#------------------------------------------------
# Run experimental task trials
#------------------------------------------------


for trial in range(Ntrial):


    ExitTrialFlag = False   # Set exit trial flag


    if TRACKER != 'mouse' and expInfo['fullscreen'] == 'Y':
        MouseInstance.setPos((10000,-10000)) # move mouse to lower-right corner of screen


#------------------------------------------------
# Run trial
#------------------------------------------------

    # TestDriftCheckCorrection = True
    TestDriftCheckCorrection = False
    
    if (TestDriftCheckCorrection and trial % 3 == 0) or trial == 0 or Data[trial].Block != Data[trial-1].Block:
        
        write_log( '\nBlock: {0} Trial: '.format(Data[trial].Block))

        InstructionString = "Block number {0} of {1} blocks\n\nPress space to continue!\n\n\n".format( Data[trial].Block, NoBlocks )

        Instruction.setText(InstructionString)
        Instruction.draw()
        win.flip()
        
        core.wait(.5)
        
        if trial == 0 or expInfo['testtiming'] == 'N':
            
            event.clearEvents()
            thisKey = event.waitKeys()
            
        
        #------------------------------------------------
        # Drift checking / correction 
        #------------------------------------------------
        escape_driftcorrection_check = False

        while not ExitTrialFlag and not escape_driftcorrection_check and TRACKER != 'none' and expInfo['eyetrackercheck'] in ["driftcheck","driftcorrection"] and expInfo['testtiming'] == 'N' and DriftCorrectionNoBlocks > 0 and (Data[trial].Block % DriftCorrectionNoBlocks) == 0:    # Do drift check/correction every DriftCorrectionNoBlocks
            
            io.clearEvents()

            tracker.setRecordingState(True)

            win.flip()

            EyeTrackerDriftCorrectionText_stim.setText(text="Prepare for Drift Check/Correction\n\n\nPlease, first please press the RETURN key to continue.")
            EyeTrackerDriftCorrectionText_stim.draw()
            FixationDot.setPos((0,0))
            FixationDot.draw()

            win.flip()

            event.clearEvents()
            thisKey = event.waitKeys(keyList=['return','escape'])
            
            if thisKey[0] == 'escape':
                escape_driftcorrection_check = True
            
            EyeTrackerDriftCorrectionText_stim.setText(text="Drift Check/Correction\n\n\nPlease, fixate on the GREEN point before pressing the SPACE bar.\n\nIf it turns RED try again.")
            EyeTrackerDriftCorrectionText_stim.draw()
            DriftCorrection_Fixation_target_stim.setPos((0,0))
            DriftCorrection_Fixation_target_stim.draw()
            
            win.flip()
            
            event.clearEvents()
            thisKey = event.waitKeys(keyList=['space','escape'])
            
            if thisKey[0] == 'escape':
                escape_driftcorrection_check = True

            # valid_driftcorrection_sample = False
            # # Loop until fixation within the driftcorrection area and <space> has been pressed on the keyboard
            # while not escape_driftcorrection_check and not valid_driftcorrection_sample:

            #     # Sample fixation location
            #     gpos_driftcorrect = tracker.getLastGazePosition()
            #     valid_gaze_pos = isinstance(gpos_driftcorrect, (tuple, list))
               
            #     # If we have a gaze position from the tracker, check whether it is inside the fixation area.
            #     if valid_gaze_pos:
                
            #         testgpos = list( map(sub,gpos_driftcorrect,DriftCorrectionXY) )  # correct the recorded fixation position with the current drift correction coordinates)
            #         gaze_in_region = valid_gaze_pos and DriftCorrectionRegion.contains(testgpos) # Test whether the fixation position is within the fixation area as a sanity check that the participant is trying to fixate the drift correction target
                    
            #         if not gaze_in_region and False:
            #             EyeTrackerDriftCorrectionText_stim.draw()
            #             DriftCorrectionRegion.draw()
            #             DriftCorrection_Fixation_target_stim_error.draw()
            #             win.flip()
                        
            #             core.wait(1.0)

            #             EyeTrackerDriftCorrectionText_stim.draw()
            #             DriftCorrection_Fixation_target_stim.draw()
            #             win.flip()
             
            #             event.clearEvents()
            #             thisKey = event.waitKeys(keyList=['space','escape'])
                        
            #             if thisKey[0] == 'escape':
            #                 escape_driftcorrection_check = True
                        
            #         else:
            #             valid_driftcorrection_sample = True


            # Record the eye position without driftcorrection
            valid_gaze_pos = False
            while not valid_gaze_pos:
                gpos_driftcorrect = tracker.getLastGazePosition()
                valid_gaze_pos = isinstance(gpos_driftcorrect, (tuple, list))
            
            tracker.setRecordingState(False)


            DriftCorrectionXY = gpos_driftcorrect
            DriftCheckXY = gpos_driftcorrect


            driftcorrection_check_make_recalibration = False


            if expInfo['eyetrackercheck'] == "driftcorrection":
                
                # # Collect the sample used for the actual drift correction coordinates
                # valid_gaze_pos = False
                # while not valid_gaze_pos:
                #     gpos_driftcorrect = tracker.getLastGazePosition()
                #     valid_gaze_pos = isinstance(gpos_driftcorrect, (tuple, list))

                #     testgpos = list( map(sub,gpos_driftcorrect,DriftCorrectionXY) )  # correct the recorded fixation position with the current drift correction coordinates)
                #     gaze_in_region = valid_gaze_pos and DriftCorrectionRegion.contains(testgpos) # Test whether the fixation position is within the fixation area as a sanity check that the participant is trying to fixate the drift correction target

                # tracker.setRecordingState(False)

                gaze_in_region = DriftCorrectionRegion.contains(gpos_driftcorrect) # Test whether the fixation position is within the fixation area as a sanity check that the participant is trying to fixate the drift correction target

                if not gaze_in_region:
                    driftcorrection_check_make_recalibration = True
                else:
                    #If it is the very first trial, then disregard the drift correction measure
                    if trial == 0:
                        # Save drift correction in note to trial
                        Data[trial].Note += 'FirstDriftCorrection({0},{1});'.format(DriftCorrectionXY[0],DriftCorrectionXY[1])
                        # Reset drift correction coordinates to (0,0)
                        DriftCorrectionXY = [0,0]
                    
                    escape_driftcorrection_check = True



            elif expInfo['eyetrackercheck'] == "driftcheck":

                # Collect the sample used for the actual drift correction coordinates
                # valid_gaze_pos = False
                # while not valid_gaze_pos:
                #     DriftCheckXY = tracker.getLastGazePosition()
                #     valid_gaze_pos = isinstance(DriftCheckXY, (tuple, list))
                # tracker.setRecordingState(False)

                gaze_in_region = DriftCheckRegion.contains(DriftCheckXY) # Test whether the fixation position is within the fixation area as a sanity check that the participant is trying to fixate the drift correction target
                    
               
                if not gaze_in_region:
                    driftcorrection_check_make_recalibration = True  # Do re-calibration of eye tracker
                else:
                    escape_driftcorrection_check = True
                    # Save drift check in note to trial
                    Data[trial].Note += 'DriftCheck({0},{1});'.format(DriftCheckXY[0],DriftCheckXY[1])



            if driftcorrection_check_make_recalibration:  # Do re-calibration of eye tracker
                
                CalibrationAfterDriftCheckText_stim.draw()
                win.flip()

                event.clearEvents()
                thisKey = event.waitKeys(keyList=['r','R','escape'])

                win.flip()

                # Recallibrate eye tracker 
                if len(thisKey)>0 and thisKey[0].upper() == 'R':
                    
                    hideWindow(win)
                    
                    # Display calibration gfx window and run calibration.
                    result = tracker.runSetupProcedure()
                    print("\nCalibration returned: \n", result, "\n")
                    
                    # Maximize the PsychoPy window if needed
                    showWindow(win)
                    
                    # Reset drift correction coordinates to (0,0)
                    DriftCorrectionXY = [0,0]

                    Data[trial].Note += 'Recalibrating_eyetracker;'
                    write_log( 'Recalibrating_eyetracker ' )
            
                escape_driftcorrection_check = False                


        event.clearEvents()
        thisKey = []


        FixationDisplay.draw()
        Ready_stim.draw()
        win.flip()


        
        event.clearEvents()
        thisKey = event.waitKeys()
        win.flip()

    write_log( '{0} '.format(trial+1) )



    #------------------------------------------------
    # Contrast callibration
    #------------------------------------------------
#    if CalibrationFlag: # Contrast calibration
#        if trial % NoTrialCal == 0 and trial > 0:
#            ptemp = 0
#            for i in range(NoTrialCal):
#                ptemp = ptemp + Data[trial-i-1].ACC
#            ptemp = ptemp / NoTrialCal
#            
#            LetterContrast = LetterContrast - ContrastStepSize / (trial/NoTrialCal) * (ptemp - ContrastCalibrationLevel)
#            
#            if LetterContrast < -1.0:
#                LetterContrast = -1.0
#            if LetterContrast > 1.0:
#                LetterContrast = 1.0



    Data[trial].LetterContrast = LetterContrast
    
    
    if ExperimentType == 1:
        for i in range(NoTargets):
            LetterStim[i].setText( Data[trial].T[ i ] )
            LetterStim[i].contrast = Data[trial].LetterContrast

    if ExperimentType == 3:

        for i in range(NoTargets):

            if Data[trial].Session <= NoPracSessions and PracShowAllTargets[Session-1] == 0: # Only show single cue and rewarded stimulus centraly

                if Data[trial].Cues[i] == 0:    # erase the other cues+targets and move them to the buttom of the screen
                    CueBox[i].setColor(BgColor)
                    ColorTarget[i].setColor(BgColor)
                    Cue[i].setPos( (0,-400*StimFactor) )
                    CueBox[i].setPos( (0,-400*StimFactor) )
                    ColorTarget[i].setPos((0,-400*StimFactor))
                else:   # Place the single cue and target in the center
                    CueBox[i].setColor(CueBgColor)
                    ColorTarget[i].setColor( StimulusTargetColorsRGB[ int( Data[trial].C[i] ) - 1 ] )
                    Cue[i].setPos( (0,0) )
                    CueBox[i].setPos( (0,0) )
                    ColorTarget[i].setPos( (0,0) )
            else:   # place all cues and colored targets for all other sessions
                CueBox[i].setColor(CueBgColor)
                ColorTarget[i].setColor( StimulusTargetColorsRGB[ int( Data[trial].C[i] ) - 1 ] )
                CueBox[i].setPos( CueLocationXY[i] )
                ColorTarget[i].setPos(ColorTargetLocationXY[i])

    for i in range(NoCueLocations):
        if Data[trial].Cues[i] > 0:
            Cue[i].setText( str( CueSymbols[ Data[trial].Cues[i] - 1] ) )
            
        else:
            Cue[i].setText("")
        
        if MaskCuePresent:
            MaskCue[i].setText("#")
        else:
            MaskCue[i].setText("")


    for i in range(NoTargets):
        flipv = bool(rnd.randint(0,1))
        fliph = bool(rnd.randint(0,1))
        ori = rnd.randint(0,3)*90
        Mask[i].setImage("MaskGS"+".gif")
        Mask[i].setOri(ori)
        Mask[i].flipVert = flipv
        Mask[i].flipHoriz = fliph

#------------------------------------------------
# Set up trial displays
#------------------------------------------------

#    win.flip() 

    if ExperimentType == 1:
        
        StimList = [Fixation]+CueArrows+CueBox+Cue
        CuesDisplay = visual.BufferImageStim(win, stim=StimList)
    
        if MaskCuePresent:
            StimList = [Fixation]+CueArrows+MaskCueBox+MaskCue+LetterStim
            LetterDisplay = visual.BufferImageStim(win, stim=StimList)
        
            StimList = [Fixation]+MaskCueBox+MaskCue+Mask
            MaskDisplay = visual.BufferImageStim(win, stim=StimList)
        else:
            StimList = [Fixation]+CueArrows+CueBox+Cue+LetterStim
            LetterDisplay = visual.BufferImageStim(win, stim=StimList)
        
            StimList = [Fixation]+CueBox+Cue+Mask
            MaskDisplay = visual.BufferImageStim(win, stim=StimList)
    
    elif ExperimentType == 2:
        
        StimList = [Fixation]+CueArrows+CueBox+Cue+PointTargetArea+PointTarget
        CuePointTargetDisplay = visual.BufferImageStim(win, stim=StimList)

        if MaskCuePresent:
            StimList = [Fixation]+CueArrows+MaskCueBox+MaskCue+PointTargetArea+PointTarget
        else:
            StimList = [Fixation]+CueArrows+CueBox+Cue+PointTargetArea+PointTarget

        CueMaskPointTargetDisplay = visual.BufferImageStim(win, stim=StimList)
        
    elif ExperimentType == 3:
        
        StimList = [Fixation]+CueArrows+ColorTarget+CueBox+Cue+ColorResponseInstruction
        CueColorTargetDisplay = visual.BufferImageStim(win, stim=StimList)

        if MaskCuePresent:
            StimList = [Fixation]+CueArrows+ColorTarget+MaskCueBox+MaskCue+ColorResponseInstruction
        else:
            StimList = [Fixation]+CueArrows+ColorTarget+CueBox+Cue+ColorResponseInstruction
        
        CueMaskColorTargetDisplay = visual.BufferImageStim(win, stim=StimList)
        
    else:
        print('Experiment type invalid')
        core.quit()
    

#------------------------------------------------
# Initiate eyetracking
#------------------------------------------------
    
    
    if TRACKER != 'none':
        io.clearEvents()
        tracker.setRecordingState(True)


#------------------------------------------------
# Calculate Start Trial Jitter Time
#------------------------------------------------

    if ExperimentType in (2,3):

        Data[trial].TrialStartJitterTime = min([TrialStartJitterOffsetTime + random.exponential(TrialStartJitterMeanTime), TrialStartJitterMaxTime]) 
        

#------------------------------------------------
# Present trial displays
#------------------------------------------------


    FixationDisplay.draw()
    win.flip() 
    
    if ExperimentType == 1:

        if not ExitTrialFlag and expInfo['testtiming'] == 'N':
            event.clearEvents()
            thisKey = event.waitKeys()
        else:
            thisKey = event.getKeys()

        if len(thisKey)>0 and thisKey[0] == 'escape':
            ExitTrialFlag = True
        
    
        for f in range(5):          # Make sure to flip a few times so that timing will be right (probably not necessary)
           FixationDisplay.draw()
           win.flip() 

        CuesDisplay.draw()
        
        if TRACKER != 'none':        # Get the latest gaze position in display coord space.
            gpos_cue = tracker.getLastGazePosition()
            
        win.flip()
        t_cue=trialClock.getTime()
        
        ISI.start(Data[trial].cueSOA/RefreshRate-delta_t)
        LetterDisplay.draw()    
#        core.wait(Data[trial].cueSOA/RefreshRate-delta_t)
#        LetterDisplay.draw()    orginal position of draw function

        ISI.complete()
        
        if TRACKER != 'none':        # Get the latest gaze position in display coord space.
            gpos_letter = tracker.getLastGazePosition()
        
        win.flip()
        t_stim=trialClock.getTime()

        ISI.start(Data[trial].ED/RefreshRate-delta_t)
        MaskDisplay.draw()
#        core.wait(Data[trial].ED/RefreshRate-delta_t)
#        MaskDisplay.draw()    orginal position of draw function
        
        ISI.complete()
        
        if TRACKER != 'none':        # Get the latest gaze position in display coord space.
            gpos_mask = tracker.getLastGazePosition()
        
        win.flip()
        t_mask=trialClock.getTime()
        
        ISI.start(MaskED/RefreshRate-delta_t)
        # Doesn't draw anything on screen, i.e., present blank screen after masks
#        core.wait(MaskED/RefreshRate-delta_t)
        
        ISI.complete()
        
        win.flip()
        t_endtrial=trialClock.getTime()


    elif ExperimentType == 2:
        
        gaze_inside_target_area = False
        gaze_cue_masked = False

        t_now = 0.0
        t_cue = 0.0
        t_cuemask = 0.0
        t_pointtarget = 0.0


        if (trial == 0 or Data[trial].Block != Data[trial-1].Block) and not ExitTrialFlag and expInfo['testtiming'] == 'N':   # Wait for a keypress in the first trial of every block.
            
#            FixationDisplay.draw()
#            Ready_stim.draw()
#            win.flip()
#
#            event.clearEvents()
#            thisKey = event.waitKeys()

            if len(thisKey)>0 and thisKey[0] == 'escape':
                ExitTrialFlag = True
        
            FixationDisplay.draw()
            win.flip() 

            core.wait(2.0)
        

        CuePointTargetDisplay.draw()
        
        core.wait(Data[trial].TrialStartJitterTime)     # Initial jitter time


        gpos_cue = tracker.getLastGazePosition() # Store gaze location before presentation of cues
        
        win.flip()
        t_cue = trialClock.getTime()
        
        tracker.clearEvents()   # Clear all events in the tracker buffer


        while not gaze_inside_target_area:  # has the target been reached?

            t_now = trialClock.getTime()
            
            if (not gaze_cue_masked) and (t_now - t_cue >= Data[trial].cueSOA/RefreshRate-delta_t):
                
                CueMaskPointTargetDisplay.draw()
                
                win.flip()
                t_cuemask=trialClock.getTime()
                
                gaze_cue_masked = True

            gpos_target = tracker.getLastGazePosition()
            valid_gaze_pos = isinstance(gpos_target, (tuple, list))
            
            if valid_gaze_pos:
                testgpos = list( map(sub, gpos_target, DriftCorrectionXY) )  # correct the recorded fixation position with the drift correction coordinates)

                Data[trial].PointTargetResponse = 0
                
                for i in range(NoTargets):
                    if PointTargetArea[i].contains(testgpos):
                        
                        gaze_inside_target_area = True
                        
                        t_pointtarget = trialClock.getTime()  # Store the time reaching the target (for calculating the RT) of the trial
                        
                        Data[trial].PointTargetResponse = i+1
                        Data[trial].CueResponseExpValue = Data[trial].Cues[i]
                        Data[trial].CueResponseValue = Data[trial].CuesVal[i]
                        Data[trial].CueResponseRank = Data[trial].CueRanks[i]
                        
                        PointTargetResponse.setPos(PointTargetLocationXY[i])    # Location of feedback point set to location of chosen trial
            
            
            # Check wither time deadline has been exceeded. If it has set the flag for gaze inside target area to True to end trial.
            if t_now - t_cue > ResponseDeadline:
                Data[trial].LateResponse = True
                t_pointtarget=trialClock.getTime()  # Store the response deadline time as the RT
                gaze_inside_target_area = True
                    
                    
        t_endtrial=trialClock.getTime()
        
        if t_cuemask <= 0.0:
            t_cuemask = t_endtrial  # set time of cue mask to the end of trial time if response happend before the initiation of the cue mask

        # Mark the chosen target location"
        if Data[trial].PointTargetResponse > 0:

            CueMaskPointTargetDisplay.draw()

            PointTargetResponse.draw()  # Feedback point
            win.flip()
            core.wait(.5)

        # Extract eye movement data - see https://psychopy.org/api/iohub/device/eyetracker_interface/GazePoint_Implementation_Notes.html
        ioevents = tracker.getEvents()
        
        j = 0
        timeoffset = 0
        
        for e in ioevents:
        #    if type(e).__name__ == 'MonocularEyeSampleEventNT':
            try:
        #        print(n,type(e).__name__, e.time, e.gaze_x, e.gaze_y)
                if j == 0:
                    timeoffset = e.time
                    
                Data[trial].EyeT[j] = e.time - timeoffset
                
                Data[trial].EyeX[j] = e.gaze_x
                Data[trial].EyeY[j] = e.gaze_y

                Data[trial].EyeP[j] = e.pupil_measure1

                # This dosen't work for some reasong, perhaps see: https://psychopy.org/api/iohub/device/eyetracker_interface/GazePoint_Implementation_Notes.html
                # if e.eye == EyeTrackerConstants.LEFT_EYE:
                #     Data[trial].EyeX[j] = e.left_gaze_x
                #     Data[trial].EyeY[j] = e.left_gaze_y
                #     Data[trial].EyeP[j] = e.left_pupil_measure_1
                # else:
                #     Data[trial].EyeX[j] = e.right_gaze_x
                #     Data[trial].EyeY[j] = e.right_gaze_y
                #     Data[trial].EyeP[j] = e.right_pupil_measure_1


                j = j + 1
                
            except:
                print(j, e)

                # Example from running with mouse simulator:
                # MonocularEyeSampleEventNT(experiment_id=0, session_id=0, device_id=0, event_id=2454, type=51, device_time=44.12594178098298, logged_time=44.12594178098298, time=44.12594178098298, confidence_interval=0, delay=0, filter_id=0, eye=22, gaze_x=-3.385258197496067, gaze_y=-3.4804564998426017, gaze_z=0, eye_cam_x=0, eye_cam_y=0, eye_cam_z=0, angle_x=0, angle_y=0, raw_x=0, raw_y=0, pupil_measure1=5, pupil_measure1_type=77, pupil_measure2=0, pupil_measure2_type=0, ppd_x=0, ppd_y=0, velocity_x=0, velocity_y=0, velocity_xy=0, status=0)
                
        Data[trial].EyeOffsetTime = timeoffset
        Data[trial].NumberEyeSamples = j

    elif ExperimentType == 3:
        
        gaze_inside_target_area = False
        gaze_cue_masked = False

        t_now = 0.0
        t_cue = 0.0
        t_cuemask = 0.0
        t_colortarget = 0.0
        t_endtrial = 0.0

        if (trial == 0 or Data[trial].Block != Data[trial-1].Block) and not ExitTrialFlag and expInfo['testtiming'] == 'N':   # Wait for a keypress in the first trial of every block.
            

            if len(thisKey)>0 and thisKey[0] == 'escape':
                ExitTrialFlag = True
        
            FixationDisplay.draw()
            win.flip() 

            core.wait(2.0)
        

        CueColorTargetDisplay.draw()
        
        core.wait(Data[trial].TrialStartJitterTime)     # Initial jitter time

        # gpos_cue = tracker.getLastGazePosition() # Store gaze location before presentation of cues
        
        win.flip()
        t_cue = trialClock.getTime()

        event.clearEvents()
        thisKey = event.waitKeys()

        t_colortarget = trialClock.getTime()

        temp = thisKey[0]

        if temp == "comma":
            temp = ","
        if temp == "period":
            temp = "."
        elif temp == "apostrophe":
            temp = "'"
        elif temp == "minus":
            temp = "-"
        else:
            temp = temp.upper()

        Data[trial].R = temp

#        keyboard.clearEvents()   # Clear all events in the keyboard buffer
                    
        t_endtrial=trialClock.getTime()
        
        # Extract keyboard data - see https://psychopy.org/api/iohub/device/eyetracker_interface/GazePoint_Implementation_Notes.html
        # keyboard = tracker.getEvents()
        
    else:
        print('Experiment type invalid')
        core.quit()

#------------------------------------------------
# Stop eyetracking and check fixation
#------------------------------------------------

    if TRACKER != 'none':
        
        tracker.setRecordingState(False)

        valid_gaze_pos = isinstance(gpos_cue, (tuple, list))
        if valid_gaze_pos:
            # Store the uncorrected (undriftcorrected) data
            Data[trial].CueGazeX = gpos_cue[0]
            Data[trial].CueGazeY = gpos_cue[1]

        if ExperimentType == 1:


            # Check fixation control
            valid_gaze_pos = isinstance(gpos_letter, (tuple, list))
            
            # If we have a gaze position from the tracker, check whether it is inside the fixation area.
            if valid_gaze_pos:
               
                testgpos = list( map(sub,gpos_letter,DriftCorrectionXY) )  # correct the recorded fixation position with the drift correction coordinates)
    
                gaze_in_region = valid_gaze_pos and FixationRegion.contains(testgpos)
                
                if not gaze_in_region:
                    FixationDot.setPos(testgpos)
                    FixationDot.draw()
                    FixationRegion.draw()
                    EyeTrackerErrorText_stim.draw()
                    win.flip()
                    core.wait(2.0)
                    
                    Data[trial].Saccade = True
                
                # Store the uncorrected (undriftcorrected) data
                Data[trial].LetterGazeX = gpos_letter[0]
                Data[trial].LetterGazeY = gpos_letter[1]
                
                valid_gaze_pos = isinstance(gpos_mask, (tuple, list))
                if valid_gaze_pos:
                    # Store the uncorrected (undriftcorrected) data
                    Data[trial].MaskGazeX = gpos_mask[0]
                    Data[trial].MaskGazeY = gpos_mask[1]
                       

        elif ExperimentType == 2:
            
            
            FixationDisplay.draw()
            win.flip() 
            
            core.wait(0.5)

            # Feedback response deadline
            if Data[trial].LateResponse:
                ResponseDeadlineErrorText_stim.draw()
                win.flip()
                core.wait(2.0)
            
            
            # Check fixation control at presentation of cues
            valid_gaze_pos = isinstance(gpos_cue, (tuple, list))
            
            # If we have a gaze position from the tracker, check whether it is inside the fixation area.
            if valid_gaze_pos:
               
                testgpos = list( map(sub,gpos_cue,DriftCorrectionXY) )  # correct the recorded fixation position with the drift correction coordinates)
    
                gaze_in_region = valid_gaze_pos and FixationRegion.contains(testgpos)
                
                if not gaze_in_region:
                    FixationDot.setPos(testgpos)
                    FixationDot.draw()
                    FixationRegion.draw()
                    EyeTrackerErrorText_stim.draw()
                    win.flip()
                    core.wait(2.0)
                    
                    Data[trial].Saccade = True
                
            # Store point target gaze position
            valid_gaze_pos = isinstance(gpos_target, (tuple, list))

            if valid_gaze_pos:
               
                testgpos = list( map(sub,gpos_target,DriftCorrectionXY) )  # correct the recorded fixation position with the drift correction coordinates)   (Probably obsolute since it is not used for checking
                
                # Store the uncorrected (undriftcorrected) data
                Data[trial].TargetGazeX = gpos_target[0]
                Data[trial].TargetGazeY = gpos_target[1]

        elif ExperimentType == 3:
            
            FixationDisplay.draw()
            win.flip() 
            
            core.wait(0.5)

        else:
            print('Experiment type invalid')
            core.quit()
                

#        win.flip()
    

#------------------------------------------------------------------
# Calculate and store timing measures
# Collect/compute responses 
# Store trial in data structure
#------------------------------------------------------------------

    Data[trial].DriftCorrectX = DriftCorrectionXY[0]
    Data[trial].DriftCorrectY = DriftCorrectionXY[1]



    if ExperimentType == 1:
        
        Data[trial].CueTime =  t_cue
        Data[trial].CueMaskTime =  t_stim
        Data[trial].StimTime =  t_stim
        Data[trial].MaskTime =  t_mask
        Data[trial].EndTrialTime =  t_endtrial
        
        Data[trial].mEDcues =  t_stim-t_cue
        Data[trial].mEDletters =  t_mask-t_stim
        Data[trial].mEDmasks =  t_endtrial - t_mask


        win.flip()

        event.clearEvents()
        thisKey = ''
        tempResp = ''

        if not ExitTrialFlag and expInfo['testtiming'] == 'N':
            stopResponse = False
        else:
            stopResponse = True
        
        while not stopResponse:
            
            thisKey = event.waitKeys()
            
            if thisKey[0] == 'escape':
                stopResponse = True
                ExitTrialFlag = True
            elif thisKey[0] == 'space' and tempResp != '':
                stopResponse = True
            else:
                if thisKey[0] == 'backspace' and len(tempResp) > 0:
                    tempResp = tempResp[:-1]
                else:
                    if len(thisKey[0]) == 1 and len(tempResp) == 0:
                        tempResp = tempResp+thisKey[0].upper()
    
            Response.setText(tempResp)
            Response.draw()
            win.flip()
    
    
        if len(tempResp) == 0:
            tempResp = "-"
            
        Data[trial].R = tempResp
            
            
        FeedbackZeroRewardFlag = False      # True if the actual reward (e.g. 0) is different from the expected reward (e.g. 4).
        
        temp = Data[trial].T
        for i in range(len(temp)):
            if tempResp.find( temp[i] ) > -1:
                Data[trial].RespLoc += "1"
                
                Data[trial].CueResponseRank = Data[trial].CueRanks[i]
                
                if Data[trial].Cues[i] > 0:
    #                Data[trial].Reward += CueValue[ Data[trial].Cues[i] - 1 ]
                    Data[trial].ExpectedReward += CueValue[ Data[trial].Cues[i] - 1 ]
    
                    if Data[trial].CuesVal[i] > 0:
                        Data[trial].Reward += CueValue[ Data[trial].CuesVal[i] - 1 ]
                        FeedbackZeroRewardFlag = False
                    else:
                        FeedbackZeroRewardFlag = True
                        
                    Data[trial].CueResponseExpValue = Data[trial].Cues[i]
                    Data[trial].CueResponseValue = Data[trial].CuesVal[i]
                        
                if Data[trial].Cues[i] == 0:
                    Data[trial].INTR += 1
                else:
                    Data[trial].ACC += 1
                    
            else:
                Data[trial].RespLoc += "0"
    
        Data[trial].ERR = len(tempResp) - Data[trial].ACC - Data[trial].INTR
    

    elif ExperimentType == 2:
        
        Data[trial].CueTime =  t_cue
        Data[trial].CueMaskTime =  t_cuemask
        Data[trial].PointTargetTime =  t_pointtarget
        Data[trial].EndTrialTime =  t_endtrial
        
        Data[trial].mEDcues =  t_cuemask-t_cue

        Data[trial].RT = t_pointtarget-t_cue

        if Data[trial].CueResponseExpValue > 0:
            Data[trial].ExpectedReward = CueValue[ Data[trial].CueResponseExpValue - 1]
        if Data[trial].CueResponseValue > 0:
            Data[trial].Reward = CueValue[ Data[trial].CueResponseValue - 1]
        

        if Data[trial].Reward == Data[trial].ExpectedReward: 
            FeedbackZeroRewardFlag = False
        else:
            FeedbackZeroRewardFlag = True
        
        if Data[trial].RT < ResponseDeadline:
            if Data[trial].CueResponseValue > 0:
                Data[trial].ACC = 1
            else:
                Data[trial].INTR = 1
        else:
            Data[trial].ERR = 1


    elif ExperimentType == 3:

        Data[trial].CueTime =  t_cue
        Data[trial].CueMaskTime =  t_cuemask
        Data[trial].ColorTargetTime =  t_colortarget
        Data[trial].EndTrialTime =  t_endtrial

#        Data[trial].mEDcues =  t_cuemask-t_cue

        Data[trial].RT = t_colortarget-t_cue

        try:
            tempResp = ResponseKeys.index(Data[trial].R) # Find the response number for the key
            tempResp = str(tempResp+1)
        except:
            tempResp = ''

        temp = Data[trial].C

        FeedbackZeroRewardFlag = False

        for i in range(len(temp)):
            if tempResp == temp[i]:
                Data[trial].RespLoc += "1"
                
                Data[trial].CueResponseRank = Data[trial].CueRanks[i]
                
                if Data[trial].Cues[i] > 0:
                    Data[trial].ExpectedReward += CueValue[ Data[trial].Cues[i] - 1 ]
    
                    if Data[trial].CuesVal[i] > 0:
                        Data[trial].Reward += CueValue[ Data[trial].CuesVal[i] - 1 ]
                        FeedbackZeroRewardFlag = False
                    else:
                        FeedbackZeroRewardFlag = True
                        
                    Data[trial].CueResponseExpValue = Data[trial].Cues[i]
                    Data[trial].CueResponseValue = Data[trial].CuesVal[i]
                        
                if Data[trial].Cues[i] == 0:
                    Data[trial].INTR += 1
                else:
                    Data[trial].ACC += 1
                    
            else:
                Data[trial].RespLoc += "0"
    
        Data[trial].ERR = len(tempResp) - Data[trial].ACC - Data[trial].INTR

    else:
        print('Experiment type invalid')
        core.quit()
        
    
#--------------------
# Calculate reward
#--------------------

    Data[trial].MaxReward = CueValue[ max(Data[trial].Cues) - 1 ]

    if not Data[trial].Saccade:
        if trial > 0:
            Data[trial].CumReward = Data[trial-1].CumReward  + (Data[trial].Reward * RewardMoneyFactor )
        else:
            Data[trial].CumReward = Data[trial].Reward * RewardMoneyFactor
    else:
        if trial > 0:
            Data[trial].CumReward = Data[trial-1].CumReward
        else:
            Data[trial].CumReward = 0.0
        
    Data[trial].CumReward = round(Data[trial].CumReward,2)

    if len(thisKey) and thisKey[0] == 'escape':
        ExitTrialFlag = True
        
#    win.flip()

#    core.wait(.5)

    
#--------------------
# Feedback
#--------------------

    if ExperimentType == 1:
        Feedback1.setPos([0*StimFactor, 0*StimFactor])
    elif ExperimentType in (2,3):
        Feedback1.setPos([0*StimFactor, 70*StimFactor])
    else:
        print('Experiment type invalid')
        core.quit()
    
    if FeedbackZeroRewardFlag:
        Feedback1.setColor(FeedbackZeroRewardColor)
    else:
        if Data[trial].Reward > 0: 
            Feedback1.setColor(FeedbackPosRewardColor)
        else:
            Feedback1.setColor(LetterColor)
        
    if MaxRewardFlag:
        Feedback1.setText( str(Data[trial].ExpectedReward) + " / " + str(Data[trial].MaxReward) )
    else:
#        Feedback1.setText( str(Data[trial].ExpectedReward) )
        Feedback1.setText( str(Data[trial].Reward) )
    Feedback1.draw()

    Feedback2.setText( "%.2f" % Data[trial].CumReward )
    Feedback2.draw()
    
    if ExperimentType in (2,3):
        
        temp = Data[trial].RT
        temp *= 1000
        Feedback3.setText( "%5.0f" % temp + " ms" )
        Feedback3.draw()
    
    Feedback4.setText( "Block  " + str(Data[trial].Block) + " / " + str(NoBlocks) + "               Trial  " + str(trial+1) + " / " + str(Ntrial) ) 
    Feedback4.draw()

    if ExperimentType in (2,3):
        Fixation.draw() # Draw fixation dot
    
    win.flip()


    if not ExitTrialFlag and expInfo['testtiming'] == 'N':
        event.clearEvents()
        
        if ExperimentType in (1,2):
            thisKey = event.waitKeys()
        elif ExperimentType == 3:
            core.wait(1.0)

    else:
        thisKey = event.getKeys()

    if len(thisKey)>0 and thisKey[0] == 'escape':
        ExitTrialFlag = True




#----------------------
# Exit trial procedure
#----------------------

    if ExitTrialFlag:

        write_log( 'Trial_escape ' )
        Data[trial].Note += 'Trial_escape;'


        ExitTrialText_stim.draw()
        win.flip()

        event.clearEvents()
        thisKey = event.waitKeys()

        win.flip()

        # Recallibrate eye tracker 
        if len(thisKey)>0 and thisKey[0].upper() == 'R' and TRACKER != 'none':

            hideWindow(win)
            
            # Display calibration gfx window and run calibration.
            result = tracker.runSetupProcedure()
            print("\nCalibration returned: \n", result, "\n")
            
            # Maximize the PsychoPy window if needed
            showWindow(win)
            
            # Reset drift correction coordinates to (0,0)
            DriftCorrectionXY = [0,0]

            Data[trial].Note += 'Recalibrating_eyetracker;'
            write_log( 'Recalibrating_eyetracker ' )
                
        elif len(thisKey)>0 and thisKey[0].upper() == 'S':

            write_log( 'Experiment_abort ' )
            break
    

        FixationDisplay.draw()
        Ready_stim.draw()
        win.flip()
        
        event.clearEvents()
        thisKey = event.waitKeys()

        if ExperimentType in (2,3):
            Fixation.draw() # Draw fixation dot
            win.flip()
            core.wait(2.0)

    event.clearEvents()
    
#------------------------------------------------
# Save data from trial
#------------------------------------------------

    try:

        f = open("Data/"+DataFileName, "a")

        f.write( '{0}\t'.format(ExperimentName) )
        f.write( '{0}\t'.format(ExperimentNumber) )
        f.write( '{0}\t'.format(expInfo['subject']) )
        f.write( '{0}\t'.format(Data[trial].Session) )
        f.write( '{0}\t'.format(Data[trial].Block) )
        f.write( '{0}\t'.format(trial+1) )
        f.write( '{0}\t'.format(Data[trial].WarmUp) )
        f.write( '{0}\t'.format(Data[trial].CueCond) )
        f.write( '{0}\t'.format(Data[trial].Cond) )
        f.write( '{0}\t'.format(Data[trial].NoCues) )
        f.write( '{0}\t'.format(Data[trial].TrialStartJitterTime) )
        f.write( '{0}\t'.format(Data[trial].cueSOA) )
        f.write( '{0}\t'.format( "".join(str(x) for x in Data[trial].Cues ) ) )
        f.write( '{0}\t'.format( "".join(str(x) for x in Data[trial].CuesVal ) ) )
        f.write( '{0}\t'.format( "".join(str(x) for x in Data[trial].CueRanks ) ) )
        f.write( '{0}\t'.format(Data[trial].LetterContrast) ) 
        f.write( '{0}\t'.format(Data[trial].ED) ) 
        f.write( '{0}\t'.format(Data[trial].T) )
        f.write( '{0}\t'.format(Data[trial].C) )
        f.write( '{0}\t'.format(Data[trial].R) )
        f.write( '{0}\t'.format(Data[trial].RespLoc) )
        f.write( '{0}\t'.format(Data[trial].PointTargetResponse) )
        f.write( '{0}\t'.format(Data[trial].RT) )
        f.write( '{0}\t'.format(Data[trial].LateResponse) )
        f.write( '{0}\t'.format(Data[trial].ACC) )
        f.write( '{0}\t'.format(Data[trial].INTR) )
        f.write( '{0}\t'.format(Data[trial].ERR) )
        f.write( '{0}\t'.format(Data[trial].CueResponseValue) )
        f.write( '{0}\t'.format(Data[trial].CueResponseExpValue) )
        f.write( '{0}\t'.format(Data[trial].CueResponseRank) )
        f.write( '{0}\t'.format(Data[trial].ExpectedReward) )
        f.write( '{0}\t'.format(Data[trial].Reward) )
        f.write( '{0}\t'.format(Data[trial].MaxReward) )
        f.write( '{0}\t'.format(Data[trial].CumReward) )
        f.write( '{0}\t'.format(Data[trial].Saccade) )
        f.write( '{0}\t'.format(Data[trial].CueGazeX - Data[trial].DriftCorrectX) )
        f.write( '{0}\t'.format(Data[trial].CueGazeY - Data[trial].DriftCorrectY) )
        f.write( '{0}\t'.format(Data[trial].LetterGazeX - Data[trial].DriftCorrectX) )
        f.write( '{0}\t'.format(Data[trial].LetterGazeY - Data[trial].DriftCorrectY) )
        f.write( '{0}\t'.format(Data[trial].MaskGazeX - Data[trial].DriftCorrectX) )
        f.write( '{0}\t'.format(Data[trial].MaskGazeY - Data[trial].DriftCorrectY) )
        f.write( '{0}\t'.format(Data[trial].TargetGazeX - Data[trial].DriftCorrectX) )
        f.write( '{0}\t'.format(Data[trial].TargetGazeY - Data[trial].DriftCorrectY) )
        f.write( '{0}\t'.format(Data[trial].DriftCorrectX ) )
        f.write( '{0}\t'.format(Data[trial].DriftCorrectY ) )
        f.write( '{0}\t'.format(RefreshRate) )
        f.write( '{0}\t'.format(MeasuredRefreshRate) )
        f.write( '{0}\t'.format(Data[trial].mEDcues) )
        f.write( '{0}\t'.format(Data[trial].mEDletters) )
        f.write( '{0}\t'.format(Data[trial].mEDmasks) )
        f.write( '{0}\t'.format(Data[trial].CueTime) )
        f.write( '{0}\t'.format(Data[trial].CueMaskTime) )
        f.write( '{0}\t'.format(Data[trial].StimTime) )
        f.write( '{0}\t'.format(Data[trial].MaskTime) )
        f.write( '{0}\t'.format(Data[trial].PointTargetTime) )
        f.write( '{0}\t'.format(Data[trial].ColorTargetTime) )
        f.write( '{0}\t'.format(Data[trial].EndTrialTime) )
        
        f.write( '{0}\t'.format(Data[trial].EyeOffsetTime) )
        
        f.write( '{0}\t'.format(Data[trial].Note) )

        f.write( '{0}\t'.format(Data[trial].NumberEyeSamples) )
        
        for i in range(0,MaxEyeRecordings):
            f.write( '{0}\t'.format(Data[trial].EyeT[i]) )
            f.write( '{0}\t'.format(Data[trial].EyeX[i] - Data[trial].DriftCorrectX) )
            f.write( '{0}\t'.format(Data[trial].EyeY[i] - Data[trial].DriftCorrectY) )
            f.write( '{0}\t'.format(Data[trial].EyeP[i]) )
        
        f.write( '\n' )
    
    except:

        print("Error in printing and writing file...")
            
    f.close()



#------------------------------------------------
# Close down eyetracker
#------------------------------------------------

if TRACKER != 'none':
    tracker.setConnectionState(False)
    io.quit()



#------------------------------------------------
# Record end time of experiment
#------------------------------------------------

# ExpEndTime = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")


#------------------------------------------------
# End experiment
#------------------------------------------------

Instruction.setColor(InstructionColor)
Instruction.setText("End of session!\n\n\nContact the Experimenter")
Instruction.draw()
win.flip()

if len(thisKey)>0 and not ExitTrialFlag:
    event.clearEvents()
    event.waitKeys()

core.quit()


