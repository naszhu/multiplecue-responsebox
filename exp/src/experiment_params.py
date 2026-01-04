"""
Experiment parameters for Multiple Cue Paradigm (MCP)
"""
from numpy import pi

# Cue and target configuration
CueValue = [1, 2, 3, 4]
CueSymbols = CueValue
StimulusTargetLetters = "ABCDEFGHIJKLMNOPQRSTUVXYZW"

# Cue sets for practice and experimental sessions
ExpCueSet = [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
PracCueSet = [[[1], [2], [3], [4]], ExpCueSet, ExpCueSet, ExpCueSet]
ExpCueSetVal = ExpCueSet
PracCueSetVal = PracCueSet

# SOA (Stimulus Onset Asynchrony) conditions in milliseconds
ExpCueSOAconds = [1, 15, 30, 50, 100]
PracCueSOAconds = [[100], [100], ExpCueSOAconds, ExpCueSOAconds]

# Exposure durations in milliseconds
PracStimED = (100, 30, 15, 5)
ExpStimED = 5
MaskED = 50

# Block configuration
PracNoBlocks = (1, 1, 1, 1)
PracRepetitions = (7, 7, 1, 1)
ExpNoBlocks = 2
ExpRepetitions = 1

# Cue-target association type 1: 4 complex associations on a line
CueTargetAssociationType = 1
NoCueLocations = 4
CueAssocList = [1, 2, 4, 3]
CueLocRotation = 0
CueDistance = 40 * 0.04  # StimFactor = 0.04
NoTargets = 4
TargetLocRotation = pi / 4

# Visual parameters
StimFactor = 0.04
FixationSize = 4 * StimFactor
CueBoxSize = 25 * StimFactor
CueTextSize = 20 * StimFactor
LetterSize = 60 * StimFactor
TargetDistance = 200 * StimFactor
MaskSize = [80 * StimFactor, 80 * StimFactor]

# Colors
CueTextColor = (-1, -1, -1)  # Black text
CueBgColor = (1, 1, 1)  # White background
LetterColor = (1, 1, 1)  # White letters
FixationPointColor = (1, -1, -1)  # Red fixation

# Reward configuration
MaxRewardFlag = True
VariableRewardFeedbackFlag = True
RewardMoneyFactor = 0.1

