"""
Experiment parameters for Cued Color Response Paradigm (CCRP)
"""
from numpy import pi

# Cue configuration - defines what cues look like and their values
CueValue = [1, 2, 3, 4]  # List of integers: reward values for each cue (1=lowest, 4=highest)
CueSymbols = CueValue  # List of integers: symbols displayed in cue boxes (same as values: 1,2,3,4)

# Cue sets - defines which cue combinations appear in each session
ExpCueSet = [[1], [2], [3], [4], [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]  # List of lists: experimental cue combinations
PracCueSet = [[[1], [2], [3], [4]], [[1], [2], [3], [4]], ExpCueSet, ExpCueSet, ExpCueSet]  # List of lists: practice cue sets per session
ExpCueSetVal = ExpCueSet  # List of lists: cue values for experimental trials (same as ExpCueSet)
PracCueSetVal = PracCueSet  # List of lists: cue values for practice trials (same as PracCueSet)

# SOA (Stimulus Onset Asynchrony) conditions - time between cue and color target in milliseconds
ExpCueSOAconds = [200]  # List of integers: SOA durations for experimental trials (200ms)
PracCueSOAconds = [[200], [200], [200], ExpCueSOAconds, ExpCueSOAconds]  # List of lists: SOA durations per practice session

# Exposure durations - how long stimuli are displayed in milliseconds
PracStimED = (200, 200, 200, 200, 200)  # Tuple of integers: stimulus exposure durations per practice session
ExpStimED = 200  # Integer: stimulus exposure duration for experimental trials (200ms)

# Block configuration - how many blocks and repetitions per session
PracNoBlocks = (5, 5, 2, 2, 2)  # Tuple of integers: number of blocks per practice session
PracRepetitions = (5, 5, 3, 3, 5)  # Tuple of integers: repetitions per condition per practice session
ExpNoBlocks = 4  # Integer: number of blocks in experimental sessions
ExpRepetitions = 5  # Integer: repetitions per condition in experimental sessions

# Cue-target association type 8: 4 associations in a cross with small cues and distances
CueTargetAssociationType = 8  # Integer: type of spatial arrangement (8 = cross pattern)
NoCueLocations = 4  # Integer: number of cue positions (4 = arranged in cross)
CueAssocList = [1, 2, 3, 4]  # List of integers: maps cue position to target position (1->1, 2->2, etc.)
CueLocRotation = pi / 4  # Float: rotation angle for cue positions in radians (45 degrees)
CueDistance = 50 * 0.04  # Float: distance from center to cue in degrees (50 * 0.04 = 2.0 degrees)
NoTargets = 4  # Integer: number of color target positions (4 = arranged around center)
TargetLocRotation = pi / 4  # Float: rotation angle for target positions in radians (45 degrees)

# Visual parameters - sizes and scaling factors
StimFactor = 0.04  # Float: scaling factor for all stimulus sizes (0.04 = small stimuli)
FixationSize = 4 * StimFactor  # Float: diameter of fixation point in degrees (4 * 0.04 = 0.16 deg)
CueBoxSize = 25 * StimFactor * 0.7  # Float: size of cue box in degrees (25 * 0.04 * 0.7 = 0.7 deg)
CueTextSize = 20 * StimFactor * 0.7  # Float: size of cue text in degrees (20 * 0.04 * 0.7 = 0.56 deg)
ColorTargetSize = 20 * StimFactor  # Float: diameter of color target circle in degrees (20 * 0.04 = 0.8 deg)
TargetDistance = 50 * StimFactor  # Float: distance from center to target in degrees (50 * 0.04 = 2.0 deg)

# Colors - RGB values from -1 to 1
CueTextColor = (-1, -1, -1)  # Tuple of 3 floats: cue text color RGB (black text)
CueBgColor = (1, 1, 1)  # Tuple of 3 floats: cue box background color RGB (white background)
FixationPointColor = (-1, -1, -1)  # Tuple of 3 floats: fixation point color RGB (black dot)

# Color targets - RGB values for the 4 colored circles
StimulusTargetColors = "1234"  # String: color labels ('1'=red, '2'=green, '3'=blue, '4'=yellow)
StimulusTargetColorsRGB = [(1, -1, -1), (-1, 1, -1), (-1, -1, 1), (1, 1, -1)]  # List of tuples: RGB values (red, green, blue, yellow)
StimulusColorNoResponses = len(StimulusTargetColorsRGB)  # Integer: number of colors (4)

# Response keys - keyboard keys mapped to each color
ResponseKeys = ["Z", "X", ".", "-"]  # List of strings: keys for colors 1,2,3,4 respectively

# Reward configuration
MaxRewardFlag = True  # Boolean: show max possible reward in feedback (True = show "expected/max")
RewardMoneyFactor = 0.1  # Float: multiplier to convert reward points to money (0.1 = 10 points = 1 unit)

# Trial start jitter - random delay before cue presentation
TrialStartJitterOffsetTime = 1.0  # Float: minimum jitter time in seconds (1.0 = 1000ms minimum)
TrialStartJitterMeanTime = 0.5  # Float: mean of exponential distribution for jitter in seconds
TrialStartJitterMaxTime = 5.0  # Float: maximum jitter time in seconds (5.0 = 5000ms maximum)
