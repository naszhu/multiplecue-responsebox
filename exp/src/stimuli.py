"""
Stimulus creation and management
"""
from psychopy import visual
from numpy import cos, sin, pi
from experiment_params import (
    FixationSize, CueBoxSize, CueTextSize, ColorTargetSize, TargetDistance,
    CueDistance, CueLocRotation, TargetLocRotation,
    CueTextColor, CueBgColor, FixationPointColor,
    NoCueLocations, CueAssocList, NoTargets, StimulusTargetColorsRGB,
    ResponseColorSize, ResponseColorDistance, StimulusColorNoResponses
)


def create_cue_locations():
    """
    Generate cue location coordinates - arranged in a cross pattern
    
    Returns:
        List of [x, y] coordinates - one pair per cue location in degrees
    """
    locations = []  # List: will contain [x, y] pairs for each cue position
    for i in range(NoCueLocations):  # Loop through 4 cue positions
        # Calculate position using polar coordinates converted to Cartesian
        x = CueDistance * cos(2 * pi / NoCueLocations * (CueAssocList[i] - 1) + CueLocRotation)  # Float: x position in degrees
        y = CueDistance * sin(2 * pi / NoCueLocations * (CueAssocList[i] - 1) + CueLocRotation)  # Float: y position in degrees
        locations.append([x, y])  # Add position to list
    return locations  # List: [[x1,y1], [x2,y2], [x3,y3], [x4,y4]]


def create_target_locations():
    """
    Generate target location coordinates - arranged around center in cross pattern
    
    Returns:
        List of [x, y] coordinates - one pair per target location in degrees
    """
    locations = []  # List: will contain [x, y] pairs for each target position
    for i in range(NoTargets):  # Loop through 4 target positions
        # Calculate position using polar coordinates converted to Cartesian
        x = TargetDistance * cos(2 * pi / NoTargets * i + TargetLocRotation)  # Float: x position in degrees
        y = TargetDistance * sin(2 * pi / NoTargets * i + TargetLocRotation)  # Float: y position in degrees
        locations.append([x, y])  # Add position to list
    return locations  # List: [[x1,y1], [x2,y2], [x3,y3], [x4,y4]]


def create_stimuli(win, cue_locations, target_locations):
    """
    Create all visual stimuli objects - fixation, cues, arrows, color targets
    
    Args:
        win: PsychoPy Window object - the display window
        cue_locations: List of [x, y] pairs - cue positions in degrees
        target_locations: List of [x, y] pairs - target positions in degrees
    
    Returns:
        Dictionary - contains all stimulus objects organized by type
    """
    stimuli = {}  # Dictionary: will store all stimulus objects
    
    # Fixation point - small circle at center of screen
    stimuli['fixation'] = visual.Circle(
        win, size=FixationSize, units='deg', fillColor=FixationPointColor  # Circle: black dot, 0.16 deg diameter
    )
    
    # Cue boxes and text - white squares with numbers inside
    stimuli['cue_boxes'] = []  # List: will contain Rect objects for cue backgrounds
    stimuli['cue_texts'] = []  # List: will contain TextStim objects for cue numbers
    stimuli['cue_arrows'] = []  # List: will contain Line objects connecting cues to targets
    
    for i in range(NoCueLocations):  # Loop through 4 cue positions
        # Cue box - white square background
        cue_box = visual.Rect(
            win, width=CueBoxSize, height=CueBoxSize, units='deg',
            fillColor=CueBgColor, pos=cue_locations[i]  # Rect: white square, 0.7 deg size
        )
        stimuli['cue_boxes'].append(cue_box)  # Add to list
        
        # Cue text - number displayed in box
        cue_text = visual.TextStim(
            win, text="", height=CueTextSize, units='deg',
            color=CueTextColor, pos=cue_locations[i]  # TextStim: black number, 0.56 deg height
        )
        stimuli['cue_texts'].append(cue_text)  # Add to list
        
        # Cue arrows - lines from cues to associated targets
        target_idx = CueAssocList[i] - 1  # Integer: convert to 0-based index (1->0, 2->1, etc.)
        arrow = visual.Line(
            win, lineColor=(0, 0, 0), units='deg'  # Line: black line connecting cue to target
        )
        arrow.setVertices([cue_locations[i], target_locations[target_idx]])  # Set endpoints: from cue to target
        stimuli['cue_arrows'].append(arrow)  # Add to list
    
    # Color targets - colored circles at target positions
    stimuli['color_targets'] = []  # List: will contain Circle objects for colored targets
    for i in range(NoTargets):  # Loop through 4 target positions
        color_target = visual.Circle(
            win, radius=ColorTargetSize/2, units='deg',  # Circle: colored circle, 0.4 deg radius
            fillColor=(0, 0, 0), pos=target_locations[i]  # Color set later per trial
        )
        stimuli['color_targets'].append(color_target)  # Add to list
    
    # Color-response instruction display - colored squares showing key mappings
    stimuli['color_response_instruction'] = []  # List: will contain Rect objects for instruction squares
    for i in range(StimulusColorNoResponses):  # Loop through 4 colors
        instruction_rect = visual.Rect(
            win, width=ResponseColorSize, height=ResponseColorSize, units='deg',  # Rect: square, 1.2 deg size
            fillColor=(0, 0, 0),  # Color set later based on session
            pos=(ResponseColorDistance * (-StimulusColorNoResponses/2 + 0.5 + i), -150 * 0.04)  # Tuple: position at bottom center
        )
        stimuli['color_response_instruction'].append(instruction_rect)  # Add to list
    
    return stimuli  # Dictionary: {'fixation': Circle, 'cue_boxes': [Rect...], 'cue_texts': [TextStim...], ...}
