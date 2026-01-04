"""
Stimulus creation and management
"""
from psychopy import visual
from numpy import cos, sin, pi
from experiment_params import (
    FixationSize, CueBoxSize, CueTextSize, LetterSize, TargetDistance,
    MaskSize, CueDistance, CueLocRotation, TargetLocRotation,
    CueTextColor, CueBgColor, LetterColor, FixationPointColor,
    NoCueLocations, CueAssocList, NoTargets, StimFactor
)


def create_cue_locations():
    """
    Generate cue location coordinates based on association type
    
    Returns:
        List of [x, y] coordinates for each cue location
    """
    locations = []
    for i in range(NoCueLocations):
        x = CueDistance * (CueAssocList[i] - 1 - 1.5)
        y = 0
        locations.append([x, y])
    return locations


def create_target_locations():
    """
    Generate target location coordinates
    
    Returns:
        List of [x, y] coordinates for each target location
    """
    locations = []
    for i in range(NoTargets):
        x = TargetDistance * cos(2 * pi / NoTargets * i + TargetLocRotation)
        y = TargetDistance * sin(2 * pi / NoTargets * i + TargetLocRotation)
        locations.append([x, y])
    return locations


def create_stimuli(win, cue_locations, target_locations):
    """
    Create all visual stimuli objects
    
    Args:
        win: PsychoPy window object
        cue_locations: List of cue [x, y] coordinates
        target_locations: List of target [x, y] coordinates
    
    Returns:
        Dictionary containing all stimulus objects
    """
    stimuli = {}
    
    # Fixation point
    stimuli['fixation'] = visual.Circle(
        win, size=FixationSize, units='deg', fillColor=FixationPointColor
    )
    
    # Cue boxes and text
    stimuli['cue_boxes'] = []
    stimuli['cue_texts'] = []
    stimuli['cue_arrows'] = []
    
    for i in range(NoCueLocations):
        # Cue box (square)
        cue_box = visual.Rect(
            win, width=CueBoxSize, height=CueBoxSize, units='deg',
            fillColor=CueBgColor, pos=cue_locations[i]
        )
        stimuli['cue_boxes'].append(cue_box)
        
        # Cue text
        cue_text = visual.TextStim(
            win, text="", height=CueTextSize, units='deg',
            color=CueTextColor, pos=cue_locations[i]
        )
        stimuli['cue_texts'].append(cue_text)
        
        # Cue arrows (lines from cues to targets)
        # Map cue location to associated target location using CueAssocList
        target_idx = CueAssocList[i] - 1  # Convert to 0-based index
        arrow = visual.Line(
            win, lineColor=(0, 0, 0), units='deg'
        )
        arrow.setVertices([cue_locations[i], target_locations[target_idx]])
        stimuli['cue_arrows'].append(arrow)
    
    # Letter stimuli
    stimuli['letter_stim'] = []
    for i in range(NoTargets):
        letter = visual.TextStim(
            win, text="", height=LetterSize, units='deg',
            color=LetterColor, pos=target_locations[i]
        )
        stimuli['letter_stim'].append(letter)
    
    # Masks - using Rectangles as simple pattern masks (no image file needed)
    stimuli['masks'] = []
    for i in range(NoTargets):
        # Create a simple rectangle mask with pattern-like appearance
        mask = visual.Rect(
            win, width=MaskSize[0], height=MaskSize[1], 
            units='deg', fillColor=(0.5, 0.5, 0.5),  # Gray mask
            lineColor=(0.3, 0.3, 0.3), lineWidth=2,  # Dark border
            pos=target_locations[i]
        )
        stimuli['masks'].append(mask)
    
    return stimuli

