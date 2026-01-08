"""
Configuration file for experiment settings
"""
import os

# Window configuration - controls how the display window appears
WINDOW_CONFIG = {
    'fullscreen': False,  # Boolean: True = fullscreen mode, False = debug window mode
    'size': (800, 600),  # Tuple of (width, height) in pixels: window size when not fullscreen
    'monitor': 'OF2A_03_5_513_lab5',  # String: name of monitor profile to use
    'units': 'deg',  # String: unit type for stimulus sizes ('deg' = degrees of visual angle)
    'color_space': 'rgb',  # String: color space format ('rgb' = red-green-blue values from -1 to 1)
    'bg_color': (0, 0, 0),  # Tuple of 3 floats: background color RGB values (0,0,0) = black
}

# Monitor settings - physical properties of the display monitor
MONITOR_CONFIG = {
    'width': 52,  # Float: monitor width in centimeters
    'distance': 60,  # Float: viewing distance from participant to monitor in centimeters
}

# Experiment configuration - participant and session information
EXPERIMENT_CONFIG = {
    'subject': 0,  # Integer: participant number (0 = default, will be set in dialog)
    'session': 1,  # Integer: session number (1 = first practice session)
    'start_block': 1,  # Integer: which block to start from (1 = first block)
}
