"""
Configuration file for experiment settings
"""
import os

# Window configuration
WINDOW_CONFIG = {
    'fullscreen': False,  # Set to True for fullscreen, False for debug window
    'size': (800, 600),  # Smaller window size for debugging or non-fullscreen
    'monitor': 'OF2A_03_5_513_lab5',
    'units': 'deg',
    'color_space': 'rgb',
    'bg_color': (0, 0, 0),  # Black background
}

# Monitor settings
MONITOR_CONFIG = {
    'width': 52,  # cm
    'distance': 60,  # cm
}

# Experiment configuration
EXPERIMENT_CONFIG = {
    'subject': 0,
    'session': 1,
    'start_block': 1,
}

