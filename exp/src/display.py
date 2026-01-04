"""
Display window setup and management
"""
from psychopy import visual, monitors
from config import WINDOW_CONFIG, MONITOR_CONFIG


def setup_monitor():
    """
    Setup and configure the monitor
    
    Returns:
        Monitor object
    """
    mon = monitors.Monitor(WINDOW_CONFIG['monitor'])
    mon.setSizePix(WINDOW_CONFIG['size'])
    mon.setWidth(MONITOR_CONFIG['width'])
    mon.setDistance(MONITOR_CONFIG['distance'])
    mon.saveMon()
    return mon


def create_window(monitor):
    """
    Create the experimental window
    
    Args:
        monitor: Monitor object
    
    Returns:
        Window object
    """
    if WINDOW_CONFIG['fullscreen']:
        win = visual.Window(
            size=WINDOW_CONFIG['size'],
            units=WINDOW_CONFIG['units'],
            fullscr=True,
            allowGUI=False,
            colorSpace=WINDOW_CONFIG['color_space'],
            monitor=WINDOW_CONFIG['monitor'],
            color=WINDOW_CONFIG['bg_color']
        )
    else:
        win = visual.Window(
            size=WINDOW_CONFIG['size'],
            units=WINDOW_CONFIG['units'],
            fullscr=False,
            allowGUI=True,
            colorSpace=WINDOW_CONFIG['color_space'],
            monitor=WINDOW_CONFIG['monitor'],
            color=WINDOW_CONFIG['bg_color']
        )
    
    return win

