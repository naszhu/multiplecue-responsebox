"""
Display window setup and management
"""
from psychopy import visual, monitors
from config import WINDOW_CONFIG, MONITOR_CONFIG


def setup_monitor():
    """
    Setup and configure the monitor - defines physical properties
    
    Returns:
        Monitor object - configured monitor profile
    """
    mon = monitors.Monitor(WINDOW_CONFIG['monitor'])  # Monitor: create monitor object with name
    mon.setSizePix(WINDOW_CONFIG['size'])  # Set: monitor resolution in pixels (800, 600)
    mon.setWidth(MONITOR_CONFIG['width'])  # Set: physical width in cm (52 cm)
    mon.setDistance(MONITOR_CONFIG['distance'])  # Set: viewing distance in cm (60 cm)
    mon.saveMon()  # Save: store monitor settings
    return mon  # Monitor: return configured monitor object


def create_window(monitor):
    """
    Create the experimental window - main display surface
    
    Args:
        monitor: Monitor object - monitor profile to use
    
    Returns:
        Window object - the display window
    """
    if WINDOW_CONFIG['fullscreen']:  # Check: fullscreen mode?
        win = visual.Window(
            size=WINDOW_CONFIG['size'],  # Tuple: (800, 600) pixels
            units=WINDOW_CONFIG['units'],  # String: 'deg' = degrees of visual angle
            fullscr=True,  # Boolean: True = fullscreen, hides taskbar
            allowGUI=False,  # Boolean: False = no GUI elements visible
            colorSpace=WINDOW_CONFIG['color_space'],  # String: 'rgb' = RGB color space
            monitor=WINDOW_CONFIG['monitor'],  # String: monitor profile name
            color=WINDOW_CONFIG['bg_color']  # Tuple: (0,0,0) = black background
        )
    else:  # Debug window mode
        win = visual.Window(
            size=WINDOW_CONFIG['size'],  # Tuple: (800, 600) pixels
            units=WINDOW_CONFIG['units'],  # String: 'deg' = degrees of visual angle
            fullscr=False,  # Boolean: False = windowed mode
            allowGUI=True,  # Boolean: True = allow GUI elements
            colorSpace=WINDOW_CONFIG['color_space'],  # String: 'rgb' = RGB color space
            monitor=WINDOW_CONFIG['monitor'],  # String: monitor profile name
            color=WINDOW_CONFIG['bg_color']  # Tuple: (0,0,0) = black background
        )
    
    return win  # Window: return window object
