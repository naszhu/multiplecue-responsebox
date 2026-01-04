"""
checkmonitor.py - Monitor refresh rate validation utility for PsychoPy experiments

This module provides a function to check and validate monitor refresh rates
to ensure accurate timing in experimental paradigms.
"""

from psychopy import core, visual
import numpy as np


def checkmonitor(win, set_resx=1920, set_resy=1080, set_framerate=100, 
                 fr_tol=10, maxnomesures=10, nomesframes=50, fullscreen=False):
    """
    Check and validate monitor refresh rate.
    
    Parameters:
    -----------
    win : psychopy.visual.Window
        The PsychoPy window object to check
    set_resx : int
        Expected horizontal resolution (default: 1920)
    set_resy : int
        Expected vertical resolution (default: 1080)
    set_framerate : int
        Expected framerate in Hz (default: 100)
    fr_tol : int
        Framerate tolerance in Hz (default: 10)
    maxnomesures : int
        Maximum number of measurements to take (default: 10)
    nomesframes : int
        Number of frames to measure per sample (default: 50)
    fullscreen : bool
        Whether window is in fullscreen mode (default: False)
    
    Returns:
    --------
    float
        Measured framerate
    """
    
    print("\n" + "="*70)
    print("Checking Monitor Settings")
    print("="*70)
    print(f"Expected Resolution: {set_resx} x {set_resy}")
    print(f"Expected Framerate: {set_framerate} Hz")
    print(f"Framerate Tolerance: ±{fr_tol} Hz")
    print(f"Fullscreen Mode: {fullscreen}")
    print("-"*70)
    
    # Get actual window size
    actual_size = win.size
    print(f"Actual Window Size: {actual_size[0]} x {actual_size[1]}")
    
    # Check resolution match
    if abs(actual_size[0] - set_resx) > 1 or abs(actual_size[1] - set_resy) > 1:
        print(f"WARNING: Resolution mismatch!")
        print(f"  Expected: {set_resx} x {set_resy}")
        print(f"  Actual: {actual_size[0]} x {actual_size[1]}")
    else:
        print("Resolution: OK")
    
    # Measure framerate
    print("\nMeasuring framerate...")
    framerates = []
    
    for measure_num in range(maxnomesures):
        # Create a simple stimulus for timing
        stim = visual.Circle(win, radius=10, fillColor='white', lineColor=None)
        
        # Wait a bit before starting measurement
        core.wait(0.1)
        
        # Measure frames
        win.flip()  # First flip to start timing
        start_time = core.getTime()
        start_frame = win.frames
        
        # Draw and flip for specified number of frames
        for _ in range(nomesframes):
            stim.draw()
            win.flip()
        
        end_time = core.getTime()
        end_frame = win.frames
        
        # Calculate framerate
        elapsed_time = end_time - start_time
        frames_elapsed = end_frame - start_frame
        
        if elapsed_time > 0:
            measured_fr = frames_elapsed / elapsed_time
            framerates.append(measured_fr)
            print(f"  Measurement {measure_num + 1}/{maxnomesures}: {measured_fr:.2f} Hz")
        else:
            print(f"  Measurement {measure_num + 1}/{maxnomesures}: Failed (zero elapsed time)")
    
    if not framerates:
        print("\nERROR: Could not measure framerate!")
        return None
    
    # Calculate statistics
    mean_fr = np.mean(framerates)
    std_fr = np.std(framerates)
    min_fr = np.min(framerates)
    max_fr = np.max(framerates)
    
    print("\n" + "-"*70)
    print("Framerate Statistics:")
    print(f"  Mean: {mean_fr:.2f} Hz")
    print(f"  Std:  {std_fr:.2f} Hz")
    print(f"  Min:  {min_fr:.2f} Hz")
    print(f"  Max:  {max_fr:.2f} Hz")
    print("-"*70)
    
    # Check if framerate is within tolerance
    fr_diff = abs(mean_fr - set_framerate)
    if fr_diff <= fr_tol:
        print(f"Framerate: OK (within ±{fr_tol} Hz tolerance)")
        print(f"  Expected: {set_framerate} Hz")
        print(f"  Measured: {mean_fr:.2f} Hz")
        print(f"  Difference: {fr_diff:.2f} Hz")
    else:
        print(f"WARNING: Framerate outside tolerance!")
        print(f"  Expected: {set_framerate} Hz")
        print(f"  Measured: {mean_fr:.2f} Hz")
        print(f"  Difference: {fr_diff:.2f} Hz (tolerance: ±{fr_tol} Hz)")
    
    print("="*70 + "\n")
    
    return mean_fr

