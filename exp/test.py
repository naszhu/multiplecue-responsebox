import serial
import time
from psychopy import visual, core, event
# from psychopy import visual, monitors



# 1. Initialize serial port [cite: 133, 190]
# Make sure the port number matches what Ubuntu recognizes (/dev/ttyACM0)
try:
    ser = serial.Serial('/dev/ttyACM0', 115200, timeout=1)
    # Give Arduino 2 seconds restart/stabilization time, this is the safety period mentioned in the paper [cite: 724]
    core.wait(2.0) 
except Exception as e:
    print(f"Serial port connection failed: {e}")
    core.quit()

# 2. Set up experiment window and stimuli
# Change units="deg" to units="pix"
win = visual.Window([640, 480], fullscr=False, color="black", units="pix")
fixation = visual.TextStim(win, text="+", color="white")
target = visual.Circle(win, radius=2, fillColor="red", lineColor="white")
feedback = visual.TextStim(win, text="", pos=(0, -5))

def run_trial(trial_num):
    # Display fixation point, random wait 1-2 seconds
    fixation.draw()
    win.flip()
    core.wait(1.5 + (time.time() % 1))

    # --- Key step A: Prepare to present stimulus ---
    target.draw()
    
    # Clear old serial port data to prevent previous false triggers from interfering [cite: 252]
    ser.reset_input_buffer()
    
    # Record visual start point at screen refresh moment 
    # win.flip() is the moment when the image actually appears on the display
    win.flip() 
    
    # --- Key step B: Send synchronization command 'S' ---
    # According to the paper, send signal to Arduino to start internal timing [cite: 151, 297]
    ser.write(b'S') 
    
    # --- Key step C: Listen for RT returned by Arduino ---
    rt_ms = None
    response_received = False
    start_watch = core.getTime()
    
    while not response_received and (core.getTime() - start_watch < 3.0): # 3 second timeout
        if ser.in_waiting > 0:
            # Read one line of data returned by Arduino (unit is microseconds) [cite: 154, 304]
            raw_data = ser.readline().decode().strip()
            try:
                rt_us = int(raw_data)
                rt_ms = rt_us / 1000.0  # Convert to milliseconds to meet experimental standards [cite: 290, 646]
                response_received = True
            except ValueError:
                continue
        
        # Allow Esc key to exit
        if 'escape' in event.getKeys():
            win.close()
            ser.close()
            core.quit()

    return rt_ms

# 3. Run 5 test trials
results = []
for i in range(5):
    rt = run_trial(i + 1)
    if rt:
        results.append(rt)
        feedback.text = f"Trial {i+1} RT: {rt:.2f} ms"
    else:
        feedback.text = "No keypress detected (Timeout)"
    
    feedback.draw()
    win.flip()
    core.wait(1.0)

# Print final statistics [cite: 54, 78]
if results:
    avg_rt = sum(results) / len(results)
    print(f"\nExperiment complete! Average reaction time: {avg_rt:.2f} ms")

win.close()
ser.close()
core.quit()