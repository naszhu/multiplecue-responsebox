# Multiple Cue Reaction Time Experiment with Custom Response Box

This repository contains code for running a multiple cue reaction time (RT) experiment using PsychoPy and a self-made response box based on Arduino. The project is designed for precise measurement of reaction times in a visual cueing task, suitable for experimental psychology or neuroscience research.

## Features

- **Precise Stimulus Timing:** Uses PsychoPy for accurate visual stimulus presentation and response collection.
- **Custom Response Box Integration:** Communicates with an Arduino-based response box via serial port for hardware-level response registration.
- **Synchronized Timing:** Sends/receives signals to/from the Arduino to ensure microsecond-level accuracy between stimulus display and response event.
- **Automatic Data Collection:** Collects RTs, provides trial-by-trial feedback, and outputs summary statistics at the end of the experiment.

## How It Works

1. **Initialization:** Sets up the serial communication with the Arduino response box and the PsychoPy experiment window.
2. **Trial Loop:** For each trial:
   - Displays a fixation point, waits for a random interval.
   - Presents a cue (red circle).
   - Sends a synchronization signal (`'S'`) to the Arduino at the exact moment the cue appears.
   - Waits for response timing data from the Arduino, which measures the physical button press latency.
   - Displays feedback on the participant's reaction time.
3. **Summary:** At the end, prints the average RT and closes all connections safely.

## Hardware Requirements

- A computer with Python and [PsychoPy](https://www.psychopy.org/) installed.
- An Arduino board flashed with appropriate response-box firmware (detects button presses and communicates via serial).
- A simple response box wired to the Arduino (one or more push buttons).
- Proper USB cable and permissions for accessing `/dev/ttyACM0` (or adjust for your system).

## Software Requirements

- Python 3.x
- PsychoPy
- PySerial

Install requirements (if needed):
```
pip install psychopy pyserial
```

## Running the Experiment

1. Ensure the Arduino is connected and recognized by your OS.
2. Update the serial port in `test.py` if it's not `/dev/ttyACM0`.
3. Run the script:
   ```
   python test.py
   ```
4. Follow the on-screen instructions. Press the designated button on the response box as soon as you see the cue.

## Notes

- Press `ESC` at any time during a trial to exit safely.
- The script displays individual feedback for 5 trials and computes the average RT at the end.



---

