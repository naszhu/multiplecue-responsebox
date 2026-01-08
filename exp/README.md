# Multiple Cue Paradigm Experiment

A clean, modular implementation of the Multiple Cue Paradigm (MCP) experiment.

## Structure

```
exp/
├── main.py                 # Main experiment script
├── src/
│   ├── config.py          # Configuration settings (window, monitor, experiment)
│   ├── experiment_params.py  # Experiment parameters (cues, SOAs, blocks)
│   ├── trial.py           # Trial class and generation functions
│   ├── stimuli.py         # Stimulus creation and location functions
│   ├── display.py         # Window and monitor setup
│   └── data_handler.py    # Data file creation and saving
└── README.md
```

## Configuration

Edit `src/config.py` to change:
- Window settings (fullscreen vs debug window)
- Monitor settings
- Subject/session information

## Running the Experiment

```bash
cd exp
python main.py
```

## Requirements

- PsychoPy
- numpy

Note: Masks are created programmatically using rectangles - no image files needed.

