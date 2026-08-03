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


---

In the Cued Color Response Paradigm (CCRP) implemented in multiplecue-responsebox, participants see four colored circles at peripheral locations and must press the matching color key (keyboard or response box) as quickly and accurately as possible to earn points that convert to money; each trial first briefly shows one or two digit cues (values 1–4) that mark which colors are rewarded and how much, so the task is not just color naming but choosing among competing rewarded options—especially on dual-cue trials, where people should prefer the higher-value cue but can still earn from the lower one—thereby measuring how reward cues guide rapid action selection under time pressure across practice and experimental sessions.