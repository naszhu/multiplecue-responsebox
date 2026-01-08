"""
Data file creation and saving
"""
import os
from datetime import datetime


def create_data_file(experiment_name, subject, session, practice):
    """
    Create data file and write header information
    
    Args:
        experiment_name: String - name of experiment ("CCRP")
        subject: Integer - participant number
        session: Integer - session number
        practice: Boolean - True if practice session, False if experimental
    
    Returns:
        Tuple - (file object, filename string)
    """
    # Create Data directory if it doesn't exist
    if not os.path.exists("Data/"):  # Check: directory exists?
        os.makedirs("Data/")  # Create: make directory
    
    # Generate filename with timestamp
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")  # String: "2025-01-04_15-46-28"
    if practice:  # Check: practice session?
        filename = f"{experiment_name}-Prac-subj-{subject:03d}-ses-{session:03d}-{timestamp}.dat"  # String: "CCRP-Prac-subj-000-ses-001-2025-01-04_15-46-28.dat"
    else:  # Experimental session
        filename = f"{experiment_name}-subj-{subject:03d}-ses-{session:03d}-{timestamp}.dat"  # String: "CCRP-subj-000-ses-006-2025-01-04_15-46-28.dat"
    
    filepath = os.path.join("Data", filename)  # String: "Data/CCRP-Prac-subj-000-ses-001-..."
    f = open(filepath, "a")  # File: open file in append mode
    
    # Write header information
    f.write(f'Experiment Name: {experiment_name}\n')  # String: "Experiment Name: CCRP\n"
    f.write(f'Subject: {subject}\n')  # String: "Subject: 0\n"
    f.write(f'Session: {session}\n')  # String: "Session: 1\n"
    f.write(f'Practice: {practice}\n')  # String: "Practice: True\n"
    f.write(f'Start time: {timestamp}\n')  # String: "Start time: 2025-01-04_15-46-28\n"
    f.write('\n')  # Empty line
    
    # Write column headers - tab-separated
    f.write('Trial\t')  # String: "Trial\t"
    f.write('Block\t')  # String: "Block\t"
    f.write('CueCond\t')  # String: "CueCond\t"
    f.write('CueSOA\t')  # String: "CueSOA\t"
    f.write('Cues\t')  # String: "Cues\t"
    f.write('TargetColors\t')  # String: "TargetColors\t"
    f.write('Response\t')  # String: "Response\t"
    f.write('RT\t')  # String: "RT\t"
    f.write('ACC\t')  # String: "ACC\t"
    f.write('Reward\t')  # String: "Reward\t"
    f.write('ExpectedReward\t')  # String: "ExpectedReward\t"
    f.write('\n')  # Newline: end of header row
    
    return f, filename  # Tuple: (File object, "CCRP-Prac-subj-000-ses-001-...")


def save_trial_data(f, trial, trial_num):
    """
    Save data from a single trial - write one row of data
    
    Args:
        f: File object - open file handle
        trial: Trial object - trial data to save
        trial_num: Integer - trial number (1, 2, 3, ...)
    """
    cues_str = "".join(str(x) for x in trial.cues)  # String: convert list to string (e.g., [1,0,2,0] -> "1020")
    
    # Write trial data - tab-separated values
    f.write(f'{trial_num}\t')  # String: "1\t"
    f.write(f'{trial.block}\t')  # String: "1\t"
    f.write(f'{trial.cue_cond}\t')  # String: "1\t"
    f.write(f'{trial.cue_soa}\t')  # String: "200\t"
    f.write(f'{cues_str}\t')  # String: "1020\t"
    f.write(f'{trial.target_colors}\t')  # String: "1234\t"
    f.write(f'{trial.response}\t')  # String: "Z\t"
    f.write(f'{trial.rt:.3f}\t')  # String: "0.523\t"
    f.write(f'{trial.acc}\t')  # String: "1\t"
    f.write(f'{trial.reward}\t')  # String: "4\t"
    f.write(f'{trial.expected_reward}\t')  # String: "4\t"
    f.write('\n')  # Newline: end of data row
    f.flush()  # Flush: write immediately to disk
