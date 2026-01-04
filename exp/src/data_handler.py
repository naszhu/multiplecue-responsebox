"""
Data file creation and saving
"""
import os
from datetime import datetime


def create_data_file(experiment_name, subject, session, practice):
    """
    Create data file and write header information
    
    Args:
        experiment_name: Name of the experiment
        subject: Subject number
        session: Session number
        practice: Whether this is a practice session
    
    Returns:
        File object and filename
    """
    # Create Data directory if it doesn't exist
    if not os.path.exists("Data/"):
        os.makedirs("Data/")
    
    # Generate filename
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    if practice:
        filename = f"{experiment_name}-Prac-subj-{subject:03d}-ses-{session:03d}-{timestamp}.dat"
    else:
        filename = f"{experiment_name}-subj-{subject:03d}-ses-{session:03d}-{timestamp}.dat"
    
    filepath = os.path.join("Data", filename)
    f = open(filepath, "a")
    
    # Write header
    f.write(f'Experiment Name: {experiment_name}\n')
    f.write(f'Subject: {subject}\n')
    f.write(f'Session: {session}\n')
    f.write(f'Practice: {practice}\n')
    f.write(f'Start time: {timestamp}\n')
    f.write('\n')
    
    # Write column headers
    f.write('Trial\t')
    f.write('Block\t')
    f.write('CueCond\t')
    f.write('CueSOA\t')
    f.write('Cues\t')
    f.write('TargetLetters\t')
    f.write('Response\t')
    f.write('RT\t')
    f.write('ACC\t')
    f.write('Reward\t')
    f.write('ExpectedReward\t')
    f.write('\n')
    
    return f, filename


def save_trial_data(f, trial, trial_num):
    """
    Save data from a single trial
    
    Args:
        f: File object
        trial: Trial object
        trial_num: Trial number
    """
    cues_str = "".join(str(x) for x in trial.cues)
    
    f.write(f'{trial_num}\t')
    f.write(f'{trial.block}\t')
    f.write(f'{trial.cue_cond}\t')
    f.write(f'{trial.cue_soa}\t')
    f.write(f'{cues_str}\t')
    f.write(f'{trial.target_letters}\t')
    f.write(f'{trial.response}\t')
    f.write(f'{trial.rt:.3f}\t')
    f.write(f'{trial.acc}\t')
    f.write(f'{trial.reward}\t')
    f.write(f'{trial.expected_reward}\t')
    f.write('\n')
    f.flush()

