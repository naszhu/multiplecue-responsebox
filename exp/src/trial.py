"""
Trial class and trial generation functions
"""
import random as rnd


class Trial:
    """Represents a single experimental trial"""
    
    def __init__(self, session, block, warmup, cue_cond, cond, cue_soa, 
                 no_cue_locations, cues, cues_val, target_colors):
        # Session and block information
        self.session = session  # Integer: session number (1-5 = practice, 6+ = experimental)
        self.block = block  # Integer: block number within session (1, 2, 3, ...)
        self.warmup = warmup  # Integer: 1 = warmup trial, 0 = regular trial
        
        # Condition identifiers
        self.cue_cond = cue_cond  # Integer: cue condition number (which cue combination)
        self.cond = cond  # Integer: full condition number (cue + SOA combination)
        self.cue_soa = cue_soa  # Integer: SOA duration in milliseconds (200ms)
        
        # Assign cues to locations - shuffle positions randomly
        self.no_cues = len(cues)  # Integer: number of active cues in this trial
        cue_order = list(range(no_cue_locations))  # List: [0, 1, 2, 3] = cue position indices
        rnd.shuffle(cue_order)  # Shuffle: randomize cue positions
        
        # Initialize cue arrays - one value per cue location
        self.cues = no_cue_locations * [0]  # List of integers: cue values at each position [0,0,0,0]
        self.cues[0:self.no_cues] = cues  # Fill first N positions with actual cue values
        
        self.cues_val = no_cue_locations * [0]  # List of integers: cue reward values [0,0,0,0]
        self.cues_val[0:self.no_cues] = cues_val  # Fill first N positions with actual reward values
        
        # Shuffle cue positions - apply random order
        temp_cues = no_cue_locations * [0]  # Temporary list for shuffled cues
        temp_cues_val = no_cue_locations * [0]  # Temporary list for shuffled values
        for i in range(no_cue_locations):
            temp_cues[i] = self.cues[cue_order[i]]  # Reorder cues according to shuffled positions
            temp_cues_val[i] = self.cues_val[cue_order[i]]  # Reorder values accordingly
        
        self.cues = temp_cues  # List of integers: final cue values at each position (e.g., [1,0,2,0])
        self.cues_val = temp_cues_val  # List of integers: final reward values at each position
        
        # Calculate cue ranks - higher value = higher rank
        self.cue_ranks = [sorted(self.cues).index(x) for x in self.cues]  # List: rank of each cue (0=lowest)
        self.cue_ranks = [no_cue_locations - x for x in self.cue_ranks]  # Invert: higher value = higher rank
        
        # Target colors - which color appears at each target position
        self.target_colors = target_colors  # String: color labels for each target (e.g., "1234")
        
        # Exposure duration
        self.ed = 0  # Integer: stimulus exposure duration in milliseconds (set later)
        
        # Response variables
        self.response = "-"  # String: key pressed by participant ("Z", "X", ".", "-", or "-" if no response)
        self.response_loc = ""  # String: which targets were selected ("1"=selected, "0"=not, e.g., "1000")
        self.rt = 0.0  # Float: reaction time in seconds (time from cue to keypress)
        self.acc = 0  # Integer: number of correct responses (1 = correct, 0 = incorrect)
        self.intr = 0  # Integer: number of intrusion errors (responded to non-cued target)
        self.err = 0  # Integer: number of errors (wrong key or no valid response)
        
        # Reward variables
        self.cue_response_value = 0  # Integer: reward value of selected cue (0-4)
        self.cue_response_exp_value = 0  # Integer: expected cue value (what cue indicated)
        self.cue_response_rank = 0  # Integer: rank of selected cue (1-4)
        self.expected_reward = 0  # Integer: expected reward points based on cues
        self.reward = 0  # Integer: actual reward points received
        self.max_reward = 0  # Integer: maximum possible reward for this trial
        self.cum_reward = 0.0  # Float: cumulative reward across all trials
        
        # Timing variables
        self.cue_time = 0.0  # Float: timestamp when cues appeared (seconds)
        self.color_target_time = 0.0  # Float: timestamp when color targets appeared (seconds)
        self.end_trial_time = 0.0  # Float: timestamp when trial ended (seconds)


def generate_block_trials(session, block, repetitions, warmup, cue_soa_conds,
                          no_cue_locations, cue_set, cue_set_val, 
                          stimulus_target_colors, no_targets, stim_ed):
    """
    Generate trials for a single block
    
    Args:
        session: Integer - session number
        block: Integer - block number
        repetitions: Integer - how many times each condition repeats
        warmup: Integer - 1 for warmup trials, 0 for regular trials
        cue_soa_conds: List - SOA durations in milliseconds [200]
        no_cue_locations: Integer - number of cue positions (4)
        cue_set: List of lists - cue combinations [[1], [2], [1,2], ...]
        cue_set_val: List of lists - cue reward values (same structure)
        stimulus_target_colors: String - available color labels "1234"
        no_targets: Integer - number of target positions (4)
        stim_ed: Integer - exposure duration in milliseconds
    
    Returns:
        List of Trial objects - shuffled randomly
    """
    block_data = []  # List: will contain all trials for this block
    
    # Generate all condition combinations
    for rep in range(repetitions):  # Repeat each condition N times
        cue_cond = 0  # Integer: counter for cue conditions
        cond = 0  # Integer: counter for full conditions
        
        for i in range(len(cue_set)):  # Loop through each cue combination
            cues = cue_set[i]  # List: cue values for this combination (e.g., [1, 2])
            cues_val = cue_set_val[i]  # List: reward values (e.g., [1, 2])
            cue_cond += 1  # Increment cue condition counter
            
            for cue_soa in cue_soa_conds:  # Loop through each SOA duration
                # Generate random color assignment to targets
                temp = list(stimulus_target_colors)  # List: ['1','2','3','4']
                rnd.shuffle(temp)  # Shuffle: randomize color order
                target_colors = ''.join(temp[:no_targets])  # String: first N colors (e.g., "3241")
                
                cond += 1  # Increment full condition counter
                
                # Create trial object
                trial = Trial(session, block, warmup, cue_cond, cond, cue_soa,
                            no_cue_locations, cues, cues_val, target_colors)
                trial.ed = stim_ed  # Set exposure duration
                
                block_data.append(trial)  # Add trial to block
    
    rnd.shuffle(block_data)  # Shuffle: randomize trial order within block
    return block_data  # List: return shuffled trials
