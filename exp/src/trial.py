"""
Trial class and trial generation functions
"""
import random as rnd


class Trial:
    """Represents a single experimental trial"""
    
    def __init__(self, session, block, warmup, cue_cond, cond, cue_soa, 
                 no_cue_locations, cues, cues_val, target_letters):
        self.session = session
        self.block = block
        self.warmup = warmup
        self.cue_cond = cue_cond
        self.cond = cond
        self.cue_soa = cue_soa
        
        # Assign cues to locations
        self.no_cues = len(cues)
        cue_order = list(range(no_cue_locations))
        rnd.shuffle(cue_order)
        
        self.cues = no_cue_locations * [0]
        self.cues[0:self.no_cues] = cues
        
        self.cues_val = no_cue_locations * [0]
        self.cues_val[0:self.no_cues] = cues_val
        
        # Shuffle cue positions
        temp_cues = no_cue_locations * [0]
        temp_cues_val = no_cue_locations * [0]
        for i in range(no_cue_locations):
            temp_cues[i] = self.cues[cue_order[i]]
            temp_cues_val[i] = self.cues_val[cue_order[i]]
        
        self.cues = temp_cues
        self.cues_val = temp_cues_val
        
        # Calculate cue ranks
        self.cue_ranks = [sorted(self.cues).index(x) for x in self.cues]
        self.cue_ranks = [no_cue_locations - x for x in self.cue_ranks]
        
        # Target letters
        self.target_letters = target_letters
        
        # Exposure duration (set later)
        self.ed = 0
        
        # Response variables
        self.response = "-"
        self.response_loc = ""
        self.rt = 0.0
        self.acc = 0
        self.intr = 0
        self.err = 0
        
        # Reward variables
        self.cue_response_value = 0
        self.cue_response_exp_value = 0
        self.cue_response_rank = 0
        self.expected_reward = 0
        self.reward = 0
        self.max_reward = 0
        self.cum_reward = 0.0
        
        # Timing variables
        self.cue_time = 0.0
        self.stim_time = 0.0
        self.mask_time = 0.0
        self.end_trial_time = 0.0


def generate_block_trials(session, block, repetitions, warmup, cue_soa_conds,
                          no_cue_locations, cue_set, cue_set_val, 
                          stimulus_target_letters, no_targets, stim_ed):
    """
    Generate trials for a single block
    
    Args:
        session: Session number
        block: Block number
        repetitions: Number of repetitions per condition
        warmup: Whether these are warmup trials (1) or not (0)
        cue_soa_conds: List of SOA conditions
        no_cue_locations: Number of cue locations
        cue_set: List of cue combinations
        cue_set_val: List of cue value combinations
        stimulus_target_letters: Available target letters
        no_targets: Number of targets
        stim_ed: Stimulus exposure duration
    
    Returns:
        List of Trial objects
    """
    block_data = []
    
    for rep in range(repetitions):
        cue_cond = 0
        cond = 0
        
        for i in range(len(cue_set)):
            cues = cue_set[i]
            cues_val = cue_set_val[i]
            cue_cond += 1
            
            for cue_soa in cue_soa_conds:
                # Generate random target letters
                temp = list(stimulus_target_letters)
                rnd.shuffle(temp)
                target_letters = ''.join(temp[:no_targets])
                
                cond += 1
                
                trial = Trial(session, block, warmup, cue_cond, cond, cue_soa,
                            no_cue_locations, cues, cues_val, target_letters)
                trial.ed = stim_ed
                
                block_data.append(trial)
    
    rnd.shuffle(block_data)
    return block_data

