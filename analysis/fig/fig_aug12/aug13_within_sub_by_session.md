=== Within-subject Condition × Session analysis (sub12) ===
RT ~ Condition * Session (ANOVA)
  Condition:           SS=12581683  F=8.02  p=7.089e-12
  Session:             SS=44179247  F=23.04  p=1.138e-46
  Condition × Session: SS=17260251  F=1.00  p=0.4807
  SS_int / SS_Condition = 1.372  (interaction relative to stable condition effect)
  SS_int / (SS_Condition+SS_int) = 0.578
ACC ~ Condition * Session (binomial GLM, LR tests)
  Condition:           Dev=32.81  df=9  p=0.0001444
  Session:             Dev=14.28  df=11  p=0.2178
  Condition × Session: Dev=130.02  df=99  p=0.01993
  Dev_int / Dev_Condition = 3.963
Two-cue condition stability across sessions (higher sd => more session-to-session change):
  (1,2)  MedRT mean=975 sd=95.6 | Acc mean=0.969 sd=0.036  (n_ses=12)
  (3,4)  MedRT mean=966 sd=93.8 | Acc mean=0.946 sd=0.040  (n_ses=12)
  (2,4)  MedRT mean=924 sd=92.8 | Acc mean=0.975 sd=0.024  (n_ses=12)
  (1,4)  MedRT mean=848 sd=89.2 | Acc mean=0.983 sd=0.016  (n_ses=12)
  (2,3)  MedRT mean=977 sd=88.3 | Acc mean=0.944 sd=0.049  (n_ses=12)
  (1,3)  MedRT mean=900 sd=69.3 | Acc mean=0.983 sd=0.025  (n_ses=12)