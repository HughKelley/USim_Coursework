# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 14:14:14 2019

@author: Hugh
"""

# script for working with netlogo output data

import pandas as pd
#import matplotlib
#import matplotlib.pyplot as plt

# relevant files are 

baseline = "output/baseline-data.csv"
immunity = "output/immunity-data.csv"
recovery = "output/recovery-data.csv"

baseline_data = pd.read_csv(baseline)
immunity_data = pd.read_csv(immunity)
recovery_data = pd.read_csv(recovery)

del baseline 
del immunity 
del recovery

list(baseline_data)

baseline_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles']
immunity_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles']
recovery_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles']


grouped_baseline = baseline_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
grouped_baseline.set_index('step', inplace = True)
grouped_baseline['perc_infected'] = grouped_baseline['infected_turtles'] / grouped_baseline['total_turtles'] * 100
baseline_plot = grouped_baseline.groupby(['initial_people','num_infect'])['perc_infected'].plot(x ='ticks', y ='percent', title = 'Baseline Scenario', use_index = False)
#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")


grouped_immunity = immunity_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
grouped_immunity.set_index('step', inplace = True)
grouped_immunity['perc_infected'] = grouped_immunity['infected_turtles'] / grouped_immunity['total_turtles'] * 100
Immunity_plot = grouped_immunity.groupby(['initial_people','num_infect'])['perc_infected'].plot(title = 'Immunity Scenario')
#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")




grouped_recovery= recovery_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
grouped_recovery.set_index('step', inplace = True)
grouped_recovery['perc_infected'] = grouped_recovery['infected_turtles'] / grouped_recovery['total_turtles'] * 100
recovery_plot = grouped_recovery.groupby(['initial_people','num_infect'])['perc_infected'].plot(title = 'Recovery Scenario')
#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")



