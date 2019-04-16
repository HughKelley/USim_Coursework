# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 14:14:14 2019

@author: Hugh
"""

# script for working with netlogo output data

import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
fig, ax = plt.subplots()

# relevant files are 

baseline = "output/baseline-table.csv"
immunity = "output/immunity-table.csv"
recovery = "output/recovery-table.csv"

baseline_data = pd.read_csv(baseline)
immunity_data = pd.read_csv(immunity)
recovery_data = pd.read_csv(recovery)

del baseline 
del immunity 
del recovery

list(baseline_data)

baseline_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles','sum_sicktime']
immunity_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles','sum_sicktime']
recovery_data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','total_turtles','sum_sicktime']


grouped_baseline = baseline_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
#grouped_baseline.set_index('step', inplace = True)
grouped_baseline['perc_infected'] = grouped_baseline['infected_turtles'] / grouped_baseline['total_turtles'] * 100
grouped_baseline['sicktime_per_turtle'] = grouped_baseline['sum_sicktime'] / grouped_baseline['total_turtles']

# use plt.close() to clear plot

baseline_plot_perc = grouped_baseline.groupby(['initial_people','num_infect'])['perc_infected'].plot(x ='ticks', y ='percent', title = 'Baseline Scenario % Infected', use_index = False)
baseline_plot_time = grouped_baseline.groupby(['initial_people','num_infect'])['sum_sicktime'].plot(x ='ticks', y ='percent', title = 'Baseline Scenario Total Sick Time', use_index = False)

# plot
#for key, group in grouped_baseline.groupby(['initial_people', 'num_infect']):
#    ax = group.plot(ax=ax, kind='line',x='step', y = 'perc_infected', c=key, label=key)

#plt.plot( 'x', 'y1', data=df, marker='o', markerfacecolor='blue', markersize=12, color='skyblue', linewidth=4)
#plt.plot( 'x', 'y2', data=df, marker='', color='olive', linewidth=2)
#plt.plot( 'x', 'y3', data=df, marker='', color='olive', linewidth=2, linestyle='dashed', label="toto")
#plt.legend()

#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")

grouped_immunity = immunity_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
grouped_immunity.set_index('step', inplace = True)
grouped_immunity['perc_infected'] = grouped_immunity['infected_turtles'] / grouped_immunity['total_turtles'] * 100
grouped_immunity['sicktime_per_turtle'] = grouped_immunity['sum_sicktime'] / grouped_immunity['total_turtles']

immunity_plot_perc = grouped_immunity.groupby(['initial_people','num_infect'])['perc_infected'].plot(x ='ticks', y ='percent', title = 'Immunity Scenario % Infected', use_index = False)

immunity_plot_time = grouped_immunity.groupby(['initial_people','num_infect'])['sum_sicktime'].plot(x ='ticks', y ='percent', title = 'Immunity Scenario Total Sick Time', use_index = False)


#Immunity_plot = grouped_immunity.groupby(['initial_people','num_infect'])['perc_infected'].plot(title = 'Immunity Scenario')
#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")

grouped_recovery= recovery_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()
grouped_recovery.set_index('step', inplace = True)
grouped_recovery['perc_infected'] = grouped_recovery['infected_turtles'] / grouped_recovery['total_turtles'] * 100
grouped_recovery['sicktime_per_turtle'] = grouped_recovery['sum_sicktime'] / grouped_recovery['total_turtles']

#recovery_plot = grouped_recovery.groupby(['initial_people','num_infect'])['perc_infected'].plot(title = 'Recovery Scenario')
#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")

recovery_plot_perc = grouped_recovery.groupby(['initial_people','num_infect'])['perc_infected'].plot(x ='ticks', y ='percent', title = 'Recovery Scenario % Infected', use_index = False)

recovery_plot_time = grouped_recovery.groupby(['initial_people','num_infect'])['sum_sicktime'].plot(x ='ticks', y ='percent', title = 'Recovery Scenario Total Sick Time', use_index = False)

# explore high immunity outcomes

explore_immunity = immunity_data.groupby(['initial_people', 'step','num_infect'], as_index = False).mean()

immunity_outcomes = explore_immunity.loc[explore_immunity['step']==60]
#immunity_outcomes.set_index(')
immunity_outcomes['perc_infected'] = immunity_outcomes['infected_turtles'] / immunity_outcomes['total_turtles'] * 100
grouped_outcomes = immunity_outcomes.groupby(['total_turtles']).mean()
grouped_outcomes['perc_infected'].plot()




