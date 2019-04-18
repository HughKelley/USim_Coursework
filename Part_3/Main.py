# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 14:14:14 2019

@author: Hugh
"""

# script for working with netlogo output data

import pandas as pd

# relevant files are 
file = "output/twenty-runs-scenario-3.csv"

data = pd.read_csv(file, header=6)  # header = 6
list(data)

data.columns = ['run_number', 'initial_people','num_infect','immune_chance','recovery_chance','step','infected_turtles','immune_turtles','total_turtles','sum_sicktime']
data['perc_infected'] = data['infected_turtles'] / data['total_turtles'] * 100
data['sicktime_per_turtle'] = data['sum_sicktime'] / (data['total_turtles'] - data['immune_turtles'])

data2 = data

values_50 = data.loc[data['step'] == 40]
values_100 = data.loc[data['step'] == 100]
values_250 = data.loc[data['step'] == 250]
values_150 = data.loc[data['step'] == 150]
values_500 = data.loc[data['step'] == 500]
values_400 = data.loc[data['step'] == 400]


ending_values = values_50
ending_mean = ending_values['perc_infected'].mean()
ending_std = ending_values['perc_infected'].std()

time_mean = ending_values['sicktime_per_turtle'].mean()
time_std = ending_values['sicktime_per_turtle'].std()


# calc margin of error

z = 1.96
n = 100
perc_s_e = ending_std/(n**0.5)
perc_margin_of_error = perc_s_e * z

time_s_e = time_std/(n**0.5)
time_margin_of_error = time_s_e * z

#grouped_data = data.groupby(['run_number'], as_index = False).mean()
data.set_index('step', inplace = True)

plot1 = data.groupby('run_number')['perc_infected'].plot()

# use plt.close() to clear plot
#baseline_plot_perc = grouped_data.groupby(['initial_people','num_infect'])['perc_infected'].plot(x ='ticks', y ='percent', title = 'Baseline Scenario % Infected', use_index = False)
#baseline_plot_time = grouped_data.groupby(['initial_people','num_infect'])['sum_sicktime'].plot(x ='ticks', y ='percent', title = 'Baseline Scenario Total Sick Time', use_index = False)


# now calculate confidence interval for mean of pop




# plot
#for key, group in grouped_baseline.groupby(['initial_people', 'num_infect']):
#    ax = group.plot(ax=ax, kind='line',x='step', y = 'perc_infected', c=key, label=key)

#plt.plot( 'x', 'y1', data=df, marker='o', markerfacecolor='blue', markersize=12, color='skyblue', linewidth=4)
#plt.plot( 'x', 'y2', data=df, marker='', color='olive', linewidth=2)
#plt.plot( 'x', 'y3', data=df, marker='', color='olive', linewidth=2, linestyle='dashed', label="toto")
#plt.legend()

#fig = plot.get_figure()
#fig.savefig("baseline_steady_state.png")





