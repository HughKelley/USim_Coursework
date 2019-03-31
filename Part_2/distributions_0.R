# plots different ditributions

#histogram with a count of 3000, a mean of 75 and a standard deviation of 5
qplot(rnorm(0:3000,mean=75,sd=5))

qplot(rpois(0:3000,lambda=75)) + stat_bin(binwidth = 1)

#what about a lambda of 0.5?
qplot(rpois(0:3000,0.5)) + stat_bin(binwidth = 1)

qplot(commute_data$Total) + geom_histogram()
qplot(log(dist), log(Total), data=commute_data) + geom_smooth(method = lm)
