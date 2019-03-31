
#First plot the commuter flows against distance and then fit a model line with a ^-2 parameter
plot1 <- qplot(commute_data$dist, commute_data$Total)
#and now the model fit...
plot1 + stat_function(fun=function(x)x^-2, geom="line", aes(colour="^-2"))

plot2 <- qplot(commute_data$vi1_origpop, commute_data$Total)
plot2 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

plot3 <- qplot(commute_data$wj2_destsal, commute_data$Total)
plot3 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

