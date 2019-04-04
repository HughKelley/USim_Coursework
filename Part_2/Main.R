# library (what a mess)

library(geojsonio)
library(sp)
library(rgdal)
library(reshape2)
library(dplyr)
library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)
library(rgdal)

# helpers 

source('~/UCL_CASA_1819/USim/USim_Coursework/Part_2/helpers_0.R')

# data

source('~/UCL_CASA_1819/USim/USim_Coursework/Part_2/Data_0.R')

# this creates a commute_data_matrix dataframe with total flows between pairs of borough



  # income, population, distance matrix , flows
  # exclude intraborough flows


# Parameters

# just guess
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(commute_data$Total)

# and apply paramters to data

vi1_mu <- (commute_data$vi1_origpop)^mu
wj2_alpha <- (commute_data$wj2_destsal)^alpha
dist_beta <- (commute_data$dist)^beta
T1 <- vi1_mu * wj2_alpha * dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
commute_data$unconstrained_Est_1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
# sum(commute_data$unconstrained_Est_1)
# [1] 1800430
# sum(commute_data$TotalNoIntra)
# [1] 1800413

# Check Goodness of fit

est_1_r_2 <- CalcRSquared(commute_data$TotalNoIntra, commute_data$unconstrained_Est_1)

est_1_RMSE <- CalcRMSE(commute_data$TotalNoIntra, commute_data$unconstrained_Est_1)
cat("R2: ", est_1_r_2,'\n',"RMSE: ", est_1_RMSE, sep = "")



# Now do it again, actually calibrating the parameters and using a log linear model with a poisson regression

total_constrained_model <- glm(TotalNoIntra ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = commute_data)

# here log() is the "link", how the two sides of the equation relate to each other

# reset parameter values to the estimates from above


k <- total_constrained_model$coefficients[1]
mu <- total_constrained_model$coefficients[2]
alpha <- total_constrained_model$coefficients[3]
beta <- total_constrained_model$coefficients[4]

# then re-estimate 

# commute_data$unconstrained_Est_2 <- exp(k*exp(mu*log(commute_data$vi1_origpop))+(alpha*log(commute_data$wj2_destsal))-(beta*log(commute_data$dist)))

commute_data$fitted_2 <- fitted(total_constrained_model)

# and check fit

est_2_r_2 <- CalcRSquared(commute_data$TotalNoIntra, commute_data$fitted_2)

est_2_RMSE <- CalcRMSE(commute_data$TotalNoIntra, commute_data$fitted_2)
cat("R2: ", est_1_r_2,'\n',"RMSE: ", est_1_RMSE, sep = "")


 







