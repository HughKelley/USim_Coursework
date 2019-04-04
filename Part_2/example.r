#We'll just use the first 7 boroughs by code, so first, create a vector of these 7 to match with our data
toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
#subset the data by the 7 sample boroughs

cdata <- commute_data

#first the origins
cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
#then the destinations
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
#now chop out the intra-borough flows
cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,]
#now unfortunately if you look at the file, for some reason the grep process has left a lot of empty data cells in the dataframe, so let's just chop out everything after the 7*7 - 7 (42) pairs we are interested in...
cdatasub <- cdatasub[1:42,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns *note that you have to be explicit about the select function in the dplyr package as MASS also has a 'select' function and R will try and use this by default. We can be explict by using the syntax package::function
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())

cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))




# Parameters

# just guess
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

# and apply paramters to data

vi1_mu <- (cdatasub$vi1_origpop)^mu
wj2_alpha <- (cdatasub$wj2_destsal)^alpha
dist_beta <- (cdatasub$dist)^beta
T1 <- vi1_mu * wj2_alpha * dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
cdatasub$unconstrained_Est_1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
# sum(commute_data$unconstrained_Est_1)
# [1] 1800430
# sum(commute_data$TotalNoIntra)
# [1] 1800413

# Check Goodness of fit

est_1_r_2 <- CalcRSquared(cdatasub$Total, cdatasub$unconstrained_Est_1)

est_1_RMSE <- CalcRMSE(cdatasub$Total, cdatasub$unconstrained_Est_1)
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










