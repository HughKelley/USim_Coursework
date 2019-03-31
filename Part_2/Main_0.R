#Fetch a GeoJson of some district-level boundaries from the ONS Geoportal. First add the URL to an object

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

# Load all data
source('~/UCL_CASA_1819/USim/Coursework/USIM_Part2/Data_0.R', echo=TRUE)

# #We'll just use the first 7 boroughs by code, so first, create a vector of these 7 to match with our data
# toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
# #subset the data by the 7 sample boroughs
# #first the origins
# c_data_sub <- c_data[grep(paste(toMatch,collapse = "|"), c_data$OrigCode),]
# #then the destinations
# c_data_sub <- c_data_sub[grep(paste(toMatch,collapse = "|"), c_data$DestCode),]
# #now chop out the intra-borough flows


c_data_sub <- commute_data[commute_data$OrigCode!=commute_data$DestCode,]
# this returns 1056 observations, (33 *33)-33 since we removed self flows. 

#now unfortunately if you look at the file, for some reason the grep process has left a lot of empty data cells in the dataframe, 
# so let's just chop out everything after the 7*7 - 7 (42) pairs we are interested in...
# c_data_sub <- c_data_sub[1:42,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns *note that you have to be explicit 
# about the select function in the dplyr package as MASS also has a 'select' function and R will try and use this by default. 
# We can be explict by using the syntax package::function

c_data_sub <- dplyr::select(c_data_sub, OrigCodeNew, DestCodeNew, Total, everything())


# draws line between each borough that you can then add attributes to
travel_network <- stplanr::od2line(flow = c_data_sub, zones = LondonBNG)
#and set the line widths to some sensible value according to the flow
w <- c_data_sub$Total / max(c_data_sub$Total) * 10
#now plot it...
plot(travel_network, lwd = w)
plot(LondonBNG, add=T)

# # who cares about leaflet
# travel_networkwgs <- spTransform(travel_network, "+init=epsg:4326")
# leaflet() %>% addTiles() %>% addPolylines(data = travel_networkwgs)


#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
c_data_sub_matrix <- reshape2::dcast(c_data_sub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))

# get some info about the data by plotting
# but it's basically just to show that this doesn't work very well
source('~/UCL_CASA_1819/USim/Coursework/USIM_Part2/data_plotting_0.R', echo=TRUE)


# The model
###################################################################################
###################################################################################
#set up some variables to hold our parameter values in:
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(c_data_sub$Total)
###################################################################################
###################################################################################

vi1_mu <- c_data_sub$vi1_origpop^mu
wj2_alpha <- c_data_sub$wj2_destsal^alpha
dist_beta <- c_data_sub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
c_data_sub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
sum(c_data_sub$unconstrainedEst1)

#turn it into a little matrix and have a look at your handy work
c_data_sub_matrix_1 <- dcast(c_data_sub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))
c_data_sub_matrix_1


# Calc goodness of fit with helping functions
source('~/UCL_CASA_1819/USim/Coursework/USIM_Part2/helpers_0.R', echo=TRUE)

uncon_r_sq_prelim <-  CalcRSquared(c_data_sub$Total,c_data_sub$unconstrainedEst1)

uncon_rmse_prelim <- CalcRMSE(c_data_sub$Total,c_data_sub$unconstrainedEst1)

# to take a look at distribution shapes
# source('~/UCL_CASA_1819/USim/Coursework/USIM_Part2/distributions_0.R', echo=TRUE)

#run the unconstrained model
uncon_model <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = c_data_sub)

#first asign the parameter values from the model to the appropriate variables
k <- uncosim$coefficients[1]
mu <- uncosim$coefficients[2]
alpha <- uncosim$coefficients[3]
beta <- -uncosim$coefficients[4]

#now plug everything back into the Equation 6 model... (be careful with the positive and negative signing of the parameters as the beta parameter may not have been saved as negative so will need to force negative)
c_data_sub$unconstrainedEst2 <- exp(k+(mu*log(c_data_sub$vi1_origpop))+(alpha*log(c_data_sub$wj2_destsal))-(beta*log(c_data_sub$dist)))

#which is exactly the same as this...
c_data_sub$unconstrainedEst2 <- (exp(k)*exp(mu*log(c_data_sub$vi1_origpop))*exp(alpha*log(c_data_sub$wj2_destsal))*exp(-beta*log(c_data_sub$dist)))

#and of course, being R, there is an even easier way of doing this...
c_data_sub$fitted <- fitted(uncon_model)

#run the model and store all of the new flow estimates in a new column in the dataframe
c_data_sub$unconstrainedEst2 <- round(c_data_sub$unconstrainedEst2,0)
sum(c_data_sub$unconstrainedEst2)

#turn it into a little matrix and have a look at your handy work
c_data_sub_matrix_2 <- dcast(c_data_sub, Orig ~ Dest, sum, value.var = "unconstrainedEst2", margins=c("Orig", "Dest"))
c_data_sub_matrix_2

uncon_r_sq <- CalcRSquared(c_data_sub$Total,c_data_sub$unconstrainedEst2)

uncon_rmse <- CalcRMSE(c_data_sub$Total,c_data_sub$unconstrainedEst2)

print("first r2: ", uncon_r_sq_prelim)

print("second r2: ", uncon_r_sq)

print("first rmse: ", uncon_rmse_prelim)

print("first rmse: ", uncon_rmse)
